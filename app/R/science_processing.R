# ------------------------------------------------------------
# Ciencias Naturales — Procesamiento y agregaciones
#
# Los .xls exportados por la plataforma DIA (Resultados de estudiantes) traen:
# - Una fila por estudiante
# - Columnas numéricas por eje temático (p. ej. "Biología", "Física", "Química")
# - "NIVEL DE LOGRO" (Nivel I/II/III)
#
# Este módulo transforma el dataset normalizado por `ingest.R` + `platform_ingest.R`
# en tablas agregadas para gráficos (por grado, curso, eje, año y período).
# ------------------------------------------------------------

course_parts <- function(curso) {
  curso_chr <- as.character(curso %||% "")
  m <- regexec("^(\\d+)_([A-Za-z])$", curso_chr, perl = TRUE)
  hit <- regmatches(curso_chr, m)

  grade <- vapply(hit, function(x) if (length(x) >= 3) suppressWarnings(as.integer(x[[2]])) else NA_integer_, integer(1))
  section <- vapply(hit, function(x) if (length(x) >= 3) toupper(x[[3]]) else NA_character_, character(1))
  course_label <- ifelse(!is.na(grade) & !is.na(section), paste0(grade, "°", section), curso_chr)

  data.frame(
    grade = grade,
    section = section,
    course = course_label,
    stringsAsFactors = FALSE
  )
}

normalize_nivel <- function(x) {
  # Asegura etiquetas consistentes: "Nivel I/II/III".
  x <- tolower(trimws(as.character(x %||% "")))
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- gsub("\\s+", " ", x)
  x <- gsub("\\.", "", x, fixed = TRUE)

  ifelse(
    x %in% c("nivel i", "niveli"),
    "Nivel I",
    ifelse(
      x %in% c("nivel ii", "nivelii"),
      "Nivel II",
      ifelse(
        x %in% c("nivel iii", "niveliii"),
        "Nivel III",
        tools::toTitleCase(x)
      )
    )
  )
}

add_science_fields <- function(df) {
  parts <- course_parts(df$curso)
  df %>%
    dplyr::mutate(
      grade = parts$grade,
      section = parts$section,
      course = parts$course,
      nivel = normalize_nivel(.data$nivel_logro)
    )
}

summarize_levels_by_grade <- function(df) {
  df2 <- add_science_fields(df) %>%
    dplyr::filter(!is.na(.data$grade), !is.na(.data$nivel), nzchar(.data$nivel))

  # Por defecto agrupa por año, período y grado.
  df2 %>%
    dplyr::count(.data$year, .data$tipo, .data$grade, .data$nivel, name = "n") %>%
    dplyr::group_by(.data$year, .data$tipo, .data$grade) %>%
    dplyr::mutate(pct = if (sum(.data$n) > 0) (100 * .data$n / sum(.data$n)) else NA_real_) %>%
    dplyr::ungroup()
}

summarize_levels_by_course <- function(df) {
  df2 <- add_science_fields(df) %>%
    dplyr::filter(!is.na(.data$grade), !is.na(.data$nivel), nzchar(.data$nivel))

  df2 %>%
    dplyr::count(.data$year, .data$tipo, .data$course, .data$grade, .data$section, .data$nivel, name = "n") %>%
    dplyr::group_by(.data$year, .data$tipo, .data$course) %>%
    dplyr::mutate(pct = if (sum(.data$n) > 0) (100 * .data$n / sum(.data$n)) else NA_real_) %>%
    dplyr::ungroup()
}

summarize_axes_by_course <- function(df, include_synthetic = FALSE) {
  parts <- course_parts(df$curso)
  axes <- detect_axes(df)
  if (!isTRUE(include_synthetic)) {
    axes <- setdiff(axes, "Promedio (todos los ejes)")
  }
  if (length(axes) == 0) {
    return(data.frame())
  }

  df_long <- df %>%
    dplyr::mutate(
      grade = parts$grade,
      section = parts$section,
      course = parts$course
    ) %>%
    dplyr::select(dplyr::all_of(c("year", "tipo", "course", "grade", "section", axes))) %>%
    tidyr::pivot_longer(cols = dplyr::all_of(axes), names_to = "axis", values_to = "pct_logro") %>%
    dplyr::filter(!is.na(.data$pct_logro))

  df_long %>%
    dplyr::group_by(.data$year, .data$tipo, .data$course, .data$grade, .data$section, .data$axis) %>%
    dplyr::summarise(
      n = dplyr::n(),
      pct_logro = mean(.data$pct_logro, na.rm = TRUE),
      .groups = "drop"
    )
}

calc_yoy_diff <- function(df, year_a, year_b, value_col) {
  # Crea una tabla wide y calcula diferencia (year_b - year_a).
  year_a <- as.integer(year_a)
  year_b <- as.integer(year_b)

  df2 <- df %>%
    dplyr::filter(.data$year %in% c(year_a, year_b))

  if (nrow(df2) == 0) return(data.frame())

  wide <- df2 %>%
    tidyr::pivot_wider(
      names_from = "year",
      values_from = dplyr::all_of(value_col),
      values_fill = 0
    )

  a_col <- as.character(year_a)
  b_col <- as.character(year_b)
  if (!a_col %in% names(wide)) wide[[a_col]] <- 0
  if (!b_col %in% names(wide)) wide[[b_col]] <- 0

  wide %>%
    dplyr::mutate(
      diff = .data[[b_col]] - .data[[a_col]],
      direction = dplyr::case_when(
        is.na(.data$diff) ~ "sin_dato",
        .data$diff > 0 ~ "mejora",
        .data$diff < 0 ~ "baja",
        TRUE ~ "igual"
      )
    )
}

summarize_student_tracking <- function(df, axis = "Promedio (todos los ejes)") {
  # Devuelve una tabla larga para graficar trayectorias por estudiante.
  #
  # Key de estudiante: (year, course, n_lista). Esto permite comparar periodos
  # dentro del mismo curso/año (lo más consistente para reportes de plataforma).
  if (is.null(df) || nrow(df) == 0) return(data.frame())

  axes <- detect_axes(df)
  if (!axis %in% axes) {
    stop("Eje/medida inválida para trayectoria: ", axis, call. = FALSE)
  }

  df2 <- add_science_fields(df) %>%
    dplyr::filter(!is.na(.data$year), !is.na(.data$course), !is.na(.data$n_lista)) %>%
    dplyr::mutate(tipo = as.character(.data$tipo))

  df2 %>%
    dplyr::group_by(.data$year, .data$tipo, .data$course, .data$grade, .data$section, .data$n_lista, .data$nombre_estudiante) %>%
    dplyr::summarise(pct = mean(.data[[axis]], na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(!is.na(.data$pct))
}

tracking_2plus_filter <- function(df_tracking) {
  # Mantiene solo estudiantes con ≥2 periodos disponibles.
  if (is.null(df_tracking) || nrow(df_tracking) == 0) return(df_tracking)

  counts <- df_tracking %>%
    dplyr::distinct(.data$year, .data$course, .data$n_lista, .data$tipo) %>%
    dplyr::count(.data$year, .data$course, .data$n_lista, name = "n_tipos")

  df_tracking %>%
    dplyr::inner_join(counts, by = c("year", "course", "n_lista")) %>%
    dplyr::filter(.data$n_tipos >= 2) %>%
    dplyr::select(-"n_tipos")
}
