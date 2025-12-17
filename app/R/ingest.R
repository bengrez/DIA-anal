# ------------------------------------------------------------
# Ingesta y normalización de datos DIA
#
# Este módulo define el "contrato" interno de la app:
# - Detecta columnas requeridas (con tolerancia a variaciones de nombre)
# - Convierte porcentajes (coma decimal, strings, %)
# - Inserta metadata (año/tipo/curso/área/RBD) cuando viene desde el filename
# - Agrega un eje sintético "Promedio (todos los ejes)" para comparaciones multi-área
#
# Funciones clave:
# - read_dia_excel(): lee .xlsx (formato plantilla) y normaliza columnas
# - canonicalize_and_clean(): renombra, tipa y limpia un data.frame crudo
# - validate_dia_data(): valida el contrato interno (ver más abajo)
# - apply_anonymity(): anonimizador (n_lista / random estable)
# ------------------------------------------------------------

`%||%` <- function(x, y) if (is.null(x)) y else x

excel_sheets_safe <- function(path) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  tryCatch(readxl::excel_sheets(path), error = function(e) character())
}

normalize_name <- function(x) {
  x <- tolower(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- gsub("[^a-z0-9]+", "", x)
  x
}

default_tipo_order <- function(tipos) {
  tipos <- as.character(tipos %||% character())
  if (length(tipos) == 0) return(character())

  key <- normalize_name(tipos)
  rules <- list(
    diagnostico = function(k) grepl("diagnostico", k),
    intermedio = function(k) grepl("intermedio", k) | grepl("monitoreo", k),
    cierre = function(k) grepl("cierre", k) | grepl("evaluacion", k)
  )

  pick <- character()
  for (nm in names(rules)) {
    idx <- which(rules[[nm]](key))
    if (length(idx) > 0) pick <- c(pick, tipos[idx])
  }
  pick <- unique(pick)
  c(pick, setdiff(tipos, pick))
}

apply_factor_orders <- function(df, tipo_levels = NULL, nivel_levels = NULL) {
  df$tipo <- as.character(df$tipo)
  df$nivel_logro <- as.character(df$nivel_logro)

  all_tipos <- sort(unique(df$tipo))
  if (!is.null(tipo_levels)) {
    tipo_levels <- unique(as.character(tipo_levels))
    tipo_levels <- c(tipo_levels, setdiff(all_tipos, tipo_levels))
  } else {
    tipo_levels <- default_tipo_order(all_tipos)
  }

  all_niveles <- sort(unique(df$nivel_logro[!is.na(df$nivel_logro) & nzchar(df$nivel_logro)]))
  if (!is.null(nivel_levels)) {
    nivel_levels <- unique(as.character(nivel_levels))
    nivel_levels <- c(nivel_levels, setdiff(all_niveles, nivel_levels))
  } else {
    nivel_levels <- all_niveles
  }

  df$tipo <- factor(df$tipo, levels = tipo_levels)
  df$nivel_logro <- factor(df$nivel_logro, levels = nivel_levels)
  df
}

required_spec <- function() {
  list(
    year = c("year"),
    tipo = c("tipo"),
    curso = c("curso"),
    n_lista = c("nlista", "nlist", "numerodelista"),
    nombre_estudiante = c("nombredelestudiante", "nombreestudiante"),
    nivel_logro = c("niveldelogro")
  )
}

match_required_columns <- function(names_vec) {
  spec <- required_spec()
  norm <- normalize_name(names_vec)

  out <- list()
  for (key in names(spec)) {
    wanted <- spec[[key]]
    idx <- which(norm %in% wanted)
    if (length(idx) == 0) {
      out[[key]] <- NA_character_
    } else {
      out[[key]] <- names_vec[[idx[[1]]]]
    }
  }
  out
}

parse_numeric_percent <- function(x) {
  if (is.numeric(x)) {
    return(as.numeric(x))
  }
  if (inherits(x, "Date") || inherits(x, "POSIXt")) {
    return(NA_real_)
  }

  x <- as.character(x)
  x <- trimws(x)
  x[x %in% c("", "NA", "N/A", "na", "n/a", "Ausente", "ausente")] <- NA_character_

  x <- gsub("%", "", x, fixed = TRUE)
  x <- gsub("\\s+", "", x)
  x <- gsub(",", ".", x, fixed = TRUE)

  x <- gsub("[^0-9.\\-]+", "", x)
  suppressWarnings(as.numeric(x))
}

canonicalize_and_clean <- function(df, source_name, meta = NULL) {
  meta <- meta %||% list()
  mapping <- match_required_columns(names(df))

  missing_required <- names(mapping)[is.na(unlist(mapping))]
  allow_from_meta <- c("year", "tipo", "curso")
  allow_from_meta <- allow_from_meta[!is.null(allow_from_meta)]
  allow_ok <- vapply(
    allow_from_meta,
    function(k) !is.null(meta[[k]]) && !all(is.na(meta[[k]])) && nzchar(as.character(meta[[k]])[[1]]),
    logical(1)
  )
  missing_required <- setdiff(missing_required, allow_from_meta[allow_ok])

  if (length(missing_required) > 0) {
    pretty <- c(
      year = "Year",
      tipo = "Tipo",
      curso = "Curso",
      n_lista = "N_lista",
      nombre_estudiante = "Nombre del Estudiante",
      nivel_logro = "NIVEL DE LOGRO"
    )
    stop(
      "Faltan columnas requeridas: ",
      paste(pretty[missing_required], collapse = ", "),
      "\nColumnas encontradas: ",
      paste(names(df), collapse = ", "),
      call. = FALSE
    )
  }

  # Renombrar solo columnas presentes (evita fallar si Year/Tipo/Curso vienen desde metadata).
  for (new_name in names(mapping)) {
    old_name <- mapping[[new_name]]
    if (!is.na(old_name) && old_name %in% names(df) && !identical(old_name, new_name)) {
      names(df)[names(df) == old_name] <- new_name
    }
  }

  if (!"year" %in% names(df) && !is.null(meta$year)) df$year <- meta$year
  if (!"tipo" %in% names(df) && !is.null(meta$tipo)) df$tipo <- meta$tipo
  if (!"curso" %in% names(df) && !is.null(meta$curso)) df$curso <- meta$curso

  df <- df %>%
    mutate(
      fuente = as.character(source_name),
      source_file = as.character(meta$source_file %||% source_name),
      area = as.character(meta$area %||% "General"),
      rbd = as.character(meta$rbd %||% NA_character_),
      hc = as.character(meta$hc %||% NA_character_),
      year = suppressWarnings(as.numeric(.data$year)),
      tipo = as.character(.data$tipo),
      curso = as.character(.data$curso),
      n_lista = suppressWarnings(as.numeric(.data$n_lista)),
      nombre_estudiante = as.character(.data$nombre_estudiante),
      nivel_logro = as.character(.data$nivel_logro)
    )

  axis_cols <- detect_axes(df)
  if (length(axis_cols) > 0) {
    df <- df %>%
      mutate(across(all_of(axis_cols), parse_numeric_percent))
  }

  # Eje sintético para comparaciones entre áreas con ejes distintos.
  if (length(axis_cols) > 0) {
    col_name <- "Promedio (todos los ejes)"
    mat <- as.matrix(df[, axis_cols, drop = FALSE])
    df[[col_name]] <- rowMeans(mat, na.rm = TRUE)
    df[[col_name]][is.nan(df[[col_name]])] <- NA_real_
  }

  df
}

read_dia_excel <- function(path, sheet = 1, source_name = NULL, source_file = NULL) {
  if (is.null(source_name)) {
    source_name <- basename(path)
  }
  if (is.null(source_file)) {
    source_file <- basename(path)
  }

  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  df <- readxl::read_excel(path, sheet = sheet, .name_repair = "minimal")
  canonicalize_and_clean(df, source_name = source_name, meta = list(source_file = source_file))
}

required_internal_cols <- function() {
  c("year", "tipo", "curso", "n_lista", "nombre_estudiante", "nivel_logro", "fuente", "area", "rbd", "hc", "source_file")
}

detect_axes <- function(df) {
  setdiff(names(df), required_internal_cols())
}

validate_dia_data <- function(df) {
  missing <- setdiff(required_internal_cols(), names(df))
  if (length(missing) > 0) {
    stop("Datos inválidos: faltan columnas internas: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  if (all(is.na(df$n_lista))) {
    stop("Columna N_lista: no se pudo convertir a numérico.", call. = FALSE)
  }

  axis_cols <- detect_axes(df)
  if (length(axis_cols) == 0) {
    stop("No se detectaron ejes/ámbitos (columnas de porcentaje).", call. = FALSE)
  }

  invisible(TRUE)
}
