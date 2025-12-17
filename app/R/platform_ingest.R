# ------------------------------------------------------------
# Importación desde plataforma DIA (.xls)
#
# Los reportes descargados desde la plataforma suelen venir como `.xls` y con
# filas informativas al inicio. La app asume:
# - Primera hoja
# - 12 filas iniciales irrelevantes; la tabla comienza en la fila 13
#
# Este módulo:
# - Infieren metadata desde el nombre del archivo (RBD, Área, Curso, Tipo DIA, Año)
# - Lee el `.xls` y lo transforma al contrato interno via `canonicalize_and_clean()`
# - Soporta carga masiva vía `fileInput()` (con `datapath` temporal + `name` real)
# ------------------------------------------------------------

re_capture <- function(pattern, x, ignore_case = FALSE) {
  m <- regexec(pattern, x, ignore.case = ignore_case, perl = TRUE)
  hit <- regmatches(x, m)[[1]]
  if (length(hit) >= 2) hit[[2]] else NA_character_
}

normalize_area <- function(area_raw) {
  if (is.na(area_raw) || !nzchar(area_raw)) return("General")
  key <- toupper(trimws(area_raw))
  # Unificar separadores: la plataforma puede usar espacios o underscores.
  key <- gsub("[_]+", " ", key)
  key <- gsub("\\s+", " ", key)
  if (identical(key, "MATEMATICA")) return("Matemática")
  if (identical(key, "LECTURA")) return("Lectura")
  if (identical(key, "CIENCIAS NATURALES")) return("Ciencias Naturales")
  tools::toTitleCase(tolower(key))
}

normalize_tipo_dia <- function(tipo_raw) {
  if (is.na(tipo_raw) || !nzchar(tipo_raw)) return(NA_character_)
  key <- tolower(tipo_raw)
  key <- iconv(key, from = "", to = "ASCII//TRANSLIT")
  key <- gsub("[^a-z]+", "", key)

  # Mantener un set pequeño y estable para filtros:
  # - Diagnóstico
  # - Monitoreo
  # - Cierre
  # - Evaluacion_Cierre (compatibilidad con archivos antiguos)
  if (grepl("diagn", key)) return("Diagnóstico")
  if (grepl("monitoreo", key) || grepl("intermedio", key)) return("Monitoreo")
  if (grepl("evaluacioncierre", key)) return("Evaluacion_Cierre")
  if (grepl("cierre", key)) return("Cierre")

  tools::toTitleCase(gsub("_", " ", tipo_raw, fixed = TRUE))
}

parse_dia_platform_filename <- function(path) {
  fname <- basename(path %||% "")
  no_ext <- sub("\\.[^.]+$", "", fname)

  rbd <- re_capture("RBD\\s*(\\d+)", no_ext, ignore_case = TRUE)

  # Extrae el bloque entre "_DIA_" y "_Resultados_de_estudiantes" para
  # identificar correctamente Área y Curso (evitando capturar "12775_D").
  dia_block <- re_capture("_DIA_(.+?)_Resultados_de_estudiantes", no_ext, ignore_case = TRUE)
  course_pattern <- "(?:\\d+|[IVX]+)_[A-Za-z]"
  course_match <- if (!is.na(dia_block) && nzchar(dia_block)) {
    m <- regexpr(course_pattern, dia_block, perl = TRUE)
    if (m[[1]] > 0) regmatches(dia_block, m) else NA_character_
  } else {
    NA_character_
  }

  curso <- if (!is.na(course_match) && nzchar(course_match)) toupper(course_match) else NA_character_

  area_raw <- if (!is.na(dia_block) && nzchar(dia_block) && !is.na(curso) && nzchar(curso)) {
    idx <- regexpr(course_pattern, dia_block, perl = TRUE)[[1]]
    area_part <- if (idx > 1) substring(dia_block, 1, idx - 1) else ""
    area_part <- sub("_+$", "", area_part)
    trimws(area_part)
  } else {
    re_capture("DIA_([^_]+)", no_ext, ignore_case = TRUE)
  }

  hc <- re_capture("\\((HC-[0-9]+)\\)", no_ext, ignore_case = TRUE)
  year_chr <- re_capture("(20\\d{2})$", no_ext, ignore_case = FALSE)

  tipo_raw <- re_capture("Equipo_docente_(.+?)_(20\\d{2})$", no_ext, ignore_case = TRUE)
  if (is.na(tipo_raw) || !nzchar(tipo_raw)) {
    # Fallback: busca tokens conocidos en el nombre.
    tipo_raw <- re_capture("(Diagnostico|Monitoreo|Evaluacion[_ ]?Cierre|Evaluacion_de_cierre|Cierre)", no_ext, ignore_case = TRUE)
  }

  list(
    source_file = fname,
    rbd = rbd,
    area_raw = area_raw,
    area = normalize_area(area_raw),
    curso = curso,
    hc = hc,
    tipo_raw = tipo_raw,
    tipo = normalize_tipo_dia(tipo_raw),
    year = suppressWarnings(as.integer(year_chr))
  )
}

read_dia_platform_xls <- function(path, dataset_name = NULL, skip = 12, original_name = NULL) {
  if (is.null(dataset_name) || !nzchar(dataset_name)) {
    dataset_name <- basename(dirname(path))
  }

  meta <- parse_dia_platform_filename(original_name %||% path)
  if (is.na(meta$curso) || !nzchar(meta$curso)) {
    stop("No se pudo detectar el Curso desde el nombre del archivo: ", basename(original_name %||% path), call. = FALSE)
  }
  if (is.na(meta$tipo) || !nzchar(meta$tipo)) {
    stop("No se pudo detectar el Tipo DIA desde el nombre del archivo: ", basename(original_name %||% path), call. = FALSE)
  }
  if (is.na(meta$year)) {
    stop("No se pudo detectar el Año desde el nombre del archivo: ", basename(original_name %||% path), call. = FALSE)
  }

  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  df <- readxl::read_excel(path, skip = skip, .name_repair = "minimal")

  canonicalize_and_clean(
    df,
    source_name = dataset_name,
    meta = list(
      year = meta$year,
      tipo = meta$tipo,
      curso = meta$curso,
      area = meta$area,
      rbd = meta$rbd,
      hc = meta$hc,
      source_file = basename(original_name %||% meta$source_file)
    )
  )
}

load_dia_platform_upload <- function(upload_df, dataset_name = NULL, skip = 12) {
  if (is.null(upload_df) || nrow(upload_df) == 0) {
    stop("No se seleccionaron archivos .xls.", call. = FALSE)
  }

  keep <- grepl("\\.xls$", upload_df$name, ignore.case = TRUE)
  upload_df <- upload_df[keep, , drop = FALSE]
  if (nrow(upload_df) == 0) {
    stop("No se encontraron archivos .xls en la selección.", call. = FALSE)
  }

  files <- as.character(upload_df$datapath)
  names <- as.character(upload_df$name)

  if (is.null(dataset_name) || !nzchar(dataset_name)) {
    folder_hint <- dirname(names[[1]] %||% "")
    if (nzchar(folder_hint) && !identical(folder_hint, ".")) {
      dataset_name <- paste0("Carpeta: ", basename(folder_hint))
    } else {
      dataset_name <- "Carpeta seleccionada"
    }
  }

  manifest <- lapply(seq_along(files), function(i) {
    meta <- parse_dia_platform_filename(names[[i]])
    data.frame(
      file = basename(names[[i]]),
      area = meta$area,
      curso = meta$curso,
      tipo = meta$tipo,
      year = meta$year,
      rbd = meta$rbd,
      hc = meta$hc,
      stringsAsFactors = FALSE
    )
  })
  manifest <- do.call(rbind, manifest)

  dfs <- vector("list", length(files))
  status <- rep("ok", length(files))
  message <- rep("", length(files))

  for (i in seq_along(files)) {
    dfs[[i]] <- tryCatch(
      read_dia_platform_xls(files[[i]], dataset_name = dataset_name, skip = skip, original_name = names[[i]]),
      error = function(e) {
        status[[i]] <<- "error"
        message[[i]] <<- e$message
        NULL
      }
    )
  }

  manifest$status <- status
  manifest$message <- message

  ok <- vapply(dfs, function(x) !is.null(x) && nrow(x) > 0, logical(1))
  df_all <- dplyr::bind_rows(dfs[ok])

  list(
    folder = NA_character_,
    dataset_name = dataset_name,
    data = df_all,
    manifest = manifest
  )
}
