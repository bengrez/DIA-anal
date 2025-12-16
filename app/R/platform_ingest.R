re_capture <- function(pattern, x, ignore_case = FALSE) {
  m <- regexec(pattern, x, ignore.case = ignore_case, perl = TRUE)
  hit <- regmatches(x, m)[[1]]
  if (length(hit) >= 2) hit[[2]] else NA_character_
}

normalize_area <- function(area_raw) {
  if (is.na(area_raw) || !nzchar(area_raw)) return("General")
  key <- toupper(trimws(area_raw))
  if (identical(key, "MATEMATICA")) return("Matemática")
  if (identical(key, "LECTURA")) return("Lectura")
  tools::toTitleCase(tolower(key))
}

normalize_tipo_dia <- function(tipo_raw) {
  if (is.na(tipo_raw) || !nzchar(tipo_raw)) return(NA_character_)
  key <- tolower(tipo_raw)
  key <- iconv(key, from = "", to = "ASCII//TRANSLIT")
  key <- gsub("[^a-z]+", "", key)

  if (grepl("diagn", key)) return("Diagnóstico")
  if (grepl("monitoreo", key) || grepl("intermedio", key)) return("Monitoreo intermedio")
  if (grepl("cierre", key)) return("Evaluación de cierre")

  tools::toTitleCase(gsub("_", " ", tipo_raw, fixed = TRUE))
}

parse_dia_platform_filename <- function(path) {
  fname <- basename(path %||% "")
  no_ext <- sub("\\.[^.]+$", "", fname)

  rbd <- re_capture("RBD\\s*(\\d+)", no_ext, ignore_case = TRUE)
  area_raw <- re_capture("DIA_([^_]+)", no_ext, ignore_case = TRUE)
  curso <- re_capture("((?:\\d+|[IVX]+)_[A-Z])", no_ext, ignore_case = FALSE)
  hc <- re_capture("\\((HC-[0-9]+)\\)", no_ext, ignore_case = TRUE)
  year_chr <- re_capture("(20\\d{2})$", no_ext, ignore_case = FALSE)

  tipo_raw <- re_capture("Equipo_docente_(.+?)_(20\\d{2})$", no_ext, ignore_case = TRUE)
  if (is.na(tipo_raw) || !nzchar(tipo_raw)) {
    # Fallback: busca tokens conocidos en el nombre.
    tipo_raw <- re_capture("(Diagnostico|Monitoreo|Evaluacion_de_cierre|Cierre)", no_ext, ignore_case = TRUE)
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

read_dia_platform_xls <- function(path, dataset_name = NULL, skip = 12) {
  if (is.null(dataset_name) || !nzchar(dataset_name)) {
    dataset_name <- basename(dirname(path))
  }

  meta <- parse_dia_platform_filename(path)
  if (is.na(meta$curso) || !nzchar(meta$curso)) {
    stop("No se pudo detectar el Curso desde el nombre del archivo: ", basename(path), call. = FALSE)
  }
  if (is.na(meta$tipo) || !nzchar(meta$tipo)) {
    stop("No se pudo detectar el Tipo DIA desde el nombre del archivo: ", basename(path), call. = FALSE)
  }
  if (is.na(meta$year)) {
    stop("No se pudo detectar el Año desde el nombre del archivo: ", basename(path), call. = FALSE)
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
      source_file = meta$source_file
    )
  )
}

load_dia_platform_folder <- function(folder_path, recursive = FALSE, dataset_name = NULL) {
  folder_path <- normalizePath(folder_path, winslash = "/", mustWork = TRUE)
  if (is.null(dataset_name) || !nzchar(dataset_name)) {
    dataset_name <- paste0("Carpeta: ", basename(folder_path))
  }

  files <- list.files(folder_path, pattern = "\\.xls$", full.names = TRUE, recursive = isTRUE(recursive))
  files <- sort(files)
  if (length(files) == 0) {
    stop("No se encontraron archivos .xls en: ", folder_path, call. = FALSE)
  }

  manifest <- lapply(files, function(f) {
    meta <- parse_dia_platform_filename(f)
    data.frame(
      file = basename(f),
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
    f <- files[[i]]
    dfs[[i]] <- tryCatch(
      read_dia_platform_xls(f, dataset_name = dataset_name, skip = 12),
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
    folder = folder_path,
    dataset_name = dataset_name,
    data = df_all,
    manifest = manifest
  )
}

