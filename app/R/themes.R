available_palettes <- function() {
  c(
    "Okabe-Ito",
    "HCL - Dark 3",
    "HCL - Set 2",
    "HCL - Viridis",
    "HCL - Inferno",
    "HCL - Plasma",
    "HCL - Turbo"
  )
}

palette_values <- function(n, palette_name) {
  if (n <= 0) return(character())
  if (identical(palette_name, "Okabe-Ito")) {
    base <- c(
      "#E69F00",
      "#56B4E9",
      "#009E73",
      "#F0E442",
      "#0072B2",
      "#D55E00",
      "#CC79A7",
      "#000000"
    )
    return(rep(base, length.out = n))
  }

  hcl_name <- sub("^HCL - ", "", palette_name)
  grDevices::hcl.colors(n, palette = hcl_name)
}

get_ui_theme <- function(preset) {
  preset <- preset %||% "classic"
  if (identical(preset, "dark")) {
    return(bslib::bs_theme(version = 5, bootswatch = "darkly"))
  }
  if (identical(preset, "minimal")) {
    return(bslib::bs_theme(version = 5, bootswatch = "minty"))
  }
  if (identical(preset, "contrast")) {
    return(bslib::bs_theme(version = 5, bootswatch = "simplex"))
  }
  bslib::bs_theme(version = 5, bootswatch = "flatly")
}

get_plot_theme <- function(preset) {
  preset <- preset %||% "classic"
  if (identical(preset, "dark")) {
    return(ggplot2::theme_dark(base_size = 12))
  }
  if (identical(preset, "minimal")) {
    return(ggplot2::theme_minimal(base_size = 12))
  }
  if (identical(preset, "contrast")) {
    return(
      ggplot2::theme_bw(base_size = 13) +
        ggplot2::theme(
          panel.grid = ggplot2::element_blank(),
          axis.text = ggplot2::element_text(color = "black"),
          axis.title = ggplot2::element_text(color = "black")
        )
    )
  }
  ggplot2::theme_classic(base_size = 12)
}

nz_or_default <- function(x, default) {
  if (is.null(x)) return(default)
  x <- as.character(x)
  if (!nzchar(x)) return(default)
  x
}

apply_labels <- function(p, labels) {
  p +
    ggplot2::labs(
      title = labels$title %||% NULL,
      subtitle = labels$subtitle %||% NULL,
      x = labels$x %||% NULL,
      y = labels$y %||% NULL
    )
}

default_title <- function(plot_type, eje = NULL, tipo_a = NULL, tipo_b = NULL) {
  if (identical(plot_type, "promedio")) return(paste("Promedio por curso y tipo —", eje))
  if (identical(plot_type, "distribucion")) return(paste("Distribución —", eje))
  if (identical(plot_type, "heatmap")) return(paste("Heatmap curso/tipo —", eje))
  if (identical(plot_type, "violin")) return(paste("Distribución (violín/ridge) —", eje))
  if (identical(plot_type, "tendencia")) return(paste("Tendencia temporal —", eje))
  if (identical(plot_type, "nivel_logro")) return("Nivel de logro (proporción)")
  if (identical(plot_type, "crecimiento")) return(paste("Crecimiento por estudiante —", eje, "(", tipo_a, "→", tipo_b, ")"))
  "Gráfico"
}

default_subtitle <- function(cursos, tipos) {
  cursos <- unique(as.character(cursos))
  tipos <- unique(as.character(tipos))
  parts <- c()
  if (length(cursos) > 0) parts <- c(parts, paste("Cursos:", paste(cursos, collapse = ", ")))
  if (length(tipos) > 0) parts <- c(parts, paste("Tipos:", paste(tipos, collapse = ", ")))
  paste(parts, collapse = " — ")
}

export_dims_px <- function(res_key) {
  if (identical(res_key, "low")) return(list(width = 1200, height = 800))
  if (identical(res_key, "high")) return(list(width = 3200, height = 2100))
  list(width = 2000, height = 1300)
}

sanitize_filename_part <- function(x) {
  x <- as.character(x %||% "")
  x <- trimws(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- gsub("[^A-Za-z0-9._-]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  if (!nzchar(x)) "NA" else x
}

make_export_filename <- function(plot_type, cursos, eje, tipo_a = NULL, tipo_b = NULL) {
  plot_key <- sanitize_filename_part(plot_type)
  eje_key <- sanitize_filename_part(eje)
  if (identical(plot_type, "nivel_logro")) {
    eje_key <- "NivelLogro"
  }

  cursos <- unique(as.character(cursos))
  curso_key <- if (length(cursos) == 1) sanitize_filename_part(cursos[[1]]) else "VariosCursos"

  extra <- ""
  if (identical(plot_type, "crecimiento")) {
    extra <- paste0("_", sanitize_filename_part(tipo_a), "_a_", sanitize_filename_part(tipo_b))
  }

  paste0("DIA_", plot_key, "_", curso_key, "_", eje_key, extra, ".png")
}

make_export_filename_v2 <- function(plot_type, cursos, years = NULL, eje = NULL, tipo_a = NULL, tipo_b = NULL) {
  plot_key <- sanitize_filename_part(plot_type)

  cursos <- unique(as.character(cursos %||% character()))
  curso_key <- if (length(cursos) == 1) sanitize_filename_part(cursos[[1]]) else "VariosCursos"

  years <- unique(as.character(years %||% character()))
  year_key <- if (length(years) == 1) sanitize_filename_part(years[[1]]) else "VariosAnios"

  eje_key <- sanitize_filename_part(eje %||% "")
  if (identical(plot_type, "nivel_logro")) {
    eje_key <- "NivelLogro"
  }
  if (!nzchar(eje_key)) eje_key <- "NA"

  extra <- ""
  if (identical(plot_type, "crecimiento")) {
    extra <- paste0("_", sanitize_filename_part(tipo_a), "_a_", sanitize_filename_part(tipo_b))
  }

  paste0("DIA_", plot_key, "_", curso_key, "_", year_key, "_", eje_key, extra, ".png")
}

save_png_ggsave <- function(plot, path, width_px, height_px, style_preset) {
  dpi <- 300
  bg <- if (identical(style_preset, "dark")) "#111111" else "white"

  ggplot2::ggsave(
    filename = path,
    plot = plot,
    width = width_px / dpi,
    height = height_px / dpi,
    units = "in",
    dpi = dpi,
    bg = bg
  )
}
