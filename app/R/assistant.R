assistant_plot_label <- function(plot_type) {
  switch(
    plot_type,
    promedio = "A) Promedio por curso y Tipo",
    distribucion = "B) Distribución (boxplot / histograma)",
    nivel_logro = "C) Nivel de logro (proporción)",
    crecimiento = "D) Crecimiento por estudiante (delta)",
    plot_type
  )
}

assistant_plot_why <- function(plot_type) {
  switch(
    plot_type,
    promedio = "Ideal cuando necesitas un resumen claro (promedios) por curso y tipo.",
    distribucion = "Útil cuando quieres ver variabilidad: dispersión, asimetrías y outliers.",
    nivel_logro = "Diseñado para reportar proporciones de niveles de logro por curso/tipo.",
    crecimiento = "Permite identificar quién mejora o baja entre dos mediciones (Tipos).",
    ""
  )
}

assistant_plot_what <- function(plot_type) {
  switch(
    plot_type,
    promedio = "Barras agrupadas: Curso en X y fill por Tipo (elige uno o varios ejes).",
    distribucion = "Boxplot u histograma (con curva de densidad) por curso/tipo (elige ejes).",
    nivel_logro = "Barras apiladas al 100% por NIVEL DE LOGRO (por curso y tipo).",
    crecimiento = "Delta por estudiante (o slope chart) comparando Tipo A vs Tipo B.",
    ""
  )
}

assistant_settings_label <- function(settings) {
  settings <- settings %||% list()
  parts <- character()

  facet_row <- settings$facet_row %||% "off"
  facet_col <- settings$facet_col %||% "off"
  if (!identical(facet_row, "off") || !identical(facet_col, "off")) {
    facet_row_lbl <- switch(
      facet_row,
      curso = "Curso",
      tipo = "Tipo",
      year = "Año",
      eje = "Eje",
      if (!identical(facet_row, "off")) facet_row else NULL
    )
    facet_col_lbl <- switch(
      facet_col,
      curso = "Curso",
      tipo = "Tipo",
      year = "Año",
      eje = "Eje",
      if (!identical(facet_col, "off")) facet_col else NULL
    )

    facet_txt <- if (!identical(facet_row, "off") && !identical(facet_col, "off")) {
      paste0("Facets: ", facet_row_lbl, " ~ ", facet_col_lbl)
    } else {
      paste0("Facets: ", facet_row_lbl %||% facet_col_lbl)
    }

    parts <- c(parts, facet_txt)
  }

  if (!is.null(settings$dist_kind)) {
    parts <- c(parts, paste0("Distribución: ", if (identical(settings$dist_kind, "hist")) "Histograma" else "Boxplot"))
  }

  if (!is.null(settings$growth_kind)) {
    parts <- c(parts, paste0("Crecimiento: ", if (identical(settings$growth_kind, "slope")) "Slope" else "Delta (barras)"))
  }

  if (!is.null(settings$rank_mode) && !identical(settings$rank_mode, "all")) {
    parts <- c(parts, "Filtro: Top + Bottom")
  }

  if (length(parts) == 0) {
    return("Sin ajustes extra.")
  }
  paste(parts, collapse = " · ")
}

assistant_recommendation <- function(
    need_level,
    need_growth,
    need_dist,
    want_facets = NULL,
    facet_year = NULL,
    facet_curso = NULL,
    facet_tipo = NULL,
    facet_eje = NULL,
    dist_hist = NULL,
    growth_delta = NULL,
    growth_topbottom = NULL
) {
  # Inputs esperados: "yes", "no" o NULL (sin responder)
  if (is.null(need_level) || !need_level %in% c("yes", "no")) {
    return(list(complete = FALSE, prompt = "Responde la pregunta 1 para empezar."))
  }

  if (identical(need_level, "yes")) {
    plot_type <- "nivel_logro"
  } else {
    if (is.null(need_growth) || !need_growth %in% c("yes", "no")) {
      return(list(complete = FALSE, prompt = "Responde la pregunta 2 para continuar."))
    }

    if (identical(need_growth, "yes")) {
      plot_type <- "crecimiento"
    } else {
      if (is.null(need_dist) || !need_dist %in% c("yes", "no")) {
        return(list(complete = FALSE, prompt = "Responde la pregunta 3 para finalizar la recomendación."))
      }
      plot_type <- if (identical(need_dist, "yes")) "distribucion" else "promedio"
    }
  }

  settings <- list(facet_row = "off", facet_col = "off")

  if (identical(want_facets, "yes")) {
    if (identical(facet_year, "yes")) {
      settings$facet_row <- "year"
    } else if (identical(facet_curso, "yes")) {
      settings$facet_row <- "curso"
    } else if (identical(facet_tipo, "yes")) {
      settings$facet_row <- "tipo"
    } else if (identical(facet_eje, "yes")) {
      settings$facet_row <- "eje"
    }
  }

  if (identical(plot_type, "distribucion")) {
    settings$dist_kind <- if (identical(dist_hist, "yes")) "hist" else "box"
  }

  if (identical(plot_type, "crecimiento")) {
    settings$growth_kind <- if (identical(growth_delta, "no")) "slope" else "delta"
    settings$rank_mode <- if (identical(growth_topbottom, "yes")) "both" else "all"
  }

  list(
    complete = TRUE,
    plot_type = plot_type,
    label = assistant_plot_label(plot_type),
    why = assistant_plot_why(plot_type),
    what_you_get = assistant_plot_what(plot_type),
    settings = settings,
    settings_label = assistant_settings_label(settings)
  )
}
