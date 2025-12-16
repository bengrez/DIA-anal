preset_capture <- function(input) {
  list(
    version = 2,
    plot_type = input$plot_type %||% "promedio",
    facet_row = input$facet_row %||% "off",
    facet_col = input$facet_col %||% "off",
    curso = input$curso %||% NULL,
    year = input$year %||% NULL,
    tipo = input$tipo %||% NULL,
    fuente = input$fuente %||% NULL,
    eje = input$eje %||% NULL,
    tipo_a = input$tipo_a %||% NULL,
    tipo_b = input$tipo_b %||% NULL,
    dist_kind = input$dist_kind %||% NULL,
    heatmap_dim = input$heatmap_dim %||% NULL,
    violin_kind = input$violin_kind %||% NULL,
    violin_group = input$violin_group %||% NULL,
    trend_group = input$trend_group %||% NULL,
    growth_kind = input$growth_kind %||% NULL,
    rank_mode = input$rank_mode %||% NULL,
    rank_n = input$rank_n %||% NULL,
    style_preset = input$style_preset %||% "classic",
    palette_fill = input$palette_fill %||% "Okabe-Ito",
    palette_color = input$palette_color %||% "Okabe-Ito",
    title = input$title %||% "",
    subtitle = input$subtitle %||% "",
    xlab = input$xlab %||% "",
    ylab = input$ylab %||% "",
    export_res = input$export_res %||% "med",
    alpha_bars = input$alpha_bars %||% 0.85,
    alpha_lines = input$alpha_lines %||% 0.9,
    tipo_order = input$tipo_order %||% NULL,
    nivel_order = input$nivel_order %||% NULL,
    anon = input$anon %||% FALSE,
    anon_method = input$anon_method %||% "n_lista",
    anon_seed = input$anon_seed %||% 1234
  )
}

preset_defaults <- function(ch) {
  ch <- ch %||% list()
  tipos <- ch$tipos %||% character()
  niveles <- ch$niveles %||% character()
  cursos <- ch$cursos %||% character()
  years <- ch$years %||% character()
  ejes <- ch$ejes %||% character()
  fuentes <- ch$fuentes %||% character()

  list(
    version = 2,
    plot_type = "promedio",
    facet_row = "off",
    facet_col = "off",
    curso = cursos,
    year = years,
    tipo = tipos,
    fuente = fuentes,
    eje = if (length(ejes) > 0) ejes[[1]] else NULL,
    tipo_a = if (length(tipos) > 0) tipos[[1]] else NULL,
    tipo_b = if (length(tipos) > 1) tipos[[2]] else if (length(tipos) > 0) tipos[[1]] else NULL,
    dist_kind = "box",
    heatmap_dim = "curso",
    violin_kind = "violin",
    violin_group = "curso",
    trend_group = "curso",
    growth_kind = "delta",
    rank_mode = "all",
    rank_n = 10,
    style_preset = "classic",
    palette_fill = "Okabe-Ito",
    palette_color = "Okabe-Ito",
    title = "",
    subtitle = "",
    xlab = "",
    ylab = "",
    export_res = "med",
    alpha_bars = 0.85,
    alpha_lines = 0.9,
    tipo_order = default_tipo_order(tipos),
    nivel_order = niveles,
    anon = TRUE,
    anon_method = "n_lista",
    anon_seed = 1234
  )
}

preset_is_applied <- function(input, preset) {
  if (is.null(preset) || !is.list(preset)) return(TRUE)

  eq <- function(a, b) identical(a %||% NULL, b %||% NULL)

  # Siempre presentes
  if (!eq(input$plot_type, preset$plot_type)) return(FALSE)
  if (!eq(input$facet_row, preset$facet_row %||% preset$facet %||% "off")) return(FALSE)
  if (!eq(input$facet_col, preset$facet_col %||% "off")) return(FALSE)
  if (!eq(input$style_preset, preset$style_preset)) return(FALSE)
  if (!eq(input$palette_fill, preset$palette_fill)) return(FALSE)
  if (!eq(input$palette_color, preset$palette_color)) return(FALSE)
  if (!eq(input$title, preset$title)) return(FALSE)
  if (!eq(input$subtitle, preset$subtitle)) return(FALSE)
  if (!eq(input$xlab, preset$xlab)) return(FALSE)
  if (!eq(input$ylab, preset$ylab)) return(FALSE)
  if (!eq(input$export_res, preset$export_res)) return(FALSE)
  if (!eq(input$alpha_bars, preset$alpha_bars)) return(FALSE)
  if (!eq(input$alpha_lines, preset$alpha_lines)) return(FALSE)
  if (!eq(isTRUE(input$anon), isTRUE(preset$anon))) return(FALSE)

  # Estos inputs podrían no existir según UI/condiciones
  if (!is.null(input$anon_method) && !eq(input$anon_method, preset$anon_method)) return(FALSE)
  if (!is.null(input$anon_seed) && !eq(input$anon_seed, preset$anon_seed)) return(FALSE)

  if (!is.null(input$fuente) && !is.null(preset$fuente) && !eq(input$fuente, preset$fuente)) return(FALSE)
  if (!is.null(input$curso) && !is.null(preset$curso) && !eq(input$curso, preset$curso)) return(FALSE)
  if (!is.null(input$year) && !is.null(preset$year) && !eq(input$year, preset$year)) return(FALSE)

  if (!is.null(input$tipo) && !is.null(preset$tipo) && !eq(input$tipo, preset$tipo)) return(FALSE)
  if (!is.null(input$eje) && !is.null(preset$eje) && !eq(input$eje, preset$eje)) return(FALSE)

  if (!is.null(input$tipo_order) && !is.null(preset$tipo_order) && !eq(input$tipo_order, preset$tipo_order)) return(FALSE)
  if (!is.null(input$nivel_order) && !is.null(preset$nivel_order) && !eq(input$nivel_order, preset$nivel_order)) return(FALSE)

  if (!is.null(input$dist_kind) && !is.null(preset$dist_kind) && !eq(input$dist_kind, preset$dist_kind)) return(FALSE)
  if (!is.null(input$heatmap_dim) && !is.null(preset$heatmap_dim) && !eq(input$heatmap_dim, preset$heatmap_dim)) return(FALSE)
  if (!is.null(input$violin_kind) && !is.null(preset$violin_kind) && !eq(input$violin_kind, preset$violin_kind)) return(FALSE)
  if (!is.null(input$violin_group) && !is.null(preset$violin_group) && !eq(input$violin_group, preset$violin_group)) return(FALSE)
  if (!is.null(input$trend_group) && !is.null(preset$trend_group) && !eq(input$trend_group, preset$trend_group)) return(FALSE)
  if (!is.null(input$tipo_a) && !is.null(preset$tipo_a) && !eq(input$tipo_a, preset$tipo_a)) return(FALSE)
  if (!is.null(input$tipo_b) && !is.null(preset$tipo_b) && !eq(input$tipo_b, preset$tipo_b)) return(FALSE)
  if (!is.null(input$growth_kind) && !is.null(preset$growth_kind) && !eq(input$growth_kind, preset$growth_kind)) return(FALSE)
  if (!is.null(input$rank_mode) && !is.null(preset$rank_mode) && !eq(input$rank_mode, preset$rank_mode)) return(FALSE)
  if (!is.null(input$rank_n) && !is.null(preset$rank_n) && !eq(input$rank_n, preset$rank_n)) return(FALSE)

  TRUE
}

preset_apply_step <- function(session, input, preset) {
  # Devuelve TRUE cuando "ya intentó aplicar todo lo posible".
  if (is.null(preset) || !is.list(preset)) return(TRUE)

  if (!is.null(preset$plot_type) && !identical(input$plot_type, preset$plot_type)) {
    updateSelectInput(session, "plot_type", selected = preset$plot_type)
  }

  if (!is.null(preset$anon) && !identical(isTRUE(input$anon), isTRUE(preset$anon))) {
    updateCheckboxInput(session, "anon", value = isTRUE(preset$anon))
  }
  if (!is.null(preset$anon_method) && !is.null(input$anon_method) && !identical(input$anon_method, preset$anon_method)) {
    updateSelectInput(session, "anon_method", selected = preset$anon_method)
  }
  if (!is.null(preset$anon_seed) && !is.null(input$anon_seed) && !identical(input$anon_seed, preset$anon_seed)) {
    updateNumericInput(session, "anon_seed", value = preset$anon_seed)
  }

  # Filtros (si existen)
  if (!is.null(preset$fuente) && !is.null(input$fuente) && !identical(input$fuente, preset$fuente)) {
    updateSelectInput(session, "fuente", selected = preset$fuente)
  }
  if (!is.null(preset$curso) && !is.null(input$curso) && !identical(input$curso, preset$curso)) {
    updateSelectInput(session, "curso", selected = preset$curso)
  }
  if (!is.null(preset$year) && !is.null(input$year) && !identical(input$year, preset$year)) {
    updateSelectInput(session, "year", selected = preset$year)
  }
  if (!is.null(preset$tipo) && !is.null(input$tipo) && !identical(input$tipo, preset$tipo)) {
    updateSelectInput(session, "tipo", selected = preset$tipo)
  }
  if (!is.null(preset$eje) && !is.null(input$eje) && !identical(input$eje, preset$eje)) {
    updateSelectInput(session, "eje", selected = preset$eje)
  }

  # Orden de factores (selectize)
  if (!is.null(preset$tipo_order) && !is.null(input$tipo_order) && !identical(input$tipo_order, preset$tipo_order)) {
    updateSelectizeInput(session, "tipo_order", selected = preset$tipo_order, server = TRUE)
  }
  if (!is.null(preset$nivel_order) && !is.null(input$nivel_order) && !identical(input$nivel_order, preset$nivel_order)) {
    updateSelectizeInput(session, "nivel_order", selected = preset$nivel_order, server = TRUE)
  }

  # Opciones por tipo de gráfico (condicionales)
  if (!is.null(preset$dist_kind) && !is.null(input$dist_kind) && !identical(input$dist_kind, preset$dist_kind)) {
    updateRadioButtons(session, "dist_kind", selected = preset$dist_kind)
  }
  if (!is.null(preset$heatmap_dim) && !is.null(input$heatmap_dim) && !identical(input$heatmap_dim, preset$heatmap_dim)) {
    updateSelectInput(session, "heatmap_dim", selected = preset$heatmap_dim)
  }
  if (!is.null(preset$violin_kind) && !is.null(input$violin_kind) && !identical(input$violin_kind, preset$violin_kind)) {
    updateRadioButtons(session, "violin_kind", selected = preset$violin_kind)
  }
  if (!is.null(preset$violin_group) && !is.null(input$violin_group) && !identical(input$violin_group, preset$violin_group)) {
    updateSelectInput(session, "violin_group", selected = preset$violin_group)
  }
  if (!is.null(preset$trend_group) && !is.null(input$trend_group) && !identical(input$trend_group, preset$trend_group)) {
    updateSelectInput(session, "trend_group", selected = preset$trend_group)
  }
  if (!is.null(preset$tipo_a) && !is.null(input$tipo_a) && !identical(input$tipo_a, preset$tipo_a)) {
    updateSelectInput(session, "tipo_a", selected = preset$tipo_a)
  }
  if (!is.null(preset$tipo_b) && !is.null(input$tipo_b) && !identical(input$tipo_b, preset$tipo_b)) {
    updateSelectInput(session, "tipo_b", selected = preset$tipo_b)
  }
  if (!is.null(preset$growth_kind) && !is.null(input$growth_kind) && !identical(input$growth_kind, preset$growth_kind)) {
    updateRadioButtons(session, "growth_kind", selected = preset$growth_kind)
  }
  if (!is.null(preset$rank_mode) && !is.null(input$rank_mode) && !identical(input$rank_mode, preset$rank_mode)) {
    updateRadioButtons(session, "rank_mode", selected = preset$rank_mode)
  }
  if (!is.null(preset$rank_n) && !is.null(input$rank_n) && !identical(input$rank_n, preset$rank_n)) {
    updateNumericInput(session, "rank_n", value = preset$rank_n)
  }

  # Estilo
  if (is.null(preset$facet_row) && !is.null(preset$facet)) {
    preset$facet_row <- preset$facet
  }
  if (is.null(preset$facet_col)) {
    preset$facet_col <- "off"
  }
  if (!is.null(preset$facet_row) && !is.null(input$facet_row) && !identical(input$facet_row, preset$facet_row)) {
    updateSelectInput(session, "facet_row", selected = preset$facet_row)
  }
  if (!is.null(preset$facet_col) && !is.null(input$facet_col) && !identical(input$facet_col, preset$facet_col)) {
    updateSelectInput(session, "facet_col", selected = preset$facet_col)
  }
  if (!is.null(preset$style_preset) && !identical(input$style_preset, preset$style_preset)) {
    updateSelectInput(session, "style_preset", selected = preset$style_preset)
  }
  if (!is.null(preset$palette_fill) && !identical(input$palette_fill, preset$palette_fill)) {
    updateSelectInput(session, "palette_fill", selected = preset$palette_fill)
  }
  if (!is.null(preset$palette_color) && !identical(input$palette_color, preset$palette_color)) {
    updateSelectInput(session, "palette_color", selected = preset$palette_color)
  }

  if (!is.null(preset$title) && !identical(input$title, preset$title)) {
    updateTextInput(session, "title", value = preset$title)
  }
  if (!is.null(preset$subtitle) && !identical(input$subtitle, preset$subtitle)) {
    updateTextInput(session, "subtitle", value = preset$subtitle)
  }
  if (!is.null(preset$xlab) && !identical(input$xlab, preset$xlab)) {
    updateTextInput(session, "xlab", value = preset$xlab)
  }
  if (!is.null(preset$ylab) && !identical(input$ylab, preset$ylab)) {
    updateTextInput(session, "ylab", value = preset$ylab)
  }

  if (!is.null(preset$export_res) && !identical(input$export_res, preset$export_res)) {
    updateSelectInput(session, "export_res", selected = preset$export_res)
  }
  if (!is.null(preset$alpha_bars) && !identical(input$alpha_bars, preset$alpha_bars)) {
    updateSliderInput(session, "alpha_bars", value = preset$alpha_bars)
  }
  if (!is.null(preset$alpha_lines) && !identical(input$alpha_lines, preset$alpha_lines)) {
    updateSliderInput(session, "alpha_lines", value = preset$alpha_lines)
  }

  TRUE
}
