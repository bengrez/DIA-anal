# ------------------------------------------------------------
# Presets de configuración (guardar/cargar)
#
# Un preset es un archivo `.rds` con el estado de la UI: tipo de gráfico,
# filtros, facets, estilo, transparencias, etc. La app puede:
# - Guardar el preset actual
# - Cargar un preset y aplicarlo gradualmente (para inputs condicionales)
#
# Compatibilidad:
# - v1: `facet` (un solo selector)
# - v2: `facet_row` + `facet_col`
# ------------------------------------------------------------

preset_normalize <- function(preset) {
  # Normaliza presets antiguos para mantener compatibilidad entre versiones.
  # - v1 usaba `facet` (un solo selector); v2 separa en `facet_row` y `facet_col`.
  # - Algunos campos pueden venir omitidos en presets guardados por usuarios.
  if (is.null(preset) || !is.list(preset)) return(NULL)

  if (is.null(preset$facet_row) && !is.null(preset$facet)) {
    preset$facet_row <- preset$facet
  }
  preset$facet_row <- preset$facet_row %||% "off"
  preset$facet_col <- preset$facet_col %||% "off"

  preset$axis_pool <- preset$axis_pool %||% "common"
  preset
}

preset_capture <- function(input) {
  # Captura el estado actual de la UI para guardarlo como preset.
  # Se almacena como una lista sencilla (serializable a .rds).
  list(
    version = 2,
    plot_type = input$plot_type %||% "promedio",
    facet_row = input$facet_row %||% "off",
    facet_col = input$facet_col %||% "off",
    area = input$area %||% NULL,
    curso = input$curso %||% NULL,
    year = input$year %||% NULL,
    tipo = input$tipo %||% NULL,
    fuente = input$fuente %||% NULL,
    eje = input$eje %||% NULL,
    axis_pool = input$axis_pool %||% NULL,
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
  # Defaults basados en los valores disponibles en el dataset cargado.
  # Sirve para inicializar la UI y para "reset" de configuración.
  ch <- ch %||% list()
  areas <- ch$areas %||% character()
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
    area = areas,
    curso = cursos,
    year = years,
    tipo = tipos,
    fuente = fuentes,
    eje = if (length(ejes) > 0) ejes[[1]] else NULL,
    axis_pool = "common",
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
  preset <- preset_normalize(preset)
  if (is.null(preset)) return(TRUE)

  eq <- function(a, b) identical(a %||% NULL, b %||% NULL)

  # Campos siempre visibles en la UI principal.
  always <- list(
    plot_type = preset$plot_type,
    facet_row = preset$facet_row,
    facet_col = preset$facet_col,
    style_preset = preset$style_preset,
    palette_fill = preset$palette_fill,
    palette_color = preset$palette_color,
    title = preset$title,
    subtitle = preset$subtitle,
    xlab = preset$xlab,
    ylab = preset$ylab,
    export_res = preset$export_res,
    alpha_bars = preset$alpha_bars,
    alpha_lines = preset$alpha_lines
  )

  for (id in names(always)) {
    if (!eq(input[[id]], always[[id]])) return(FALSE)
  }

  if (!eq(isTRUE(input$anon), isTRUE(preset$anon))) return(FALSE)

  # Campos condicionales: solo se validan si existen en la UI y en el preset.
  optional <- c(
    "anon_method",
    "anon_seed",
    "fuente",
    "area",
    "curso",
    "year",
    "tipo",
    "eje",
    "axis_pool",
    "tipo_order",
    "nivel_order",
    "dist_kind",
    "heatmap_dim",
    "violin_kind",
    "violin_group",
    "trend_group",
    "tipo_a",
    "tipo_b",
    "growth_kind",
    "rank_mode",
    "rank_n"
  )

  for (id in optional) {
    if (!is.null(input[[id]]) && !is.null(preset[[id]]) && !eq(input[[id]], preset[[id]])) return(FALSE)
  }

  TRUE
}

preset_apply_step <- function(session, input, preset) {
  # Devuelve TRUE cuando "ya intentó aplicar todo lo posible".
  preset <- preset_normalize(preset)
  if (is.null(preset)) return(TRUE)

  update_if_changed <- function(id, value, updater, arg_name = "selected", extra = list()) {
    if (is.null(value)) return(invisible(FALSE))
    current <- input[[id]]
    if (is.null(current) || identical(current, value)) return(invisible(FALSE))

    args <- c(list(session = session, inputId = id), setNames(list(value), arg_name), extra)
    do.call(updater, args)
    invisible(TRUE)
  }

  # 1) Forzar estados que habilitan inputs condicionales.
  update_if_changed("plot_type", preset$plot_type, updateSelectInput, "selected")
  if (!identical(isTRUE(input$anon), isTRUE(preset$anon))) {
    updateCheckboxInput(session, "anon", value = isTRUE(preset$anon))
  }

  # 2) Filtros y opciones generales (si existen en la UI).
  select_ids <- c(
    "fuente",
    "area",
    "curso",
    "year",
    "tipo",
    "eje",
    "heatmap_dim",
    "violin_group",
    "trend_group",
    "tipo_a",
    "tipo_b",
    "facet_row",
    "facet_col",
    "style_preset",
    "palette_fill",
    "palette_color",
    "export_res",
    "anon_method"
  )
  for (id in select_ids) {
    update_if_changed(id, preset[[id]], updateSelectInput, "selected")
  }

  radio_ids <- c("axis_pool", "dist_kind", "violin_kind", "growth_kind", "rank_mode")
  for (id in radio_ids) {
    update_if_changed(id, preset[[id]], updateRadioButtons, "selected")
  }

  if (!is.null(preset$anon_seed)) update_if_changed("anon_seed", preset$anon_seed, updateNumericInput, "value")
  if (!is.null(preset$rank_n)) update_if_changed("rank_n", preset$rank_n, updateNumericInput, "value")

  if (!is.null(preset$tipo_order)) {
    update_if_changed("tipo_order", preset$tipo_order, updateSelectizeInput, "selected", extra = list(server = TRUE))
  }
  if (!is.null(preset$nivel_order)) {
    update_if_changed("nivel_order", preset$nivel_order, updateSelectizeInput, "selected", extra = list(server = TRUE))
  }

  update_if_changed("title", preset$title, updateTextInput, "value")
  update_if_changed("subtitle", preset$subtitle, updateTextInput, "value")
  update_if_changed("xlab", preset$xlab, updateTextInput, "value")
  update_if_changed("ylab", preset$ylab, updateTextInput, "value")

  if (!is.null(preset$alpha_bars)) update_if_changed("alpha_bars", preset$alpha_bars, updateSliderInput, "value")
  if (!is.null(preset$alpha_lines)) update_if_changed("alpha_lines", preset$alpha_lines, updateSliderInput, "value")

  TRUE
}
preset_normalize <- function(preset) {
  # Normaliza presets antiguos para mantener compatibilidad entre versiones.
  # - v1 usaba `facet` (un solo selector); v2 separa en `facet_row` y `facet_col`.
  # - Algunos campos pueden venir omitidos en presets guardados por usuarios.
  if (is.null(preset) || !is.list(preset)) return(NULL)

  if (is.null(preset$facet_row) && !is.null(preset$facet)) {
    preset$facet_row <- preset$facet
  }
  preset$facet_row <- preset$facet_row %||% "off"
  preset$facet_col <- preset$facet_col %||% "off"

  preset$axis_pool <- preset$axis_pool %||% "common"
  preset
}
