interpretation_points <- function(
    plot_type,
    ejes = NULL,
    cursos = NULL,
    tipos = NULL,
    years = NULL,
    facet = "off",
    dist_kind = NULL,
    tipo_a = NULL,
    tipo_b = NULL,
    growth_kind = NULL,
    rank_mode = NULL,
    anonymous = FALSE,
    show_mean = FALSE,
    show_median = FALSE,
    show_band = FALSE,
    show_labels = FALSE,
    show_corr = FALSE
) {
  ejes <- unique(as.character(ejes %||% character()))
  cursos <- unique(as.character(cursos %||% character()))
  tipos <- unique(as.character(tipos %||% character()))
  years <- unique(as.character(years %||% character()))

  scope <- c()
  if (length(years) > 0) scope <- c(scope, paste0("Año(s): ", paste(years, collapse = ", ")))
  if (length(cursos) > 0) scope <- c(scope, paste0("Curso(s): ", paste(cursos, collapse = ", ")))
  if (length(tipos) > 0) scope <- c(scope, paste0("Tipo(s): ", paste(tipos, collapse = ", ")))
  if (length(ejes) > 0) scope <- c(scope, paste0("Eje(s): ", paste(ejes, collapse = ", ")))
  if (identical(facet, "off")) {
    facet_txt <- "Sin facets"
  } else {
    facet_txt <- paste0("Facets: ", facet)
  }

  common <- c(
    paste(scope, collapse = " · "),
    facet_txt,
    if (isTRUE(anonymous)) "Modo anónimo: activado" else "Modo anónimo: desactivado"
  )

  if (identical(plot_type, "promedio")) {
    overlays <- c()
    if (isTRUE(show_mean)) overlays <- c(overlays, "línea de promedio")
    if (isTRUE(show_median)) overlays <- c(overlays, "línea de mediana")
    if (isTRUE(show_band)) overlays <- c(overlays, "banda de variabilidad (p25–p75/sd)")
    if (isTRUE(show_labels)) overlays <- c(overlays, "etiquetas con n y percentiles")

    return(list(
      title = "Cómo leer este gráfico (promedios)",
      bullets = c(
        common,
        "Muestra el promedio (%) por Curso y Tipo.",
        "Útil para comparar desempeño entre evaluaciones (Tipo) dentro de cada Curso.",
        "Los valores ausentes se excluyen del cálculo del promedio.",
        if (length(overlays) > 0) paste("Capas activas:", paste(overlays, collapse = "; "))
      )
    ))
  }

  if (identical(plot_type, "distribucion")) {
    kind_txt <- if (identical(dist_kind, "hist")) "Histograma + densidad" else "Boxplot"
    overlays <- c()
    if (isTRUE(show_mean)) overlays <- c(overlays, "línea de promedio")
    if (isTRUE(show_median)) overlays <- c(overlays, "línea de mediana")
    if (isTRUE(show_band)) overlays <- c(overlays, "banda de p25–p75 o sd")
    if (isTRUE(show_labels)) overlays <- c(overlays, "etiquetas de n/percentiles")
    if (isTRUE(show_corr)) overlays <- c(overlays, "correlación/regresión")
    return(list(
      title = "Cómo leer este gráfico (distribución)",
      bullets = c(
        common,
        paste0("Modo: ", kind_txt, "."),
        "Útil para ver variabilidad y outliers (no solo el promedio).",
        "Si comparas Tipos, revisa si la distribución se desplaza (mejora/baja) o se vuelve más dispersa.",
        if (length(overlays) > 0) paste("Capas activas:", paste(overlays, collapse = "; "))
      )
    ))
  }

  if (identical(plot_type, "nivel_logro")) {
    return(list(
      title = "Cómo leer este gráfico (nivel de logro)",
      bullets = c(
        common,
        "Muestra la proporción de estudiantes por NIVEL DE LOGRO (barras apiladas al 100%).",
        "Útil para reportes institucionales y comparaciones por curso/tipo.",
        "Este gráfico no recalcula el nivel de logro: usa el valor del archivo."
      )
    ))
  }

  if (identical(plot_type, "crecimiento")) {
    gkind_txt <- if (identical(growth_kind, "slope")) "Slope chart" else "Delta (barras)"
    rank_txt <- if (identical(rank_mode, "both")) "Mostrando Top + Bottom N" else if (identical(rank_mode, "top")) "Mostrando Top N" else if (identical(rank_mode, "bottom")) "Mostrando Bottom N" else "Mostrando todos"
    overlays <- c()
    if (isTRUE(show_mean)) overlays <- c(overlays, "línea de promedio Δ")
    if (isTRUE(show_median)) overlays <- c(overlays, "línea de mediana Δ")
    if (isTRUE(show_band)) overlays <- c(overlays, "banda de p25–p75 o sd")
    if (isTRUE(show_labels)) overlays <- c(overlays, "anotación de n y percentiles")
    return(list(
      title = "Cómo leer este gráfico (crecimiento)",
      bullets = c(
        common,
        paste0("Comparación: ", tipo_a %||% "Tipo A", " → ", tipo_b %||% "Tipo B", "."),
        paste0("Visualización: ", gkind_txt, "."),
        rank_txt,
        "Delta positivo indica mejora; delta negativo indica baja.",
        if (length(overlays) > 0) paste("Capas activas:", paste(overlays, collapse = "; "))
      )
    ))
  }

  list(title = "Texto sugerido", bullets = common)
}

