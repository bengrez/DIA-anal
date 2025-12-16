interpretation_points <- function(
    plot_type,
    ejes = NULL,
    cursos = NULL,
    tipos = NULL,
    years = NULL,
    facet_row = "off",
    facet_col = "off",
    dist_kind = NULL,
    heatmap_dim = NULL,
    violin_kind = NULL,
    violin_group = NULL,
    trend_group = NULL,
    tipo_a = NULL,
    tipo_b = NULL,
    growth_kind = NULL,
    rank_mode = NULL,
    anonymous = FALSE
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
  facet_row <- facet_row %||% "off"
  facet_col <- facet_col %||% "off"
  if (identical(facet_row, "off") && identical(facet_col, "off")) {
    facet_txt <- "Sin facets"
  } else if (!identical(facet_row, "off") && !identical(facet_col, "off")) {
    facet_txt <- paste0("Facets: ", facet_row, " ~ ", facet_col)
  } else {
    facet_txt <- paste0("Facets: ", if (!identical(facet_row, "off")) facet_row else facet_col)
  }

  common <- c(
    paste(scope, collapse = " · "),
    facet_txt,
    if (isTRUE(anonymous)) "Modo anónimo: activado" else "Modo anónimo: desactivado"
  )

  if (identical(plot_type, "promedio")) {
    return(list(
      title = "Cómo leer este gráfico (promedios)",
      bullets = c(
        common,
        "Muestra el promedio (%) por Curso y Tipo.",
        "Útil para comparar desempeño entre evaluaciones (Tipo) dentro de cada Curso.",
        "Los valores ausentes se excluyen del cálculo del promedio."
      )
    ))
  }

  if (identical(plot_type, "distribucion")) {
    kind_txt <- if (identical(dist_kind, "hist")) "Histograma + densidad" else "Boxplot"
    return(list(
      title = "Cómo leer este gráfico (distribución)",
      bullets = c(
        common,
        paste0("Modo: ", kind_txt, "."),
        "Útil para ver variabilidad y outliers (no solo el promedio).",
        "Si comparas Tipos, revisa si la distribución se desplaza (mejora/baja) o se vuelve más dispersa."
      )
    ))
  }

  if (identical(plot_type, "heatmap")) {
    axis_txt <- if (identical(heatmap_dim, "tipo")) "Tipo" else "Curso"
    return(list(
      title = "Cómo leer este gráfico (heatmap)",
      bullets = c(
        common,
        paste0("Eje vs ", axis_txt, " con escala de color por promedio (%)."),
        "Colores más intensos indican valores promedio más altos.",
        "Útil para detectar patrones rápidos entre ejes y grupos."
      )
    ))
  }

  if (identical(plot_type, "violin")) {
    mode_txt <- if (identical(violin_kind, "ridge")) "Ridgeline" else "Violín"
    group_txt <- if (identical(violin_group, "tipo")) "Tipo" else "Curso"
    return(list(
      title = "Cómo leer este gráfico (distribución violín/ridgeline)",
      bullets = c(
        common,
        paste0("Distribución por ", group_txt, " (", mode_txt, ")."),
        "Permite ver densidad, simetría y presencia de outliers.",
        "Comparar el ancho/altura ayuda a notar variabilidad entre grupos."
      )
    ))
  }

  if (identical(plot_type, "tendencia")) {
    group_txt <- if (identical(trend_group, "tipo")) "Tipo" else "Curso"
    return(list(
      title = "Cómo leer este gráfico (tendencia temporal)",
      bullets = c(
        common,
        paste0("Líneas por ", group_txt, " mostrando la evolución anual."),
        "La pendiente indica mejora o retroceso en el tiempo.",
        "Útil para informes de progreso y seguimiento longitudinal."
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
    return(list(
      title = "Cómo leer este gráfico (crecimiento)",
      bullets = c(
        common,
        paste0("Comparación: ", tipo_a %||% "Tipo A", " → ", tipo_b %||% "Tipo B", "."),
        paste0("Visualización: ", gkind_txt, "."),
        rank_txt,
        "Delta positivo indica mejora; delta negativo indica baja."
      )
    ))
  }

  list(title = "Texto sugerido", bullets = common)
}
