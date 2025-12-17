# ------------------------------------------------------------
# Ciencias Naturales — Gráficos (ggplot2)
#
# Todas las funciones retornan un objeto ggplot. En la app se convierten
# a plotly (interactivo) usando `plotly::ggplotly()`.
# ------------------------------------------------------------

science_theme <- function() {
  ggplot2::theme_minimal(base_size = 12, base_family = "Arial") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      axis.title = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 10),
      legend.title = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 10),
      panel.grid.major = ggplot2::element_line(color = "#CCCCCC", linewidth = 0.3),
      panel.grid.minor = ggplot2::element_blank()
    )
}

scale_pct_0_100 <- function() {
  ggplot2::scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 20),
    labels = scales::label_percent(scale = 1)
  )
}

order_tipo_for_facets <- function(tipo) {
  # Orden por defecto para Periodo/Tipo DIA (útil para facets y tablas).
  known <- c("Diagnóstico", "Monitoreo", "Cierre", "Evaluacion_Cierre")
  tipo_chr <- as.character(tipo %||% "")
  tipo_chr[tipo_chr == ""] <- NA_character_
  present <- unique(stats::na.omit(tipo_chr))
  lvls <- c(intersect(known, present), setdiff(present, known))
  factor(tipo_chr, levels = lvls)
}

plot_levels_distribution <- function(df_levels) {
  if (is.null(df_levels) || nrow(df_levels) == 0) {
    return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::ggtitle("Sin datos"))
  }

  dfp <- df_levels %>%
    dplyr::mutate(
      grade_lbl = factor(.data$grade, levels = sort(unique(.data$grade)), labels = paste0(sort(unique(.data$grade)), "°")),
      nivel = factor(.data$nivel, levels = c("Nivel I", "Nivel II", "Nivel III")),
      tipo = order_tipo_for_facets(.data$tipo)
    )

  p <- ggplot2::ggplot(dfp, ggplot2::aes(x = .data$grade_lbl, y = .data$pct, fill = .data$nivel)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.7), width = 0.7) +
    ggplot2::geom_hline(yintercept = 60, linetype = "dashed", color = "#666666") +
    ggplot2::scale_fill_manual(
      values = c(
        "Nivel I" = "#E74C3C",
        "Nivel II" = "#F39C12",
        "Nivel III" = "#27AE60"
      )
    ) +
    scale_pct_0_100() +
    ggplot2::labs(
      title = "Distribución de estudiantes por nivel de logro",
      x = "Grado",
      y = "Porcentaje de estudiantes",
      fill = "Nivel"
    ) +
    science_theme() +
    ggplot2::theme(legend.position = "bottom")

  # Facets si hay múltiples años/períodos en el resumen.
  years_n <- length(unique(dfp$year))
  periods_n <- length(unique(dfp$tipo))
  if (years_n > 1 && periods_n > 1) {
    p <- p + ggplot2::facet_grid(year ~ tipo)
  } else if (years_n > 1) {
    p <- p + ggplot2::facet_wrap(~year)
  } else if (periods_n > 1) {
    p <- p + ggplot2::facet_wrap(~tipo)
  }

  p
}

plot_axes_by_course <- function(df_axes) {
  if (is.null(df_axes) || nrow(df_axes) == 0) {
    return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::ggtitle("Sin datos"))
  }

  order_courses <- df_axes %>%
    dplyr::distinct(.data$course, .data$grade, .data$section) %>%
    dplyr::arrange(.data$grade, .data$section) %>%
    dplyr::pull(.data$course)

  dfp <- df_axes %>%
    dplyr::mutate(
      course = factor(.data$course, levels = order_courses),
      tipo = order_tipo_for_facets(.data$tipo)
    )

  p <- ggplot2::ggplot(dfp, ggplot2::aes(x = .data$course, y = .data$pct_logro, fill = .data$axis)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.7) +
    ggplot2::geom_hline(yintercept = 60, linetype = "dashed", color = "#666666") +
    scale_pct_0_100() +
    ggplot2::labs(
      title = "Desempeño por eje temático",
      x = "Curso",
      y = "% logro (promedio)",
      fill = "Eje"
    ) +
    science_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), legend.position = "bottom")

  years_n <- length(unique(dfp$year))
  periods_n <- length(unique(dfp$tipo))
  if (years_n > 1 && periods_n > 1) {
    p <- p + ggplot2::facet_grid(year ~ tipo)
  } else if (years_n > 1) {
    p <- p + ggplot2::facet_wrap(~year)
  } else if (periods_n > 1) {
    p <- p + ggplot2::facet_wrap(~tipo)
  }

  p
}

plot_axes_heatmap <- function(df_axes) {
  if (is.null(df_axes) || nrow(df_axes) == 0) {
    return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::ggtitle("Sin datos"))
  }

  order_courses <- df_axes %>%
    dplyr::distinct(.data$course, .data$grade, .data$section) %>%
    dplyr::arrange(.data$grade, .data$section) %>%
    dplyr::pull(.data$course)

  dfp <- df_axes %>%
    dplyr::mutate(
      course = factor(.data$course, levels = order_courses),
      tipo = order_tipo_for_facets(.data$tipo)
    )

  p <- ggplot2::ggplot(dfp, ggplot2::aes(x = .data$course, y = .data$axis, fill = .data$pct_logro)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.3) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%", .data$pct_logro)), size = 3) +
    ggplot2::scale_fill_gradient2(
      low = "#E74C3C",
      mid = "#F39C12",
      high = "#27AE60",
      midpoint = 50,
      limits = c(0, 100),
      labels = scales::label_percent(scale = 1),
      name = "% logro"
    ) +
    ggplot2::labs(
      title = "Heatmap: ejes × cursos",
      x = "Curso",
      y = "Eje temático"
    ) +
    science_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  years_n <- length(unique(dfp$year))
  periods_n <- length(unique(dfp$tipo))
  if (years_n > 1 && periods_n > 1) {
    p <- p + ggplot2::facet_grid(year ~ tipo)
  } else if (years_n > 1) {
    p <- p + ggplot2::facet_wrap(~year)
  } else if (periods_n > 1) {
    p <- p + ggplot2::facet_wrap(~tipo)
  }

  p
}

plot_yoy_diff <- function(df_diff, title, x_col, facet_col = NULL) {
  if (is.null(df_diff) || nrow(df_diff) == 0) {
    return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::ggtitle("Sin datos"))
  }

  dfp <- df_diff %>%
    dplyr::mutate(
      direction = factor(.data$direction, levels = c("baja", "igual", "mejora", "sin_dato"))
    )

  p <- ggplot2::ggplot(dfp, ggplot2::aes(x = .data[[x_col]], y = .data$diff, fill = .data$direction)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::geom_hline(yintercept = 0, color = "#666666") +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%+.1f", .data$diff)), vjust = ifelse(dfp$diff >= 0, -0.3, 1.2), size = 3) +
    ggplot2::scale_fill_manual(
      values = c(
        "mejora" = "#27AE60",
        "baja" = "#E74C3C",
        "igual" = "#95A5A6",
        "sin_dato" = "#BDC3C7"
      ),
      name = NULL
    ) +
    ggplot2::labs(title = title, x = NULL, y = "Diferencia (puntos porcentuales)") +
    ggplot2::theme_minimal(base_size = 12, base_family = "Arial") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      axis.text = ggplot2::element_text(size = 10),
      legend.position = "bottom"
    )

  if (!is.null(facet_col) && facet_col %in% names(dfp)) {
    p <- p + ggplot2::facet_wrap(stats::as.formula(paste0("~", facet_col)))
  }

  p
}

plot_yoy_axes_by_course <- function(df_diff) {
  if (is.null(df_diff) || nrow(df_diff) == 0) {
    return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::ggtitle("Sin datos"))
  }

  order_courses <- df_diff %>%
    dplyr::distinct(.data$course, .data$grade, .data$section) %>%
    dplyr::arrange(.data$grade, .data$section) %>%
    dplyr::pull(.data$course)

  dfp <- df_diff %>%
    dplyr::mutate(
      course = factor(.data$course, levels = order_courses),
      direction = factor(.data$direction, levels = c("baja", "igual", "mejora", "sin_dato"))
    )

  ggplot2::ggplot(dfp, ggplot2::aes(x = .data$course, y = .data$diff, fill = .data$direction)) +
    ggplot2::geom_hline(yintercept = 0, color = "#666666") +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%+.1f", .data$diff)),
      vjust = ifelse(dfp$diff >= 0, -0.3, 1.2),
      size = 3
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "mejora" = "#27AE60",
        "baja" = "#E74C3C",
        "igual" = "#95A5A6",
        "sin_dato" = "#BDC3C7"
      ),
      name = NULL
    ) +
    ggplot2::labs(title = "Cambio año vs año (ejes temáticos)", x = "Curso", y = "Diferencia (p.p.)") +
    ggplot2::facet_wrap(~axis, ncol = 1) +
    ggplot2::theme_minimal(base_size = 12, base_family = "Arial") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
}
