plot_distribucion <- function(df, eje, kind, facet, palette_fill, plot_theme) {
  df_plot <- df %>%
    transmute(
      curso = .data$curso,
      tipo = .data$tipo,
      valor = .data[[eje]]
    )

  df_plot$tipo <- factor(df_plot$tipo)
  levels_fill <- levels(df_plot$tipo)
  colors <- palette_values(length(levels_fill), palette_fill)
  names(colors) <- levels_fill

  if (identical(kind, "hist")) {
    p <- ggplot(df_plot, aes(x = .data$valor, fill = .data$tipo)) +
      geom_histogram(position = "identity", alpha = 0.6, bins = 20) +
      scale_fill_manual(values = colors) +
      labs(x = paste0(eje, " (%)"), y = "Frecuencia", fill = "Tipo") +
      plot_theme

    if (identical(facet, "curso")) {
      p <- p + facet_wrap(~curso)
    } else if (identical(facet, "tipo")) {
      p <- p + facet_wrap(~tipo)
    }
    return(p)
  }

  p <- ggplot(df_plot, aes(x = .data$curso, y = .data$valor, fill = .data$tipo)) +
    geom_boxplot(outlier.alpha = 0.25, width = 0.7) +
    scale_y_continuous(labels = scales::label_percent(scale = 1)) +
    scale_fill_manual(values = colors) +
    labs(x = "Curso", y = paste0(eje, " (%)"), fill = "Tipo") +
    plot_theme +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))

  if (identical(facet, "tipo")) {
    p <- p + facet_wrap(~tipo)
  } else if (identical(facet, "curso")) {
    p <- p + facet_wrap(~curso)
  }
  p
}

