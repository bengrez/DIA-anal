plot_distribucion <- function(df, ejes, kind, facet_row, facet_col, palette_fill, palette_color, alpha_bars, alpha_lines, plot_theme) {
  ejes <- unique(as.character(ejes %||% character()))
  if (length(ejes) == 0) {
    stop("Selecciona al menos 1 eje.", call. = FALSE)
  }

  df_plot <- df %>%
    select(year, area, curso, tipo, all_of(ejes)) %>%
    tidyr::pivot_longer(cols = all_of(ejes), names_to = "eje", values_to = "valor")

  if (is.factor(df_plot$tipo)) {
    df_plot$tipo <- droplevels(df_plot$tipo)
  } else {
    df_plot$tipo <- factor(df_plot$tipo, levels = unique(df_plot$tipo))
  }
  levels_fill <- levels(df_plot$tipo)
  colors <- palette_values(length(levels_fill), palette_fill)
  names(colors) <- levels_fill

  if (identical(kind, "hist")) {
    line_colors <- palette_values(length(levels_fill), palette_color %||% palette_fill)
    names(line_colors) <- levels_fill

    p <- ggplot(df_plot, aes(x = .data$valor)) +
      geom_histogram(
        aes(y = after_stat(density), fill = .data$tipo),
        position = "identity",
        alpha = alpha_bars %||% 0.6,
        bins = 20
      ) +
      geom_density(
        aes(y = after_stat(density), color = .data$tipo),
        linewidth = 1,
        alpha = alpha_lines %||% 0.9
      ) +
      scale_fill_manual(values = colors) +
      scale_color_manual(values = line_colors, guide = "none") +
      labs(x = "Porcentaje (%)", y = "Densidad", fill = "Tipo") +
      plot_theme

    return(apply_facets(p, data = df_plot, facet_row = facet_row %||% "off", facet_col = facet_col %||% "off"))
  }

  p <- ggplot(df_plot, aes(x = .data$curso, y = .data$valor, fill = .data$tipo)) +
    geom_boxplot(outlier.alpha = 0.25, width = 0.7, alpha = alpha_bars %||% 0.85) +
    scale_y_continuous(labels = scales::label_percent(scale = 1)) +
    scale_fill_manual(values = colors) +
    labs(x = "Curso", y = "Porcentaje (%)", fill = "Tipo") +
    plot_theme +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))

  apply_facets(p, data = df_plot, facet_row = facet_row %||% "off", facet_col = facet_col %||% "off")
}
