plot_promedio <- function(df, eje, palette_fill, plot_theme) {
  df_sum <- df %>%
    group_by(.data$curso, .data$tipo) %>%
    summarise(valor = mean(.data[[eje]], na.rm = TRUE), .groups = "drop")

  levels_fill <- sort(unique(df_sum$tipo))
  colors <- palette_values(length(levels_fill), palette_fill)
  names(colors) <- levels_fill

  ggplot(df_sum, aes(x = .data$curso, y = .data$valor, fill = .data$tipo)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    scale_y_continuous(labels = scales::label_percent(scale = 1)) +
    scale_fill_manual(values = colors) +
    labs(x = "Curso", y = "Promedio (%)", fill = "Tipo") +
    plot_theme +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
}

