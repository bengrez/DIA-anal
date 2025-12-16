plot_promedio <- function(df, ejes, facet_row, facet_col, palette_fill, alpha_bars, plot_theme) {
  ejes <- unique(as.character(ejes %||% character()))
  if (length(ejes) == 0) {
    stop("Selecciona al menos 1 eje.", call. = FALSE)
  }

  df_long <- df %>%
    select(year, curso, tipo, all_of(ejes)) %>%
    tidyr::pivot_longer(cols = all_of(ejes), names_to = "eje", values_to = "valor")

  df_sum <- df_long %>%
    group_by(.data$year, .data$curso, .data$tipo, .data$eje) %>%
    summarise(valor = mean(.data$valor, na.rm = TRUE), .groups = "drop")

  if (is.factor(df_sum$tipo)) {
    df_sum$tipo <- droplevels(df_sum$tipo)
  } else {
    df_sum$tipo <- factor(df_sum$tipo, levels = unique(df_sum$tipo))
  }
  levels_fill <- levels(df_sum$tipo)
  colors <- palette_values(length(levels_fill), palette_fill)
  names(colors) <- levels_fill

  p <- ggplot(df_sum, aes(x = .data$curso, y = .data$valor, fill = .data$tipo)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7, alpha = alpha_bars %||% 0.85) +
    scale_y_continuous(labels = scales::label_percent(scale = 1)) +
    scale_fill_manual(values = colors) +
    labs(x = "Curso", y = "Promedio (%)", fill = "Tipo") +
    plot_theme +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))

  apply_facets(p, data = df_sum, facet_row = facet_row %||% "off", facet_col = facet_col %||% "off")
}
