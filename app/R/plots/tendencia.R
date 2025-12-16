plot_tendencia <- function(
    df,
    ejes,
    facet_row,
    facet_col,
    group_var = "curso",
    palette_color,
    alpha_lines,
    plot_theme) {
  ejes <- unique(as.character(ejes %||% character()))
  if (length(ejes) == 0) {
    stop("Selecciona al menos 1 eje.", call. = FALSE)
  }

  group_var <- match.arg(group_var, c("curso", "tipo"))

  df_long <- df %>%
    select(year, curso, tipo, all_of(ejes)) %>%
    tidyr::pivot_longer(cols = all_of(ejes), names_to = "eje", values_to = "valor") %>%
    group_by(.data$year, .data[[group_var]], .data$eje) %>%
    summarise(valor = mean(.data$valor, na.rm = TRUE), .groups = "drop") %>%
    mutate(year = as.integer(.data$year))

  group_levels <- if (is.factor(df_long[[group_var]])) {
    droplevels(df_long[[group_var]])
  } else {
    factor(df_long[[group_var]], levels = unique(df_long[[group_var]]))
  }
  df_long[[group_var]] <- group_levels
  levels_color <- levels(group_levels)
  colors <- palette_values(length(levels_color), palette_color)
  names(colors) <- levels_color

  p <- ggplot(df_long, aes(x = .data$year, y = .data$valor, color = .data[[group_var]], group = .data[[group_var]])) +
    geom_line(linewidth = 1, alpha = alpha_lines %||% 0.9) +
    geom_point(size = 2) +
    scale_color_manual(values = colors, name = tools::toTitleCase(group_var)) +
    scale_y_continuous(labels = scales::label_percent(scale = 1)) +
    scale_x_continuous(breaks = sort(unique(df_long$year))) +
    labs(x = "Año", y = "Promedio (%)") +
    plot_theme

  if (identical(facet_row, group_var)) facet_row <- "off"
  if (identical(facet_col, group_var)) facet_col <- "off"
  apply_facets(p, data = df_long, facet_row = facet_row %||% "off", facet_col = facet_col %||% "off")
}
