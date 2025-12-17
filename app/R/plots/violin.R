# ------------------------------------------------------------
# Plot extra — Violín / Ridgeline
#
# Alternativa visual para distribución por curso o tipo, útil para
# comparar formas de distribución sin depender solo de boxplot.
# ------------------------------------------------------------

plot_violin <- function(
    df,
    ejes,
    facet_row,
    facet_col,
    group_var = "curso",
    kind = "violin",
    palette_fill,
    alpha_bars,
    alpha_lines,
    plot_theme) {
  ejes <- unique(as.character(ejes %||% character()))
  if (length(ejes) == 0) {
    stop("Selecciona al menos 1 eje.", call. = FALSE)
  }

  group_var <- match.arg(group_var, c("curso", "tipo"))
  kind <- match.arg(kind, c("violin", "ridge"))

  df_plot <- df %>%
    select(year, area, curso, tipo, all_of(ejes)) %>%
    tidyr::pivot_longer(cols = all_of(ejes), names_to = "eje", values_to = "valor")

  group_levels <- if (is.factor(df_plot[[group_var]])) {
    droplevels(df_plot[[group_var]])
  } else {
    factor(df_plot[[group_var]], levels = unique(df_plot[[group_var]]))
  }
  df_plot[[group_var]] <- group_levels
  levels_fill <- levels(group_levels)
  colors <- palette_values(length(levels_fill), palette_fill)
  names(colors) <- levels_fill

  if (identical(kind, "ridge")) {
    p <- ggplot(df_plot, aes(y = .data[[group_var]], x = .data$valor, fill = .data[[group_var]])) +
      ggridges::geom_density_ridges(alpha = alpha_lines %||% 0.9, color = "white") +
      scale_fill_manual(values = colors, guide = "none") +
      scale_x_continuous(labels = scales::label_percent(scale = 1)) +
      labs(x = "Porcentaje (%)", y = if (identical(group_var, "curso")) "Curso" else "Tipo") +
      plot_theme
  } else {
    p <- ggplot(df_plot, aes(x = .data[[group_var]], y = .data$valor, fill = .data[[group_var]])) +
      geom_violin(trim = FALSE, alpha = alpha_bars %||% 0.75) +
      geom_boxplot(width = 0.2, outlier.alpha = 0.3, alpha = alpha_lines %||% 0.9, color = "black") +
      scale_fill_manual(values = colors, guide = "none") +
      scale_y_continuous(labels = scales::label_percent(scale = 1)) +
      labs(x = if (identical(group_var, "curso")) "Curso" else "Tipo", y = "Porcentaje (%)") +
      plot_theme +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
  }

  if (identical(facet_row, group_var)) facet_row <- "off"
  if (identical(facet_col, group_var)) facet_col <- "off"
  apply_facets(p, data = df_plot, facet_row = facet_row %||% "off", facet_col = facet_col %||% "off")
}
