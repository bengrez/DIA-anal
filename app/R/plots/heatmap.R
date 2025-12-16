plot_heatmap <- function(df, ejes, facet, axis_dim = "curso", palette_fill, plot_theme) {
  ejes <- unique(as.character(ejes %||% character()))
  if (length(ejes) == 0) {
    stop("Selecciona al menos 1 eje.", call. = FALSE)
  }

  axis_dim <- match.arg(axis_dim, c("curso", "tipo"))

  df_long <- df %>%
    select(year, curso, tipo, all_of(ejes)) %>%
    tidyr::pivot_longer(cols = all_of(ejes), names_to = "eje", values_to = "valor") %>%
    group_by(.data$year, .data$curso, .data$tipo, .data$eje) %>%
    summarise(valor = mean(.data$valor, na.rm = TRUE), .groups = "drop")

  fill_pal <- palette_values(7, palette_fill)

  p <- ggplot(df_long, aes(x = .data[[axis_dim]], y = .data$eje, fill = .data$valor)) +
    geom_tile(color = "white", linewidth = 0.3) +
    scale_fill_gradientn(colors = fill_pal, labels = scales::label_percent(scale = 1), name = "Promedio (%)") +
    labs(x = if (identical(axis_dim, "curso")) "Curso" else "Tipo", y = "Eje") +
    plot_theme +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))

  facet <- facet %||% "off"
  if (identical(facet, "year")) {
    p <- p + facet_wrap(~year)
  } else if (identical(facet, "tipo") && !identical(axis_dim, "tipo")) {
    p <- p + facet_wrap(~tipo)
  } else if (identical(facet, "curso") && !identical(axis_dim, "curso")) {
    p <- p + facet_wrap(~curso)
  } else if (identical(facet, "eje")) {
    p <- p + facet_wrap(~eje)
  }

  p
}
