# ------------------------------------------------------------
# Plot extra — Heatmap (curso/tipo × eje)
#
# Útil cuando hay muchos ejes y se quiere ver un "mapa" de promedios.
# (No es parte del MVP original, pero se mantiene por utilidad.)
# ------------------------------------------------------------

plot_heatmap <- function(df, ejes, facet_row, facet_col, axis_dim = "curso", palette_fill, plot_theme) {
  ejes <- unique(as.character(ejes %||% character()))
  if (length(ejes) == 0) {
    stop("Selecciona al menos 1 eje.", call. = FALSE)
  }

  axis_dim <- match.arg(axis_dim, c("curso", "tipo"))

  df_long <- df %>%
    select(year, area, curso, tipo, all_of(ejes)) %>%
    tidyr::pivot_longer(cols = all_of(ejes), names_to = "eje", values_to = "valor") %>%
    group_by(.data$year, .data$area, .data$curso, .data$tipo, .data$eje) %>%
    summarise(valor = mean(.data$valor, na.rm = TRUE), .groups = "drop")

  fill_pal <- palette_values(7, palette_fill)

  p <- ggplot(df_long, aes(x = .data[[axis_dim]], y = .data$eje, fill = .data$valor)) +
    geom_tile(color = "white", linewidth = 0.3) +
    scale_fill_gradientn(colors = fill_pal, labels = scales::label_percent(scale = 1), name = "Promedio (%)") +
    labs(x = if (identical(axis_dim, "curso")) "Curso" else "Tipo", y = "Eje") +
    plot_theme +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))

  if (identical(facet_row, axis_dim)) facet_row <- "off"
  if (identical(facet_col, axis_dim)) facet_col <- "off"
  apply_facets(p, data = df_long, facet_row = facet_row %||% "off", facet_col = facet_col %||% "off")
}
