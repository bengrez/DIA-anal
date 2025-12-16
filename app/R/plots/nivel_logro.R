plot_nivel_logro <- function(df, facet_row, facet_col, palette_fill, alpha_bars, plot_theme) {
  df_count <- df %>%
    mutate(nivel_logro_chr = as.character(.data$nivel_logro)) %>%
    filter(!is.na(.data$nivel_logro_chr), nzchar(.data$nivel_logro_chr)) %>%
    count(.data$year, .data$curso, .data$tipo, .data$nivel_logro, name = "n")

  nivel_chr <- as.character(df$nivel_logro)
  base_levels <- sort(unique(nivel_chr[!is.na(nivel_chr) & nzchar(nivel_chr)]))
  if (is.factor(df$nivel_logro)) {
    base_levels <- levels(droplevels(df$nivel_logro))
  }
  df_count$nivel_logro <- factor(df_count$nivel_logro, levels = base_levels)
  levels_fill <- levels(df_count$nivel_logro)
  colors <- palette_values(length(levels_fill), palette_fill)
  names(colors) <- levels_fill

  facet_row <- facet_row %||% "off"
  facet_col <- facet_col %||% "off"

  facet_cols <- c(facet_key_to_col(facet_row), facet_key_to_col(facet_col))
  facet_cols <- unique(facet_cols[!is.null(facet_cols)])

  # Definir X según qué variables ya están en facets (para evitar barras mezcladas).
  x_parts <- c("curso")
  if (!("tipo" %in% facet_cols)) x_parts <- c(x_parts, "tipo")
  if (!("year" %in% facet_cols)) x_parts <- c(x_parts, "year")

  if (length(x_parts) == 1) {
    df_count <- df_count %>% mutate(x_label = .data$curso)
    x_lab <- "Curso"
  } else {
    df_count$x_label <- do.call(interaction, c(df_count[x_parts], list(sep = " | ", drop = TRUE)))
    x_lab <- paste(tools::toTitleCase(x_parts), collapse = " | ")
  }

  p <- ggplot(df_count, aes(x = .data$x_label, y = .data$n, fill = .data$nivel_logro)) +
    geom_col(position = "fill", alpha = alpha_bars %||% 0.85) +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = colors) +
    labs(x = x_lab, y = "Proporción", fill = "Nivel de logro") +
    plot_theme +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))

  apply_facets(p, data = df_count, facet_row = facet_row, facet_col = facet_col)
}
