plot_nivel_logro <- function(df, facet, palette_fill, plot_theme) {
  df_count <- df %>%
    filter(!is.na(.data$nivel_logro), nzchar(.data$nivel_logro)) %>%
    count(.data$year, .data$curso, .data$tipo, .data$nivel_logro, name = "n")

  df_count$nivel_logro <- factor(df_count$nivel_logro)
  levels_fill <- levels(df_count$nivel_logro)
  colors <- palette_values(length(levels_fill), palette_fill)
  names(colors) <- levels_fill

  if (identical(facet, "off")) {
    df_count <- df_count %>% mutate(curso_tipo = paste(.data$curso, .data$tipo, .data$year, sep = " | "))
    return(
      ggplot(df_count, aes(x = .data$curso_tipo, y = .data$n, fill = .data$nivel_logro)) +
        geom_col(position = "fill") +
        scale_y_continuous(labels = scales::percent) +
        scale_fill_manual(values = colors) +
        labs(x = "Curso | Tipo | Año", y = "Proporción", fill = "Nivel de logro") +
        plot_theme +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
    )
  }

  if (identical(facet, "curso")) {
    return(
      ggplot(df_count, aes(x = .data$tipo, y = .data$n, fill = .data$nivel_logro)) +
        geom_col(position = "fill") +
        scale_y_continuous(labels = scales::percent) +
        scale_fill_manual(values = colors) +
        facet_wrap(~curso) +
        labs(x = "Tipo", y = "Proporción", fill = "Nivel de logro") +
        plot_theme
    )
  }

  if (identical(facet, "year")) {
    return(
      ggplot(df_count, aes(x = .data$curso, y = .data$n, fill = .data$nivel_logro)) +
        geom_col(position = "fill") +
        scale_y_continuous(labels = scales::percent) +
        scale_fill_manual(values = colors) +
        facet_wrap(~year) +
        labs(x = "Curso", y = "Proporción", fill = "Nivel de logro") +
        plot_theme +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
    )
  }

  ggplot(df_count, aes(x = .data$curso, y = .data$n, fill = .data$nivel_logro)) +
    geom_col(position = "fill") +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = colors) +
    facet_wrap(~tipo) +
    labs(x = "Curso", y = "Proporción", fill = "Nivel de logro") +
    plot_theme +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
}
