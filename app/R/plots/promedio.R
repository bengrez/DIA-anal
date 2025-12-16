plot_promedio <- function(
    df,
    ejes,
    facet,
    palette_fill,
    alpha_bars,
    plot_theme,
    show_mean = FALSE,
    show_median = FALSE,
    show_band = FALSE,
    band_metric = c("p25p75", "sd"),
    label_stats = FALSE
) {
  ejes <- unique(as.character(ejes %||% character()))
  if (length(ejes) == 0) {
    stop("Selecciona al menos 1 eje.", call. = FALSE)
  }

  band_metric <- match.arg(band_metric)

  df_long <- df %>%
    select(year, curso, tipo, all_of(ejes)) %>%
    tidyr::pivot_longer(cols = all_of(ejes), names_to = "eje", values_to = "valor")

  df_sum <- df_long %>%
    group_by(.data$year, .data$curso, .data$tipo, .data$eje) %>%
    summarise(valor = mean(.data$valor, na.rm = TRUE), .groups = "drop")

  stats_grouping <- list()
  if (identical(facet, "eje")) stats_grouping <- c(stats_grouping, rlang::sym("eje"))
  if (identical(facet, "year")) stats_grouping <- c(stats_grouping, rlang::sym("year"))
  if (identical(facet, "curso")) stats_grouping <- c(stats_grouping, rlang::sym("curso"))

  df_stats <- df_long %>%
    group_by(!!!stats_grouping) %>%
    summarise(
      n = sum(!is.na(.data$valor)),
      promedio = mean(.data$valor, na.rm = TRUE),
      mediana = median(.data$valor, na.rm = TRUE),
      sd = dplyr::coalesce(stats::sd(.data$valor, na.rm = TRUE), 0),
      p25 = stats::quantile(.data$valor, probs = 0.25, na.rm = TRUE, names = FALSE),
      p75 = stats::quantile(.data$valor, probs = 0.75, na.rm = TRUE, names = FALSE),
      .groups = "drop"
    )

  bar_stats <- df_long %>%
    group_by(.data$year, .data$curso, .data$tipo, .data$eje) %>%
    summarise(
      n = sum(!is.na(.data$valor)),
      p25 = stats::quantile(.data$valor, probs = 0.25, na.rm = TRUE, names = FALSE),
      p75 = stats::quantile(.data$valor, probs = 0.75, na.rm = TRUE, names = FALSE),
      mediana = median(.data$valor, na.rm = TRUE),
      .groups = "drop"
    )

  band_stats <- df_stats %>%
    mutate(
      ymin = if (identical(band_metric, "p25p75")) .data$p25 else .data$promedio - (.data$sd %||% 0),
      ymax = if (identical(band_metric, "p25p75")) .data$p75 else .data$promedio + (.data$sd %||% 0)
    )

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

  if (isTRUE(show_band)) {
    p <- p + geom_rect(
      data = band_stats,
      aes(ymin = .data$ymin, ymax = .data$ymax),
      xmin = -Inf, xmax = Inf,
      alpha = 0.08,
      inherit.aes = FALSE,
      fill = "grey60"
    )
  }

  if (isTRUE(show_mean)) {
    p <- p + geom_hline(data = df_stats, aes(yintercept = .data$promedio), linetype = "dashed", color = "#2c3e50")
  }

  if (isTRUE(show_median)) {
    p <- p + geom_hline(data = df_stats, aes(yintercept = .data$mediana), linetype = "dotted", color = "#8e44ad")
  }

  if (isTRUE(label_stats)) {
    labels_df <- df_sum %>%
      left_join(bar_stats, by = c("year", "curso", "tipo", "eje")) %>%
      mutate(label = paste0("n=", .data$n, "\nP25-75: ", scales::label_percent(accuracy = 0.1, scale = 1)(.data$p25), "–", scales::label_percent(accuracy = 0.1, scale = 1)(.data$p75)))

    p <- p + geom_text(
      data = labels_df,
      aes(label = .data$label),
      position = position_dodge(width = 0.8),
      vjust = -0.3,
      size = 3,
      color = "#34495e"
    )
  }

  facet <- facet %||% "off"
  if (identical(facet, "eje")) {
    p <- p + facet_wrap(~eje)
  } else if (identical(facet, "year")) {
    p <- p + facet_wrap(~year)
  } else if (identical(facet, "tipo")) {
    p <- p + facet_wrap(~tipo)
  } else if (identical(facet, "curso")) {
    p <- p + facet_wrap(~curso)
  }

  p
}
