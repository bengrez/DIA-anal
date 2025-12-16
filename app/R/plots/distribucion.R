plot_distribucion <- function(
    df,
    ejes,
    kind,
    facet,
    palette_fill,
    palette_color,
    alpha_bars,
    alpha_lines,
    plot_theme,
    show_mean = FALSE,
    show_median = FALSE,
    show_band = FALSE,
    band_metric = c("p25p75", "sd"),
    label_stats = FALSE,
    show_corr = FALSE
) {
  ejes <- unique(as.character(ejes %||% character()))
  if (length(ejes) == 0) {
    stop("Selecciona al menos 1 eje.", call. = FALSE)
  }

  band_metric <- match.arg(band_metric)

  df_plot <- df %>%
    select(year, curso, tipo, all_of(ejes)) %>%
    tidyr::pivot_longer(cols = all_of(ejes), names_to = "eje", values_to = "valor")

  stats_grouping <- list()
  if (identical(facet, "eje")) stats_grouping <- c(stats_grouping, rlang::sym("eje"))
  if (identical(facet, "year")) stats_grouping <- c(stats_grouping, rlang::sym("year"))
  if (identical(facet, "curso")) stats_grouping <- c(stats_grouping, rlang::sym("curso"))
  if (identical(facet, "tipo")) stats_grouping <- c(stats_grouping, rlang::sym("tipo"))

  df_stats <- df_plot %>%
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

  band_stats <- df_stats %>%
    mutate(
      low = if (identical(band_metric, "p25p75")) .data$p25 else .data$promedio - (.data$sd %||% 0),
      high = if (identical(band_metric, "p25p75")) .data$p75 else .data$promedio + (.data$sd %||% 0)
    )

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

    if (isTRUE(show_band)) {
      p <- p + geom_rect(
        data = band_stats,
        aes(xmin = .data$low, xmax = .data$high),
        ymin = -Inf, ymax = Inf,
        alpha = 0.08,
        inherit.aes = FALSE,
        fill = "grey70"
      )
    }

    if (isTRUE(show_mean)) {
      p <- p + geom_vline(data = df_stats, aes(xintercept = .data$promedio), linetype = "dashed", color = "#2c3e50")
    }

    if (isTRUE(show_median)) {
      p <- p + geom_vline(data = df_stats, aes(xintercept = .data$mediana), linetype = "dotted", color = "#8e44ad")
    }

    if (isTRUE(label_stats)) {
      label_df <- df_plot %>%
        group_by(.data$year, .data$curso, .data$tipo, .data$eje) %>%
        summarise(
          n = sum(!is.na(.data$valor)),
          mediana = median(.data$valor, na.rm = TRUE),
          p25 = stats::quantile(.data$valor, probs = 0.25, na.rm = TRUE, names = FALSE),
          p75 = stats::quantile(.data$valor, probs = 0.75, na.rm = TRUE, names = FALSE),
          .groups = "drop"
        )

      p <- p + geom_text(
        data = label_df,
        aes(x = .data$mediana, y = Inf, label = paste0("n=", .data$n, " | P25-75: ", scales::label_percent(accuracy = 0.1, scale = 1)(.data$p25), "–", scales::label_percent(accuracy = 0.1, scale = 1)(.data$p75))),
        vjust = 1.2,
        size = 3,
        color = "#34495e",
        inherit.aes = FALSE
      )
    }

    if (identical(facet, "curso")) {
      p <- p + facet_wrap(~curso)
    } else if (identical(facet, "tipo")) {
      p <- p + facet_wrap(~tipo)
    } else if (identical(facet, "year")) {
      p <- p + facet_wrap(~year)
    } else if (identical(facet, "eje")) {
      p <- p + facet_wrap(~eje)
    }

    if (isTRUE(show_corr) && length(ejes) >= 2) {
      cor_df <- df %>%
        select(all_of(ejes[1:2])) %>%
        filter(stats::complete.cases(.))
      if (nrow(cor_df) >= 2) {
        cor_val <- stats::cor(cor_df[[1]], cor_df[[2]])
        fit <- stats::lm(cor_df[[2]] ~ cor_df[[1]])
        lbl <- paste0(
          "Cor: ", round(cor_val, 3),
          " | β: ", round(stats::coef(fit)[[2]], 3)
        )
        p <- p + annotate("text", x = -Inf, y = Inf, label = lbl, hjust = -0.1, vjust = 1.2, color = "#2c3e50")
      }
    }
    return(p)
  }

  p <- ggplot(df_plot, aes(x = .data$curso, y = .data$valor, fill = .data$tipo)) +
    geom_boxplot(outlier.alpha = 0.25, width = 0.7, alpha = alpha_bars %||% 0.85) +
    scale_y_continuous(labels = scales::label_percent(scale = 1)) +
    scale_fill_manual(values = colors) +
    labs(x = "Curso", y = "Porcentaje (%)", fill = "Tipo") +
    plot_theme +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))

  if (isTRUE(show_band)) {
    p <- p + geom_rect(
      data = band_stats,
      aes(ymin = .data$low, ymax = .data$high),
      xmin = -Inf, xmax = Inf,
      alpha = 0.08,
      inherit.aes = FALSE,
      fill = "grey70"
    )
  }

  if (isTRUE(show_mean)) {
    p <- p + geom_hline(data = df_stats, aes(yintercept = .data$promedio), linetype = "dashed", color = "#2c3e50")
  }

  if (isTRUE(show_median)) {
    p <- p + geom_hline(data = df_stats, aes(yintercept = .data$mediana), linetype = "dotted", color = "#8e44ad")
  }

  if (isTRUE(label_stats)) {
    labels_df <- df_plot %>%
      group_by(.data$year, .data$curso, .data$tipo, .data$eje) %>%
      summarise(
        n = sum(!is.na(.data$valor)),
        mediana = median(.data$valor, na.rm = TRUE),
        p25 = stats::quantile(.data$valor, probs = 0.25, na.rm = TRUE, names = FALSE),
        p75 = stats::quantile(.data$valor, probs = 0.75, na.rm = TRUE, names = FALSE),
        .groups = "drop"
      ) %>%
      mutate(label = paste0("n=", .data$n, "\nP25-75: ", scales::label_percent(accuracy = 0.1, scale = 1)(.data$p25), "–", scales::label_percent(accuracy = 0.1, scale = 1)(.data$p75)))

    p <- p + geom_text(
      data = labels_df,
      aes(label = .data$label),
      position = position_dodge(width = 0.8),
      vjust = -0.35,
      size = 3,
      color = "#34495e"
    )
  }

  if (identical(facet, "tipo")) {
    p <- p + facet_wrap(~tipo)
  } else if (identical(facet, "curso")) {
    p <- p + facet_wrap(~curso)
  } else if (identical(facet, "year")) {
    p <- p + facet_wrap(~year)
  } else if (identical(facet, "eje")) {
    p <- p + facet_wrap(~eje)
  }
  p
}
