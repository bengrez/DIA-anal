plot_crecimiento <- function(
    df,
    eje,
    tipo_a,
    tipo_b,
    kind,
    rank_mode,
    rank_n,
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
    label_stats = FALSE
) {
  band_metric <- match.arg(band_metric)

  df_delta <- calc_delta_por_estudiante(df, eje = eje, tipo_a = tipo_a, tipo_b = tipo_b) %>%
    mutate(
      direccion = dplyr::case_when(
        is.na(.data$delta) ~ "Sin dato",
        .data$delta >= 0 ~ "Mejora",
        TRUE ~ "Baja"
      )
    )

  df_delta <- df_delta %>%
    arrange(desc(.data$delta))

  if (!identical(rank_mode, "all")) {
    n <- as.integer(rank_n %||% 10)
    n <- max(n, 1)

    if (identical(rank_mode, "top")) {
      df_delta <- df_delta %>% slice_head(n = n)
    } else if (identical(rank_mode, "bottom")) {
      df_delta <- df_delta %>% arrange(.data$delta) %>% slice_head(n = n)
    } else if (identical(rank_mode, "both")) {
      top <- df_delta %>% slice_head(n = n)
      bottom <- df_delta %>% arrange(.data$delta) %>% slice_head(n = n)
      df_delta <- bind_rows(top, bottom) %>% distinct(.data$curso, .data$n_lista, .keep_all = TRUE)
    }
  }

  # Paletas
  fill_levels <- sort(unique(df_delta$direccion))
  fill_colors <- palette_values(length(fill_levels), palette_fill)
  names(fill_colors) <- fill_levels

  color_levels <- fill_levels
  color_colors <- palette_values(length(color_levels), palette_color)
  names(color_colors) <- color_levels

  if (identical(kind, "slope")) {
    df_long <- df_delta %>%
      select(year, curso, n_lista, nombre_estudiante, direccion, valor_A, valor_B) %>%
      pivot_longer(
        cols = c("valor_A", "valor_B"),
      names_to = "momento",
      values_to = "valor"
    ) %>%
      mutate(
        momento = factor(.data$momento, levels = c("valor_A", "valor_B"), labels = c(tipo_a, tipo_b))
      )

    delta_stats <- df_delta %>%
      summarise(
        n = sum(!is.na(.data$delta)),
        promedio = mean(.data$delta, na.rm = TRUE),
        mediana = median(.data$delta, na.rm = TRUE),
        sd = dplyr::coalesce(stats::sd(.data$delta, na.rm = TRUE), 0),
        p25 = stats::quantile(.data$delta, probs = 0.25, na.rm = TRUE, names = FALSE),
        p75 = stats::quantile(.data$delta, probs = 0.75, na.rm = TRUE, names = FALSE)
      ) %>%
      mutate(low = if (identical(band_metric, "p25p75")) .data$p25 else .data$promedio - (.data$sd %||% 0), high = if (identical(band_metric, "p25p75")) .data$p75 else .data$promedio + (.data$sd %||% 0))

    p <- ggplot(df_long, aes(x = .data$momento, y = .data$valor, group = .data$nombre_estudiante, color = .data$direccion)) +
      geom_line(alpha = alpha_lines %||% 0.8) +
      geom_point(size = 2, alpha = alpha_lines %||% 0.9) +
      scale_y_continuous(labels = scales::label_percent(scale = 1)) +
      scale_color_manual(values = color_colors) +
      labs(x = NULL, y = paste0(eje, " (%)"), color = NULL) +
      plot_theme

    if (isTRUE(show_band)) {
      p <- p + annotate(
        "rect",
        xmin = -Inf,
        xmax = Inf,
        ymin = delta_stats$low,
        ymax = delta_stats$high,
        alpha = 0.08,
        fill = "grey70"
      )
    }

    if (isTRUE(show_mean)) {
      p <- p + geom_hline(yintercept = delta_stats$promedio, linetype = "dashed", color = "#2c3e50")
    }

    if (isTRUE(show_median)) {
      p <- p + geom_hline(yintercept = delta_stats$mediana, linetype = "dotted", color = "#8e44ad")
    }

    if (isTRUE(label_stats)) {
      lbl <- paste0(
        "n=", delta_stats$n,
        " | Mediana Δ: ", round(delta_stats$mediana, 1),
        " | P25-75: ", round(delta_stats$p25, 1), "–", round(delta_stats$p75, 1)
      )
      p <- p + annotate("text", x = -Inf, y = Inf, label = lbl, hjust = -0.1, vjust = 1.2, color = "#34495e")
    }

    if (identical(facet, "curso")) {
      p <- p + facet_wrap(~curso)
    } else if (identical(facet, "year")) {
      p <- p + facet_wrap(~year)
    }
    return(p)
  }

  df_delta <- df_delta %>%
    mutate(estudiante = paste0(.data$nombre_estudiante, " (", .data$n_lista, ")"))

  delta_stats <- df_delta %>%
    summarise(
      n = sum(!is.na(.data$delta)),
      promedio = mean(.data$delta, na.rm = TRUE),
      mediana = median(.data$delta, na.rm = TRUE),
      sd = dplyr::coalesce(stats::sd(.data$delta, na.rm = TRUE), 0),
      p25 = stats::quantile(.data$delta, probs = 0.25, na.rm = TRUE, names = FALSE),
      p75 = stats::quantile(.data$delta, probs = 0.75, na.rm = TRUE, names = FALSE)
    ) %>%
    mutate(low = if (identical(band_metric, "p25p75")) .data$p25 else .data$promedio - (.data$sd %||% 0), high = if (identical(band_metric, "p25p75")) .data$p75 else .data$promedio + (.data$sd %||% 0))

  p <- ggplot(df_delta, aes(x = reorder(.data$estudiante, .data$delta), y = .data$delta, fill = .data$direccion)) +
    geom_col(width = 0.75, alpha = alpha_bars %||% 0.85) +
    coord_flip() +
    scale_y_continuous(labels = scales::label_number(accuracy = 0.1, suffix = " pp")) +
    scale_fill_manual(values = fill_colors) +
    labs(x = NULL, y = paste0("Delta ", tipo_b, " - ", tipo_a, " (pp)"), fill = NULL) +
    plot_theme

  if (isTRUE(show_band)) {
    p <- p + geom_rect(
      data = delta_stats,
      aes(ymin = .data$low, ymax = .data$high),
      xmin = -Inf, xmax = Inf,
      inherit.aes = FALSE,
      alpha = 0.08,
      fill = "grey70"
    )
  }

  if (isTRUE(show_mean)) {
    p <- p + geom_hline(yintercept = delta_stats$promedio, linetype = "dashed", color = "#2c3e50")
  }

  if (isTRUE(show_median)) {
    p <- p + geom_hline(yintercept = delta_stats$mediana, linetype = "dotted", color = "#8e44ad")
  }

  if (isTRUE(label_stats)) {
    lbl <- paste0(
      "n=", delta_stats$n,
      " | Mediana Δ: ", round(delta_stats$mediana, 1),
      " | P25-75: ", round(delta_stats$p25, 1), "–", round(delta_stats$p75, 1)
    )
    p <- p + annotate("text", x = -Inf, y = Inf, label = lbl, hjust = -0.1, vjust = 1.2, color = "#34495e")
  }

  if (identical(facet, "curso")) {
    p <- p + facet_wrap(~curso, scales = "free_y")
  } else if (identical(facet, "year")) {
    p <- p + facet_wrap(~year, scales = "free_y")
  }
  p
}
