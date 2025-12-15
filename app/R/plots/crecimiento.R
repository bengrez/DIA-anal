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
    plot_theme
) {
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
      select(.data$curso, .data$n_lista, .data$nombre_estudiante, .data$direccion, .data$valor_A, .data$valor_B) %>%
      pivot_longer(
        cols = c("valor_A", "valor_B"),
        names_to = "momento",
        values_to = "valor"
      ) %>%
      mutate(
        momento = factor(.data$momento, levels = c("valor_A", "valor_B"), labels = c(tipo_a, tipo_b))
      )

    p <- ggplot(df_long, aes(x = .data$momento, y = .data$valor, group = .data$nombre_estudiante, color = .data$direccion)) +
      geom_line(alpha = 0.8) +
      geom_point(size = 2, alpha = 0.9) +
      scale_y_continuous(labels = scales::label_percent(scale = 1)) +
      scale_color_manual(values = color_colors) +
      labs(x = NULL, y = paste0(eje, " (%)"), color = NULL) +
      plot_theme

    if (identical(facet, "curso")) {
      p <- p + facet_wrap(~curso)
    }
    return(p)
  }

  df_delta <- df_delta %>%
    mutate(estudiante = paste0(.data$nombre_estudiante, " (", .data$n_lista, ")"))

  p <- ggplot(df_delta, aes(x = reorder(.data$estudiante, .data$delta), y = .data$delta, fill = .data$direccion)) +
    geom_col(width = 0.75) +
    coord_flip() +
    scale_y_continuous(labels = scales::label_number(accuracy = 0.1, suffix = " pp")) +
    scale_fill_manual(values = fill_colors) +
    labs(x = NULL, y = paste0("Delta ", tipo_b, " - ", tipo_a, " (pp)"), fill = NULL) +
    plot_theme

  if (identical(facet, "curso")) {
    p <- p + facet_wrap(~curso, scales = "free_y")
  }
  p
}
