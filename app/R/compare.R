compare_means <- function(df, eje, fuente_a, fuente_b) {
  if (is.null(fuente_a) || is.null(fuente_b) || !nzchar(fuente_a) || !nzchar(fuente_b)) {
    stop("Selecciona Fuente A y Fuente B.", call. = FALSE)
  }
  if (identical(fuente_a, fuente_b)) {
    stop("Fuente A y Fuente B deben ser distintas.", call. = FALSE)
  }

  df_ab <- df %>%
    dplyr::filter(.data$fuente %in% c(fuente_a, fuente_b)) %>%
    dplyr::group_by(.data$fuente, .data$year, .data$curso, .data$tipo) %>%
    dplyr::summarise(
      n = sum(!is.na(.data[[eje]])),
      promedio = mean(.data[[eje]], na.rm = TRUE),
      .groups = "drop"
    )

  wide <- df_ab %>%
    tidyr::pivot_wider(names_from = "fuente", values_from = c("promedio", "n"))

  prom_a <- paste0("promedio_", fuente_a)
  prom_b <- paste0("promedio_", fuente_b)

  if (!prom_a %in% names(wide) || !prom_b %in% names(wide)) {
    stop("No hay datos suficientes para comparar las fuentes seleccionadas.", call. = FALSE)
  }

  wide %>%
    dplyr::mutate(delta = .data[[prom_b]] - .data[[prom_a]]) %>%
    dplyr::arrange(.data$year, .data$curso, .data$tipo)
}

plot_compare <- function(df_compare, fuente_a, fuente_b, view = "delta", facet = "off", alpha_bars = 0.85, plot_theme) {
  if (nrow(df_compare) == 0) {
    return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::ggtitle("Sin datos para comparar"))
  }

  view <- view %||% "delta"

  if (identical(view, "means")) {
    # Reconstruye formato largo desde columnas de promedios
    prom_cols <- grep("^promedio_", names(df_compare), value = TRUE)
    df_long <- df_compare %>%
      tidyr::pivot_longer(cols = dplyr::all_of(prom_cols), names_to = "fuente", values_to = "promedio") %>%
      dplyr::mutate(fuente = sub("^promedio_", "", .data$fuente))

    p <- ggplot2::ggplot(df_long, ggplot2::aes(x = .data$curso, y = .data$promedio, fill = .data$fuente)) +
      ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.7, alpha = alpha_bars %||% 0.85) +
      ggplot2::scale_y_continuous(labels = scales::label_percent(scale = 1)) +
      ggplot2::labs(x = "Curso", y = "Promedio (%)", fill = "Fuente") +
      plot_theme
  } else {
    p <- ggplot2::ggplot(df_compare, ggplot2::aes(x = .data$curso, y = .data$delta, fill = .data$tipo)) +
      ggplot2::geom_col(width = 0.7, alpha = alpha_bars %||% 0.85) +
      ggplot2::scale_y_continuous(labels = scales::label_number(accuracy = 0.1, suffix = " pp")) +
      ggplot2::labs(x = "Curso", y = paste0("Delta (", fuente_b, " - ", fuente_a, ")"), fill = "Tipo") +
      plot_theme +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
  }

  facet <- facet %||% "off"
  if (identical(facet, "tipo")) {
    p <- p + ggplot2::facet_wrap(~tipo)
  } else if (identical(facet, "curso")) {
    p <- p + ggplot2::facet_wrap(~curso)
  } else if (identical(facet, "year")) {
    p <- p + ggplot2::facet_wrap(~year)
  }

  p
}

