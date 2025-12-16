table_promedio <- function(df, ejes) {
  ejes <- unique(as.character(ejes %||% character()))
  if (length(ejes) == 0) {
    stop("Selecciona al menos 1 eje.", call. = FALSE)
  }

  df %>%
    select(year, curso, tipo, all_of(ejes)) %>%
    tidyr::pivot_longer(cols = all_of(ejes), names_to = "eje", values_to = "valor") %>%
    group_by(.data$year, .data$curso, .data$tipo, .data$eje) %>%
    summarise(
      n = sum(!is.na(.data$valor)),
      promedio = mean(.data$valor, na.rm = TRUE),
      mediana = median(.data$valor, na.rm = TRUE),
      sd = stats::sd(.data$valor, na.rm = TRUE),
      p25 = stats::quantile(.data$valor, probs = 0.25, na.rm = TRUE, names = FALSE),
      p75 = stats::quantile(.data$valor, probs = 0.75, na.rm = TRUE, names = FALSE),
      .groups = "drop"
    ) %>%
    arrange(.data$year, .data$curso, .data$eje, .data$tipo)
}

table_distribucion_stats <- function(df, ejes) {
  ejes <- unique(as.character(ejes %||% character()))
  if (length(ejes) == 0) {
    stop("Selecciona al menos 1 eje.", call. = FALSE)
  }

  df %>%
    select(year, curso, tipo, all_of(ejes)) %>%
    tidyr::pivot_longer(cols = all_of(ejes), names_to = "eje", values_to = "valor") %>%
    group_by(.data$year, .data$curso, .data$tipo, .data$eje) %>%
    summarise(
      n = sum(!is.na(.data$valor)),
      promedio = mean(.data$valor, na.rm = TRUE),
      mediana = median(.data$valor, na.rm = TRUE),
      sd = stats::sd(.data$valor, na.rm = TRUE),
      p25 = stats::quantile(.data$valor, probs = 0.25, na.rm = TRUE, names = FALSE),
      p75 = stats::quantile(.data$valor, probs = 0.75, na.rm = TRUE, names = FALSE),
      .groups = "drop"
    ) %>%
    arrange(.data$year, .data$curso, .data$eje, .data$tipo)
}

table_distribucion_corr <- function(df, ejes) {
  ejes <- unique(as.character(ejes %||% character()))
  if (length(ejes) < 2) {
    return(tibble::tibble())
  }

  eje_x <- ejes[[1]]
  eje_y <- ejes[[2]]

  df_pairs <- df %>%
    select(year, curso, all_of(c(eje_x, eje_y))) %>%
    filter(stats::complete.cases(.))

  if (nrow(df_pairs) == 0) {
    return(tibble::tibble())
  }

  df_pairs %>%
    group_by(.data$year, .data$curso) %>%
    summarise(
      n = n(),
      correlacion = stats::cor(.data[[eje_x]], .data[[eje_y]]),
      pendiente = stats::coef(stats::lm(.data[[eje_y]] ~ .data[[eje_x]]))[[2]],
      intercepto = stats::coef(stats::lm(.data[[eje_y]] ~ .data[[eje_x]]))[[1]],
      .groups = "drop"
    ) %>%
    mutate(eje_x = eje_x, eje_y = eje_y) %>%
    select(.data$year, .data$curso, .data$eje_x, .data$eje_y, .data$n, .data$correlacion, .data$pendiente, .data$intercepto)
}

table_nivel_logro <- function(df) {
  df_count <- df %>%
    mutate(nivel_chr = as.character(.data$nivel_logro)) %>%
    filter(!is.na(.data$nivel_chr), nzchar(.data$nivel_chr)) %>%
    count(.data$year, .data$curso, .data$tipo, .data$nivel_logro, name = "n")

  df_count %>%
    group_by(.data$year, .data$curso, .data$tipo) %>%
    mutate(proporcion = .data$n / sum(.data$n)) %>%
    ungroup() %>%
    arrange(.data$year, .data$curso, .data$tipo, .data$nivel_logro)
}

table_crecimiento <- function(df, eje, tipo_a, tipo_b) {
  calc_delta_por_estudiante(df, eje = eje, tipo_a = tipo_a, tipo_b = tipo_b) %>%
    arrange(.data$year, .data$curso, desc(.data$delta))
}

current_table <- function(
    df,
    plot_type,
    ejes = NULL,
    dist_kind = NULL,
    tipo_a = NULL,
    tipo_b = NULL,
    show_corr = FALSE
) {
  if (identical(plot_type, "promedio")) {
    return(table_promedio(df, ejes = ejes))
  }
  if (identical(plot_type, "distribucion")) {
    stats <- table_distribucion_stats(df, ejes = ejes)
    if (isTRUE(show_corr) && length(ejes %||% character()) >= 2) {
      corr <- table_distribucion_corr(df, ejes = ejes)
      stats <- dplyr::bind_rows(
        dplyr::mutate(stats, tabla = "resumen"),
        dplyr::mutate(corr, tabla = "correlacion")
      )
    }
    # Para informes suele ser más útil el resumen estadístico.
    return(stats)
  }
  if (identical(plot_type, "nivel_logro")) {
    return(table_nivel_logro(df))
  }
  if (identical(plot_type, "crecimiento")) {
    if (is.null(ejes) || length(ejes) == 0) stop("Selecciona un eje.", call. = FALSE)
    eje1 <- as.character(ejes)[[1]]
    return(table_crecimiento(df, eje = eje1, tipo_a = tipo_a, tipo_b = tipo_b))
  }
  data.frame()
}

