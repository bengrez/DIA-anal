table_promedio <- function(df, ejes) {
  ejes <- unique(as.character(ejes %||% character()))
  if (length(ejes) == 0) {
    stop("Selecciona al menos 1 eje.", call. = FALSE)
  }

  df %>%
    select(year, area, curso, tipo, all_of(ejes)) %>%
    tidyr::pivot_longer(cols = all_of(ejes), names_to = "eje", values_to = "valor") %>%
    group_by(.data$year, .data$area, .data$curso, .data$tipo, .data$eje) %>%
    summarise(
      n = sum(!is.na(.data$valor)),
      promedio = mean(.data$valor, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(.data$year, .data$area, .data$curso, .data$eje, .data$tipo)
}

table_distribucion_stats <- function(df, ejes) {
  ejes <- unique(as.character(ejes %||% character()))
  if (length(ejes) == 0) {
    stop("Selecciona al menos 1 eje.", call. = FALSE)
  }

  df %>%
    select(year, area, curso, tipo, all_of(ejes)) %>%
    tidyr::pivot_longer(cols = all_of(ejes), names_to = "eje", values_to = "valor") %>%
    group_by(.data$year, .data$area, .data$curso, .data$tipo, .data$eje) %>%
    summarise(
      n = sum(!is.na(.data$valor)),
      promedio = mean(.data$valor, na.rm = TRUE),
      mediana = median(.data$valor, na.rm = TRUE),
      sd = stats::sd(.data$valor, na.rm = TRUE),
      p25 = stats::quantile(.data$valor, probs = 0.25, na.rm = TRUE, names = FALSE),
      p75 = stats::quantile(.data$valor, probs = 0.75, na.rm = TRUE, names = FALSE),
      .groups = "drop"
    ) %>%
    arrange(.data$year, .data$area, .data$curso, .data$eje, .data$tipo)
}

table_nivel_logro <- function(df) {
  df_count <- df %>%
    mutate(nivel_chr = as.character(.data$nivel_logro)) %>%
    filter(!is.na(.data$nivel_chr), nzchar(.data$nivel_chr)) %>%
    count(.data$year, .data$area, .data$curso, .data$tipo, .data$nivel_logro, name = "n")

  df_count %>%
    group_by(.data$year, .data$area, .data$curso, .data$tipo) %>%
    mutate(proporcion = .data$n / sum(.data$n)) %>%
    ungroup() %>%
    arrange(.data$year, .data$area, .data$curso, .data$tipo, .data$nivel_logro)
}

table_crecimiento <- function(df, eje, tipo_a, tipo_b) {
  calc_delta_por_estudiante(df, eje = eje, tipo_a = tipo_a, tipo_b = tipo_b) %>%
    arrange(.data$year, .data$area, .data$curso, desc(.data$delta))
}

table_heatmap <- function(df, ejes, axis_dim = "curso") {
  ejes <- unique(as.character(ejes %||% character()))
  if (length(ejes) == 0) {
    stop("Selecciona al menos 1 eje.", call. = FALSE)
  }

  axis_dim <- match.arg(axis_dim, c("curso", "tipo"))

  df %>%
    select(year, area, curso, tipo, all_of(ejes)) %>%
    tidyr::pivot_longer(cols = all_of(ejes), names_to = "eje", values_to = "valor") %>%
    group_by(.data$year, .data$area, .data[[axis_dim]], .data$eje) %>%
    summarise(n = sum(!is.na(.data$valor)), promedio = mean(.data$valor, na.rm = TRUE), .groups = "drop") %>%
    arrange(.data$year, .data$area, .data$eje, .data[[axis_dim]])
}

table_violin <- function(df, ejes, group_var = "curso") {
  ejes <- unique(as.character(ejes %||% character()))
  if (length(ejes) == 0) {
    stop("Selecciona al menos 1 eje.", call. = FALSE)
  }

  group_var <- match.arg(group_var, c("curso", "tipo"))

  df %>%
    select(year, area, curso, tipo, all_of(ejes)) %>%
    tidyr::pivot_longer(cols = all_of(ejes), names_to = "eje", values_to = "valor") %>%
    group_by(.data$year, .data$area, .data[[group_var]], .data$eje) %>%
    summarise(
      n = sum(!is.na(.data$valor)),
      promedio = mean(.data$valor, na.rm = TRUE),
      mediana = median(.data$valor, na.rm = TRUE),
      sd = stats::sd(.data$valor, na.rm = TRUE),
      p25 = stats::quantile(.data$valor, probs = 0.25, na.rm = TRUE, names = FALSE),
      p75 = stats::quantile(.data$valor, probs = 0.75, na.rm = TRUE, names = FALSE),
      .groups = "drop"
    ) %>%
    arrange(.data$year, .data$area, .data$eje, .data[[group_var]])
}

table_tendencia <- function(df, ejes, group_var = "curso") {
  ejes <- unique(as.character(ejes %||% character()))
  if (length(ejes) == 0) {
    stop("Selecciona al menos 1 eje.", call. = FALSE)
  }

  group_var <- match.arg(group_var, c("curso", "tipo"))

  df %>%
    select(year, area, curso, tipo, all_of(ejes)) %>%
    tidyr::pivot_longer(cols = all_of(ejes), names_to = "eje", values_to = "valor") %>%
    group_by(.data$year, .data$area, .data[[group_var]], .data$eje) %>%
    summarise(
      n = sum(!is.na(.data$valor)),
      promedio = mean(.data$valor, na.rm = TRUE),
      sd = stats::sd(.data$valor, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(.data$year, .data$area, .data$eje, .data[[group_var]])
}

current_table <- function(
    df,
    plot_type,
    ejes = NULL,
    dist_kind = NULL,
    tipo_a = NULL,
    tipo_b = NULL,
    heatmap_dim = NULL,
    violin_group = NULL,
    trend_group = NULL
) {
  if (identical(plot_type, "promedio")) {
    return(table_promedio(df, ejes = ejes))
  }
  if (identical(plot_type, "distribucion")) {
    # Para informes suele ser más útil el resumen estadístico.
    return(table_distribucion_stats(df, ejes = ejes))
  }
  if (identical(plot_type, "heatmap")) {
    return(table_heatmap(df, ejes = ejes, axis_dim = heatmap_dim %||% "curso"))
  }
  if (identical(plot_type, "violin")) {
    return(table_violin(df, ejes = ejes, group_var = violin_group %||% "curso"))
  }
  if (identical(plot_type, "tendencia")) {
    return(table_tendencia(df, ejes = ejes, group_var = trend_group %||% "curso"))
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
