apply_anonymity <- function(df, anonymous = FALSE) {
  if (!isTRUE(anonymous)) {
    return(df)
  }
  df$nombre_estudiante <- paste0("ID_", df$n_lista)
  df
}

pivot_axes_long <- function(df) {
  axes <- detect_axes(df)
  tidyr::pivot_longer(
    df,
    cols = dplyr::all_of(axes),
    names_to = "eje",
    values_to = "valor"
  )
}

calc_delta_por_estudiante <- function(df, eje, tipo_a, tipo_b) {
  axes <- detect_axes(df)
  if (!eje %in% axes) {
    stop("Eje inválido: ", eje, call. = FALSE)
  }

df2 <- df %>%
    filter(.data$tipo %in% c(tipo_a, tipo_b)) %>%
    transmute(
      fuente = .data$fuente,
      curso = .data$curso,
      n_lista = .data$n_lista,
      nombre_estudiante = .data$nombre_estudiante,
      tipo = .data$tipo,
      valor = .data[[eje]]
    ) %>%
    mutate(
      tipo_comp = dplyr::case_when(
        .data$tipo == tipo_a ~ "A",
        .data$tipo == tipo_b ~ "B",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(.data$tipo_comp))

  df_wide <- df2 %>%
    group_by(.data$fuente, .data$curso, .data$n_lista, .data$nombre_estudiante, .data$tipo_comp) %>%
    summarise(valor = mean(.data$valor, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = "tipo_comp", values_from = "valor") %>%
    rename(valor_A = A, valor_B = B) %>%
    mutate(delta = .data$valor_B - .data$valor_A)

  df_wide
}
