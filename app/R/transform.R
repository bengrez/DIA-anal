apply_anonymity <- function(df, anonymous = FALSE, method = "n_lista", seed = 1234) {
  if (!isTRUE(anonymous)) {
    return(df)
  }

  method <- method %||% "n_lista"
  if (!method %in% c("n_lista", "random")) {
    method <- "n_lista"
  }

  if (identical(method, "n_lista")) {
    df$nombre_estudiante <- paste0("ID_", df$n_lista)
    return(df)
  }

  # IDs aleatorios pero reproducibles (por semilla) para evitar exponer nombres.
  key <- paste(df$curso, df$n_lista, sep = "__")
  keys <- unique(key)
  keys <- sort(keys)

  chars <- c(LETTERS, 0:9)
  seed <- suppressWarnings(as.integer(seed))
  if (is.na(seed)) seed <- 1234
  set.seed(seed)

  make_code <- function() paste0(sample(chars, size = 6, replace = TRUE), collapse = "")
  codes <- vapply(seq_along(keys), function(i) make_code(), character(1))
  # Reintenta si hay colisiones (poco probable, pero posible).
  tries <- 0
  while (anyDuplicated(codes) > 0 && tries < 20) {
    dup_idx <- which(duplicated(codes))
    codes[dup_idx] <- vapply(dup_idx, function(i) make_code(), character(1))
    tries <- tries + 1
  }
  if (anyDuplicated(codes) > 0) {
    # Fallback determinístico.
    codes <- sprintf("%06d", seq_along(keys))
  }

  map <- setNames(codes, keys)
  df$nombre_estudiante <- paste0("ID_", unname(map[key]))
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
      year = .data$year,
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
    group_by(.data$fuente, .data$year, .data$curso, .data$n_lista, .data$nombre_estudiante, .data$tipo_comp) %>%
    summarise(valor = mean(.data$valor, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = "tipo_comp", values_from = "valor") %>%
    rename(valor_A = A, valor_B = B) %>%
    mutate(delta = .data$valor_B - .data$valor_A)

  df_wide
}
