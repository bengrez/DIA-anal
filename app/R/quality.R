quality_axes_missing <- function(df) {
  axes <- detect_axes(df)
  if (length(axes) == 0) return(data.frame())

  out <- lapply(axes, function(ax) {
    v <- df[[ax]]
    n_total <- length(v)
    n_missing <- sum(is.na(v))
    data.frame(
      eje = ax,
      n_total = n_total,
      n_missing = n_missing,
      pct_missing = if (n_total > 0) (100 * n_missing / n_total) else NA_real_,
      check.names = FALSE
    )
  })
  dplyr::bind_rows(out) %>% dplyr::arrange(dplyr::desc(.data$pct_missing))
}

quality_axes_out_of_range <- function(df, min_val = 0, max_val = 100) {
  axes <- detect_axes(df)
  if (length(axes) == 0) return(data.frame())

  out <- lapply(axes, function(ax) {
    v <- df[[ax]]
    ok <- is.na(v) | (v >= min_val & v <= max_val)
    n_total <- length(v)
    n_bad <- sum(!ok, na.rm = TRUE)
    data.frame(
      eje = ax,
      n_total = n_total,
      n_fuera_rango = n_bad,
      pct_fuera_rango = if (n_total > 0) (100 * n_bad / n_total) else NA_real_,
      check.names = FALSE
    )
  })
  dplyr::bind_rows(out) %>% dplyr::arrange(dplyr::desc(.data$pct_fuera_rango))
}

quality_duplicates <- function(df) {
  key <- c("fuente", "year", "area", "curso", "tipo", "n_lista")
  missing <- setdiff(key, names(df))
  if (length(missing) > 0) return(data.frame())

  df %>%
    dplyr::count(dplyr::across(dplyr::all_of(key)), name = "n") %>%
    dplyr::filter(.data$n > 1) %>%
    dplyr::arrange(dplyr::desc(.data$n))
}

quality_summary <- function(df) {
  list(
    axes_missing = quality_axes_missing(df),
    axes_out_of_range = quality_axes_out_of_range(df),
    duplicates = quality_duplicates(df)
  )
}
