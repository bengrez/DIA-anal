# ------------------------------------------------------------
# Analítica avanzada — procesamiento (Phase 3)
#
# Este módulo implementa análisis “Phase 3” basados en el esquema real de los
# .xls de Ciencias Naturales disponibles hoy:
# - Por estudiante: ejes temáticos (% logro) + NIVEL DE LOGRO + metadata (year/tipo/curso)
#
# NOTA: funcionalidades que requieren habilidades/indicadores no se activan
# mientras no existan esas columnas en los datos importados.
# ------------------------------------------------------------

period_order_key <- function(tipo) {
  # Orden estable para periodos dentro del año.
  tipo_chr <- as.character(tipo %||% "")
  key <- rep(99L, length(tipo_chr))
  key[tipo_chr == "Diagnóstico"] <- 1L
  key[tipo_chr == "Monitoreo"] <- 2L
  key[tipo_chr == "Cierre"] <- 3L
  key
}

period_levels <- function(tipo) {
  tipo_chr <- as.character(tipo %||% "")
  present <- unique(stats::na.omit(tipo_chr))
  known <- c("Diagnóstico", "Monitoreo", "Cierre")
  lvls <- c(intersect(known, present), setdiff(present, known))
  factor(tipo_chr, levels = lvls)
}

identify_cohorts <- function(df, min_points = 2, period = NULL) {
  # Identifica “cohortes” como trayectorias de sección que avanzan 1 grado por año:
  # p.ej. 5°A (2023) -> 6°A (2024) -> 7°A (2025).
  #
  # Este tracking es a nivel de curso/sección (no matching individual).
  if (is.null(df) || nrow(df) == 0) return(list(cohorts = data.frame(), map = data.frame()))

  df2 <- add_science_fields(df)
  df2 <- df2 %>% dplyr::filter(!is.na(.data$grade), !is.na(.data$section), !is.na(.data$year))
  if (!is.null(period) && length(period) > 0 && any(nzchar(as.character(period)))) {
    df2 <- df2 %>% dplyr::filter(.data$tipo %in% as.character(period))
  }
  if (nrow(df2) == 0) return(list(cohorts = data.frame(), map = data.frame()))

  nodes <- df2 %>%
    dplyr::distinct(.data$section, .data$grade, .data$year, .data$course) %>%
    dplyr::arrange(.data$section, .data$year, .data$grade)

  key <- paste(nodes$section, nodes$grade, nodes$year, sep = "__")
  course_by_key <- stats::setNames(as.character(nodes$course), key)
  has_key <- stats::setNames(rep(TRUE, length(key)), key)

  pred_key <- paste(nodes$section, nodes$grade - 1, nodes$year - 1, sep = "__")
  starts <- nodes[!pred_key %in% key, , drop = FALSE]
  if (nrow(starts) == 0) return(list(cohorts = data.frame(), map = data.frame()))

  map_rows <- list()
  cohort_rows <- list()
  cohort_idx <- 0L

  for (i in seq_len(nrow(starts))) {
    section <- as.character(starts$section[[i]])
    grade0 <- as.integer(starts$grade[[i]])
    year0 <- as.integer(starts$year[[i]])

    grades <- integer()
    years <- integer()

    g <- grade0
    y <- year0
    while (TRUE) {
      k <- paste(section, g, y, sep = "__")
      if (is.na(has_key[k])) break
      grades <- c(grades, g)
      years <- c(years, y)
      g <- g + 1L
      y <- y + 1L
    }

    if (length(years) < as.integer(min_points)) next

    cohort_idx <- cohort_idx + 1L
    end_grade <- grades[[length(grades)]]
    end_year <- years[[length(years)]]
    cohort_id <- paste0(grade0, "°", section, " (", year0, "→", end_grade, "°", section, " ", end_year, ")")

    cohort_rows[[cohort_idx]] <- data.frame(
      cohort_id = cohort_id,
      section = section,
      start_grade = grade0,
      start_year = year0,
      end_grade = end_grade,
      end_year = end_year,
      n_points = length(years),
      stringsAsFactors = FALSE
    )

    map_rows[[cohort_idx]] <- data.frame(
      cohort_id = cohort_id,
      section = section,
      grade = grades,
      year = years,
      course = unname(course_by_key[paste(section, grades, years, sep = "__")]),
      time_index = seq_along(years),
      time_label = paste0(grades, "°-", years),
      stringsAsFactors = FALSE
    )
  }

  cohorts <- dplyr::bind_rows(cohort_rows)
  map <- dplyr::bind_rows(map_rows)

  list(cohorts = cohorts, map = map)
}

metric_options <- function(df) {
  # Métricas disponibles según columnas presentes.
  if (is.null(df) || nrow(df) == 0) return(character())
  axes <- detect_axes(df)
  out <- c("% Nivel III", "% Nivel I", "% Nivel II")
  if ("Promedio (todos los ejes)" %in% axes) out <- c(out, "Promedio (todos los ejes)")
  out <- c(out, setdiff(axes, "Promedio (todos los ejes)"))
  unique(out)
}

summarise_course_metric <- function(df, metric, period = NULL) {
  if (is.null(df) || nrow(df) == 0) return(data.frame())

  df2 <- add_science_fields(df) %>%
    dplyr::filter(!is.na(.data$year), !is.na(.data$course), !is.na(.data$grade), !is.na(.data$section))

  if (!is.null(period) && length(period) > 0) {
    df2 <- df2 %>% dplyr::filter(.data$tipo %in% as.character(period))
  }
  if (nrow(df2) == 0) return(data.frame())

  metric <- as.character(metric %||% "")
  if (!nzchar(metric)) return(data.frame())

  if (metric %in% c("% Nivel I", "% Nivel II", "% Nivel III")) {
    level <- gsub("^%\\s*", "", metric)
    df2 <- df2 %>% dplyr::mutate(nivel = normalize_nivel(.data$nivel_logro))
    df_out <- df2 %>%
      dplyr::group_by(.data$year, .data$tipo, .data$course, .data$grade, .data$section) %>%
      dplyr::summarise(
        n = dplyr::n(),
        value = 100 * mean(.data$nivel == level, na.rm = TRUE),
        .groups = "drop"
      )

    df_out %>%
      dplyr::mutate(
        se = 100 * sqrt((.data$value / 100) * (1 - (.data$value / 100)) / .data$n),
        ci_low = pmax(0, .data$value - 1.96 * .data$se),
        ci_high = pmin(100, .data$value + 1.96 * .data$se)
      )
  } else {
    axes <- detect_axes(df2)
    if (!metric %in% axes) stop("Métrica inválida: ", metric, call. = FALSE)

    df_out <- df2 %>%
      dplyr::group_by(.data$year, .data$tipo, .data$course, .data$grade, .data$section) %>%
      dplyr::summarise(
        n = dplyr::n(),
        value = mean(.data[[metric]], na.rm = TRUE),
        sd = stats::sd(.data[[metric]], na.rm = TRUE),
        .groups = "drop"
      )

    df_out %>%
      dplyr::mutate(
        se = .data$sd / sqrt(.data$n),
        ci_low = pmax(0, .data$value - 1.96 * .data$se),
        ci_high = pmin(100, .data$value + 1.96 * .data$se)
      ) %>%
      dplyr::select(-"sd")
  }
}

join_cohort_timepoints <- function(course_metric_tbl, cohort_map) {
  if (is.null(course_metric_tbl) || nrow(course_metric_tbl) == 0) return(data.frame())
  if (is.null(cohort_map) || nrow(cohort_map) == 0) return(data.frame())

  cohort_map %>%
    dplyr::inner_join(course_metric_tbl, by = c("year", "course", "grade", "section")) %>%
    dplyr::arrange(.data$cohort_id, .data$time_index)
}

cohort_levels_distribution <- function(df, cohort_map, period = NULL) {
  if (is.null(df) || nrow(df) == 0) return(data.frame())
  if (is.null(cohort_map) || nrow(cohort_map) == 0) return(data.frame())

  df2 <- add_science_fields(df)
  df2 <- df2 %>% dplyr::filter(!is.na(.data$year), !is.na(.data$course), !is.na(.data$nivel))
  if (!is.null(period) && length(period) > 0) {
    df2 <- df2 %>% dplyr::filter(.data$tipo %in% as.character(period))
  }
  if (nrow(df2) == 0) return(data.frame())

  levels_tbl <- df2 %>%
    dplyr::count(.data$year, .data$course, .data$grade, .data$section, .data$nivel, name = "n") %>%
    dplyr::group_by(.data$year, .data$course) %>%
    dplyr::mutate(pct = 100 * .data$n / sum(.data$n)) %>%
    dplyr::ungroup()

  cohort_map %>%
    dplyr::inner_join(levels_tbl, by = c("year", "course", "grade", "section")) %>%
    dplyr::mutate(nivel = factor(.data$nivel, levels = c("Nivel I", "Nivel II", "Nivel III"))) %>%
    dplyr::arrange(.data$cohort_id, .data$time_index, .data$nivel)
}

cramers_v <- function(chisq, n, r, c) {
  denom <- n * min(r - 1, c - 1)
  if (is.na(denom) || denom <= 0) return(NA_real_)
  sqrt(as.numeric(chisq) / denom)
}

chi_square_levels_by_grade <- function(df, year_a, year_b, period = NULL) {
  # Chi-cuadrado de distribución de Nivel I/II/III entre 2 años, por grado.
  if (is.null(df) || nrow(df) == 0) return(data.frame())

  df2 <- add_science_fields(df) %>%
    dplyr::filter(.data$year %in% c(as.integer(year_a), as.integer(year_b)))
  if (!is.null(period) && length(period) > 0) {
    df2 <- df2 %>% dplyr::filter(.data$tipo %in% as.character(period))
  }

  df2 <- df2 %>% dplyr::filter(!is.na(.data$grade), !is.na(.data$nivel), nzchar(.data$nivel))
  if (nrow(df2) == 0) return(data.frame())

  grades <- sort(unique(df2$grade))
  out <- lapply(grades, function(g) {
    sub <- df2 %>% dplyr::filter(.data$grade == g)
    tbl <- sub %>% dplyr::count(.data$year, .data$nivel, name = "n") %>%
      tidyr::pivot_wider(names_from = "nivel", values_from = "n", values_fill = 0)

    mat <- as.matrix(tbl[, setdiff(names(tbl), "year"), drop = FALSE])
    if (nrow(mat) < 2 || ncol(mat) < 2) return(NULL)

    # Si hay conteos bajos, usa simulación para p-value más estable.
    simulate <- any(mat < 5)
    res <- stats::chisq.test(mat, simulate.p.value = simulate)
    n <- sum(mat)
    v <- cramers_v(res$statistic, n = n, r = nrow(mat), c = ncol(mat))

    data.frame(
      grade = g,
      year_a = as.integer(year_a),
      year_b = as.integer(year_b),
      statistic = as.numeric(res$statistic),
      p_value = as.numeric(res$p.value),
      significant = isTRUE(res$p.value < 0.05),
      effect_v = v,
      note = if (simulate) "p simulado (conteos bajos)" else "",
      stringsAsFactors = FALSE
    )
  })

  dplyr::bind_rows(out)
}

cohens_d <- function(x, y) {
  x <- stats::na.omit(x)
  y <- stats::na.omit(y)
  n1 <- length(x)
  n2 <- length(y)
  if (n1 < 2 || n2 < 2) return(NA_real_)
  s1 <- stats::sd(x)
  s2 <- stats::sd(y)
  sp <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2))
  if (is.na(sp) || sp == 0) return(NA_real_)
  (mean(x) - mean(y)) / sp
}

rank_biserial_from_wilcox <- function(w_stat, n1, n2) {
  # W = suma de rangos del grupo 1. Convertimos a U.
  u <- as.numeric(w_stat) - (n1 * (n1 + 1)) / 2
  (2 * u) / (n1 * n2) - 1
}

compare_sections_metric <- function(df, grade, year, period, section_a, section_b, metric) {
  # Compara 2 secciones dentro de un mismo grado/año/periodo usando valores por estudiante.
  if (is.null(df) || nrow(df) == 0) return(list(ok = FALSE, message = "Sin datos."))

  df2 <- add_science_fields(df) %>%
    dplyr::filter(
      .data$grade == as.integer(grade),
      .data$year == as.integer(year),
      .data$tipo %in% as.character(period),
      .data$section %in% c(as.character(section_a), as.character(section_b))
    )

  metric <- as.character(metric %||% "")
  if (!nzchar(metric)) return(list(ok = FALSE, message = "Métrica inválida."))

  if (metric %in% c("% Nivel I", "% Nivel II", "% Nivel III")) {
    level <- gsub("^%\\s*", "", metric)
    df2 <- df2 %>% dplyr::mutate(nivel = normalize_nivel(.data$nivel_logro))
    df2$value <- ifelse(df2$nivel == level, 100, 0)
  } else {
    axes <- detect_axes(df2)
    if (!metric %in% axes) return(list(ok = FALSE, message = paste0("Métrica no encontrada: ", metric)))
    df2$value <- df2[[metric]]
  }

  x <- df2$value[df2$section == as.character(section_a)]
  y <- df2$value[df2$section == as.character(section_b)]
  x <- stats::na.omit(x)
  y <- stats::na.omit(y)

  if (length(x) < 3 || length(y) < 3) {
    return(list(ok = FALSE, message = "Se requieren ≥3 estudiantes por sección para comparar."))
  }

  sh1 <- tryCatch(stats::shapiro.test(x)$p.value, error = function(e) NA_real_)
  sh2 <- tryCatch(stats::shapiro.test(y)$p.value, error = function(e) NA_real_)

  use_t <- is.finite(sh1) && is.finite(sh2) && sh1 > 0.05 && sh2 > 0.05
  if (isTRUE(use_t)) {
    test <- stats::t.test(x, y)
    eff <- cohens_d(x, y)
    list(
      ok = TRUE,
      test = "t-test (Welch)",
      statistic = as.numeric(test$statistic),
      p_value = as.numeric(test$p.value),
      mean_a = mean(x),
      mean_b = mean(y),
      diff = mean(x) - mean(y),
      ci_low = as.numeric(test$conf.int[[1]]),
      ci_high = as.numeric(test$conf.int[[2]]),
      effect = eff,
      effect_label = "Cohen's d",
      n_a = length(x),
      n_b = length(y)
    )
  } else {
    test <- stats::wilcox.test(x, y)
    eff <- rank_biserial_from_wilcox(test$statistic, n1 = length(x), n2 = length(y))
    list(
      ok = TRUE,
      test = "Mann-Whitney U",
      statistic = as.numeric(test$statistic),
      p_value = as.numeric(test$p.value),
      mean_a = mean(x),
      mean_b = mean(y),
      diff = mean(x) - mean(y),
      ci_low = NA_real_,
      ci_high = NA_real_,
      effect = eff,
      effect_label = "Rank-biserial r",
      n_a = length(x),
      n_b = length(y)
    )
  }
}

axes_correlation <- function(df, method = "pearson", period = NULL, years = NULL) {
  # Correlación entre ejes (a nivel agregado por curso/año/periodo).
  if (is.null(df) || nrow(df) == 0) return(list(ok = FALSE, message = "Sin datos."))

  df2 <- add_science_fields(df)
  if (!is.null(years) && length(years) > 0) {
    df2 <- df2 %>% dplyr::filter(.data$year %in% suppressWarnings(as.integer(years)))
  }
  if (!is.null(period) && length(period) > 0) {
    df2 <- df2 %>% dplyr::filter(.data$tipo %in% as.character(period))
  }

  axes <- setdiff(detect_axes(df2), "Promedio (todos los ejes)")
  if (length(axes) < 2) return(list(ok = FALSE, message = "Se requieren ≥2 ejes para correlación."))

  agg <- df2 %>%
    dplyr::group_by(.data$year, .data$tipo, .data$course, .data$grade, .data$section) %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(axes), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

  mat <- as.matrix(agg[, axes, drop = FALSE])
  cor_mat <- stats::cor(mat, use = "pairwise.complete.obs", method = method)

  # p-values por par
  p_mat <- matrix(NA_real_, nrow = ncol(mat), ncol = ncol(mat))
  colnames(p_mat) <- colnames(cor_mat)
  rownames(p_mat) <- rownames(cor_mat)
  for (i in seq_len(ncol(mat))) {
    for (j in seq_len(ncol(mat))) {
      if (i == j) {
        p_mat[i, j] <- NA_real_
      } else {
        xi <- mat[, i]
        xj <- mat[, j]
        ok <- stats::complete.cases(xi, xj)
        if (sum(ok) >= 3) {
          p_mat[i, j] <- tryCatch(stats::cor.test(xi[ok], xj[ok], method = method)$p.value, error = function(e) NA_real_)
        }
      }
    }
  }

  list(ok = TRUE, axes = axes, cor = cor_mat, p = p_mat, agg = agg)
}

pca_axes <- function(cor_result, scale = TRUE) {
  if (is.null(cor_result) || !isTRUE(cor_result$ok)) return(list(ok = FALSE, message = "Sin datos para PCA."))
  axes <- cor_result$axes
  agg <- cor_result$agg
  mat <- as.matrix(agg[, axes, drop = FALSE])
  ok <- stats::complete.cases(mat)
  if (sum(ok) < 3) return(list(ok = FALSE, message = "Se requieren ≥3 filas completas para PCA."))
  fit <- stats::prcomp(mat[ok, , drop = FALSE], center = TRUE, scale. = isTRUE(scale))
  list(ok = TRUE, fit = fit, meta = agg[ok, , drop = FALSE], axes = axes)
}
