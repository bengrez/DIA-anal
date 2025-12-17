# ------------------------------------------------------------
# Analítica avanzada — gráficos (Phase 3)
#
# Gráficos basados en cohortes (curso/sección), evolución intra-año,
# significancia y correlación/PCA.
# ------------------------------------------------------------

plot_cohort_trajectory <- function(df_cohort, metric_label, ref_value = NULL, show_ci = TRUE) {
  if (is.null(df_cohort) || nrow(df_cohort) == 0) {
    return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::ggtitle("Sin datos"))
  }

  dfp <- df_cohort %>%
    dplyr::mutate(
      cohort_id = factor(.data$cohort_id),
      time_label = paste0(.data$grade, "°-", .data$year)
    )

  # Nota: se grafica contra `year` para mantener una escala temporal comparable
  # entre cohortes con distintos puntos de inicio.
  years_breaks <- sort(unique(dfp$year))

  p <- ggplot2::ggplot(
    dfp,
    ggplot2::aes(
      x = .data$year,
      y = .data$value,
      color = .data$cohort_id,
      group = .data$cohort_id,
      text = .data$time_label
    )
  ) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = 2.2) +
    ggplot2::scale_x_continuous(breaks = years_breaks) +
    scale_pct_0_100() +
    ggplot2::labs(
      title = "Trayectoria de cohortes (curso/sección)",
      subtitle = metric_label,
      x = "Año",
      y = metric_label,
      color = "Cohorte"
    ) +
    science_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  if (isTRUE(show_ci) && all(c("ci_low", "ci_high") %in% names(dfp))) {
    p <- p + ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$ci_low, ymax = .data$ci_high, fill = .data$cohort_id),
      alpha = 0.12,
      color = NA,
      inherit.aes = FALSE,
      data = dfp
    ) +
      ggplot2::guides(fill = "none")
  }

  if (is.numeric(ref_value) && is.finite(ref_value)) {
    p <- p + ggplot2::geom_hline(yintercept = ref_value, linetype = "dashed", color = "#555555")
  }

  p
}

plot_cohort_levels_area <- function(df_levels) {
  if (is.null(df_levels) || nrow(df_levels) == 0) {
    return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::ggtitle("Sin datos"))
  }

  dfp <- df_levels %>%
    dplyr::mutate(
      cohort_id = factor(.data$cohort_id),
      nivel = factor(.data$nivel, levels = c("Nivel I", "Nivel II", "Nivel III"))
    )

  ggplot2::ggplot(dfp, ggplot2::aes(x = .data$time_index, y = .data$pct / 100, fill = .data$nivel)) +
    ggplot2::geom_area(position = "stack", alpha = 0.85) +
    ggplot2::scale_y_continuous(labels = scales::label_percent()) +
    ggplot2::scale_fill_manual(
      values = c("Nivel I" = "#E74C3C", "Nivel II" = "#F39C12", "Nivel III" = "#27AE60"),
      name = "Nivel"
    ) +
    ggplot2::scale_x_continuous(
      breaks = dfp$time_index,
      labels = dfp$time_label
    ) +
    ggplot2::labs(title = "Composición de la cohorte (niveles de logro)", x = "Progresión (grado-año)", y = "Proporción") +
    ggplot2::facet_wrap(~cohort_id, ncol = 1) +
    science_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), legend.position = "bottom")
}

plot_within_year_evolution <- function(df_within, metric_label) {
  if (is.null(df_within) || nrow(df_within) == 0) {
    return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::ggtitle("Sin datos"))
  }

  dfp <- df_within %>%
    dplyr::mutate(
      tipo = period_levels(.data$tipo),
      trend = factor(.data$trend, levels = c("baja", "igual", "mejora"))
    )

  ggplot2::ggplot(dfp, ggplot2::aes(x = .data$tipo, y = .data$value, group = .data$course, color = .data$trend)) +
    ggplot2::geom_line(alpha = 0.55, linewidth = 0.8) +
    ggplot2::geom_point(alpha = 0.7, size = 1.8) +
    ggplot2::geom_hline(yintercept = 60, linetype = "dashed", color = "#666666") +
    scale_pct_0_100() +
    ggplot2::scale_color_manual(
      values = c("mejora" = "#27AE60", "igual" = "#95A5A6", "baja" = "#E74C3C"),
      name = "Tendencia"
    ) +
    ggplot2::labs(title = "Evolución dentro del año (Inicio → Monitoreo → Cierre)", subtitle = metric_label, x = "Periodo", y = metric_label) +
    ggplot2::facet_wrap(~grade, scales = "free_x") +
    science_theme() +
    ggplot2::theme(legend.position = "bottom")
}

sig_stars <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) return("***")
  if (p < 0.01) return("**")
  if (p < 0.05) return("*")
  "ns"
}

plot_correlation_heatmap <- function(cor_mat, p_mat = NULL, title = "Correlación entre ejes") {
  if (is.null(cor_mat) || nrow(cor_mat) == 0) {
    return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::ggtitle("Sin datos"))
  }

  df <- as.data.frame(as.table(cor_mat), stringsAsFactors = FALSE)
  colnames(df) <- c("var1", "var2", "cor")
  if (!is.null(p_mat) && all(dim(p_mat) == dim(cor_mat))) {
    df$p <- as.vector(p_mat)
    df$label <- paste0(sprintf("%.2f", df$cor), " ", vapply(df$p, sig_stars, character(1)))
  } else {
    df$label <- sprintf("%.2f", df$cor)
  }

  ggplot2::ggplot(df, ggplot2::aes(x = .data$var1, y = .data$var2, fill = .data$cor)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.3) +
    ggplot2::geom_text(ggplot2::aes(label = .data$label), size = 3) +
    ggplot2::scale_fill_gradient2(low = "#2C7BB6", mid = "white", high = "#D7191C", midpoint = 0, limits = c(-1, 1), name = "r") +
    ggplot2::labs(title = title, x = NULL, y = NULL) +
    science_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

plot_pca_scree <- function(pca_fit) {
  if (is.null(pca_fit)) {
    return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::ggtitle("Sin datos"))
  }
  var <- (pca_fit$sdev^2) / sum(pca_fit$sdev^2)
  df <- data.frame(pc = seq_along(var), variance = var)
  ggplot2::ggplot(df, ggplot2::aes(x = .data$pc, y = .data$variance)) +
    ggplot2::geom_col(fill = "#4C78A8") +
    ggplot2::geom_line(color = "#2C3E50") +
    ggplot2::geom_point(color = "#2C3E50") +
    ggplot2::scale_y_continuous(labels = scales::label_percent()) +
    ggplot2::labs(title = "PCA: varianza explicada", x = "Componente principal", y = "% varianza") +
    science_theme()
}

plot_pca_biplot <- function(pca_fit, meta = NULL, color_col = NULL) {
  if (is.null(pca_fit)) {
    return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::ggtitle("Sin datos"))
  }

  scores <- as.data.frame(pca_fit$x[, 1:2, drop = FALSE])
  colnames(scores) <- c("PC1", "PC2")
  if (!is.null(meta) && nrow(meta) == nrow(scores)) {
    scores <- cbind(scores, meta)
  }

  loads <- as.data.frame(pca_fit$rotation[, 1:2, drop = FALSE])
  colnames(loads) <- c("PC1", "PC2")
  loads$var <- rownames(loads)

  # Escala simple para flechas
  mult <- 0.8 * max(abs(scores$PC1), abs(scores$PC2))
  loads$PC1s <- loads$PC1 * mult
  loads$PC2s <- loads$PC2 * mult

  aes_color <- if (!is.null(color_col) && color_col %in% names(scores)) ggplot2::aes(color = .data[[color_col]]) else ggplot2::aes()

  ggplot2::ggplot(scores, ggplot2::aes(x = .data$PC1, y = .data$PC2)) +
    ggplot2::geom_point(aes_color, alpha = 0.7, size = 2) +
    ggplot2::geom_segment(
      data = loads,
      ggplot2::aes(x = 0, y = 0, xend = .data$PC1s, yend = .data$PC2s),
      arrow = ggplot2::arrow(length = grid::unit(0.15, "inches")),
      color = "#2C3E50",
      alpha = 0.7
    ) +
    ggplot2::geom_text(
      data = loads,
      ggplot2::aes(x = .data$PC1s, y = .data$PC2s, label = .data$var),
      size = 3,
      hjust = 0.5,
      vjust = -0.5
    ) +
    ggplot2::labs(title = "PCA: biplot (PC1 vs PC2)", x = "PC1", y = "PC2") +
    science_theme() +
    ggplot2::theme(legend.position = "bottom")
}
