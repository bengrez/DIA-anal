assistant_plot_label <- function(plot_type) {
  switch(
    plot_type,
    promedio = "A) Promedio por curso y Tipo",
    distribucion = "B) Distribución (boxplot / histograma)",
    nivel_logro = "C) Nivel de logro (proporción)",
    crecimiento = "D) Crecimiento por estudiante (delta)",
    plot_type
  )
}

assistant_plot_why <- function(plot_type) {
  switch(
    plot_type,
    promedio = "Ideal cuando necesitas un resumen claro (promedios) por curso y tipo.",
    distribucion = "Útil cuando quieres ver variabilidad: dispersión, asimetrías y outliers.",
    nivel_logro = "Diseñado para reportar proporciones de niveles de logro por curso/tipo.",
    crecimiento = "Permite identificar quién mejora o baja entre dos mediciones (Tipos).",
    ""
  )
}

assistant_plot_what <- function(plot_type) {
  switch(
    plot_type,
    promedio = "Barras agrupadas: Curso en X y fill por Tipo (elige uno o varios ejes).",
    distribucion = "Boxplot u histograma (con curva de densidad) por curso/tipo (elige ejes).",
    nivel_logro = "Barras apiladas al 100% por NIVEL DE LOGRO (por curso y tipo).",
    crecimiento = "Delta por estudiante (o slope chart) comparando Tipo A vs Tipo B.",
    ""
  )
}

assistant_recommendation <- function(need_level, need_growth, need_dist) {
  # Inputs esperados: "yes", "no" o NULL (sin responder)
  if (is.null(need_level) || !need_level %in% c("yes", "no")) {
    return(list(complete = FALSE, prompt = "Responde la pregunta 1 para empezar."))
  }

  if (identical(need_level, "yes")) {
    plot_type <- "nivel_logro"
    return(list(
      complete = TRUE,
      plot_type = plot_type,
      label = assistant_plot_label(plot_type),
      why = assistant_plot_why(plot_type),
      what_you_get = assistant_plot_what(plot_type)
    ))
  }

  if (is.null(need_growth) || !need_growth %in% c("yes", "no")) {
    return(list(complete = FALSE, prompt = "Responde la pregunta 2 para continuar."))
  }

  if (identical(need_growth, "yes")) {
    plot_type <- "crecimiento"
    return(list(
      complete = TRUE,
      plot_type = plot_type,
      label = assistant_plot_label(plot_type),
      why = assistant_plot_why(plot_type),
      what_you_get = assistant_plot_what(plot_type)
    ))
  }

  if (is.null(need_dist) || !need_dist %in% c("yes", "no")) {
    return(list(complete = FALSE, prompt = "Responde la pregunta 3 para finalizar la recomendación."))
  }

  plot_type <- if (identical(need_dist, "yes")) "distribucion" else "promedio"
  list(
    complete = TRUE,
    plot_type = plot_type,
    label = assistant_plot_label(plot_type),
    why = assistant_plot_why(plot_type),
    what_you_get = assistant_plot_what(plot_type)
  )
}

