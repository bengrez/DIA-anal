# DIA — Ciencias Naturales (Chile)
# Shiny dashboard para analizar resultados DIA (5°–8°).

required_packages <- c(
  "shiny",
  "shinydashboard",
  "dplyr",
  "tidyr",
  "ggplot2",
  "readxl",
  "plotly",
  "DT",
  "scales",
  "writexl"
)

missing <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  stop(
    "Faltan paquetes R: ",
    paste(missing, collapse = ", "),
    "\nEjecuta: Rscript app/scripts/install_deps.R",
    call. = FALSE
  )
}

library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(DT)

source(file.path("R", "ingest.R"))
source(file.path("R", "platform_ingest.R"))
source(file.path("R", "transform.R"))
source(file.path("R", "science_processing.R"))
source(file.path("R", "science_plots.R"))
source(file.path("R", "template.R"))

options(shiny.maxRequestSize = 200 * 1024^2) # 200 MB

folder_picker_js <- "
(function() {
  function enableFolderPicker() {
    var el = document.querySelector('#folder_files input[type=file]');
    if (!el) return;
    if (!el.hasAttribute('webkitdirectory')) {
      el.setAttribute('webkitdirectory', '');
      el.setAttribute('directory', '');
      el.setAttribute('multiple', '');
    }
  }
  document.addEventListener('DOMContentLoaded', enableFolderPicker);
  document.addEventListener('shiny:connected', enableFolderPicker);
  setInterval(enableFolderPicker, 750);
})();
"

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "DIA — Ciencias Naturales"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Inicio", tabName = "home", icon = icon("home")),
      menuItem("Asistente", tabName = "assistant", icon = icon("magic")),
      menuItem("Overview", tabName = "overview", icon = icon("chart-column")),
      menuItem("Ejes Temáticos", tabName = "axes", icon = icon("th")),
      menuItem("Comparaciones", tabName = "comparisons", icon = icon("arrows-left-right")),
      menuItem("Datos", tabName = "data", icon = icon("table"))
    ),
    hr(),
    tags$details(
      open = TRUE,
      tags$summary(tags$strong("1) Carga de datos (.xls)")),
      fileInput("folder_files", "Seleccionar carpeta o archivos…", accept = c(".xls"), multiple = TRUE),
      actionButton("folder_load", "Cargar datos", icon = icon("upload"), class = "btn-primary"),
      uiOutput("load_status_ui"),
      downloadButton("download_template_xlsx", "Descargar plantilla (.xlsx)"),
      tags$small(class = "text-muted", "Tip: usa Chrome/Edge y selecciona una carpeta completa (o Ctrl+A dentro de la carpeta).")
    ),
    tags$details(
      open = TRUE,
      tags$summary(tags$strong("2) Filtros")),
      checkboxGroupInput("year_filter", "Año", choices = NULL),
      checkboxGroupInput("period_filter", "Periodo", choices = NULL),
      checkboxGroupInput("grade_filter", "Grado", choices = NULL),
      checkboxGroupInput("section_filter", "Sección", choices = NULL)
    ),
    tags$details(
      open = FALSE,
      tags$summary(tags$strong("3) Privacidad y exportación")),
      checkboxInput("anon", "Modo anónimo (ocultar nombres)", value = TRUE),
      downloadButton("download_filtered_csv", "Descargar datos filtrados (CSV)")
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(
        HTML(
          "
          /* No mostrar nombres de archivos en el input (evita confusión y reduce ruido visual) */
          .shiny-file-input input[type=file] { color: transparent !important; }
          .shiny-file-input .fileinput-filename { display: none !important; }

          /* details/summary más compactos en el sidebar */
          .main-sidebar details > summary { cursor: pointer; margin: 6px 0; }
          .main-sidebar details[open] > summary { margin-bottom: 8px; }
          "
        )
      ),
      tags$script(HTML(folder_picker_js))
    ),
    tabItems(
      tabItem(
        tabName = "home",
        fluidRow(
          box(
            width = 12,
            title = "Objetivo",
            status = "primary",
            solidHeader = TRUE,
            tags$p(
              "Esta app ayuda a docentes a visualizar resultados DIA de ",
              tags$strong("Ciencias Naturales"),
              " (5°–8°) sin hacer análisis manual."
            ),
            tags$ul(
              tags$li("Carga una carpeta con archivos .xls descargados desde la plataforma DIA."),
              tags$li("Filtra por Año / Periodo / Grado / Sección."),
              tags$li("Explora gráficos y descarga PNG + CSV para informes.")
            ),
            tags$p(tags$em("Los datos se procesan localmente: no se suben a servidores."))
          )
        )
      ),
      tabItem(
        tabName = "assistant",
        fluidRow(
          box(
            width = 5,
            title = "Asistente (sí / no)",
            status = "info",
            solidHeader = TRUE,
            radioButtons(
              "q_levels",
              "¿Quieres ver la distribución de estudiantes por nivel de logro (I/II/III)?",
              choices = c("Sí" = "si", "No" = "no"),
              selected = "si"
            ),
            radioButtons(
              "q_axes",
              "¿Quieres comparar desempeño por ejes temáticos?",
              choices = c("Sí" = "si", "No" = "no"),
              selected = "no"
            ),
            radioButtons(
              "q_yoy",
              "¿Quieres comparar dos años (YoY)?",
              choices = c("Sí" = "si", "No" = "no"),
              selected = "no"
            ),
            actionButton("assistant_go", "Ir a la sección recomendada", icon = icon("arrow-right"))
          ),
          box(
            width = 7,
            title = "Recomendación",
            status = "primary",
            solidHeader = TRUE,
            uiOutput("assistant_reco_ui")
          )
        )
      ),
      tabItem(
      tabName = "overview",
        fluidRow(
          valueBoxOutput("vb_rows", width = 3),
          valueBoxOutput("vb_courses", width = 3),
          valueBoxOutput("vb_best_course", width = 3),
          valueBoxOutput("vb_worst_course", width = 3)
        ),
        fluidRow(
          box(
            width = 12,
            title = "Distribución de estudiantes por nivel de logro",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("plot_levels", height = 420),
            br(),
            fluidRow(
              column(2, downloadButton("dl_levels_png", "PNG")),
              column(2, downloadButton("dl_levels_csv", "CSV"))
            ),
            br(),
            DTOutput("table_levels")
          )
        )
      ),
      tabItem(
        tabName = "axes",
        fluidRow(
          box(
            width = 4,
            title = "Vista",
            status = "info",
            solidHeader = TRUE,
            radioButtons(
              "axes_scope",
              "Ejes según grado",
              choices = c("Todos" = "all", "5°–6°" = "56", "7°–8°" = "78"),
              selected = "all"
            ),
            tags$small(class = "text-muted", "Tip: 5°–6° y 7°–8° tienen ejes distintos.")
          ),
          box(
            width = 8,
            title = "Desempeño por eje temático (promedio)",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("plot_axes_bar", height = 420),
            br(),
            fluidRow(
              column(2, downloadButton("dl_axes_png", "PNG")),
              column(2, downloadButton("dl_axes_csv", "CSV"))
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Heatmap: ejes × cursos",
            status = "warning",
            solidHeader = TRUE,
            plotlyOutput("plot_axes_heatmap", height = 520)
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Tabla: ejes por curso",
            status = "default",
            solidHeader = TRUE,
            DTOutput("table_axes")
          )
        )
      ),
      tabItem(
        tabName = "comparisons",
        fluidRow(
          box(
            width = 4,
            title = "Años a comparar",
            status = "info",
            solidHeader = TRUE,
            uiOutput("yoy_years_ui"),
            tags$small(class = "text-muted", "El cálculo usa los filtros de Periodo/Grado/Sección.")
          ),
          box(
            width = 8,
            title = "Cambio año vs año — niveles (p.p.)",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("plot_yoy_levels", height = 360),
            br(),
            fluidRow(
              column(2, downloadButton("dl_yoy_levels_png", "PNG")),
              column(2, downloadButton("dl_yoy_levels_csv", "CSV"))
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Cambio año vs año — ejes temáticos (p.p.)",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("plot_yoy_axes", height = 520),
            br(),
            fluidRow(
              column(2, downloadButton("dl_yoy_axes_png", "PNG")),
              column(2, downloadButton("dl_yoy_axes_csv", "CSV"))
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Tablas YoY",
            status = "default",
            solidHeader = TRUE,
            tabsetPanel(
              tabPanel("Niveles", DTOutput("table_yoy_levels")),
              tabPanel("Ejes", DTOutput("table_yoy_axes"))
            )
          )
        )
      ),
      tabItem(
        tabName = "data",
        fluidRow(
          box(
            width = 12,
            title = "Cobertura: estudiantes por curso (filtrado)",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("plot_coverage", height = 320)
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Archivos importados",
            status = "info",
            solidHeader = TRUE,
            DTOutput("table_manifest")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Vista previa (datos filtrados)",
            status = "default",
            solidHeader = TRUE,
            numericInput("preview_n", "Filas a mostrar", value = 50, min = 10, max = 500, step = 10),
            DTOutput("table_preview")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  state <- reactiveVal(NULL)

  assistant_target_tab <- reactive({
    if (identical(input$q_levels, "si")) return("overview")
    if (identical(input$q_axes, "si")) return("axes")
    if (identical(input$q_yoy, "si")) return("comparisons")
    "data"
  })

  output$assistant_reco_ui <- renderUI({
    tab <- assistant_target_tab()
    lbl <- switch(
      tab,
      overview = "Overview → Distribución por nivel de logro",
      axes = "Ejes Temáticos → Desempeño por eje",
      comparisons = "Comparaciones → Cambio año vs año (YoY)",
      data = "Datos → Revisa cobertura y archivos importados"
    )
    tagList(
      tags$p(tags$strong("Sugerencia:"), " ", lbl),
      tags$p(class = "text-muted", "Si aún no cargas datos, parte por “1) Carga de datos” en el panel izquierdo.")
    )
  })

  observeEvent(input$assistant_go, {
    updateTabItems(session, "tabs", selected = assistant_target_tab())
  })

  observeEvent(input$folder_load, {
    validate(need(!is.null(input$folder_files) && nrow(input$folder_files) > 0, "Selecciona una carpeta o archivos .xls primero."))
    state(NULL)

    folder_hint <- dirname(input$folder_files$name[[1]] %||% "")
    dataset_name <- if (nzchar(folder_hint) && !identical(folder_hint, ".")) {
      paste0("Carpeta: ", basename(folder_hint))
    } else {
      "Carpeta seleccionada"
    }

    res <- tryCatch(
      load_dia_platform_upload(input$folder_files, dataset_name = dataset_name, skip = 12),
      error = function(e) {
        showNotification(e$message, type = "error", duration = NULL)
        NULL
      }
    )

    if (is.null(res) || is.null(res$data) || nrow(res$data) == 0) return()
    state(res)
  })

  output$download_template_xlsx <- downloadHandler(
    filename = function() "plantilla_dia_ciencias.xlsx",
    content = function(file) {
      write_dia_excel_template(file)
    }
  )

  output$load_status_ui <- renderUI({
    st <- state()
    if (is.null(st)) {
      return(tags$small(class = "text-muted", "Sin datos cargados."))
    }
    ok_n <- sum(st$manifest$status == "ok", na.rm = TRUE)
    err_n <- sum(st$manifest$status == "error", na.rm = TRUE)
    tagList(
      tags$small(class = "text-muted", paste0("Archivos: ", nrow(st$manifest), " · OK: ", ok_n, " · Error: ", err_n)),
      if (err_n > 0) tags$small(class = "text-danger", "Revisa la pestaña “Datos” para ver archivos con error.")
    )
  })

  data_raw <- reactive({
    st <- state()
    validate(need(!is.null(st), "Carga una carpeta con archivos .xls para comenzar."))
    st$data
  })

  data_clean <- reactive({
    df <- data_raw()
    validation_error <- tryCatch(
      {
        validate_dia_data(df)
        NULL
      },
      error = function(e) e$message
    )
    validate(need(is.null(validation_error), validation_error))

    apply_anonymity(df, anonymous = isTRUE(input$anon), method = "n_lista", seed = 1234)
  })

  # Choices dinámicos (años, periodos, grados, secciones)
  observeEvent(state(), {
    st <- state()
    if (is.null(st) || is.null(st$data) || nrow(st$data) == 0) return()

    df <- st$data
    years <- sort(unique(df$year))
    periods_raw <- unique(as.character(df$tipo))
    known <- c("Diagnóstico", "Monitoreo", "Cierre", "Evaluacion_Cierre")
    periods <- c(intersect(known, periods_raw), setdiff(periods_raw, known))
    period_choices <- setNames(periods, periods)
    if ("Evaluacion_Cierre" %in% period_choices) {
      names(period_choices)[period_choices == "Evaluacion_Cierre"] <- "Evaluación de cierre"
    }

    parts <- course_parts(df$curso)
    grades <- sort(unique(parts$grade[!is.na(parts$grade)]))
    sections <- sort(unique(parts$section[!is.na(parts$section)]))

    updateCheckboxGroupInput(
      session,
      "year_filter",
      choices = as.character(years),
      selected = as.character(years)
    )
    updateCheckboxGroupInput(
      session,
      "period_filter",
      choices = period_choices,
      selected = periods
    )
    updateCheckboxGroupInput(
      session,
      "grade_filter",
      choices = stats::setNames(as.character(grades), paste0(grades, "°")),
      selected = as.character(grades)
    )
    updateCheckboxGroupInput(
      session,
      "section_filter",
      choices = sections,
      selected = sections
    )
  }, ignoreInit = TRUE)

  filter_grade_section <- function(df) {
    if (!is.null(input$grade_filter) && length(input$grade_filter) > 0) {
      parts <- course_parts(df$curso)
      keep <- parts$grade %in% suppressWarnings(as.integer(input$grade_filter))
      df <- df[keep, , drop = FALSE]
    }
    if (!is.null(input$section_filter) && length(input$section_filter) > 0) {
      parts <- course_parts(df$curso)
      keep <- parts$section %in% as.character(input$section_filter)
      df <- df[keep, , drop = FALSE]
    }
    df
  }

  data_filtered <- reactive({
    df <- data_clean()

    if (!is.null(input$year_filter) && length(input$year_filter) > 0) {
      df <- df %>% filter(.data$year %in% suppressWarnings(as.integer(input$year_filter)))
    }
    if (!is.null(input$period_filter) && length(input$period_filter) > 0) {
      df <- df %>% filter(.data$tipo %in% as.character(input$period_filter))
    }
    filter_grade_section(df)
  })

  data_filtered_no_year <- reactive({
    # Para YoY: ignora filtro de año y usa solo Periodo/Grado/Sección.
    df <- data_clean()
    if (!is.null(input$period_filter) && length(input$period_filter) > 0) {
      df <- df %>% filter(.data$tipo %in% as.character(input$period_filter))
    }
    filter_grade_section(df)
  })

  # --- Summary boxes ---------------------------------------------------------
  output$vb_rows <- renderValueBox({
    df <- data_filtered()
    valueBox(
      value = format(nrow(df), big.mark = ".", decimal.mark = ","),
      subtitle = "Filas (estudiantes)",
      icon = icon("users"),
      color = "aqua"
    )
  })

  output$vb_courses <- renderValueBox({
    df <- data_filtered()
    n_courses <- length(unique(df$curso))
    valueBox(
      value = n_courses,
      subtitle = "Cursos (códigos)",
      icon = icon("graduation-cap"),
      color = "purple"
    )
  })

  course_global_means <- reactive({
    df <- data_filtered()
    col_name <- "Promedio (todos los ejes)"
    if (!col_name %in% names(df)) return(data.frame())

    parts <- course_parts(df$curso)
    df$course <- parts$course

    df %>%
      group_by(.data$course) %>%
      summarise(pct = mean(.data[[col_name]], na.rm = TRUE), .groups = "drop") %>%
      filter(!is.na(.data$pct)) %>%
      arrange(desc(.data$pct))
  })

  output$vb_best_course <- renderValueBox({
    stats <- course_global_means()
    if (nrow(stats) == 0) {
      return(valueBox(value = "NA", subtitle = "Mejor curso", icon = icon("arrow-up"), color = "green"))
    }
    best <- stats[1, ]
    valueBox(
      value = paste0(best$course, " · ", sprintf("%.1f%%", best$pct)),
      subtitle = "Mejor curso (promedio global)",
      icon = icon("arrow-up"),
      color = "green"
    )
  })

  output$vb_worst_course <- renderValueBox({
    stats <- course_global_means()
    if (nrow(stats) == 0) {
      return(valueBox(value = "NA", subtitle = "Peor curso", icon = icon("arrow-down"), color = "red"))
    }
    worst <- stats[nrow(stats), ]
    valueBox(
      value = paste0(worst$course, " · ", sprintf("%.1f%%", worst$pct)),
      subtitle = "Peor curso (promedio global)",
      icon = icon("arrow-down"),
      color = "red"
    )
  })

  # --- Plot 1: Achievement levels -------------------------------------------
  levels_summary <- reactive({
    summarize_levels_by_grade(data_filtered())
  })

  output$plot_levels <- renderPlotly({
    df <- levels_summary()
    validate(need(nrow(df) > 0, "No hay datos para graficar (revisa filtros)."))
    gg <- plot_levels_distribution(df)
    plotly::ggplotly(gg, tooltip = c("x", "y", "fill"))
  })

  output$table_levels <- renderDT({
    df <- levels_summary() %>%
      mutate(pct = round(.data$pct, 1)) %>%
      arrange(.data$year, .data$tipo, .data$grade, .data$nivel)
    datatable(df, options = list(pageLength = 10), rownames = FALSE)
  })

  output$dl_levels_csv <- downloadHandler(
    filename = function() "niveles_resumen.csv",
    content = function(file) {
      df <- levels_summary()
      utils::write.csv(df, file = file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  output$dl_levels_png <- downloadHandler(
    filename = function() "niveles.png",
    content = function(file) {
      gg <- plot_levels_distribution(levels_summary())
      ggplot2::ggsave(file, gg, width = 10, height = 6, dpi = 300)
    }
  )

  # --- Plot 2: Axes performance ---------------------------------------------
  axes_summary <- reactive({
    df <- summarize_axes_by_course(data_filtered(), include_synthetic = FALSE)
    scope <- input$axes_scope %||% "all"
    if (!is.null(df) && nrow(df) > 0 && "grade" %in% names(df)) {
      if (identical(scope, "56")) df <- df %>% filter(.data$grade %in% c(5, 6))
      if (identical(scope, "78")) df <- df %>% filter(.data$grade %in% c(7, 8))
    }
    df
  })

  output$plot_axes_bar <- renderPlotly({
    df <- axes_summary()
    validate(need(nrow(df) > 0, "No hay datos para graficar ejes (revisa filtros)."))
    gg <- plot_axes_by_course(df)
    plotly::ggplotly(gg, tooltip = c("x", "y", "fill"))
  })

  output$plot_axes_heatmap <- renderPlotly({
    df <- axes_summary()
    validate(need(nrow(df) > 0, "No hay datos para graficar ejes (revisa filtros)."))
    gg <- plot_axes_heatmap(df)
    plotly::ggplotly(gg, tooltip = c("x", "y", "fill"))
  })

  output$dl_axes_csv <- downloadHandler(
    filename = function() "ejes_resumen.csv",
    content = function(file) {
      df <- axes_summary()
      utils::write.csv(df, file = file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  output$dl_axes_png <- downloadHandler(
    filename = function() "ejes.png",
    content = function(file) {
      gg <- plot_axes_by_course(axes_summary())
      ggplot2::ggsave(file, gg, width = 10, height = 6, dpi = 300)
    }
  )

  output$table_axes <- renderDT({
    df <- axes_summary() %>%
      mutate(pct_logro = round(.data$pct_logro, 1)) %>%
      arrange(.data$year, .data$tipo, .data$grade, .data$course, .data$axis)
    datatable(df, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
  })

  # --- YoY -------------------------------------------------------------------
  output$yoy_years_ui <- renderUI({
    df <- data_filtered_no_year()
    years <- sort(unique(df$year))
    if (length(years) < 2) {
      return(tags$p("Se requieren al menos 2 años para comparar."))
    }
    default_a <- if (2024 %in% years) 2024 else years[[1]]
    default_b <- if (2025 %in% years) 2025 else years[[length(years)]]
    tagList(
      selectInput("yoy_a", "Año base", choices = years, selected = default_a),
      selectInput("yoy_b", "Año comparación", choices = years, selected = default_b)
    )
  })

  yoy_levels_tbl <- reactive({
    req(input$yoy_a, input$yoy_b)
    df_lvl <- summarize_levels_by_grade(data_filtered_no_year())
    calc_yoy_diff(df_lvl, year_a = input$yoy_a, year_b = input$yoy_b, value_col = "pct")
  })

  yoy_axes_tbl <- reactive({
    req(input$yoy_a, input$yoy_b)
    df_ax <- summarize_axes_by_course(data_filtered_no_year(), include_synthetic = FALSE)
    calc_yoy_diff(df_ax, year_a = input$yoy_a, year_b = input$yoy_b, value_col = "pct_logro")
  })

  output$plot_yoy_levels <- renderPlotly({
    df <- yoy_levels_tbl()
    validate(need(nrow(df) > 0, "No hay datos YoY (revisa filtros)."))
    df$grade_lbl <- paste0(df$grade, "°")
    gg <- plot_yoy_diff(df, title = "Cambio 2024 vs 2025 (niveles)", x_col = "nivel", facet_col = "grade_lbl")
    plotly::ggplotly(gg, tooltip = c("x", "y", "fill"))
  })

  output$plot_yoy_axes <- renderPlotly({
    df <- yoy_axes_tbl()
    validate(need(nrow(df) > 0, "No hay datos YoY (revisa filtros)."))
    gg <- plot_yoy_axes_by_course(df)
    plotly::ggplotly(gg, tooltip = c("x", "y", "fill"))
  })

  output$table_yoy_levels <- renderDT({
    df <- yoy_levels_tbl()
    datatable(df, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$table_yoy_axes <- renderDT({
    df <- yoy_axes_tbl()
    datatable(df, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$dl_yoy_levels_csv <- downloadHandler(
    filename = function() "yoy_niveles.csv",
    content = function(file) {
      utils::write.csv(yoy_levels_tbl(), file = file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  output$dl_yoy_levels_png <- downloadHandler(
    filename = function() "yoy_niveles.png",
    content = function(file) {
      df <- yoy_levels_tbl()
      df$grade_lbl <- paste0(df$grade, "°")
      gg <- plot_yoy_diff(df, title = "Cambio año vs año (niveles)", x_col = "nivel", facet_col = "grade_lbl")
      ggplot2::ggsave(file, gg, width = 10, height = 6, dpi = 300)
    }
  )

  output$dl_yoy_axes_csv <- downloadHandler(
    filename = function() "yoy_ejes.csv",
    content = function(file) {
      utils::write.csv(yoy_axes_tbl(), file = file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  output$dl_yoy_axes_png <- downloadHandler(
    filename = function() "yoy_ejes.png",
    content = function(file) {
      gg <- plot_yoy_axes_by_course(yoy_axes_tbl())
      ggplot2::ggsave(file, gg, width = 10, height = 10, dpi = 300)
    }
  )

  # --- Data tab --------------------------------------------------------------
  output$plot_coverage <- renderPlotly({
    df <- data_filtered()
    validate(need(nrow(df) > 0, "No hay datos para mostrar cobertura."))

    parts <- course_parts(df$curso)
    df$course <- parts$course

    coverage <- df %>%
      count(.data$year, .data$tipo, .data$course, name = "n_estudiantes") %>%
      arrange(.data$year, .data$tipo, .data$course)

    gg <- ggplot2::ggplot(coverage, ggplot2::aes(x = .data$course, y = .data$n_estudiantes, fill = .data$tipo)) +
      ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.7, alpha = 0.9) +
      ggplot2::labs(title = NULL, x = "Curso", y = "N° estudiantes", fill = "Periodo") +
      ggplot2::theme_minimal(base_family = "Arial") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), legend.position = "bottom")

    if (length(unique(coverage$year)) > 1) {
      gg <- gg + ggplot2::facet_wrap(~year)
    }

    plotly::ggplotly(gg, tooltip = c("x", "y", "fill"))
  })

  output$table_manifest <- renderDT({
    st <- state()
    validate(need(!is.null(st), "Sin datos cargados."))
    df <- st$manifest %>%
      mutate(
        Estado = ifelse(.data$status == "ok", "OK", "Error"),
        Archivo = .data$file
      ) %>%
      select(.data$Archivo, .data$curso, .data$tipo, .data$year, .data$Estado, .data$message)
    datatable(df, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
  })

  output$table_preview <- renderDT({
    df <- data_filtered()
    n <- input$preview_n %||% 50
    df2 <- utils::head(df, n)
    datatable(df2, options = list(pageLength = min(50, n), scrollX = TRUE), rownames = FALSE)
  })

  output$download_filtered_csv <- downloadHandler(
    filename = function() "datos_filtrados.csv",
    content = function(file) {
      df <- data_filtered()
      utils::write.csv(df, file = file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
}

shinyApp(ui, server)
