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
source(file.path("R", "advanced_processing.R"))
source(file.path("R", "advanced_plots.R"))

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
      menuItem("Trayectoria", tabName = "tracking", icon = icon("chart-line")),
      menuItem("Analítica", tabName = "analytics", icon = icon("microscope")),
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
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Datos (tabla)",
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
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
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
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
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            tabsetPanel(
              tabPanel("Niveles", DTOutput("table_yoy_levels")),
              tabPanel("Ejes", DTOutput("table_yoy_axes"))
            )
          )
        )
      ),
      tabItem(
        tabName = "tracking",
        fluidRow(
          box(
            width = 4,
            title = "Selección",
            status = "info",
            solidHeader = TRUE,
            uiOutput("tracking_controls_ui"),
            checkboxInput("tracking_only_2plus", "Solo estudiantes con ≥2 evaluaciones", value = TRUE),
            sliderInput("tracking_n", "Máx. estudiantes a mostrar", min = 10, max = 200, value = 60, step = 10),
            tags$small(class = "text-muted", "Recomendado: elige 1 curso + 1 año para una lectura clara.")
          ),
          box(
            width = 8,
            title = "Trayectoria por estudiante (porcentaje)",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("plot_tracking", height = 520),
            br(),
            fluidRow(
              column(2, downloadButton("dl_tracking_png", "PNG")),
              column(2, downloadButton("dl_tracking_csv", "CSV"))
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Datos (tabla)",
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            DTOutput("table_tracking")
          )
        )
      ),
      tabItem(
        tabName = "analytics",
        tabsetPanel(
          tabPanel(
            "Cohortes",
            fluidRow(
              box(
                width = 4,
                title = "Controles",
                status = "info",
                solidHeader = TRUE,
                uiOutput("cohort_controls_ui"),
                checkboxInput("cohort_show_ci", "Mostrar banda 95% (si aplica)", value = TRUE),
                actionButton("cohort_refresh", "Actualizar cohortes", icon = icon("rotate"))
              ),
              box(
                width = 8,
                title = "Trayectoria de cohortes (sección)",
                status = "primary",
                solidHeader = TRUE,
                plotlyOutput("plot_cohort_trajectory", height = 520),
                br(),
                fluidRow(
                  column(2, downloadButton("dl_cohort_png", "PNG")),
                  column(2, downloadButton("dl_cohort_csv", "CSV"))
                )
              )
            ),
            fluidRow(
              box(
                width = 12,
                title = "Composición por niveles (stacked area)",
                status = "warning",
                solidHeader = TRUE,
                plotlyOutput("plot_cohort_area", height = 560)
              )
            ),
            fluidRow(
              box(
                width = 12,
                title = "Datos (tabla)",
                status = "info",
                solidHeader = TRUE,
                collapsible = TRUE,
                collapsed = TRUE,
                DTOutput("table_cohort")
              )
            )
          ),
          tabPanel(
            "Dentro del año",
            fluidRow(
              box(
                width = 4,
                title = "Controles",
                status = "info",
                solidHeader = TRUE,
                uiOutput("within_controls_ui"),
                tags$small(class = "text-muted", "Usa periodos disponibles (Diagnóstico/Monitoreo/Cierre).")
              ),
              box(
                width = 8,
                title = "Evolución dentro del año",
                status = "primary",
                solidHeader = TRUE,
                plotlyOutput("plot_within_year", height = 520)
              )
            )
          ),
          tabPanel(
            "Pruebas estadísticas",
            fluidRow(
              box(
                width = 4,
                title = "Controles",
                status = "info",
                solidHeader = TRUE,
                uiOutput("stats_controls_ui")
              ),
              box(
                width = 8,
                title = "Resultados",
                status = "primary",
                solidHeader = TRUE,
                uiOutput("stats_summary_ui")
              )
            ),
            fluidRow(
              box(
                width = 12,
                title = "Tabla de resultados",
                status = "info",
                solidHeader = TRUE,
                collapsible = TRUE,
                collapsed = TRUE,
                DTOutput("table_stats")
              )
            )
          ),
          tabPanel(
            "Correlación y PCA",
            fluidRow(
              box(
                width = 4,
                title = "Controles",
                status = "info",
                solidHeader = TRUE,
                uiOutput("corr_controls_ui"),
                downloadButton("dl_corr_csv", "Descargar matriz (CSV)")
              ),
              box(
                width = 8,
                title = "Correlación entre ejes",
                status = "primary",
                solidHeader = TRUE,
                plotlyOutput("plot_corr", height = 520)
              )
            ),
            fluidRow(
              box(
                width = 6,
                title = "PCA: varianza explicada",
                status = "info",
                solidHeader = TRUE,
                plotlyOutput("plot_pca_scree", height = 360)
              ),
              box(
                width = 6,
                title = "PCA: biplot",
                status = "info",
                solidHeader = TRUE,
                plotlyOutput("plot_pca_biplot", height = 360)
              )
            )
          ),
          tabPanel(
            "Ayuda",
            fluidRow(
              box(
                width = 12,
                title = "Guía rápida (Phase 3)",
                status = "primary",
                solidHeader = TRUE,
                tags$p("Estas herramientas avanzadas trabajan con los datos disponibles hoy (ejes + nivel de logro)."),
                tags$ul(
                  tags$li(tags$strong("Cohortes:"), " sigue una sección (A/B/…) a través de años, asumiendo progresión 1 grado/año."),
                  tags$li(tags$strong("Dentro del año:"), " compara Diagnóstico → Monitoreo → Cierre para cada curso/año."),
                  tags$li(tags$strong("Pruebas:"), " estima si cambios son significativos (p-value) y reporta tamaños de efecto."),
                  tags$li(tags$strong("Correlación/PCA:"), " explora relaciones entre ejes a nivel agregado por curso.")
                ),
                tags$p(class = "text-muted", "Si en el futuro importas reportes con habilidades/indicadores, se pueden activar análisis adicionales.")
              )
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
            collapsible = TRUE,
            collapsed = TRUE,
            DTOutput("table_manifest")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Vista previa (datos filtrados)",
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
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
    known <- c("Diagnóstico", "Monitoreo", "Cierre")
    periods <- c(intersect(known, periods_raw), setdiff(periods_raw, known))
    period_choices <- setNames(periods, periods)

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

  base_filtered_no_period <- reactive({
    # Para análisis que necesitan múltiples periodos (cohortes intra-año, etc.)
    df <- data_clean()
    if (!is.null(input$year_filter) && length(input$year_filter) > 0) {
      df <- df %>% filter(.data$year %in% suppressWarnings(as.integer(input$year_filter)))
    }
    filter_grade_section(df)
  })

  # --- Tracking --------------------------------------------------------------
  output$tracking_controls_ui <- renderUI({
    df <- data_clean()
    validate(need(nrow(df) > 0, "Carga datos para habilitar trayectoria."))

    parts <- course_parts(df$curso)
    df$course <- parts$course
    courses <- sort(unique(df$course))
    years <- sort(unique(df$year))

    axes <- detect_axes(df)
    axis_choices <- c("Promedio (todos los ejes)", setdiff(axes, "Promedio (todos los ejes)"))
    axis_choices <- axis_choices[axis_choices %in% axes]

    tagList(
      selectInput("tracking_course", "Curso", choices = courses, selected = courses[[1]]),
      selectInput("tracking_year", "Año", choices = years, selected = years[[length(years)]]),
      selectInput("tracking_axis", "Medida", choices = axis_choices, selected = axis_choices[[1]])
    )
  })

  tracking_data <- reactive({
    req(input$tracking_course, input$tracking_year, input$tracking_axis)

    df <- data_clean()
    parts <- course_parts(df$curso)
    df$course <- parts$course

    df <- df %>%
      filter(.data$course %in% as.character(input$tracking_course)) %>%
      filter(.data$year %in% suppressWarnings(as.integer(input$tracking_year)))

    # Nota: para trayectoria ignoramos el filtro de Periodo para no “romper” la condición ≥2.
    track <- summarize_student_tracking(df, axis = as.character(input$tracking_axis))
    if (isTRUE(input$tracking_only_2plus)) {
      track <- tracking_2plus_filter(track)
    }

    n_show <- suppressWarnings(as.integer(input$tracking_n %||% 60))
    if (is.na(n_show) || n_show <= 0) n_show <- 60

    # Submuestreo estable para no saturar el gráfico.
    ids <- unique(paste(track$year, track$course, track$n_lista, sep = "__"))
    if (length(ids) > n_show) {
      keep_ids <- ids[seq_len(n_show)]
      keep <- paste(track$year, track$course, track$n_lista, sep = "__") %in% keep_ids
      track <- track[keep, , drop = FALSE]
    }

    track
  })

  output$plot_tracking <- renderPlotly({
    df <- tracking_data()
    validate(need(nrow(df) > 0, "No hay estudiantes con datos suficientes para trayectoria (revisa filtros)."))
    gg <- plot_student_tracking(df, title = paste0("Trayectoria: ", input$tracking_axis))
    plotly::ggplotly(gg, tooltip = c("x", "y"))
  })

  output$table_tracking <- renderDT({
    df <- tracking_data() %>%
      mutate(pct = round(.data$pct, 1)) %>%
      arrange(.data$course, .data$n_lista, .data$tipo)
    datatable(df, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
  })

  output$dl_tracking_csv <- downloadHandler(
    filename = function() "trayectoria_estudiantes.csv",
    content = function(file) {
      utils::write.csv(tracking_data(), file = file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  output$dl_tracking_png <- downloadHandler(
    filename = function() "trayectoria_estudiantes.png",
    content = function(file) {
      gg <- plot_student_tracking(tracking_data(), title = paste0("Trayectoria: ", input$tracking_axis))
      ggplot2::ggsave(file, gg, width = 12, height = 8, dpi = 300)
    }
  )

  # --- Analítica (Phase 3) --------------------------------------------------
  output$cohort_controls_ui <- renderUI({
    df <- base_filtered_no_period()
    validate(need(nrow(df) > 0, "Carga datos para habilitar cohortes."))

    metric_choices <- metric_options(df)
    parts <- course_parts(df$curso)
    periods <- sort(unique(as.character(df$tipo)))
    years <- sort(unique(df$year))

    tagList(
      selectInput("cohort_period", "Periodo", choices = periods, selected = if ("Cierre" %in% periods) "Cierre" else periods[[1]]),
      selectInput("cohort_metric", "Métrica", choices = metric_choices, selected = metric_choices[[1]]),
      sliderInput("cohort_min_points", "Mínimo de puntos (años)", min = 2, max = 4, value = 2, step = 1),
      uiOutput("cohort_picker_ui"),
      tags$small(class = "text-muted", "Cohorte = misma sección avanzando 1 grado por año (no matching individual).")
    )
  })

  cohorts_state <- reactiveVal(list(cohorts = data.frame(), map = data.frame()))

  observeEvent(list(state(), input$cohort_refresh, input$cohort_period, input$cohort_min_points), {
    df <- base_filtered_no_period()
    if (is.null(df) || nrow(df) == 0) return()
    period <- input$cohort_period %||% NULL
    min_points <- as.integer(input$cohort_min_points %||% 2)
    if (is.na(min_points) || min_points < 2) min_points <- 2
    cohorts_state(identify_cohorts(df, min_points = min_points, period = period))
  }, ignoreInit = TRUE)

  output$cohort_picker_ui <- renderUI({
    cs <- cohorts_state()
    cohorts <- cs$cohorts
    if (is.null(cohorts) || nrow(cohorts) == 0) {
      return(tags$p(class = "text-muted", "No se detectaron cohortes con esos filtros."))
    }
    cohort_ids <- cohorts$cohort_id
    checkboxGroupInput("cohort_ids", "Cohortes", choices = cohort_ids, selected = cohort_ids)
  })

  cohort_metric_tbl <- reactive({
    df <- base_filtered_no_period()
    req(input$cohort_metric, input$cohort_period)
    summarise_course_metric(df, metric = input$cohort_metric, period = input$cohort_period)
  })

  cohort_joined <- reactive({
    cs <- cohorts_state()
    cm <- cohort_metric_tbl()
    if (is.null(cs$map) || nrow(cs$map) == 0) return(data.frame())
    df <- join_cohort_timepoints(cm, cs$map)
    if (!is.null(input$cohort_ids) && length(input$cohort_ids) > 0) {
      df <- df %>% filter(.data$cohort_id %in% as.character(input$cohort_ids))
    }
    df
  })

  cohort_ref_value <- reactive({
    df <- cohort_metric_tbl()
    if (is.null(df) || nrow(df) == 0) return(NA_real_)
    mean(df$value, na.rm = TRUE)
  })

  output$plot_cohort_trajectory <- renderPlotly({
    df <- cohort_joined()
    validate(need(nrow(df) > 0, "No hay datos para cohortes (revisa filtros)."))
    metric_lbl <- as.character(input$cohort_metric %||% "Métrica")
    gg <- plot_cohort_trajectory(
      df,
      metric_label = metric_lbl,
      ref_value = cohort_ref_value(),
      show_ci = isTRUE(input$cohort_show_ci)
    )
    plotly::ggplotly(gg, tooltip = c("text", "y", "colour"))
  })

  output$table_cohort <- renderDT({
    df <- cohort_joined() %>%
      mutate(value = round(.data$value, 1), ci_low = round(.data$ci_low, 1), ci_high = round(.data$ci_high, 1)) %>%
      arrange(.data$cohort_id, .data$time_index)
    datatable(df, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
  })

  output$dl_cohort_csv <- downloadHandler(
    filename = function() "cohortes_metricas.csv",
    content = function(file) {
      utils::write.csv(cohort_joined(), file = file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  output$dl_cohort_png <- downloadHandler(
    filename = function() "cohortes_trayectoria.png",
    content = function(file) {
      gg <- plot_cohort_trajectory(
        cohort_joined(),
        metric_label = as.character(input$cohort_metric %||% "Métrica"),
        ref_value = cohort_ref_value(),
        show_ci = isTRUE(input$cohort_show_ci)
      )
      ggplot2::ggsave(file, gg, width = 12, height = 7, dpi = 300)
    }
  )

  cohort_levels_tbl <- reactive({
    df <- base_filtered_no_period()
    cs <- cohorts_state()
    req(input$cohort_period)
    if (is.null(cs$map) || nrow(cs$map) == 0) return(data.frame())
    out <- cohort_levels_distribution(df, cs$map, period = input$cohort_period)
    if (!is.null(input$cohort_ids) && length(input$cohort_ids) > 0) {
      out <- out %>% filter(.data$cohort_id %in% as.character(input$cohort_ids))
    }
    out
  })

  output$plot_cohort_area <- renderPlotly({
    df <- cohort_levels_tbl()
    validate(need(nrow(df) > 0, "No hay datos para composición de cohortes."))
    gg <- plot_cohort_levels_area(df)
    plotly::ggplotly(gg, tooltip = c("x", "y", "fill"))
  })

  output$within_controls_ui <- renderUI({
    df <- base_filtered_no_period()
    validate(need(nrow(df) > 0, "Carga datos para habilitar evolución dentro del año."))
    years <- sort(unique(df$year))
    periods <- sort(unique(as.character(df$tipo)))
    metric_choices <- metric_options(df)

    tagList(
      selectInput("within_year", "Año", choices = years, selected = years[[length(years)]]),
      checkboxGroupInput("within_periods", "Periodos", choices = periods, selected = periods),
      selectInput("within_metric", "Métrica", choices = metric_choices, selected = metric_choices[[1]])
    )
  })

  within_year_tbl <- reactive({
    req(input$within_year, input$within_periods, input$within_metric)
    df <- base_filtered_no_period() %>%
      filter(.data$year %in% as.integer(input$within_year)) %>%
      filter(.data$tipo %in% as.character(input$within_periods))

    metric <- as.character(input$within_metric)
    course_tbl <- summarise_course_metric(df, metric = metric, period = NULL) %>%
      dplyr::mutate(tipo = as.character(.data$tipo))

    if (nrow(course_tbl) == 0) return(data.frame())

    # Tendencia (delta entre primer y último periodo disponible).
    course_tbl <- course_tbl %>% dplyr::mutate(tipo_ord = period_order_key(.data$tipo))
    trend_tbl <- course_tbl %>%
      dplyr::group_by(.data$year, .data$course) %>%
      dplyr::summarise(
        first = .data$value[which.min(.data$tipo_ord)],
        last = .data$value[which.max(.data$tipo_ord)],
        delta = last - first,
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        trend = dplyr::case_when(.data$delta > 1 ~ "mejora", .data$delta < -1 ~ "baja", TRUE ~ "igual")
      )

    dplyr::left_join(course_tbl, trend_tbl, by = c("year", "course"))
  })

  output$plot_within_year <- renderPlotly({
    df <- within_year_tbl()
    validate(need(nrow(df) > 0, "No hay datos para evolución dentro del año."))
    gg <- plot_within_year_evolution(df, metric_label = as.character(input$within_metric))
    plotly::ggplotly(gg, tooltip = c("x", "y", "colour"))
  })

  # --- Pruebas estadísticas --------------------------------------------------
  output$stats_controls_ui <- renderUI({
    df <- base_filtered_no_period()
    validate(need(nrow(df) > 0, "Carga datos para habilitar pruebas."))

    years <- sort(unique(df$year))
    periods <- sort(unique(as.character(df$tipo)))
    parts <- course_parts(df$curso)
    grades <- sort(unique(parts$grade[!is.na(parts$grade)]))
    sections <- sort(unique(parts$section[!is.na(parts$section)]))
    metrics <- metric_options(df)

    tagList(
      selectInput(
        "stats_mode",
        "Análisis",
        choices = c(
          "Cambio de niveles entre años (Chi²)" = "chi",
          "Comparación de secciones (A vs B)" = "sections"
        ),
        selected = "chi"
      ),
      conditionalPanel(
        condition = "input.stats_mode == 'chi'",
        selectInput("chi_year_a", "Año A", choices = years, selected = if (2024 %in% years) 2024 else years[[1]]),
        selectInput("chi_year_b", "Año B", choices = years, selected = if (2025 %in% years) 2025 else years[[length(years)]]),
        selectInput("chi_period", "Periodo", choices = periods, selected = if ("Cierre" %in% periods) "Cierre" else periods[[1]])
      ),
      conditionalPanel(
        condition = "input.stats_mode == 'sections'",
        selectInput("sec_year", "Año", choices = years, selected = years[[length(years)]]),
        selectInput("sec_period", "Periodo", choices = periods, selected = if ("Cierre" %in% periods) "Cierre" else periods[[1]]),
        selectInput("sec_grade", "Grado", choices = grades, selected = grades[[1]]),
        selectInput("sec_a", "Sección A", choices = sections, selected = sections[[1]]),
        selectInput("sec_b", "Sección B", choices = sections, selected = if (length(sections) >= 2) sections[[2]] else sections[[1]]),
        selectInput("sec_metric", "Métrica", choices = metrics, selected = metrics[[1]])
      )
    )
  })

  stats_result <- reactive({
    df <- base_filtered_no_period()
    req(input$stats_mode)

    if (identical(input$stats_mode, "chi")) {
      req(input$chi_year_a, input$chi_year_b, input$chi_period)
      tbl <- chi_square_levels_by_grade(df, year_a = input$chi_year_a, year_b = input$chi_year_b, period = input$chi_period)
      return(list(mode = "chi", table = tbl))
    }

    req(input$sec_year, input$sec_period, input$sec_grade, input$sec_a, input$sec_b, input$sec_metric)
    res <- compare_sections_metric(
      df,
      grade = input$sec_grade,
      year = input$sec_year,
      period = input$sec_period,
      section_a = input$sec_a,
      section_b = input$sec_b,
      metric = input$sec_metric
    )
    tbl <- if (isTRUE(res$ok)) {
      data.frame(
        grade = as.integer(input$sec_grade),
        year = as.integer(input$sec_year),
        period = as.character(input$sec_period),
        section_a = as.character(input$sec_a),
        section_b = as.character(input$sec_b),
        metric = as.character(input$sec_metric),
        test = res$test,
        p_value = res$p_value,
        diff = res$diff,
        ci_low = res$ci_low,
        ci_high = res$ci_high,
        effect = res$effect,
        effect_label = res$effect_label,
        n_a = res$n_a,
        n_b = res$n_b,
        stringsAsFactors = FALSE
      )
    } else {
      data.frame()
    }
    list(mode = "sections", table = tbl, detail = res)
  })

  output$stats_summary_ui <- renderUI({
    res <- stats_result()
    if (is.null(res)) return(NULL)

    if (identical(res$mode, "chi")) {
      tbl <- res$table
      if (is.null(tbl) || nrow(tbl) == 0) return(tags$p("Sin resultados (revisa filtros)."))
      sig_n <- sum(tbl$significant, na.rm = TRUE)
      tagList(
        tags$p(tags$strong("Chi² por grado"), ": compara distribución de Nivel I/II/III entre dos años."),
        tags$p("Grados con cambio significativo (p < 0.05): ", sig_n, " / ", nrow(tbl)),
        tags$p(class = "text-muted", "Efecto (V de Cramér): 0.1 pequeño, 0.3 mediano, 0.5 grande (regla general).")
      )
    } else {
      det <- res$detail
      if (!isTRUE(det$ok)) return(tags$p(det$message %||% "Sin resultados."))
      sig <- if (isTRUE(det$p_value < 0.05)) "Sí" else "No"
      tagList(
        tags$p(tags$strong("Comparación de secciones"), ": ", det$test),
        tags$p("Diferencia (A - B): ", sprintf("%+.1f", det$diff), " p.p.  ·  p = ", signif(det$p_value, 3), "  ·  Significativo: ", sig),
        tags$p(class = "text-muted", paste0(det$effect_label, ": ", ifelse(is.na(det$effect), "NA", sprintf("%.2f", det$effect))))
      )
    }
  })

  output$table_stats <- renderDT({
    res <- stats_result()
    tbl <- res$table %||% data.frame()
    if (nrow(tbl) == 0) return(datatable(data.frame(Mensaje = "Sin resultados"), rownames = FALSE))
    datatable(tbl, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
  })

  # --- Correlación y PCA -----------------------------------------------------
  output$corr_controls_ui <- renderUI({
    df <- base_filtered_no_period()
    validate(need(nrow(df) > 0, "Carga datos para habilitar correlación."))

    years <- sort(unique(df$year))
    periods <- sort(unique(as.character(df$tipo)))

    tagList(
      checkboxGroupInput("corr_years", "Años", choices = years, selected = years),
      checkboxGroupInput("corr_periods", "Periodos", choices = periods, selected = periods),
      selectInput("corr_method", "Método", choices = c("Pearson" = "pearson", "Spearman" = "spearman"), selected = "pearson")
    )
  })

  corr_res <- reactive({
    df <- base_filtered_no_period()
    req(input$corr_years, input$corr_periods, input$corr_method)
    axes_correlation(df, method = input$corr_method, period = input$corr_periods, years = input$corr_years)
  })

  output$plot_corr <- renderPlotly({
    res <- corr_res()
    validate(need(isTRUE(res$ok), res$message %||% "No hay datos para correlación."))
    gg <- plot_correlation_heatmap(res$cor, res$p, title = "Correlación entre ejes (agregado por curso)")
    plotly::ggplotly(gg, tooltip = c("x", "y", "fill"))
  })

  output$dl_corr_csv <- downloadHandler(
    filename = function() "correlacion_ejes.csv",
    content = function(file) {
      res <- corr_res()
      validate(need(isTRUE(res$ok), "Sin datos para descargar."))
      utils::write.csv(res$cor, file = file, row.names = TRUE, fileEncoding = "UTF-8")
    }
  )

  pca_res <- reactive({
    pca_axes(corr_res(), scale = TRUE)
  })

  output$plot_pca_scree <- renderPlotly({
    res <- pca_res()
    validate(need(isTRUE(res$ok), res$message %||% "No hay datos para PCA."))
    gg <- plot_pca_scree(res$fit)
    plotly::ggplotly(gg, tooltip = c("x", "y"))
  })

  output$plot_pca_biplot <- renderPlotly({
    res <- pca_res()
    validate(need(isTRUE(res$ok), res$message %||% "No hay datos para PCA."))
    meta <- res$meta
    meta$grade <- paste0(meta$grade, "°")
    gg <- plot_pca_biplot(res$fit, meta = meta, color_col = "grade")
    plotly::ggplotly(gg, tooltip = c("x", "y", "colour"))
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
