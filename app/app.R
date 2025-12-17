## App Shiny (MVP) para gráficos DIA
## UI en español, datos locales, exportación PNG.

# --- Dependencias (CRAN) -----------------------------------------------------
# Se valida al inicio para entregar mensajes amigables a usuarios no técnicos.
required_packages <- c(
  "shiny",
  "bslib",
  "ggplot2",
  "readxl",
  "writexl",
  "zip",
  "dplyr",
  "tidyr",
  "scales",
  "ggridges"
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
library(bslib)
library(ggplot2)
library(dplyr)
library(tidyr)

# --- Módulos internos --------------------------------------------------------
# Se cargan como scripts sueltos (no es un paquete R) para facilitar el
# desarrollo y el empaquetado como app de escritorio.
source(file.path("R", "ingest.R"))
source(file.path("R", "platform_ingest.R"))
source(file.path("R", "assistant.R"))
source(file.path("R", "template.R"))
source(file.path("R", "interpretation.R"))
source(file.path("R", "tables.R"))
source(file.path("R", "quality.R"))
source(file.path("R", "compare.R"))
source(file.path("R", "export_helpers.R"))
source(file.path("R", "presets.R"))
source(file.path("R", "transform.R"))
source(file.path("R", "themes.R"))
source(file.path("R", "plots", "promedio.R"))
source(file.path("R", "plots", "distribucion.R"))
source(file.path("R", "plots", "heatmap.R"))
source(file.path("R", "plots", "violin.R"))
source(file.path("R", "plots", "tendencia.R"))
source(file.path("R", "plots", "nivel_logro.R"))
source(file.path("R", "plots", "crecimiento.R"))

options(shiny.maxRequestSize = 100 * 1024^2) # 100 MB

# --- UI ----------------------------------------------------------------------
# Nota: el selector de carpeta usa el diálogo nativo del navegador. Para poder
# elegir carpeta (y no solo archivos) activamos el atributo `webkitdirectory`
# en el `<input type=file>`. Esto funciona bien en Chrome/Edge/Electron.
ui <- tagList(
  tags$head(
    tags$style(
      HTML(
        "
        /* Limpieza del panel izquierdo: no mostrar nombres de archivos seleccionados */
        .shiny-file-input input[type=file] { color: transparent !important; }
        .shiny-file-input input[type=file]::file-selector-button { color: inherit !important; }
        .shiny-file-input input[type=file]::-webkit-file-upload-button { color: inherit !important; }
        .shiny-file-input .fileinput-filename { display: none !important; }
        "
      )
    ),
    tags$script(
      HTML(
        "
        // Habilita selección de carpeta (Chrome/Edge/Electron) usando el file picker nativo.
        // Shiny no expone esto de forma nativa, pero el input soporta `webkitdirectory`.
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
      )
    )
  ),
  page_sidebar(
  title = "Gráficos DIA (MVP)",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  sidebar = sidebar(
    width = 360,
    accordion(
      id = "sidebar_acc",
      open = "load",
      accordion_panel(
        title = "1) Carga de datos",
        value = "load",
        radioButtons(
          "data_mode",
          "Origen de datos",
          choices = c(
            "Excel (.xlsx) — formato de la app" = "xlsx",
            "Carpeta DIA plataforma (.xls) — muchos cursos/áreas" = "folder"
          ),
          selected = "xlsx"
        ),
        conditionalPanel(
          condition = "input.data_mode == 'xlsx'",
          fileInput("file1", "Archivo 1 (obligatorio)", accept = ".xlsx"),
          uiOutput("sheet1_ui"),
          fileInput("file2", "Archivo 2 (opcional)", accept = ".xlsx"),
          uiOutput("sheet2_ui"),
          downloadButton("download_template", "Descargar plantilla Excel")
        ),
        conditionalPanel(
          condition = "input.data_mode == 'folder'",
          tags$small(class = "text-muted", "Lee archivos .xls exportados por la plataforma DIA (tabla desde fila 13)."),
          fileInput("folder_files", "Seleccionar carpeta…", accept = c(".xls"), multiple = TRUE),
          tags$small(
            class = "text-muted",
            "Tip: en el diálogo, selecciona la carpeta (no un archivo) para cargar todos los .xls de una vez.",
            "Si tu navegador no lo permite, abre la carpeta y selecciona todos los .xls (Ctrl+A)."
          ),
          actionButton("folder_load", "Cargar carpeta", class = "btn-primary"),
          uiOutput("folder_load_ui")
        ),
        checkboxInput("anon", "Modo anónimo (no exportar nombres)", value = TRUE),
        conditionalPanel(
          condition = "input.anon == true",
          selectInput(
            "anon_method",
            "Tipo de anonimización",
            choices = c("ID por N_lista" = "n_lista", "ID aleatorio (estable)" = "random"),
            selected = "n_lista"
          ),
          numericInput("anon_seed", "Semilla (IDs aleatorios)", value = 1234, min = 1, max = 999999, step = 1)
        ),
        hr(),
        uiOutput("summary_ui")
      ),
      accordion_panel(
        title = "2) Gráfico",
        value = "plot",
        selectInput(
          "plot_type",
          "Tipo de gráfico",
          choices = c(
            "A) Promedio por curso y Tipo" = "promedio",
            "B) Distribución" = "distribucion",
            "C) Heatmap (curso/tipo × eje)" = "heatmap",
            "D) Violín / Ridgeline" = "violin",
            "E) Tendencia temporal" = "tendencia",
            "F) Nivel de logro" = "nivel_logro",
            "G) Crecimiento por estudiante (delta)" = "crecimiento"
          )
        ),
        uiOutput("filters_ui"),
        hr(),
        fluidRow(
          column(
            6,
            selectInput(
              "facet_row",
              "Facets (fila)",
              choices = c(
                "OFF" = "off",
                "Área" = "area",
                "Curso" = "curso",
                "Tipo" = "tipo",
                "Año" = "year",
                "Eje" = "eje"
              ),
              selected = "off"
            )
          ),
          column(
            6,
            selectInput(
              "facet_col",
              "Facets (columna)",
              choices = c(
                "OFF" = "off",
                "Área" = "area",
                "Curso" = "curso",
                "Tipo" = "tipo",
                "Año" = "year",
                "Eje" = "eje"
              ),
              selected = "off"
            )
          )
        )
      ),
      accordion_panel(
        title = "3) Orden y transparencia",
        value = "order",
        uiOutput("order_ui"),
        hr(),
        sliderInput("alpha_bars", "Transparencia (barras)", min = 0.1, max = 1, value = 0.85, step = 0.05),
        sliderInput("alpha_lines", "Transparencia (líneas)", min = 0.1, max = 1, value = 0.9, step = 0.05)
      ),
      accordion_panel(
        title = "4) Estilo y exportación",
        value = "style",
        selectInput(
          "style_preset",
          "Plantilla de estilo",
          choices = c(
            "Informe clásico" = "classic",
            "Minimal" = "minimal",
            "Oscuro" = "dark",
            "Alto contraste" = "contrast"
          ),
          selected = "classic"
        ),
        selectInput("palette_fill", "Paleta (fill)", choices = available_palettes(), selected = "Okabe-Ito"),
        selectInput("palette_color", "Paleta (color)", choices = available_palettes(), selected = "Okabe-Ito"),
        hr(),
        textInput("title", "Título", value = ""),
        textInput("subtitle", "Subtítulo", value = ""),
        textInput("xlab", "Eje X", value = ""),
        textInput("ylab", "Eje Y", value = ""),
        hr(),
        selectInput(
          "export_res",
          "Resolución PNG",
          choices = c("Baja (1200×800)" = "low", "Media (2000×1300)" = "med", "Alta (3200×2100)" = "high"),
          selected = "med"
        ),
        downloadButton("download_png", "Descargar PNG")
      ),
      accordion_panel(
        title = "5) Configuración",
        value = "config",
        downloadButton("download_preset", "Guardar configuración (.rds)"),
        fileInput("preset_in", "Cargar configuración (.rds)", accept = ".rds"),
        actionButton("preset_reset", "Restaurar defaults")
      )
    )
  ),
  layout_columns(
    navset_card_tab(
      id = "main_tabs",
      nav_panel(
        title = "Inicio",
        value = "inicio",
        card(
          card_header("Generador de gráficos DIA"),
          tags$p("Objetivo: ayudarte a generar gráficos claros y exportables (PNG/ZIP) desde archivos Excel, sin escribir código."),
          tags$h5("Flujo recomendado"),
          tags$ol(
            tags$li("Carga 1–2 archivos Excel en la sección “1) Carga de datos”."),
            tags$li("Elige el tipo de gráfico y ajusta filtros (curso, año, tipo, eje)."),
            tags$li("Opcional: usa facets (fila/columna) para comparar dos variables."),
            tags$li("Ajusta estilo (plantillas, paletas, títulos)."),
            tags$li("Exporta PNG o ZIP (informe / lote).")
          ),
          tags$h5("Privacidad"),
          tags$ul(
            tags$li("Los datos se procesan localmente en tu computador."),
            tags$li("Activa “Modo anónimo” para evitar exportar nombres reales.")
          ),
          tags$div(
            style = "display:flex; gap: 8px; flex-wrap: wrap;",
            actionButton("go_grafico", "Ir a gráficos", class = "btn-primary"),
            actionButton("go_asistente", "Usar asistente", class = "btn-secondary")
          )
        )
      ),
      nav_panel(
        title = "Data",
        value = "data",
        layout_columns(
          card(
            card_header("Resumen de datos cargados"),
            uiOutput("summary_ui"),
            uiOutput("data_details_ui")
          ),
          card(
            card_header("Alumnos por curso y tipo"),
            plotOutput("data_overview_plot", height = "360px")
          ),
          col_widths = c(6, 6)
        ),
        card(
          card_header("Tabla resumen (combinaciones)"),
          tableOutput("data_summary_table")
        ),
        conditionalPanel(
          condition = "input.data_mode == 'folder'",
          card(
            card_header("Archivos importados (DIA plataforma)"),
            checkboxInput("show_filenames", "Mostrar nombre del archivo (avanzado)", value = FALSE),
            tableOutput("folder_manifest")
          )
        )
      ),
      nav_panel(
        title = "Gráfico",
        value = "grafico",
        plotOutput("plot", height = "650px"),
        hr(),
        accordion(
          open = NULL,
          accordion_panel(
            title = "Texto sugerido para informe",
            uiOutput("interpretation_ui")
          ),
          accordion_panel(
            title = "Tablas (descarga CSV/XLSX)",
            uiOutput("tables_ui")
          ),
          accordion_panel(
            title = "Calidad de datos",
            uiOutput("quality_ui")
          ),
          accordion_panel(
            title = "Comparar archivos",
            uiOutput("compare_ui")
          ),
          accordion_panel(
            title = "Exportar (datos + ZIPs)",
            uiOutput("export_ui")
          )
        )
      ),
      nav_panel(
        title = "Asistente",
        value = "asistente",
        uiOutput("assistant_ui")
      )
    ),
    col_widths = c(12)
  )
)
)

# --- Server ------------------------------------------------------------------
# Contiene toda la lógica reactiva: carga de datos, filtros, gráficos,
# tablas, calidad y exportación.
server <- function(input, output, session) {
  # Recomendación para empaquetado tipo RInno/Electron:
  # asegurar que al cerrar la ventana se termine la sesión de R.
  if (!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }

  observeEvent(list(input$facet_row, input$facet_col), {
    req(input$facet_row, input$facet_col)
    if (!identical(input$facet_row, "off") && identical(input$facet_row, input$facet_col)) {
      updateSelectInput(session, "facet_col", selected = "off")
      showNotification("Facet columna no puede ser igual a facet fila; se desactivó la columna.", type = "message")
    }
  }, ignoreInit = TRUE)

  observeEvent(list(input$area, input$facet_row, input$facet_col), {
    req(input$area, input$facet_row, input$facet_col)
    if (length(input$area) > 1 && identical(input$facet_row, "off") && identical(input$facet_col, "off")) {
      updateSelectInput(session, "facet_row", selected = "area")
      showNotification("Seleccionaste múltiples áreas: se activó Facets (fila) = Área.", type = "message")
    }
  }, ignoreInit = TRUE)

  observeEvent(input$go_grafico, {
    updateTabsetPanel(session, "main_tabs", selected = "grafico")
  })

  observeEvent(input$go_asistente, {
    updateTabsetPanel(session, "main_tabs", selected = "asistente")
  })

  folder_state <- reactiveVal(NULL)

  observeEvent(input$folder_load, {
    validate(need(!is.null(input$folder_files) && nrow(input$folder_files) > 0, "Selecciona una carpeta (o archivos .xls) primero."))
    folder_state(NULL)

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

    if (is.null(res)) return()

    folder_state(res)
    ok_n <- sum(res$manifest$status == "ok", na.rm = TRUE)
    err_n <- sum(res$manifest$status == "error", na.rm = TRUE)
    showNotification(
      paste0("Carpeta cargada: ", ok_n, " archivo(s) OK", if (err_n > 0) paste0(" · ", err_n, " con error") else ""),
      type = if (err_n > 0) "warning" else "message"
    )
  })

  output$folder_load_ui <- renderUI({
    st <- folder_state()
    if (is.null(st)) {
      if (is.null(input$folder_files) || nrow(input$folder_files) == 0) {
        return(tags$small(class = "text-muted", "Selecciona una carpeta (o archivos .xls) y luego presiona “Cargar carpeta”."))
      }
      return(tags$small(class = "text-muted", paste0("Archivos seleccionados: ", nrow(input$folder_files), " · Presiona “Cargar carpeta”.")))
    }

    ok_n <- sum(st$manifest$status == "ok", na.rm = TRUE)
    err_n <- sum(st$manifest$status == "error", na.rm = TRUE)
    tagList(
      tags$small(class = "text-muted", paste0("Archivos: ", nrow(st$manifest), " · OK: ", ok_n, " · Error: ", err_n)),
      if (err_n > 0) tags$small(class = "text-danger", "Revisa la columna “message” para ver el motivo.")
    )
  })

  output$folder_manifest <- renderTable({
    st <- folder_state()
    if (is.null(st)) return(NULL)

    df <- st$manifest
    if (!isTRUE(input$show_filenames)) {
      df$file <- NULL
    }

    keep <- intersect(names(df), c("file", "area", "curso", "tipo", "year", "status", "message"))
    df <- df[, keep, drop = FALSE]

    if ("status" %in% names(df)) {
      df$status <- ifelse(df$status == "ok", "OK", "Error")
    }

    rename <- c(
      file = "Archivo",
      area = "Área",
      curso = "Curso",
      tipo = "Tipo",
      year = "Año",
      status = "Estado",
      message = "Mensaje"
    )
    names(df) <- unname(rename[names(df)])
    df
  })

  output$sheet1_ui <- renderUI({
    req(input$file1)
    sheets <- excel_sheets_safe(input$file1$datapath)
    selectInput("sheet1", "Hoja (Archivo 1)", choices = sheets, selected = sheets[[1]])
  })

  output$sheet2_ui <- renderUI({
    req(input$file2)
    sheets <- excel_sheets_safe(input$file2$datapath)
    selectInput("sheet2", "Hoja (Archivo 2)", choices = sheets, selected = sheets[[1]])
  })

  output$assistant_ui <- renderUI({
    tagList(
      card(
        card_header("Asistente para elegir el gráfico"),
        p(
          "Responde preguntas de Sí/No y la app te recomendará el tipo de gráfico más útil para tu informe.",
          "Luego puedes aplicar la recomendación y ajustar filtros/estilo."
        ),
        radioButtons(
          "asst_need_level",
          "1) ¿Tu informe necesita mostrar categorías de NIVEL DE LOGRO (barras apiladas por nivel)?",
          choices = c("Sí" = "yes", "No" = "no"),
          selected = character(0),
          inline = TRUE
        ),
        conditionalPanel(
          condition = "input.asst_need_level == 'no'",
          radioButtons(
            "asst_need_growth",
            "2) ¿Necesitas mostrar el cambio por estudiante entre dos Tipos (delta o slope chart)?",
            choices = c("Sí" = "yes", "No" = "no"),
            selected = character(0),
            inline = TRUE
          )
        ),
        conditionalPanel(
          condition = "input.asst_need_level == 'no' && input.asst_need_growth == 'no'",
          radioButtons(
            "asst_need_dist",
            "3) ¿Te importa más la distribución/variabilidad que el promedio (boxplot/histograma)?",
            choices = c("Sí" = "yes", "No" = "no"),
            selected = character(0),
            inline = TRUE
          )
        ),
        hr(),
        accordion(
          open = NULL,
          accordion_panel(
            title = "Ajustes opcionales (Sí/No)",
            radioButtons(
              "asst_want_facets",
              "4) ¿Quieres separar el gráfico en paneles (facets) para comparar?",
              choices = c("Sí" = "yes", "No" = "no"),
              selected = "no",
              inline = TRUE
            ),
            conditionalPanel(
              condition = "input.asst_want_facets == 'yes'",
              radioButtons(
                "asst_facet_year",
                "4.1) ¿Comparas Años?",
                choices = c("Sí" = "yes", "No" = "no"),
                selected = "no",
                inline = TRUE
              ),
              conditionalPanel(
                condition = "input.asst_facet_year == 'no'",
                radioButtons(
                  "asst_facet_curso",
                  "4.2) ¿Comparas Cursos?",
                  choices = c("Sí" = "yes", "No" = "no"),
                  selected = "no",
                  inline = TRUE
                )
              ),
              conditionalPanel(
                condition = "input.asst_facet_year == 'no' && input.asst_facet_curso == 'no'",
                radioButtons(
                  "asst_facet_tipo",
                  "4.3) ¿Comparas Tipos?",
                  choices = c("Sí" = "yes", "No" = "no"),
                  selected = "no",
                  inline = TRUE
                )
              ),
              conditionalPanel(
                condition = "input.asst_facet_year == 'no' && input.asst_facet_curso == 'no' && input.asst_facet_tipo == 'no'",
                radioButtons(
                  "asst_facet_eje",
                  "4.4) ¿Comparas Ejes?",
                  choices = c("Sí" = "yes", "No" = "no"),
                  selected = "no",
                  inline = TRUE
                )
              )
            ),
            conditionalPanel(
              condition = "input.asst_need_level == 'no' && input.asst_need_growth == 'no' && input.asst_need_dist == 'yes'",
              radioButtons(
                "asst_dist_hist",
                "5) Para distribución: ¿prefieres histograma (forma) en vez de boxplot?",
                choices = c("Sí" = "yes", "No" = "no"),
                selected = "no",
                inline = TRUE
              )
            ),
            conditionalPanel(
              condition = "input.asst_need_level == 'no' && input.asst_need_growth == 'yes'",
              radioButtons(
                "asst_growth_delta",
                "5) Para crecimiento: ¿prefieres barras de delta (ranking) en vez de líneas (slope)?",
                choices = c("Sí" = "yes", "No" = "no"),
                selected = "yes",
                inline = TRUE
              ),
              radioButtons(
                "asst_growth_topbottom",
                "6) Para crecimiento: ¿mostrar solo Top + Bottom N en vez de todos?",
                choices = c("Sí" = "yes", "No" = "no"),
                selected = "no",
                inline = TRUE
              )
            )
          )
        ),
        hr(),
        uiOutput("assistant_result_ui"),
        actionButton("asst_apply", "Aplicar recomendación", class = "btn-primary")
      )
    )
  })

  output$download_template <- downloadHandler(
    filename = function() {
      "plantilla_DIA.xlsx"
    },
    content = function(file) {
      write_dia_excel_template(file)
    }
  )

  preset_pending <- reactiveVal(NULL)

  sanitize_preset <- function(preset, ch) {
    if (is.null(preset) || !is.list(preset)) return(NULL)
    ch <- ch %||% list()

    sanitize_multi <- function(x, allowed) {
      if (is.null(x) || is.null(allowed)) return(x)
      x <- unique(as.character(x))
      allowed <- unique(as.character(allowed))
      keep <- intersect(x, allowed)
      if (length(keep) == 0) allowed else keep
    }
    sanitize_single <- function(x, allowed) {
      if (is.null(x) || is.null(allowed)) return(x)
      allowed <- unique(as.character(allowed))
      x <- as.character(x)[[1]]
      if (x %in% allowed) x else allowed[[1]]
    }

    facet_allowed <- c("off", "area", "curso", "tipo", "year", "eje")
    preset$facet_row <- sanitize_single(preset$facet_row %||% preset$facet %||% "off", facet_allowed)
    preset$facet_col <- sanitize_single(preset$facet_col %||% "off", facet_allowed)
    if (!identical(preset$facet_row, "off") && identical(preset$facet_row, preset$facet_col)) {
      preset$facet_col <- "off"
    }

    preset$area <- sanitize_multi(preset$area, ch$areas)
    preset$curso <- sanitize_multi(preset$curso, ch$cursos)
    preset$year <- sanitize_multi(preset$year, ch$years)
    preset$tipo <- sanitize_multi(preset$tipo, ch$tipos)
    preset$fuente <- sanitize_multi(preset$fuente, ch$fuentes)
    preset$eje <- sanitize_multi(preset$eje, ch$ejes)
    preset$axis_pool <- sanitize_single(preset$axis_pool %||% "common", c("common", "all"))
    preset$tipo_a <- sanitize_single(preset$tipo_a, ch$tipos)
    preset$tipo_b <- sanitize_single(preset$tipo_b, ch$tipos)
    preset$tipo_order <- sanitize_multi(preset$tipo_order, ch$tipos)
    preset$nivel_order <- sanitize_multi(preset$nivel_order, ch$niveles)

    preset
  }

  output$download_preset <- downloadHandler(
    filename = function() {
      paste0("config_DIA_", format(Sys.Date(), "%Y-%m-%d"), ".rds")
    },
    content = function(file) {
      cfg <- preset_capture(input)
      saveRDS(cfg, file = file)
    }
  )

  observeEvent(input$preset_in, {
    req(input$preset_in$datapath)
    cfg <- readRDS(input$preset_in$datapath)
    if (!is.null(input$file1)) {
      cfg <- sanitize_preset(cfg, choices())
    }
    preset_pending(cfg)
    showNotification("Configuración cargada. Aplicando…", type = "message")
  })

  observeEvent(input$preset_reset, {
    req(input$file1)
    cfg <- preset_defaults(choices())
    preset_pending(cfg)
    showNotification("Restaurando defaults…", type = "message")
  })

  observe({
    cfg <- preset_pending()
    if (is.null(cfg)) return()
    preset_apply_step(session, input, cfg)
    if (preset_is_applied(input, cfg)) {
      preset_pending(NULL)
      showNotification("Configuración aplicada.", type = "message")
    }
  })

  output$interpretation_ui <- renderUI({
    pts <- interpretation_points(
      plot_type = input$plot_type %||% "promedio",
      ejes = input$eje %||% NULL,
      cursos = input$curso %||% NULL,
      tipos = input$tipo %||% NULL,
      years = input$year %||% NULL,
      areas = input$area %||% NULL,
      facet_row = input$facet_row %||% "off",
      facet_col = input$facet_col %||% "off",
      dist_kind = input$dist_kind %||% NULL,
      heatmap_dim = input$heatmap_dim %||% NULL,
      violin_kind = input$violin_kind %||% NULL,
      violin_group = input$violin_group %||% NULL,
      trend_group = input$trend_group %||% NULL,
      tipo_a = input$tipo_a %||% NULL,
      tipo_b = input$tipo_b %||% NULL,
      growth_kind = input$growth_kind %||% NULL,
      rank_mode = input$rank_mode %||% NULL,
      anonymous = isTRUE(input$anon)
    )

    tagList(
      tags$h4(pts$title),
      tags$ul(lapply(pts$bullets, tags$li))
    )
  })

  assistant_rec <- reactive({
    assistant_recommendation(
      need_level = input$asst_need_level %||% NULL,
      need_growth = input$asst_need_growth %||% NULL,
      need_dist = input$asst_need_dist %||% NULL,
      want_facets = input$asst_want_facets %||% NULL,
      facet_year = input$asst_facet_year %||% NULL,
      facet_curso = input$asst_facet_curso %||% NULL,
      facet_tipo = input$asst_facet_tipo %||% NULL,
      facet_eje = input$asst_facet_eje %||% NULL,
      dist_hist = input$asst_dist_hist %||% NULL,
      growth_delta = input$asst_growth_delta %||% NULL,
      growth_topbottom = input$asst_growth_topbottom %||% NULL
    )
  })

  output$assistant_result_ui <- renderUI({
    rec <- assistant_rec()
    if (!isTRUE(rec$complete)) {
      return(tags$div(class = "text-muted", rec$prompt))
    }

    tagList(
      h4(paste0("Recomendación: ", rec$label)),
      tags$p(tags$strong("Ajustes sugeridos: "), rec$settings_label),
      tags$ul(
        tags$li(rec$why),
        tags$li(rec$what_you_get)
      ),
      tags$small(class = "text-muted", "Puedes cambiar la recomendación ajustando tus respuestas.")
    )
  })

  asst_pending <- reactiveVal(NULL)

  observeEvent(input$asst_apply, {
    rec <- assistant_rec()
    if (!isTRUE(rec$complete)) {
      showNotification("Responde las preguntas para obtener una recomendación.", type = "warning")
      return()
    }

    updateSelectInput(session, "plot_type", selected = rec$plot_type)
    asst_pending(rec$settings %||% NULL)
    updateTabsetPanel(session, "main_tabs", selected = "grafico")
    showNotification(paste0("Se seleccionó: ", rec$label), type = "message")
  })

  observe({
    pending <- asst_pending()
    if (is.null(pending)) {
      return()
    }

    if (!is.null(pending$facet_row) && !identical(input$facet_row, pending$facet_row)) {
      updateSelectInput(session, "facet_row", selected = pending$facet_row)
    }
    if (!is.null(pending$facet_col) && !identical(input$facet_col, pending$facet_col)) {
      updateSelectInput(session, "facet_col", selected = pending$facet_col)
    }

    if (identical(input$plot_type, "distribucion") &&
      !is.null(pending$dist_kind) &&
      !is.null(input$dist_kind) &&
      !identical(input$dist_kind, pending$dist_kind)) {
      updateRadioButtons(session, "dist_kind", selected = pending$dist_kind)
    }

    if (identical(input$plot_type, "crecimiento")) {
      if (!is.null(pending$growth_kind) && !is.null(input$growth_kind) && !identical(input$growth_kind, pending$growth_kind)) {
        updateRadioButtons(session, "growth_kind", selected = pending$growth_kind)
      }
      if (!is.null(pending$rank_mode) && !is.null(input$rank_mode) && !identical(input$rank_mode, pending$rank_mode)) {
        updateRadioButtons(session, "rank_mode", selected = pending$rank_mode)
      }
    }

    done <- TRUE
    if (!is.null(pending$facet_row) && !identical(input$facet_row, pending$facet_row)) done <- FALSE
    if (!is.null(pending$facet_col) && !identical(input$facet_col, pending$facet_col)) done <- FALSE
    if (identical(input$plot_type, "distribucion") && !is.null(pending$dist_kind)) {
      if (is.null(input$dist_kind) || !identical(input$dist_kind, pending$dist_kind)) done <- FALSE
    }
    if (identical(input$plot_type, "crecimiento")) {
      if (!is.null(pending$growth_kind) && (is.null(input$growth_kind) || !identical(input$growth_kind, pending$growth_kind))) done <- FALSE
      if (!is.null(pending$rank_mode) && (is.null(input$rank_mode) || !identical(input$rank_mode, pending$rank_mode))) done <- FALSE
    }

    if (done) {
      asst_pending(NULL)
    }
  })

  data_raw <- reactive({
    mode <- input$data_mode %||% "xlsx"

    if (identical(mode, "folder")) {
      st <- folder_state()
      validate(need(!is.null(st), "Selecciona una carpeta y presiona “Cargar carpeta”."))
      validate(need(nrow(st$data) > 0, "No se pudieron cargar filas desde la carpeta (revisa errores)."))
      return(st$data)
    }

    req(input$file1)
    df1 <- tryCatch(
      read_dia_excel(
        path = input$file1$datapath,
        sheet = input$sheet1 %||% 1,
        source_name = "Archivo 1",
        source_file = input$file1$name
      ),
      error = function(e) {
        validate(need(FALSE, paste0("Error en Archivo 1: ", e$message)))
      }
    )

    if (is.null(input$file2)) {
      return(df1)
    }

    df2 <- tryCatch(
      read_dia_excel(
        path = input$file2$datapath,
        sheet = input$sheet2 %||% 1,
        source_name = "Archivo 2",
        source_file = input$file2$name
      ),
      error = function(e) {
        validate(need(FALSE, paste0("Error en Archivo 2: ", e$message)))
      }
    )

    bind_rows(df1, df2)
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
    apply_anonymity(
      df,
      anonymous = isTRUE(input$anon),
      method = input$anon_method %||% "n_lista",
      seed = input$anon_seed %||% 1234
    )
  })

  choices <- reactive({
    df <- data_clean()
    list(
      areas = sort(unique(df$area)),
      cursos = sort(unique(df$curso)),
      tipos = sort(unique(df$tipo)),
      years = sort(unique(df$year)),
      niveles = sort(unique(df$nivel_logro[!is.na(df$nivel_logro) & nzchar(df$nivel_logro)])),
      ejes = detect_axes(df),
      fuentes = sort(unique(df$fuente)),
      rbds = sort(unique(df$rbd[!is.na(df$rbd) & nzchar(df$rbd)]))
    )
  })

  output$order_ui <- renderUI({
    ch <- choices()
    tipo_selected <- isolate(input$tipo_order)
    if (is.null(tipo_selected) || length(tipo_selected) == 0) {
      tipo_selected <- default_tipo_order(ch$tipos)
    } else {
      tipo_selected <- c(tipo_selected, setdiff(ch$tipos, tipo_selected))
    }

    nivel_selected <- isolate(input$nivel_order)
    if (is.null(nivel_selected) || length(nivel_selected) == 0) {
      nivel_selected <- ch$niveles
    } else {
      nivel_selected <- c(nivel_selected, setdiff(ch$niveles, nivel_selected))
    }

    tagList(
      selectizeInput(
        "tipo_order",
        "Orden de Tipo (arrastra para reordenar)",
        choices = ch$tipos,
        selected = tipo_selected,
        multiple = TRUE,
        options = list(plugins = c("drag_drop", "remove_button"), persist = TRUE)
      ),
      selectizeInput(
        "nivel_order",
        "Orden NIVEL DE LOGRO (arrastra para reordenar)",
        choices = ch$niveles,
        selected = nivel_selected,
        multiple = TRUE,
        options = list(plugins = c("drag_drop", "remove_button"), persist = TRUE)
      )
    )
  })

  output$summary_ui <- renderUI({
    df <- data_clean()
    ch <- choices()

    tagList(
      layout_columns(
        value_box(
          title = "Filas",
          value = format(nrow(df), big.mark = ".", decimal.mark = ","),
          theme = "primary"
        ),
        value_box(
          title = "Cursos",
          value = length(ch$cursos),
          theme = "info"
        ),
        value_box(
          title = "Áreas",
          value = length(ch$areas),
          theme = "info"
        ),
        value_box(
          title = "Tipos",
          value = length(ch$tipos),
          theme = "info"
        ),
        value_box(
          title = "Ejes",
          value = length(ch$ejes),
          theme = "secondary"
        ),
        col_widths = c(3, 3, 3, 3)
      ),
      if (length(ch$ejes) > 0) {
        tags$small(
          "Ejes detectados: ",
          paste(ch$ejes, collapse = ", ")
        )
      }
    )
  })

  output$data_details_ui <- renderUI({
    df <- data_clean()
    ch <- choices()

    rbds <- sort(unique(df$rbd[!is.na(df$rbd) & nzchar(df$rbd)]))
    years <- sort(unique(df$year[!is.na(df$year)]))

    details <- list(
      tags$small(class = "text-muted", paste0("Años: ", paste(years, collapse = ", ")))
    )

    if (length(rbds) > 0) {
      details <- c(details, list(tags$small(class = "text-muted", paste0("RBD: ", paste(rbds, collapse = ", ")))))
    }

    if (length(ch$tipos) > 0) {
      details <- c(details, list(tags$small(class = "text-muted", paste0("Tipos: ", paste(ch$tipos, collapse = ", ")))))
    }

    if (identical(input$data_mode %||% "xlsx", "folder")) {
      st <- folder_state()
      if (!is.null(st)) {
        ok_n <- sum(st$manifest$status == "ok", na.rm = TRUE)
        err_n <- sum(st$manifest$status == "error", na.rm = TRUE)
        details <- c(
          details,
          list(tags$small(class = "text-muted", paste0("Archivos (carpeta): ", nrow(st$manifest), " · OK: ", ok_n, " · Error: ", err_n)))
        )
      }
    }

    tagList(details)
  })

  output$data_overview_plot <- renderPlot({
    df <- data_clean()

    counts <- df %>%
      count(year, area, curso, tipo, name = "n") %>%
      arrange(year, area, curso, tipo)

    validate(need(nrow(counts) > 0, "No hay datos para resumir."))

    gg <- ggplot(counts, aes(x = curso, y = n, fill = tipo)) +
      geom_col(position = position_dodge(width = 0.8), alpha = 0.85) +
      labs(x = "Curso", y = "N estudiantes", fill = "Tipo") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    years_n <- length(unique(counts$year))
    areas_n <- length(unique(counts$area))
    if (years_n > 1 && areas_n > 1) {
      gg <- gg + facet_grid(year ~ area, scales = "free_x")
    } else if (years_n > 1) {
      gg <- gg + facet_wrap(~year, scales = "free_x")
    } else if (areas_n > 1) {
      gg <- gg + facet_wrap(~area, scales = "free_x")
    }

    gg
  })

  output$data_summary_table <- renderTable({
    df <- data_clean()

    tbl <- df %>%
      count(year, area, curso, tipo, name = "N estudiantes") %>%
      arrange(year, area, curso, tipo)

    names(tbl) <- c("Año", "Área", "Curso", "Tipo", "N estudiantes")
    tbl
  })

  output$filters_ui <- renderUI({
    ch <- choices()
    df_all <- data_clean()

    ui_list <- list()

    # Helpers para preservar selecciones cuando cambian choices
    preserve_multi <- function(current, allowed, default = allowed) {
      current <- unique(as.character(current %||% character()))
      allowed <- unique(as.character(allowed %||% character()))
      if (length(allowed) == 0) return(character())
      if (length(current) == 0) return(default)
      keep <- intersect(current, allowed)
      if (length(keep) == 0) default else keep
    }

    # Área (siempre)
    area_selected <- preserve_multi(isolate(input$area), ch$areas, default = ch$areas)
    ui_list <- c(
      ui_list,
      list(selectInput("area", "Área(s)", choices = ch$areas, selected = area_selected, multiple = TRUE))
    )

    if (length(ch$fuentes) > 1) {
      fuente_selected <- preserve_multi(isolate(input$fuente), ch$fuentes, default = ch$fuentes)
      ui_list <- c(
        ui_list,
        list(selectInput("fuente", "Archivo(s)", choices = ch$fuentes, selected = fuente_selected, multiple = TRUE))
      )
    }

    curso_selected <- preserve_multi(isolate(input$curso), ch$cursos, default = ch$cursos)
    ui_list <- c(ui_list, list(selectInput("curso", "Curso(s)", choices = ch$cursos, selected = curso_selected, multiple = TRUE)))

    year_selected <- preserve_multi(isolate(input$year), ch$years, default = ch$years)
    ui_list <- c(
      ui_list,
      list(
        selectInput(
          "year",
          "Año(s)",
          choices = ch$years,
          selected = year_selected,
          multiple = TRUE
        )
      )
    )

    if (!identical(input$plot_type, "crecimiento")) {
      tipo_selected <- preserve_multi(isolate(input$tipo), ch$tipos, default = ch$tipos)
      ui_list <- c(
        ui_list,
        list(selectInput("tipo", "Tipo(s)", choices = ch$tipos, selected = tipo_selected, multiple = TRUE))
      )
    }

    needs_axes <- input$plot_type %in% c("promedio", "distribucion", "crecimiento", "heatmap", "violin", "tendencia")
    if (isTRUE(needs_axes)) {
      df_area <- df_all %>% filter(.data$area %in% area_selected)
      axes_all <- detect_axes(df_area)
      axes_all <- axes_all[vapply(axes_all, function(ax) any(!is.na(df_area[[ax]])), logical(1))]

      pool_mode <- input$axis_pool %||% "common"
      axes_use <- axes_all
      if (identical(pool_mode, "common") && length(area_selected) > 1) {
        per_area <- lapply(area_selected, function(a) {
          df_a <- df_area %>% filter(.data$area == a)
          axes_all[vapply(axes_all, function(ax) any(!is.na(df_a[[ax]])), logical(1))]
        })
        axes_use <- Reduce(intersect, per_area)
      }
      if (length(axes_use) == 0) axes_use <- axes_all

      if (length(area_selected) > 1) {
        ui_list <- c(
          ui_list,
          list(
            radioButtons(
              "axis_pool",
              "Ejes disponibles",
              choices = c("Comunes entre áreas" = "common", "Todos (unión)" = "all"),
              selected = pool_mode,
              inline = TRUE
            )
          )
        )
      }

      eje_selected <- preserve_multi(isolate(input$eje), axes_use, default = axes_use[[1]])
      ui_list <- c(ui_list, list(selectInput("eje", "Eje / ámbito", choices = axes_use, selected = eje_selected, multiple = TRUE)))
    }

    if (identical(input$plot_type, "distribucion")) {
      ui_list <- c(
        ui_list,
        list(
          radioButtons(
            "dist_kind",
            "Tipo de distribución",
            choices = c("Boxplot" = "box", "Histograma" = "hist"),
            inline = TRUE
          )
        )
      )
    }

    if (identical(input$plot_type, "heatmap")) {
      ui_list <- c(
        ui_list,
        list(
          selectInput(
            "heatmap_dim",
            "Eje horizontal",
            choices = c("Curso" = "curso", "Tipo" = "tipo"),
            selected = "curso"
          )
        )
      )
    }

    if (identical(input$plot_type, "violin")) {
      ui_list <- c(
        ui_list,
        list(
          selectInput(
            "violin_group",
            "Agrupar por",
            choices = c("Curso" = "curso", "Tipo" = "tipo"),
            selected = "curso"
          ),
          radioButtons(
            "violin_kind",
            "Tipo de distribución",
            choices = c("Violín" = "violin", "Ridgeline" = "ridge"),
            inline = TRUE
          )
        )
      )
    }

    if (identical(input$plot_type, "tendencia")) {
      ui_list <- c(
        ui_list,
        list(
          selectInput(
            "trend_group",
            "Línea por",
            choices = c("Curso" = "curso", "Tipo" = "tipo"),
            selected = "curso"
          )
        )
      )
    }

    if (identical(input$plot_type, "crecimiento")) {
      ui_list <- c(
        ui_list,
        list(
          selectInput("tipo_a", "Tipo A", choices = ch$tipos, selected = ch$tipos[[1]]),
          selectInput("tipo_b", "Tipo B", choices = ch$tipos, selected = ch$tipos[[min(2, length(ch$tipos))]]),
          radioButtons(
            "growth_kind",
            "Visualización",
            choices = c("Delta (barras)" = "delta", "Slope chart" = "slope"),
            inline = TRUE
          ),
          radioButtons(
            "rank_mode",
            "Filtrar estudiantes",
            choices = c("Todos" = "all", "Top N" = "top", "Bottom N" = "bottom", "Top N + Bottom N" = "both"),
            inline = FALSE
          ),
          numericInput("rank_n", "N", value = 10, min = 1, max = 200, step = 1)
        )
      )
    }

    do.call(tagList, ui_list)
  })

  filtered_data <- reactive({
    df <- data_clean()
    if (!is.null(input$area)) {
      df <- df %>% filter(.data$area %in% input$area)
    }
    if (!is.null(input$fuente)) {
      df <- df %>% filter(.data$fuente %in% input$fuente)
    }
    if (!is.null(input$curso)) {
      df <- df %>% filter(.data$curso %in% input$curso)
    }
    if (!is.null(input$year)) {
      df <- df %>% filter(.data$year %in% input$year)
    }
    if (!is.null(input$tipo) && !identical(input$plot_type, "crecimiento")) {
      df <- df %>% filter(.data$tipo %in% input$tipo)
    }
    apply_factor_orders(
      df,
      tipo_levels = input$tipo_order %||% NULL,
      nivel_levels = input$nivel_order %||% NULL
    )
  })

  current_table_data <- reactive({
    df <- filtered_data()
    current_table(
      df = df,
      plot_type = input$plot_type %||% "promedio",
      ejes = input$eje %||% NULL,
      dist_kind = input$dist_kind %||% NULL,
      tipo_a = input$tipo_a %||% NULL,
      tipo_b = input$tipo_b %||% NULL,
      heatmap_dim = input$heatmap_dim %||% NULL,
      violin_group = input$violin_group %||% NULL,
      trend_group = input$trend_group %||% NULL
    )
  })

  output$tables_ui <- renderUI({
    tagList(
      card(
        card_header("Tabla asociada al gráfico"),
        p(class = "text-muted", "Muestra un resumen; descarga el archivo para ver la tabla completa."),
        numericInput("table_preview_n", "Filas a mostrar", value = 30, min = 5, max = 200, step = 5),
        tableOutput("table_preview"),
        hr(),
        selectInput("table_format", "Formato", choices = c("CSV" = "csv", "Excel (.xlsx)" = "xlsx"), selected = "csv"),
        downloadButton("download_table", "Descargar tabla")
      )
    )
  })

  output$table_preview <- renderTable({
    df <- current_table_data()
    n <- input$table_preview_n %||% 30
    utils::head(df, n)
  })

  output$download_table <- downloadHandler(
    filename = function() {
      fmt <- input$table_format %||% "csv"
      paste0("tabla_", input$plot_type %||% "plot", ".", if (identical(fmt, "xlsx")) "xlsx" else "csv")
    },
    content = function(file) {
      df <- current_table_data()
      fmt <- input$table_format %||% "csv"
      if (identical(fmt, "xlsx")) {
        if (!requireNamespace("writexl", quietly = TRUE)) {
          stop("Falta el paquete 'writexl'.", call. = FALSE)
        }
        writexl::write_xlsx(list(Tabla = df), path = file)
      } else {
        utils::write.csv(df, file = file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    }
  )

  quality_data <- reactive({
    df <- data_clean()
    quality_summary(df)
  })

  output$quality_ui <- renderUI({
    tagList(
      card(
        card_header("Calidad de datos (datos cargados)"),
        p(class = "text-muted", "Revisa ausencias, valores fuera de rango (0–100) y posibles duplicados."),
        hr(),
        tags$h5("Ausencias por eje"),
        tableOutput("quality_missing"),
        hr(),
        tags$h5("Fuera de rango por eje"),
        tableOutput("quality_range"),
        hr(),
        tags$h5("Duplicados (fuente, año, curso, tipo, N_lista)"),
        tableOutput("quality_dups"),
        hr(),
        downloadButton("download_quality", "Descargar reporte (Excel)")
      )
    )
  })

  output$quality_missing <- renderTable({
    q <- quality_data()
    utils::head(q$axes_missing, 50)
  })

  output$quality_range <- renderTable({
    q <- quality_data()
    utils::head(q$axes_out_of_range, 50)
  })

  output$quality_dups <- renderTable({
    q <- quality_data()
    utils::head(q$duplicates, 50)
  })

  output$download_quality <- downloadHandler(
    filename = function() {
      "reporte_calidad.xlsx"
    },
    content = function(file) {
      if (!requireNamespace("writexl", quietly = TRUE)) {
        stop("Falta el paquete 'writexl'.", call. = FALSE)
      }
      q <- quality_data()
      writexl::write_xlsx(
        list(
          ausencias_por_eje = q$axes_missing,
          fuera_de_rango_por_eje = q$axes_out_of_range,
          duplicados = q$duplicates
        ),
        path = file
      )
    }
  )

  compare_inputs <- reactive({
    df <- filtered_data()
    fuentes <- sort(unique(df$fuente))
    list(
      fuentes = fuentes,
      ejes = detect_axes(df)
    )
  })

  output$compare_ui <- renderUI({
    ch <- compare_inputs()
    if (length(ch$fuentes) < 2) {
      return(
        card(
          card_header("Comparar archivos"),
          p("Carga 2 archivos (o selecciona 2 fuentes) para habilitar la comparación.")
        )
      )
    }

    tagList(
      card(
        card_header("Comparar dos fuentes"),
        selectInput("cmp_a", "Fuente A", choices = ch$fuentes, selected = ch$fuentes[[1]]),
        selectInput("cmp_b", "Fuente B", choices = ch$fuentes, selected = ch$fuentes[[2]]),
        selectInput("cmp_eje", "Eje / ámbito", choices = ch$ejes, selected = ch$ejes[[1]]),
        radioButtons(
          "cmp_view",
          "Vista",
          choices = c("Delta (B - A)" = "delta", "Promedios (A y B)" = "means"),
          selected = "delta",
          inline = TRUE
        ),
        selectInput(
          "cmp_facet",
          "Facets",
          choices = c("OFF" = "off", "Por Año" = "year", "Por Curso" = "curso", "Por Tipo" = "tipo"),
          selected = "year"
        ),
        plotOutput("compare_plot", height = "550px"),
        hr(),
        tableOutput("compare_table"),
        downloadButton("download_compare", "Descargar comparación (CSV)")
      )
    )
  })

  compare_table_data <- reactive({
    df <- filtered_data()
    req(input$cmp_a, input$cmp_b, input$cmp_eje)
    compare_means(df, eje = input$cmp_eje, fuente_a = input$cmp_a, fuente_b = input$cmp_b)
  })

  output$compare_plot <- renderPlot({
    dfc <- compare_table_data()
    plot_compare(
      df_compare = dfc,
      fuente_a = input$cmp_a,
      fuente_b = input$cmp_b,
      view = input$cmp_view %||% "delta",
      facet = input$cmp_facet %||% "year",
      alpha_bars = input$alpha_bars %||% 0.85,
      plot_theme = get_plot_theme(input$style_preset)
    )
  }, res = 96)

  output$compare_table <- renderTable({
    utils::head(compare_table_data(), 50)
  })

  output$download_compare <- downloadHandler(
    filename = function() {
      "comparacion.csv"
    },
    content = function(file) {
      dfc <- compare_table_data()
      utils::write.csv(dfc, file = file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  output$export_ui <- renderUI({
    tagList(
      card(
        card_header("Exportación masiva e informe"),
        p(class = "text-muted", "Usa la configuración actual (filtros, orden y estilo)."),
        tags$h5("Exportación masiva (ZIP)"),
        checkboxInput("batch_by_curso", "Separar por Curso", value = TRUE),
        checkboxInput("batch_by_year", "Separar por Año", value = FALSE),
        checkboxInput("batch_by_eje", "Separar por Eje (cuando aplique)", value = TRUE),
        downloadButton("download_batch_zip", "Descargar ZIP (gráficos)"),
        hr(),
        tags$h5("Informe (ZIP)"),
        downloadButton("download_report_zip", "Descargar ZIP (informe)"),
        hr(),
        tags$h5("Exportar datos (CSV/XLSX)"),
        selectInput("data_scope", "Qué datos", choices = c("Filtrados" = "filtered", "Cargados (completo)" = "clean"), selected = "filtered"),
        selectInput("data_format", "Formato", choices = c("CSV" = "csv", "Excel (.xlsx)" = "xlsx"), selected = "csv"),
        downloadButton("download_data", "Descargar datos")
      )
    )
  })

  output$download_data <- downloadHandler(
    filename = function() {
      fmt <- input$data_format %||% "csv"
      paste0("datos_", input$data_scope %||% "filtered", ".", if (identical(fmt, "xlsx")) "xlsx" else "csv")
    },
    content = function(file) {
      scope <- input$data_scope %||% "filtered"
      fmt <- input$data_format %||% "csv"
      df <- if (identical(scope, "clean")) data_clean() else filtered_data()
      if (identical(fmt, "xlsx")) {
        if (!requireNamespace("writexl", quietly = TRUE)) {
          stop("Falta el paquete 'writexl'.", call. = FALSE)
        }
        writexl::write_xlsx(list(Datos = df), path = file)
      } else {
        utils::write.csv(df, file = file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    }
  )

  unique_path <- function(dir, filename) {
    base <- tools::file_path_sans_ext(filename)
    ext <- tools::file_ext(filename)
    ext <- if (nzchar(ext)) paste0(".", ext) else ""

    candidate <- file.path(dir, filename)
    if (!file.exists(candidate)) return(candidate)

    i <- 2
    repeat {
      candidate <- file.path(dir, paste0(base, "_", i, ext))
      if (!file.exists(candidate)) return(candidate)
      i <- i + 1
      if (i > 9999) stop("No se pudo crear un nombre de archivo único.", call. = FALSE)
    }
  }

  output$download_report_zip <- downloadHandler(
    filename = function() {
      paste0("informe_DIA_", format(Sys.Date(), "%Y-%m-%d"), ".zip")
    },
    content = function(file) {
      if (!requireNamespace("zip", quietly = TRUE)) {
        stop("Falta el paquete 'zip'.", call. = FALSE)
      }

      td <- tempfile("dia_informe_")
      dir.create(td, recursive = TRUE, showWarnings = FALSE)

      dims <- export_dims_px(input$export_res %||% "med")
      plot_path <- file.path(td, "grafico.png")
      save_png_ggsave(
        plot = current_plot(),
        path = plot_path,
        width_px = dims$width,
        height_px = dims$height,
        style_preset = input$style_preset %||% "classic"
      )

      tab <- current_table_data()
      tab_csv <- file.path(td, "tabla.csv")
      utils::write.csv(tab, file = tab_csv, row.names = FALSE, fileEncoding = "UTF-8")

      pts <- interpretation_points(
        plot_type = input$plot_type %||% "promedio",
        ejes = input$eje %||% NULL,
        cursos = input$curso %||% NULL,
        tipos = input$tipo %||% NULL,
        years = input$year %||% NULL,
        areas = input$area %||% NULL,
        facet_row = input$facet_row %||% "off",
        facet_col = input$facet_col %||% "off",
        dist_kind = input$dist_kind %||% NULL,
        heatmap_dim = input$heatmap_dim %||% NULL,
        violin_kind = input$violin_kind %||% NULL,
        violin_group = input$violin_group %||% NULL,
        trend_group = input$trend_group %||% NULL,
        tipo_a = input$tipo_a %||% NULL,
        tipo_b = input$tipo_b %||% NULL,
        growth_kind = input$growth_kind %||% NULL,
        rank_mode = input$rank_mode %||% NULL,
        anonymous = isTRUE(input$anon)
      )

      bullets_html <- paste0("<li>", html_escape(pts$bullets), "</li>", collapse = "")
      html <- paste0(
        "<!doctype html><html><head><meta charset='utf-8'>",
        "<title>Informe DIA</title>",
        "<style>body{font-family:Arial, sans-serif; margin:24px;} img{max-width:100%; height:auto;} .muted{color:#666;}</style>",
        "</head><body>",
        "<h1>Informe DIA</h1>",
        "<p class='muted'>Generado: ", html_escape(format(Sys.time(), "%Y-%m-%d %H:%M")), "</p>",
        "<h2>Gráfico</h2>",
        "<img src='grafico.png' alt='Gráfico'>",
        "<h2>", html_escape(pts$title), "</h2>",
        "<ul>", bullets_html, "</ul>",
        "<h2>Tabla (CSV)</h2>",
        "<p class='muted'>Archivo: tabla.csv</p>",
        df_to_html_table(tab, max_rows = 200),
        "</body></html>"
      )
      write_text_file(file.path(td, "informe.html"), html)

      files <- list.files(td, full.names = TRUE)
      zip::zipr(zipfile = file, files = files, root = td)
    }
  )

  output$download_batch_zip <- downloadHandler(
    filename = function() {
      paste0("export_DIA_", format(Sys.Date(), "%Y-%m-%d"), ".zip")
    },
    content = function(file) {
      if (!requireNamespace("zip", quietly = TRUE)) {
        stop("Falta el paquete 'zip'.", call. = FALSE)
      }

      df <- filtered_data()
      validate(need(nrow(df) > 0, "No hay datos para exportar con los filtros actuales."))

      plot_type <- input$plot_type %||% "promedio"
      cursos <- input$curso %||% sort(unique(df$curso))
      years <- input$year %||% sort(unique(df$year))
      ejes <- input$eje %||% detect_axes(df)

      by_curso <- isTRUE(input$batch_by_curso)
      by_year <- isTRUE(input$batch_by_year)
      by_eje <- isTRUE(input$batch_by_eje)

      curso_groups <- if (by_curso) lapply(as.character(cursos), function(x) x) else list(cursos)
      year_groups <- if (by_year) lapply(as.character(years), function(x) x) else list(years)

      axes_applicable <- plot_type %in% c("promedio", "distribucion", "crecimiento")
      eje_groups <- if (axes_applicable) {
        if (by_eje) lapply(as.character(ejes), function(x) x) else list(ejes)
      } else {
        list(NULL)
      }

      td <- tempfile("dia_export_")
      dir.create(td, recursive = TRUE, showWarnings = FALSE)

      # Exporta una tabla resumen general.
      tab <- current_table_data()
      utils::write.csv(tab, file = file.path(td, "tabla.csv"), row.names = FALSE, fileEncoding = "UTF-8")

      dims <- export_dims_px(input$export_res %||% "med")
      plot_theme <- get_plot_theme(input$style_preset)

      made <- 0L
      for (cg in curso_groups) {
        for (yg in year_groups) {
          df_sub <- df
          if (length(cg) > 0) df_sub <- df_sub %>% filter(.data$curso %in% cg)
          if (length(yg) > 0) df_sub <- df_sub %>% filter(.data$year %in% yg)
          if (nrow(df_sub) == 0) next

          for (eg in eje_groups) {
            df_job <- df_sub

            eje_label <- NULL
            if (axes_applicable) {
              eg_vec <- as.character(eg %||% character())
              if (plot_type == "crecimiento") {
                eje_label <- eg_vec[[1]]
              } else if (length(eg_vec) == 1) {
                eje_label <- eg_vec[[1]]
              } else {
                eje_label <- "VariosEjes"
              }
            }

            p <- NULL
            if (identical(plot_type, "promedio")) {
              p <- plot_promedio(
                df = df_job,
                ejes = eg,
                facet_row = input$facet_row %||% "off",
                facet_col = input$facet_col %||% "off",
                palette_fill = input$palette_fill,
                alpha_bars = input$alpha_bars %||% 0.85,
                plot_theme = plot_theme
              )
            } else if (identical(plot_type, "distribucion")) {
              p <- plot_distribucion(
                df = df_job,
                ejes = eg,
                kind = input$dist_kind %||% "box",
                facet_row = input$facet_row %||% "off",
                facet_col = input$facet_col %||% "off",
                palette_fill = input$palette_fill,
                palette_color = input$palette_color,
                alpha_bars = input$alpha_bars %||% 0.85,
                alpha_lines = input$alpha_lines %||% 0.9,
                plot_theme = plot_theme
              )
            } else if (identical(plot_type, "nivel_logro")) {
              p <- plot_nivel_logro(
                df = df_job,
                facet_row = input$facet_row %||% "off",
                facet_col = input$facet_col %||% "off",
                palette_fill = input$palette_fill,
                alpha_bars = input$alpha_bars %||% 0.85,
                plot_theme = plot_theme
              )
            } else if (identical(plot_type, "crecimiento")) {
              p <- plot_crecimiento(
                df = df_job,
                eje = as.character(eg %||% character())[[1]],
                tipo_a = input$tipo_a,
                tipo_b = input$tipo_b,
                kind = input$growth_kind %||% "delta",
                rank_mode = input$rank_mode %||% "all",
                rank_n = input$rank_n %||% 10,
                facet_row = input$facet_row %||% "off",
                facet_col = input$facet_col %||% "off",
                palette_fill = input$palette_fill,
                palette_color = input$palette_color,
                alpha_bars = input$alpha_bars %||% 0.85,
                alpha_lines = input$alpha_lines %||% 0.9,
                plot_theme = plot_theme
              )
            }

            if (is.null(p)) next

            filename <- make_export_filename_v2(
              plot_type = plot_type,
              cursos = cg,
              years = yg,
              areas = unique(df_job$area),
              eje = eje_label,
              tipo_a = input$tipo_a %||% NULL,
              tipo_b = input$tipo_b %||% NULL
            )
            out_path <- unique_path(td, filename)
            save_png_ggsave(
              plot = p,
              path = out_path,
              width_px = dims$width,
              height_px = dims$height,
              style_preset = input$style_preset %||% "classic"
            )
            made <- made + 1L
          }
        }
      }

      readme <- paste0(
        "Export DIA\n",
        "Fecha: ", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n",
        "Plot: ", plot_type, "\n",
        "Cursos: ", paste(cursos, collapse = ", "), "\n",
        "Años: ", paste(years, collapse = ", "), "\n",
        "Ejes: ", paste(ejes, collapse = ", "), "\n",
        "Anon: ", if (isTRUE(input$anon)) "ON" else "OFF", "\n",
        "Archivos generados: ", made, "\n"
      )
      write_text_file(file.path(td, "README.txt"), readme)

      files <- list.files(td, full.names = TRUE)
      zip::zipr(zipfile = file, files = files, root = td)
    }
  )

  current_plot <- reactive({
    df <- filtered_data()
    ch <- choices()

    validate(
      need(nrow(df) > 0, "No hay filas después de aplicar filtros.")
    )

    facet_row_eff <- input$facet_row %||% "off"
    facet_col_eff <- input$facet_col %||% "off"
    n_areas <- length(unique(df$area))
    if (n_areas > 1) {
      if (identical(facet_row_eff, "off") && identical(facet_col_eff, "off")) {
        facet_row_eff <- "area"
      } else if (!("area" %in% c(facet_row_eff, facet_col_eff))) {
        validate(
          need(
            FALSE,
            "Seleccionaste múltiples áreas. Para comparar, usa facets por Área (fila o columna) o filtra una sola Área."
          )
        )
      }
    }

    ui_theme <- get_ui_theme(input$style_preset)
    if (is.function(session$setCurrentTheme)) {
      session$setCurrentTheme(ui_theme)
    }

    eje_first <- (input$eje %||% NULL)
    if (is.character(eje_first) && length(eje_first) > 0) {
      eje_first <- eje_first[[1]]
    }
    title_default <- default_title(input$plot_type, eje_first, input$tipo_a %||% NULL, input$tipo_b %||% NULL)
    subtitle_default <- default_subtitle(input$curso %||% character(), input$tipo %||% character(), input$area %||% character())

    labels <- list(
      title = nz_or_default(input$title, title_default),
      subtitle = nz_or_default(input$subtitle, subtitle_default),
      x = nz_or_default(input$xlab, NULL),
      y = nz_or_default(input$ylab, NULL)
    )

    plot_theme <- get_plot_theme(input$style_preset)

    if (identical(input$plot_type, "promedio")) {
      p <- plot_promedio(
        df = df,
        ejes = input$eje,
        facet_row = facet_row_eff,
        facet_col = facet_col_eff,
        palette_fill = input$palette_fill,
        alpha_bars = input$alpha_bars %||% 0.85,
        plot_theme = plot_theme
      )
    } else if (identical(input$plot_type, "heatmap")) {
      p <- plot_heatmap(
        df = df,
        ejes = input$eje,
        facet_row = facet_row_eff,
        facet_col = facet_col_eff,
        axis_dim = input$heatmap_dim %||% "curso",
        palette_fill = input$palette_fill,
        plot_theme = plot_theme
      )
    } else if (identical(input$plot_type, "violin")) {
      p <- plot_violin(
        df = df,
        ejes = input$eje,
        facet_row = facet_row_eff,
        facet_col = facet_col_eff,
        group_var = input$violin_group %||% "curso",
        kind = input$violin_kind %||% "violin",
        palette_fill = input$palette_fill,
        alpha_bars = input$alpha_bars %||% 0.85,
        alpha_lines = input$alpha_lines %||% 0.9,
        plot_theme = plot_theme
      )
    } else if (identical(input$plot_type, "tendencia")) {
      p <- plot_tendencia(
        df = df,
        ejes = input$eje,
        facet_row = facet_row_eff,
        facet_col = facet_col_eff,
        group_var = input$trend_group %||% "curso",
        palette_color = input$palette_color,
        alpha_lines = input$alpha_lines %||% 0.9,
        plot_theme = plot_theme
      )
    } else if (identical(input$plot_type, "distribucion")) {
      p <- plot_distribucion(
        df = df,
        ejes = input$eje,
        kind = input$dist_kind %||% "box",
        facet_row = facet_row_eff,
        facet_col = facet_col_eff,
        palette_fill = input$palette_fill,
        palette_color = input$palette_color,
        alpha_bars = input$alpha_bars %||% 0.85,
        alpha_lines = input$alpha_lines %||% 0.9,
        plot_theme = plot_theme
      )
    } else if (identical(input$plot_type, "nivel_logro")) {
      p <- plot_nivel_logro(
        df = df,
        facet_row = facet_row_eff,
        facet_col = facet_col_eff,
        palette_fill = input$palette_fill,
        alpha_bars = input$alpha_bars %||% 0.85,
        plot_theme = plot_theme
      )
    } else if (identical(input$plot_type, "crecimiento")) {
      validate(need(!identical(input$tipo_a, input$tipo_b), "Tipo A y Tipo B deben ser distintos."))
      p <- plot_crecimiento(
        df = df,
        eje = (input$eje %||% character())[[1]],
        tipo_a = input$tipo_a,
        tipo_b = input$tipo_b,
        kind = input$growth_kind %||% "delta",
        rank_mode = input$rank_mode %||% "all",
        rank_n = input$rank_n %||% 10,
        facet_row = facet_row_eff,
        facet_col = facet_col_eff,
        palette_fill = input$palette_fill,
        palette_color = input$palette_color,
        alpha_bars = input$alpha_bars %||% 0.85,
        alpha_lines = input$alpha_lines %||% 0.9,
        plot_theme = plot_theme
      )
    } else {
      p <- ggplot() + theme_void() + ggtitle("Selecciona un gráfico")
    }

    apply_labels(p, labels)
  })

  output$plot <- renderPlot({
    current_plot()
  }, res = 96)

  outputOptions(output, "plot", suspendWhenHidden = FALSE)

  output$download_png <- downloadHandler(
    filename = function() {
      df <- filtered_data()
      plot_type <- input$plot_type %||% "promedio"
      cursos <- input$curso %||% character()
      years <- input$year %||% character()
      areas <- input$area %||% character()

      eje_label <- NULL
      if (plot_type %in% c("promedio", "distribucion", "crecimiento", "heatmap", "violin", "tendencia")) {
        ejes <- input$eje %||% character()
        if (length(ejes) > 0) {
          if (identical(plot_type, "crecimiento")) {
            eje_label <- as.character(ejes)[[1]]
          } else if (length(ejes) == 1) {
            eje_label <- as.character(ejes)[[1]]
          } else {
            eje_label <- "VariosEjes"
          }
        }
      }

      make_export_filename_v2(
        plot_type = plot_type,
        cursos = cursos,
        years = years,
        areas = areas,
        eje = eje_label,
        tipo_a = input$tipo_a %||% NULL,
        tipo_b = input$tipo_b %||% NULL
      )
    },
    content = function(file) {
      p <- current_plot()
      dims <- export_dims_px(input$export_res %||% "med")
      save_png_ggsave(
        plot = p,
        path = file,
        width_px = dims$width,
        height_px = dims$height,
        style_preset = input$style_preset %||% "classic"
      )
    }
  )
}

shinyApp(ui, server)
