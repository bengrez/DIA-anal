## App Shiny (MVP) para gráficos DIA
## UI en español, datos locales, exportación PNG.

required_packages <- c(
  "shiny",
  "bslib",
  "ggplot2",
  "readxl",
  "writexl",
  "dplyr",
  "tidyr",
  "scales"
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

source(file.path("R", "ingest.R"))
source(file.path("R", "assistant.R"))
source(file.path("R", "template.R"))
source(file.path("R", "transform.R"))
source(file.path("R", "themes.R"))
source(file.path("R", "plots", "promedio.R"))
source(file.path("R", "plots", "distribucion.R"))
source(file.path("R", "plots", "nivel_logro.R"))
source(file.path("R", "plots", "crecimiento.R"))

options(shiny.maxRequestSize = 100 * 1024^2) # 100 MB

ui <- page_sidebar(
  title = "Gráficos DIA (MVP)",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  sidebar = sidebar(
    width = 360,
    accordion(
      id = "sidebar_acc",
      open = "load",
      accordion_panel(
        title = "1) Carga de datos (Excel .xlsx)",
        value = "load",
        fileInput("file1", "Archivo 1 (obligatorio)", accept = ".xlsx"),
        uiOutput("sheet1_ui"),
        fileInput("file2", "Archivo 2 (opcional)", accept = ".xlsx"),
        uiOutput("sheet2_ui"),
        checkboxInput("anon", "Modo anónimo (no exportar nombres)", value = TRUE),
        downloadButton("download_template", "Descargar plantilla Excel"),
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
            "C) Nivel de logro" = "nivel_logro",
            "D) Crecimiento por estudiante (delta)" = "crecimiento"
          )
        ),
        uiOutput("filters_ui"),
        hr(),
        selectInput(
          "facet",
          "Facets",
          choices = c(
            "OFF" = "off",
            "Por Curso" = "curso",
            "Por Tipo" = "tipo",
            "Por Año" = "year",
            "Por Eje" = "eje"
          ),
          selected = "off"
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
      )
    )
  ),
  layout_columns(
    navset_card_tab(
      id = "main_tabs",
      nav_panel(
        title = "Gráfico",
        value = "grafico",
        plotOutput("plot", height = "650px")
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

server <- function(input, output, session) {
  # Recomendación para empaquetado tipo RInno/Electron:
  # asegurar que al cerrar la ventana se termine la sesión de R.
  if (!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }

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

  assistant_rec <- reactive({
    assistant_recommendation(
      need_level = input$asst_need_level %||% NULL,
      need_growth = input$asst_need_growth %||% NULL,
      need_dist = input$asst_need_dist %||% NULL
    )
  })

  output$assistant_result_ui <- renderUI({
    rec <- assistant_rec()
    if (!isTRUE(rec$complete)) {
      return(tags$div(class = "text-muted", rec$prompt))
    }

    tagList(
      h4(paste0("Recomendación: ", rec$label)),
      tags$ul(
        tags$li(rec$why),
        tags$li(rec$what_you_get)
      ),
      tags$small(class = "text-muted", "Puedes cambiar la recomendación ajustando tus respuestas.")
    )
  })

  observeEvent(input$asst_apply, {
    rec <- assistant_rec()
    if (!isTRUE(rec$complete)) {
      showNotification("Responde las preguntas para obtener una recomendación.", type = "warning")
      return()
    }

    updateSelectInput(session, "plot_type", selected = rec$plot_type)
    updateTabsetPanel(session, "main_tabs", selected = "grafico")
    showNotification(paste0("Se seleccionó: ", rec$label), type = "message")
  })

  data_raw <- reactive({
    req(input$file1)
    df1 <- tryCatch(
      read_dia_excel(
        path = input$file1$datapath,
        sheet = input$sheet1 %||% 1,
        source_name = input$file1$name
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
        source_name = input$file2$name
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
    apply_anonymity(df, anonymous = isTRUE(input$anon))
  })

  choices <- reactive({
    df <- data_clean()
    list(
      cursos = sort(unique(df$curso)),
      tipos = sort(unique(df$tipo)),
      years = sort(unique(df$year)),
      niveles = sort(unique(df$nivel_logro[!is.na(df$nivel_logro) & nzchar(df$nivel_logro)])),
      ejes = detect_axes(df),
      fuentes = sort(unique(df$fuente))
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
          title = "Tipos",
          value = length(ch$tipos),
          theme = "info"
        ),
        value_box(
          title = "Ejes",
          value = length(ch$ejes),
          theme = "secondary"
        ),
        col_widths = c(4, 4, 4)
      ),
      if (length(ch$ejes) > 0) {
        tags$small(
          "Ejes detectados: ",
          paste(ch$ejes, collapse = ", ")
        )
      }
    )
  })

  output$filters_ui <- renderUI({
    ch <- choices()
    df <- data_clean()

    ui_list <- list()

    if (length(ch$fuentes) > 1) {
      ui_list <- c(
        ui_list,
        list(
          selectInput("fuente", "Archivo(s)", choices = ch$fuentes, selected = ch$fuentes, multiple = TRUE)
        )
      )
    }

    ui_list <- c(ui_list, list(selectInput("curso", "Curso(s)", choices = ch$cursos, selected = ch$cursos, multiple = TRUE)))

    ui_list <- c(
      ui_list,
      list(
        selectInput(
          "year",
          "Año(s)",
          choices = ch$years,
          selected = ch$years,
          multiple = TRUE
        )
      )
    )

    if (!identical(input$plot_type, "crecimiento")) {
      ui_list <- c(
        ui_list,
        list(selectInput("tipo", "Tipo(s)", choices = ch$tipos, selected = ch$tipos, multiple = TRUE))
      )
    }

    if (input$plot_type %in% c("promedio", "distribucion", "crecimiento")) {
      ui_list <- c(
        ui_list,
        list(
          selectInput("eje", "Eje / ámbito", choices = ch$ejes, selected = ch$ejes[[1]], multiple = TRUE)
        )
      )
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

  current_plot <- reactive({
    df <- filtered_data()
    ch <- choices()

    validate(
      need(nrow(df) > 0, "No hay filas después de aplicar filtros.")
    )

    ui_theme <- get_ui_theme(input$style_preset)
    if (is.function(session$setCurrentTheme)) {
      session$setCurrentTheme(ui_theme)
    }

    eje_first <- (input$eje %||% NULL)
    if (is.character(eje_first) && length(eje_first) > 0) {
      eje_first <- eje_first[[1]]
    }
    title_default <- default_title(input$plot_type, eje_first, input$tipo_a %||% NULL, input$tipo_b %||% NULL)
    subtitle_default <- default_subtitle(input$curso %||% character(), input$tipo %||% character())

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
        facet = input$facet %||% "off",
        palette_fill = input$palette_fill,
        alpha_bars = input$alpha_bars %||% 0.85,
        plot_theme = plot_theme
      )
    } else if (identical(input$plot_type, "distribucion")) {
      p <- plot_distribucion(
        df = df,
        ejes = input$eje,
        kind = input$dist_kind %||% "box",
        facet = input$facet %||% "off",
        palette_fill = input$palette_fill,
        palette_color = input$palette_color,
        alpha_bars = input$alpha_bars %||% 0.85,
        alpha_lines = input$alpha_lines %||% 0.9,
        plot_theme = plot_theme
      )
    } else if (identical(input$plot_type, "nivel_logro")) {
      p <- plot_nivel_logro(
        df = df,
        facet = input$facet %||% "tipo",
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
        facet = input$facet %||% "curso",
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
      eje <- input$eje %||% NULL
      cursos <- input$curso %||% character()
      make_export_filename(
        plot_type = input$plot_type,
        cursos = cursos,
        eje = eje,
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
