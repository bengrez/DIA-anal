args <- commandArgs(trailingOnly = TRUE)
app_dir <- normalizePath(dirname(sub("^--file=", "", commandArgs()[grep("^--file=", commandArgs())][1])), winslash = "/", mustWork = TRUE)

options(shiny.port = 0)
options(shiny.launch.browser = TRUE)

shiny::runApp(appDir = app_dir, display.mode = "normal")

