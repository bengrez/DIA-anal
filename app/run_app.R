# Entrypoint para ejecutar la app Shiny con parámetros estables.
#
# Evita `port=0` (que en algunos entornos muestra 127.0.0.1:0) y permite
# configurar el puerto con `DIA_PORT` (por defecto 3838).

args <- commandArgs(trailingOnly = TRUE)
script_file <- sub("^--file=", "", commandArgs()[grep("^--file=", commandArgs())][1])
app_dir <- normalizePath(dirname(script_file), winslash = "/", mustWork = TRUE)

# Evitar port=0: en algunos entornos termina mostrando "Listening on ...:0"
# y el navegador no puede conectarse. Usamos un puerto fijo con override por env var.
port <- as.integer(Sys.getenv("DIA_PORT", "3838"))
if (is.na(port) || port <= 0) port <- 3838

options(shiny.port = port)
options(shiny.host = "127.0.0.1")
options(shiny.launch.browser = TRUE)

message("Iniciando en: http://127.0.0.1:", port)
shiny::runApp(appDir = app_dir, display.mode = "normal", host = "127.0.0.1", port = port)
