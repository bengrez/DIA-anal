# Instalador de dependencias CRAN para la app.
#
# Se ejecuta desde `run_app.bat` antes de iniciar Shiny.
# La idea es que el usuario (docente) no tenga que instalar paquetes manualmente.

required <- c(
  "shiny",
  "shinydashboard",
  "plotly",
  "DT",
  "readxl",
  "writexl",
  "ggplot2",
  "dplyr",
  "tidyr",
  "scales"
)

options(repos = c(CRAN = "https://cloud.r-project.org"))

installed <- rownames(installed.packages())
missing <- setdiff(required, installed)

if (length(missing) == 0) {
  message("OK: paquetes ya instalados.")
  quit(status = 0)
}

message("Instalando paquetes faltantes: ", paste(missing, collapse = ", "))
install.packages(missing)
message("OK: instalación completada.")
