required <- c(
  "shiny",
  "bslib",
  "ggplot2",
  "readxl",
  "writexl",
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
