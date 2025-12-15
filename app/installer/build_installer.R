options(repos = c(CRAN = "https://cloud.r-project.org"))

if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

if (!requireNamespace("RInno", quietly = TRUE)) {
  remotes::install_github("ficonsulting/RInno")
}

library(RInno)

script_file <- sub("^--file=", "", commandArgs()[grep("^--file=", commandArgs())][1])
script_dir <- dirname(normalizePath(script_file, winslash = "/", mustWork = TRUE))
app_dir <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = TRUE)

# Instala Inno Setup (si hace falta) y compila el instalador.
RInno::install_inno()
RInno::create_app(app_name = "DIA-anal", app_dir = app_dir, include_R = TRUE)
RInno::compile_iss()

