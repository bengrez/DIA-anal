options(repos = c(CRAN = "https://cloud.r-project.org"))

script_file <- sub("^--file=", "", commandArgs()[grep("^--file=", commandArgs())][1])
script_dir <- dirname(normalizePath(script_file, winslash = "/", mustWork = TRUE))

app_dir <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = TRUE)
repo_dir <- normalizePath(file.path(app_dir, ".."), winslash = "/", mustWork = TRUE)

source(file.path(app_dir, "R", "ingest.R"))
source(file.path(app_dir, "R", "transform.R"))
source(file.path(app_dir, "R", "themes.R"))
source(file.path(app_dir, "R", "plots", "promedio.R"))
source(file.path(app_dir, "R", "plots", "distribucion.R"))
source(file.path(app_dir, "R", "plots", "nivel_logro.R"))
source(file.path(app_dir, "R", "plots", "crecimiento.R"))

require_pkgs <- c("ggplot2", "dplyr", "tidyr", "scales", "readxl")
missing <- require_pkgs[!vapply(require_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  stop("Faltan paquetes: ", paste(missing, collapse = ", "), call. = FALSE)
}

library(ggplot2)
library(dplyr)
library(tidyr)

data_path <- file.path(repo_dir, "data_basica.xlsx")
if (!file.exists(data_path)) {
  stop("No se encontró: ", data_path, call. = FALSE)
}

df <- read_dia_excel(data_path, sheet = 1, source_name = basename(data_path))
validate_dia_data(df)
df <- apply_anonymity(df, anonymous = TRUE)
df <- apply_factor_orders(df)

eje <- detect_axes(df)[1]
theme_plot <- get_plot_theme("classic")

out_dir <- file.path(repo_dir, "tmp_smoke")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

p1 <- plot_promedio(
  df,
  ejes = eje,
  facet_row = "off",
  facet_col = "off",
  palette_fill = "Okabe-Ito",
  alpha_bars = 0.85,
  plot_theme = theme_plot
)
save_png_ggsave(p1, file.path(out_dir, "promedio.png"), 1200, 800, style_preset = "classic")

p2 <- plot_distribucion(
  df,
  ejes = eje,
  kind = "box",
  facet_row = "tipo",
  facet_col = "off",
  palette_fill = "Okabe-Ito",
  palette_color = "Okabe-Ito",
  alpha_bars = 0.75,
  alpha_lines = 0.9,
  plot_theme = theme_plot
)
save_png_ggsave(p2, file.path(out_dir, "distribucion.png"), 1200, 800, style_preset = "classic")

p3 <- plot_nivel_logro(df, facet_row = "tipo", facet_col = "off", palette_fill = "Okabe-Ito", alpha_bars = 0.85, plot_theme = theme_plot)
save_png_ggsave(p3, file.path(out_dir, "nivel_logro.png"), 1200, 800, style_preset = "classic")

tipos <- sort(unique(df$tipo))
if (length(tipos) >= 2) {
  p4 <- plot_crecimiento(
    df,
    eje = eje,
    tipo_a = tipos[[1]],
    tipo_b = tipos[[2]],
    kind = "delta",
    rank_mode = "top",
    rank_n = 10,
    facet_row = "curso",
    facet_col = "off",
    palette_fill = "Okabe-Ito",
    palette_color = "Okabe-Ito",
    alpha_bars = 0.85,
    alpha_lines = 0.9,
    plot_theme = theme_plot
  )
  save_png_ggsave(p4, file.path(out_dir, "crecimiento.png"), 1200, 800, style_preset = "classic")
}

message("OK. PNGs generados en: ", out_dir)
