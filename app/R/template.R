# ------------------------------------------------------------
# Plantilla Excel (formato .xlsx de la app)
#
# Permite generar un archivo de ejemplo con:
# - Encabezados requeridos por la app
# - Una fila ficticia (Juanito Juanete) para orientar a docentes
# ------------------------------------------------------------

dia_excel_template_df <- function() {
  data.frame(
    Year = 2025,
    Tipo = "Diagnóstico",
    Curso = "5_A",
    N_lista = 1,
    `Nombre del Estudiante` = "Juanito Juanete",
    `Ciencias de la vida` = "58,33",
    `Ciencias Físicas y Químicas` = "71,25",
    `Ciencias de la Tierra y el Universo` = "64,10",
    `NIVEL DE LOGRO` = "Nivel II",
    check.names = FALSE
  )
}

write_dia_excel_template <- function(path) {
  if (!requireNamespace("writexl", quietly = TRUE)) {
    stop("Falta el paquete 'writexl'. Ejecuta: Rscript app/scripts/install_deps.R", call. = FALSE)
  }
  df <- dia_excel_template_df()
  writexl::write_xlsx(list(Datos = df), path = path)
}
