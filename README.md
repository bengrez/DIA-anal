# DIA-anal

Repositorio para **analizar datos DIA** y generar **gráficos** a partir de archivos Excel escolares.

## MVP (R + Shiny): app para docentes
La app principal vive en `app/` y está pensada para uso local (los datos no se suben a servidores).

- Ejecutar en Windows: doble click en `run_app.bat`
- Documentación: `app/README.md`
- Soporta carga masiva desde carpeta de `.xls` exportados por la plataforma DIA (por curso/área).

## Datos de ejemplo
En la raíz del repo están:
- `data_basica.xlsx`
- `data_med.xlsx`

## Utilidades Python (scaffold)
Existe un scaffold inicial en `src/dia_plots/` con un CLI básico (`dia-plots`) para inspección/plots simples.
