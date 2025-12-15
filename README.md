# DIA-plots

Herramienta para **leer, analizar y generar gráficos** específicos para pruebas **DIA** (series, comparativas, reportes y visualizaciones que se definan en el plan).

## Quickstart (Windows / PowerShell)
- Crear entorno e instalar dependencias: `powershell -NoProfile -ExecutionPolicy Bypass -File .\scripts\bootstrap.ps1`
- Activar entorno: `.\.venv\Scripts\Activate.ps1`
- Ver info de un Excel: `dia-plots info .\data_basica.xlsx`

## Objetivo inmediato
- Definir un pipeline reproducible: **ingesta → validación → análisis → gráficos → exportación**.
- Estandarizar la entrada (Excel/CSV) y producir salidas (PNG/SVG/PDF + tablas).

## Datos locales
En este workspace existen:
- `data_basica.xlsx`
- `data_med.xlsx`

(En el plan definiremos el esquema: hojas, columnas, unidades, y reglas.)

## Estado del repo base (DIA-anal)
Se intentó clonar `https://github.com/bengrez/DIA-anal` en `./DIA-anal`, pero el repositorio aparece vacío (sin commits). Si hay otra URL/branch, o es privado, pásame el enlace correcto y lo integro.

## Estructura
- `src/dia_plots/`: librería (IO + plots + CLI)
- `scripts/`: utilidades de bootstrap

## CLI (actual)
- `dia-plots info <archivo.xlsx>`: lista hojas/columnas
- `dia-plots plot xy <archivo.xlsx> --sheet <hoja> --x <col> --y <col> --out <salida.png>`

Siguiente paso: compárteme tu plan estructurado (métricas/cálculos y gráficos exactos) y lo implementamos sobre estos archivos de ejemplo.

