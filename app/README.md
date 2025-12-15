# App de escritorio (R + Shiny) - Gráficos DIA

App local para docentes: carga 1–2 archivos Excel y genera gráficos listos para exportar en PNG (y paquetes ZIP).

## Ejecutar (Windows)
1. Asegura tener R instalado (incluye `Rscript.exe`).
2. Doble click en `run_app.bat` (en la raíz del repo).

La primera vez puede instalar paquetes automáticamente (requiere internet).
Si el navegador no se abre, entra manualmente a `http://127.0.0.1:3838/`.

Opcional: instalar dependencias desde consola:
- `C:\Program Files\R\R-4.4.2\bin\Rscript.exe app/scripts/install_deps.R`

## Plantilla Excel
En la app puedes descargar una plantilla `.xlsx` con encabezados requeridos y una fila de ejemplo (Juanito Juanete): botón **Descargar plantilla Excel** en la sección de carga.

## Contrato de datos (Excel .xlsx)
Columnas requeridas (se aceptan variaciones menores de mayúsculas/guiones bajos):
- `Year` (numérico)
- `Tipo` (categorías como Diagnóstico/Intermedio/Cierre)
- `Curso`
- `N_lista` (o `N_Lista`)
- `Nombre del Estudiante`
- `NIVEL DE LOGRO` (no se recalcula)

Ejes/ámbitos:
- Columnas adicionales (nombres variables).
- Valores en porcentaje (0–100), se aceptan decimales con coma (`58,33`).
- Ausentes: `NA`, `Ausente`, vacío → `NA`.

## MVP: gráficos incluidos
A) Promedio por curso y Tipo (barras, eje seleccionable)  
B) Distribución (boxplot o histograma, por curso, eje seleccionable)  
C) Nivel de logro (barras apiladas proporcionales, por curso y Tipo)  
D) Crecimiento por estudiante (delta o slope chart, compara 2 Tipos)

## Herramientas extra (tabs)
- **Tablas**: tabla derivada del gráfico actual (preview + descarga CSV/XLSX).
- **Calidad**: reporte de faltantes, fuera de rango (0–100) y duplicados (descarga XLSX).
- **Comparar**: compara 2 archivos (promedios y deltas) y genera un gráfico + descarga CSV.
- **Exportar**: exporta datos filtrados/limpios (CSV/XLSX) y genera ZIPs (lote o mini‑informe).

## Configuración (presets)
En **Configuración** puedes:
- Descargar tu configuración actual (`.rds`).
- Cargar una configuración (`.rds`) para reproducir un gráfico/informe.
- Restaurar valores por defecto.

## Exportación
- **PNG** en 3 resoluciones (baja/media/alta).
- **ZIP informe**: `grafico.png` + `tabla.csv` + `informe.html`.
- **ZIP lote**: muchos PNGs (por curso/año/eje) + `tabla.csv` + `README.txt`.

## Ideas para expandir (roadmap)
- Exportación a PDF/Word (plantilla de informe institucional).
- Reporte por curso automático (todas las gráficas + tablas en un paquete).
- Más gráficos (heatmap por eje/curso, “semáforo” por umbrales, comparaciones inter‑anuales).
- Validaciones adicionales (consistencia entre columnas, rangos por eje, alertas configurables).
- Guardar “perfiles de establecimiento” (paletas, títulos, orden de niveles/tipos, logos).
- Importar CSV/Google Sheets, y soporte multi‑archivos (>2).

## Empaquetado (referencia)
Guía y script base en `app/installer/` (RInno + Inno Setup).
