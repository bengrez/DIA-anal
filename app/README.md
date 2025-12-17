# App Shiny (local) — DIA Ciencias Naturales

App para docentes (Chile): carga archivos `.xls` descargados desde la plataforma DIA y genera gráficos + tablas listos para informes.

## Ejecutar (Windows)
1. Instala R (incluye `Rscript.exe`).
2. Doble click en `run_app.bat` (en la raíz del repo).

La primera vez puede instalar paquetes automáticamente (requiere internet).  
Si el navegador no se abre, entra a `http://127.0.0.1:3838/`.

Opcional: instalar dependencias desde consola:
- `C:\Program Files\R\R-4.4.2\bin\Rscript.exe app/scripts/install_deps.R`

## Cargar datos (plataforma DIA)
En el panel izquierdo:
1. **1) Carga de datos (.xls)** → **Seleccionar carpeta o archivos…**
2. Selecciona una carpeta completa (recomendado) o todos los `.xls` con `Ctrl+A`
3. Presiona **Cargar datos**

La app asume:
- 1 hoja por archivo (`Hoja1`).
- Las primeras 12 filas son informativas; la tabla comienza en la fila 13 (`skip = 12`).

### Contexto desde el nombre de archivo
La app infiere automáticamente:
- `RBD` (ej. `12775`)
- Área (ej. `Ciencias Naturales`)
- `Curso` (ej. `5_A`, `6_B`)
- `Periodo/Tipo DIA` (`Diagnóstico`, `Monitoreo`, `Cierre`)
- `Año` (ej. `2025`)

Ejemplos:
- `RBD12775_DIA_CIENCIAS NATURALES_5_A_Resultados_de_estudiantes_Equipo_docente_Monitoreo_2025.xls`
- `RBD12775_DIA_CIENCIAS NATURALES_5_A_Resultados_de_estudiantes_Equipo_docente_Evaluacion_Cierre_2023.xls`

Nota: `Evaluacion_Cierre` se considera equivalente a `Cierre` en la app.

## Qué gráficos incluye (versión actual)
- **Distribución por nivel de logro** (Nivel I/II/III) con línea de referencia 60%.
- **Desempeño por eje temático** (barras por curso) + **heatmap** ejes × cursos.
- **Comparaciones YoY** (diferencia en puntos porcentuales entre 2 años) para niveles y ejes.

Nota: los `.xls` actuales de la plataforma para Ciencias Naturales traen porcentajes por eje + nivel de logro por estudiante. Si en el futuro agregas reportes con habilidades/indicadores, la app puede extenderse para soportarlos.

## Descargas
- PNG (300 DPI) y CSV por gráfico.
- CSV con los datos filtrados (panel izquierdo → “Privacidad y exportación”).

## Modo anónimo
Activa **Modo anónimo** para reemplazar `Nombre del Estudiante` por `ID_<N_lista>` (no exporta nombres reales).

## Plantilla `.xlsx` (opcional)
Botón **Descargar plantilla (.xlsx)**: genera un archivo de ejemplo con encabezados y 1 fila ficticia (Juanito Juanete), útil para pruebas.

## Troubleshooting
- Si tu navegador no deja “seleccionar carpeta”, abre la carpeta y selecciona todos los `.xls` (Ctrl+A).
- Si el puerto `3838` está ocupado: define `DIA_PORT` antes de ejecutar (ej. `set DIA_PORT=3840`).

## Empaquetado (referencia)
Guía y script base en `app/installer/` (RInno + Inno Setup).
