# App de escritorio (R + Shiny) — Gráficos DIA

App local para docentes: carga archivos Excel y genera gráficos listos para exportar en PNG.

## Ejecutar (Windows)
1. Asegura tener R instalado (incluye `Rscript.exe`).
2. Doble click en `run_app.bat` (en la raíz del repo).

La primera vez puede instalar paquetes automáticamente (requiere internet).

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

## Empaquetado (referencia)
Guía y script base en `app/installer/` (RInno + Inno Setup).
