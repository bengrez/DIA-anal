# Empaquetado como app de escritorio (Windows)

La app funciona localmente con doble click en `run_app.bat`, pero para distribuirla a docentes sin R instalado conviene empaquetarla.

## Opción A: RInno (Electron + Inno Setup)
Requisitos:
- Windows
- R (recomendado 4.4+)
- Rtools (si el sistema lo requiere para dependencias)

Pasos (alto nivel):
1. Instalar `remotes` (si no existe): `install.packages("remotes")`
2. Instalar `RInno` desde GitHub: `remotes::install_github("ficonsulting/RInno")`
3. Ejecutar el script: `Rscript app/installer/build_installer.R`

Notas:
- RInno usa Inno Setup; el script puede instalarlo con `RInno::install_inno()`.
- Para un instalador “standalone” (incluyendo R), revisa `include_R = TRUE` en `build_installer.R`.

