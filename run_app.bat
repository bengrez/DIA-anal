@echo off
setlocal enabledelayedexpansion

set ROOT_DIR=%~dp0
set APP_DIR=%ROOT_DIR%app

if not exist "%APP_DIR%\\app.R" (
  echo ERROR: No se encontro la app en "%APP_DIR%".
  pause
  exit /b 1
)

set RSCRIPT=
for /f "usebackq delims=" %%i in (`where Rscript.exe 2^>nul`) do (
  set RSCRIPT=%%i
  goto :have_rscript
)

for /f "usebackq delims=" %%i in (`powershell -NoProfile -Command "$r = Get-ChildItem 'C:\Program Files\R' -Directory -Filter 'R-*' -ErrorAction SilentlyContinue | Sort-Object Name -Descending | Select-Object -First 1; if ($r) { Join-Path $r.FullName 'bin\\Rscript.exe' }"`) do set RSCRIPT=%%i

:have_rscript
if "%RSCRIPT%"=="" (
  echo ERROR: No se encontro Rscript.exe. Instala R o agrega R\bin a PATH.
  pause
  exit /b 1
)

if not exist "%RSCRIPT%" (
  echo ERROR: No se encontro Rscript.exe en "%RSCRIPT%".
  pause
  exit /b 1
)

echo Usando Rscript: %RSCRIPT%

echo Instalando dependencias (si faltan)...
"%RSCRIPT%" "%APP_DIR%\\scripts\\install_deps.R"
if errorlevel 1 (
  echo ERROR: Fallo la instalacion de paquetes.
  pause
  exit /b 1
)

echo Iniciando app...
"%RSCRIPT%" "%APP_DIR%\\run_app.R"

endlocal
