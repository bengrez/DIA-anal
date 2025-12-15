Param(
  [string]$VenvPath = ".venv"
)

$ErrorActionPreference = "Stop"

if (-not (Get-Command py -ErrorAction SilentlyContinue)) {
  throw "No se encontró el launcher 'py'. Instala Python o habilita el launcher."
}

if (-not (Test-Path $VenvPath)) {
  py -m venv $VenvPath
}

$python = Join-Path $VenvPath "Scripts/python.exe"
& $python -m pip install --upgrade pip
& $python -m pip install -e ".[dev]"

Write-Host "OK. Activa el entorno: $VenvPath\\Scripts\\Activate.ps1"

