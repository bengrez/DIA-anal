from __future__ import annotations

from pathlib import Path

import pandas as pd


def list_excel_sheets(path: Path) -> list[str]:
    path = Path(path)
    if not path.exists():
        raise FileNotFoundError(str(path))
    xls = pd.ExcelFile(path)
    return list(map(str, xls.sheet_names))


def read_excel_sheet(path: Path, sheet_name: str | int = 0) -> pd.DataFrame:
    path = Path(path)
    if not path.exists():
        raise FileNotFoundError(str(path))
    return pd.read_excel(path, sheet_name=sheet_name, engine="openpyxl")

