from __future__ import annotations

import argparse
import sys
from pathlib import Path

from dia_plots.io import list_excel_sheets, read_excel_sheet
from dia_plots.plotting import save_xy_plot


def _cmd_info(args: argparse.Namespace) -> int:
    path = Path(args.path)
    sheets = list_excel_sheets(path)
    print(f"{path.name}")
    for sheet in sheets:
        df = read_excel_sheet(path, sheet_name=sheet)
        cols = ", ".join(map(str, df.columns.tolist()))
        print(f"- {sheet}: {len(df)} filas; columnas: {cols}")
    return 0


def _cmd_plot_xy(args: argparse.Namespace) -> int:
    path = Path(args.path)
    df = read_excel_sheet(path, sheet_name=args.sheet)

    if args.x not in df.columns:
        raise SystemExit(f"Columna X no existe: {args.x}")
    if args.y not in df.columns:
        raise SystemExit(f"Columna Y no existe: {args.y}")

    out_path = Path(args.out)
    save_xy_plot(
        df=df,
        x=args.x,
        y=args.y,
        title=args.title,
        out_path=out_path,
    )
    print(str(out_path))
    return 0


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(prog="dia-plots", description="Utilidades de gráficos para DIA.")
    sub = parser.add_subparsers(dest="cmd", required=True)

    p_info = sub.add_parser("info", help="Muestra hojas y columnas de un Excel.")
    p_info.add_argument("path", help="Ruta a .xlsx")
    p_info.set_defaults(func=_cmd_info)

    p_plot = sub.add_parser("plot", help="Comandos de plot.")
    plot_sub = p_plot.add_subparsers(dest="plot_cmd", required=True)

    p_xy = plot_sub.add_parser("xy", help="Gráfico X vs Y de una hoja.")
    p_xy.add_argument("path", help="Ruta a .xlsx")
    p_xy.add_argument("--sheet", default=0, help="Nombre/índice de hoja (default: 0)")
    p_xy.add_argument("--x", required=True, help="Nombre de columna X")
    p_xy.add_argument("--y", required=True, help="Nombre de columna Y")
    p_xy.add_argument("--title", default=None, help="Título (opcional)")
    p_xy.add_argument("--out", required=True, help="Ruta de salida (png/svg/pdf)")
    p_xy.set_defaults(func=_cmd_plot_xy)

    return parser


def main(argv: list[str] | None = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)
    try:
        return int(args.func(args))
    except BrokenPipeError:
        return 0
    except KeyboardInterrupt:
        return 130
    except Exception as exc:
        print(f"ERROR: {exc}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())

