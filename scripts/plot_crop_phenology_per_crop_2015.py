"""Render one dates map and one GDD map per crop, directly in phenology_map.

    python scripts/plot_crop_phenology_per_crop_2015.py

The input is the fixed-day 2015 candidate in TEMIR_ready_input. Date color
scales are always DOY 1..365. Gray cells contain the crop but lack a usable
phenology estimate; per-cell reasons are retained in the source CSV files.
"""

from __future__ import annotations

import argparse
import math
from pathlib import Path

import cartopy.crs as ccrs
import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.colors import Normalize
import pandas as pd

from plot_crop_phenology_2015_mean_days import CROPS, country_boundary, load_tables
from plot_crop_phenology_maps_2015 import plot_panel, save_figure, style_axis


ROOT = Path(r"G:\My Drive\Angus_FYP\data\crop_phenology")
DATE_FIELDS = [
    ("planting_date", "Planting / nursery sowing"),
    ("DOE", "Emergence"),
    ("DOF", "Flowering / heading proxy"),
    ("DOH", "Maturity / harvest"),
]
GDD_FIELDS = [
    ("GDDemer", "GDD to emergence"),
    ("GDDrepr", "GDD to flowering / heading"),
    ("GDDmat", "GDD to maturity / harvest"),
]


def note_for(name: str, suffix: str) -> str:
    if suffix == "mean_days_gapfill":
        if name == "maize":
            return "SOS is emergence; missing stages use EOS−RGS or nearby direct records (≤3°), flagged in the CSV."
        if name == "soybean":
            return "Book region means; cold southern spring cells may use the book summer calendar, flagged in the CSV."
        if name == "rice_1":
            return "TR is transplanting; cold single-season cells use TR as a provisional field-establishment proxy."
    if name == "maize":
        return "SOS is the emergence proxy; planting = SOS − 14 days."
    if name == "spring_wheat":
        return "GR&EM is the emergence proxy; planting = GR&EM − 20 days. Winter wheat is excluded."
    if name == "soybean":
        return "Dates are book means for northern spring, central summer, or southern spring soybean."
    if name == "rice_1":
        return "TR is observed transplanting; sowing and emergence are inferred for early or middle/single rice."
    return "TR is observed transplanting; nursery sowing and emergence use late-rice mean offsets."


def draw_dates(name: str, label: str, df: pd.DataFrame, boundary,
               output_dir: Path, suffix: str) -> None:
    fig, axes = plt.subplots(2, 2, figsize=(14, 9),
                             subplot_kw={"projection": ccrs.PlateCarree()})
    fig.subplots_adjust(left=0.055, right=0.965, top=0.89, bottom=0.15,
                        wspace=0.06, hspace=0.17)
    norm = Normalize(1, 365)
    titles = dict(DATE_FIELDS)
    titles["planting_date"] = "Nursery sowing" if name.startswith("rice") else "Planting"
    if name == "rice_1" and suffix == "mean_days_gapfill":
        titles["planting_date"] = "Nursery sowing / field setup proxy"
        titles["DOE"] = "Emergence / field establishment"
    if name in {"rice_1", "rice_2", "spring_wheat"}:
        titles["DOF"] = "Heading proxy"
    elif name == "soybean":
        titles["DOF"] = "Flowering"
    else:
        titles["DOF"] = "Start of reproductive stage"
    for ax, (column, _) in zip(axes.flat, DATE_FIELDS):
        style_axis(ax, boundary, show_y=True, show_x=True)
        mesh = plot_panel(ax, df, column, norm, "viridis", boundary,
                          missing=True)
        ax.set_title(titles[column], loc="left", fontsize=12, pad=3)
    bar = fig.colorbar(mesh, ax=axes.ravel().tolist(), orientation="horizontal",
                       shrink=0.58, fraction=0.048, pad=0.07)
    bar.set_label("Day of year (1–365)")
    good = int(df.status.eq("ok").sum())
    fig.suptitle(f"{label} · 2015 phenology dates · {good:,}/{len(df):,} usable cells",
                 x=0.055, y=0.965, ha="left", fontsize=17,
                 fontweight="semibold")
    fig.text(0.055, 0.045, note_for(name, suffix), fontsize=10, color="#52616a")
    fig.text(0.055, 0.022,
             "Gray: crop cell with unavailable or unusable timing; see source CSV status.",
             fontsize=9, color="#52616a")
    save_figure(fig, output_dir, f"{name}_phenology_dates_2015_{suffix}")


def draw_gdd(name: str, label: str, df: pd.DataFrame, boundary,
             output_dir: Path, suffix: str) -> None:
    fig, axes = plt.subplots(1, 3, figsize=(19, 6.5),
                             subplot_kw={"projection": ccrs.PlateCarree()})
    fig.subplots_adjust(left=0.045, right=0.965, top=0.82, bottom=0.24,
                        wspace=0.06)
    accepted = df.loc[df.status.eq("ok")]
    for ax, (column, title) in zip(axes, GDD_FIELDS):
        style_axis(ax, boundary, show_y=True, show_x=True)
        maximum = int(math.ceil(accepted[column].max() / 50) * 50)
        norm = Normalize(0, maximum)
        mesh = plot_panel(ax, df, column, norm, "YlOrRd", boundary,
                          missing=True)
        ax.set_title(title, loc="left", fontsize=12, pad=3)
        bar = fig.colorbar(mesh, ax=ax, orientation="horizontal",
                           shrink=0.87, fraction=0.055, pad=0.08)
        bar.set_label("°C d")
    good = len(accepted)
    fig.suptitle(f"{label} · 2015 GDD requirements · {good:,}/{len(df):,} usable cells",
                 x=0.045, y=0.96, ha="left", fontsize=17,
                 fontweight="semibold")
    fig.text(0.045, 0.07,
             "GDDemer uses ERA5 TSOIL1; GDDrepr and GDDmat use ERA5 T2M. "
             "Daily accumulation follows TEMIR's JULES response.",
             fontsize=10, color="#52616a")
    fig.text(0.045, 0.035,
             "Gray: crop cell with unavailable or unusable timing; see source CSV status.",
             fontsize=9, color="#52616a")
    save_figure(fig, output_dir, f"{name}_phenology_gdd_2015_{suffix}")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=ROOT)
    parser.add_argument("--input-dir", type=Path)
    parser.add_argument("--output-dir", type=Path)
    parser.add_argument("--suffix", default="mean_days")
    parser.add_argument("--overwrite", action="store_true")
    args = parser.parse_args()
    input_dir = args.input_dir or args.root / "TEMIR_ready_input" / "mean_days_2015_draft"
    output_dir = args.output_dir or args.root / "phenology_map"
    tables = load_tables(input_dir)
    expected = [output_dir / f"{name}_phenology_{kind}_2015_{args.suffix}.{ext}"
                for name in CROPS for kind in ("dates", "gdd")
                for ext in ("png", "pdf")]
    existing = [path for path in expected if path.exists()]
    if existing and not args.overwrite:
        raise FileExistsError(f"Per-crop maps exist; pass --overwrite: {existing[:3]}")
    output_dir.mkdir(parents=True, exist_ok=True)
    boundary = country_boundary(args.root)
    for name, label in CROPS.items():
        df = tables[name]
        if not df.status.eq("ok").any():
            raise ValueError(f"No usable cells for {name}")
        draw_dates(name, label, df, boundary, output_dir, args.suffix)
        draw_gdd(name, label, df, boundary, output_dir, args.suffix)
        print(f"Wrote {name}: dates and GDD maps to {output_dir}", flush=True)


if __name__ == "__main__":
    main()
