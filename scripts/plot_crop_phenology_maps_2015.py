"""Plot China maps from the draft 2015 crop GDD requirement tables.

Usage (PowerShell)::

    python scripts/plot_crop_phenology_maps_2015.py

The five phenology figures contain maize, spring wheat, late rice (LR),
and combined single/early rice (SR&ER). A separate crop-fraction figure includes soybean.
Only rows with status == "ok" are colored in the phenology figures; gray
cells contain the crop but have no usable timing estimate.
"""

from __future__ import annotations

import argparse
import json
import math
import shutil
import subprocess
from pathlib import Path

import cartopy.crs as ccrs
import cartopy.io.shapereader as shpreader
import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from matplotlib.colors import Normalize


BASE = Path(r"G:\My Drive\Angus_FYP\data\crop_phenology")
DEFAULT_INPUT = BASE / "gdd_requirements_2015"
DEFAULT_OUTPUT = BASE / "phenology_map" / "season_labels_corrected_2015"
DEFAULT_SOYBEAN = (
    BASE.parent / "crop_distribution" / "soybean" / "soybean_distribution.RData"
)

CROPS = {
    "maize": "Maize",
    "spring_wheat": "Spring wheat",
    "rice_lr": "Late rice (LR)",
    "rice_sr_er": "Single / early rice (SR&ER)",
}
VARIABLES = {
    "GDDrepr": ("GDD to reproductive stage", "°C d", "YlGnBu"),
    "GDDmat": ("GDD to maturity/harvest", "°C d", "YlOrRd"),
    "DOE": ("Emergence / transplanting day", "Day of year", "viridis"),
    "DOF": ("Reproductive-stage day", "Day of year", "viridis"),
    "DOH": ("Maturity / harvest day", "Day of year", "viridis"),
}
EXTENT = (73, 135, 18, 54)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--input-dir", type=Path, default=DEFAULT_INPUT)
    parser.add_argument("--output-dir", type=Path, default=DEFAULT_OUTPUT)
    parser.add_argument("--soybean-rdata", type=Path, default=DEFAULT_SOYBEAN)
    parser.add_argument("--rscript", type=Path, default=None)
    parser.add_argument(
        "--maps", nargs="+", choices=[*VARIABLES, "crop_fractions"],
        default=[*VARIABLES, "crop_fractions"],
        help="Select maps to regenerate (default: all)",
    )
    return parser.parse_args()


def load_gdd_tables(input_dir: Path) -> dict[str, pd.DataFrame]:
    tables = {}
    for crop in CROPS:
        path = input_dir / f"{crop}_gdd_requirements_2015.csv"
        df = pd.read_csv(path)
        required = {"lat", "lon", "crop_fraction", "status", *VARIABLES}
        missing = required - set(df)
        if missing:
            raise ValueError(f"{path}: missing columns {sorted(missing)}")
        if df.duplicated(["lat", "lon"]).any():
            raise ValueError(f"{path}: duplicate grid cells")
        if not df["crop_fraction"].between(0, 1).all():
            raise ValueError(f"{path}: crop fractions outside [0, 1]")
        ok = df.loc[df.status.eq("ok")]
        if ok[list(VARIABLES)].isna().any().any():
            raise ValueError(f"{path}: status=ok contains missing plotted values")
        if not ((ok.DOE <= ok.DOF) & (ok.DOF <= ok.DOH)).all():
            raise ValueError(f"{path}: status=ok contains out-of-order stages")
        if not (ok.GDDrepr <= ok.GDDmat).all():
            raise ValueError(f"{path}: status=ok contains GDDrepr > GDDmat")
        tables[crop] = df
    return tables


def find_rscript(explicit: Path | None) -> Path:
    if explicit:
        if not explicit.exists():
            raise FileNotFoundError(explicit)
        return explicit
    found = shutil.which("Rscript")
    if found:
        return Path(found)
    candidates = sorted(Path(r"C:\Program Files\R").glob(r"R-*\bin\Rscript.exe"))
    if candidates:
        return candidates[-1]
    raise FileNotFoundError(
        "Rscript is required to extract the soybean fraction from RData; "
        "pass --rscript PATH or provide soybean_fraction_2015.csv in --output-dir"
    )


def load_soybean(rdata_path: Path, output_dir: Path, rscript: Path | None) -> pd.DataFrame:
    csv_path = output_dir / "soybean_fraction_2015.csv"
    if not csv_path.exists():
        code = (
            "a<-commandArgs(trailingOnly=TRUE); e<-new.env(); "
            "load(a[1], envir=e); "
            "stopifnot(exists('df_soybean', envir=e)); "
            "write.csv(e[['df_soybean']][,c('lon','lat','soybean_fraction')], "
            "a[2], row.names=FALSE)"
        )
        subprocess.run(
            [str(find_rscript(rscript)), "-e", code, str(rdata_path), str(csv_path)],
            check=True,
            capture_output=True,
            text=True,
        )
    df = pd.read_csv(csv_path)
    if df.duplicated(["lat", "lon"]).any():
        raise ValueError(f"{csv_path}: duplicate grid cells")
    if not df.soybean_fraction.between(0, 1).all():
        raise ValueError(f"{csv_path}: fractions outside [0, 1]")
    return df.rename(columns={"soybean_fraction": "crop_fraction"})


def grid_values(df: pd.DataFrame, column: str, valid_only: bool = False):
    """Return half-degree cell edges, values, and cells with unusable estimates."""
    source = df.loc[df.status.eq("ok")] if valid_only else df
    lons = np.arange(df.lon.min(), df.lon.max() + 0.25, 0.5)
    lats = np.arange(df.lat.min(), df.lat.max() + 0.25, 0.5)
    data = np.full((len(lats), len(lons)), np.nan)
    if not source.empty:
        x = np.rint((source.lon.to_numpy() - lons[0]) / 0.5).astype(int)
        y = np.rint((source.lat.to_numpy() - lats[0]) / 0.5).astype(int)
        data[y, x] = source[column].to_numpy()
    invalid = np.full(data.shape, np.nan)
    if valid_only:
        bad = df.loc[~df.status.eq("ok")]
        if not bad.empty:
            x = np.rint((bad.lon.to_numpy() - lons[0]) / 0.5).astype(int)
            y = np.rint((bad.lat.to_numpy() - lats[0]) / 0.5).astype(int)
            invalid[y, x] = 1.0
    return lons[0] - 0.25 + np.arange(len(lons) + 1) * 0.5, (
        lats[0] - 0.25 + np.arange(len(lats) + 1) * 0.5
    ), data, invalid


def get_boundary(output_dir: Path):
    """Keep a local Natural Earth copy for repeatable offline reruns."""
    boundary_dir = output_dir / "natural_earth_110m"
    boundary_dir.mkdir(exist_ok=True)
    stem = "ne_110m_admin_0_countries"
    local = boundary_dir / f"{stem}.shp"
    if not local.exists():
        source = Path(
            shpreader.natural_earth(
                resolution="110m", category="cultural", name="admin_0_countries"
            )
        )
        for extension in (".shp", ".shx", ".dbf", ".prj", ".cpg"):
            source_file = source.with_suffix(extension)
            if source_file.exists():
                shutil.copy2(source_file, boundary_dir / source_file.name)
    records = list(shpreader.Reader(str(local)).records())
    region = [
        record.geometry
        for record in records
        if record.attributes.get("ADMIN") in {"China", "Taiwan"}
    ]
    if not region:
        raise ValueError("Natural Earth country geometries for China not found")
    return region


def style_axis(ax, geometries, show_y: bool, show_x: bool) -> None:
    ax.set_extent(EXTENT, crs=ccrs.PlateCarree())
    ax.set_facecolor("#edf3f6")
    ax.add_geometries(
        geometries, ccrs.PlateCarree(), facecolor="#f4f5f1", edgecolor="none", zorder=0
    )
    ax.add_geometries(
        geometries, ccrs.PlateCarree(), facecolor="none", edgecolor="#34434a",
        linewidth=0.55, zorder=4
    )
    ax.set_xticks([80, 90, 100, 110, 120, 130], crs=ccrs.PlateCarree())
    ax.set_yticks([20, 30, 40, 50], crs=ccrs.PlateCarree())
    ax.set_xticklabels([f"{v}°E" for v in [80, 90, 100, 110, 120, 130]])
    ax.set_yticklabels([f"{v}°N" for v in [20, 30, 40, 50]])
    ax.tick_params(axis="both", which="major", labelsize=8, length=2, color="#65737b")
    if not show_x:
        ax.tick_params(labelbottom=False)
    if not show_y:
        ax.tick_params(labelleft=False)
    for spine in ax.spines.values():
        spine.set_edgecolor("#9aa6ac")
        spine.set_linewidth(0.55)


def plot_panel(ax, df, column, norm, cmap, geometries, missing=False):
    xedges, yedges, values, invalid = grid_values(df, column, valid_only=missing)
    if missing:
        ax.pcolormesh(
            xedges, yedges, np.ma.masked_invalid(invalid), cmap="Greys",
            vmin=0, vmax=2, shading="flat", transform=ccrs.PlateCarree(), zorder=1
        )
    mesh = ax.pcolormesh(
        xedges, yedges, np.ma.masked_invalid(values), cmap=cmap, norm=norm,
        shading="flat", transform=ccrs.PlateCarree(), zorder=2
    )
    return mesh


def value_range(tables: dict[str, pd.DataFrame], variable: str) -> tuple[float, float]:
    if variable in {"DOE", "DOF", "DOH"}:
        return 1.0, 365.0
    values = pd.concat(
        [df.loc[df.status.eq("ok"), variable] for df in tables.values()],
        ignore_index=True,
    )
    return 0.0, float(math.ceil(values.max() / 100) * 100)


def save_figure(fig, output_dir: Path, stem: str) -> None:
    fig.savefig(output_dir / f"{stem}.png", dpi=210, facecolor="white")
    fig.savefig(output_dir / f"{stem}.pdf", facecolor="white")
    plt.close(fig)


def map_stem(variable: str) -> str:
    stem = f"china_2015_{variable.lower()}"
    return f"{stem}_doy1-365" if variable in {"DOE", "DOF", "DOH"} else stem


def map_variable(variable, tables, geometries, output_dir):
    title, units, cmap = VARIABLES[variable]
    lo, hi = value_range(tables, variable)
    norm = Normalize(lo, hi)
    fig, axes = plt.subplots(
        2, 2, figsize=(15.2, 10.0), subplot_kw={"projection": ccrs.PlateCarree()}
    )
    fig.subplots_adjust(left=0.055, right=0.90, bottom=0.115, top=0.86, wspace=0.06, hspace=0.18)
    for index, (ax, (crop, label)) in enumerate(zip(axes.flat, CROPS.items())):
        df = tables[crop]
        mesh = plot_panel(ax, df, variable, norm, cmap, geometries, missing=True)
        style_axis(ax, geometries, show_y=index % 2 == 0, show_x=index >= 2)
        good = int(df.status.eq("ok").sum())
        ax.set_title(f"{label}  ·  {good:,}/{len(df):,} cells", loc="left", fontsize=12, pad=8)
    cax = fig.add_axes([0.925, 0.20, 0.018, 0.57])
    day_ticks = [1, 60, 120, 180, 240, 300, 365]
    colorbar = fig.colorbar(
        mesh, cax=cax, ticks=day_ticks if variable in {"DOE", "DOF", "DOH"} else None
    )
    colorbar.set_label(units, fontsize=10)
    colorbar.ax.tick_params(labelsize=9)
    fig.suptitle(f"China crop phenology, 2015  |  {title}", x=0.055, y=0.95,
                 ha="left", fontsize=18, fontweight="semibold")
    fig.text(0.055, 0.905, "Common color scale across crops; gray = crop present, timing unavailable or excluded.",
             fontsize=10, color="#52616a")
    fig.text(0.055, 0.035,
             "DOE = emergence (rice: transplanting); DOF = reproductive-stage proxy "
             "(rice: heading); DOH = maturity/harvest. GDD from daily ERA5 T2M; "
             "maize VGS sets DOF. Boundaries: Natural Earth 1:110m.",
             fontsize=9, color="#52616a")
    save_figure(fig, output_dir, map_stem(variable))
    return {"min": lo, "max": hi, "units": units}


def map_fraction(tables, soybean, geometries, output_dir):
    fig, axes = plt.subplots(
        2, 2, figsize=(15.2, 10.0), subplot_kw={"projection": ccrs.PlateCarree()}
    )
    fig.subplots_adjust(left=0.055, right=0.90, bottom=0.115, top=0.86, wspace=0.06, hspace=0.18)
    sources = [
        ("Maize", tables["maize"]),
        ("Wheat", tables["spring_wheat"]),
        ("Rice (both seasons)", tables["rice_sr_er"]),
        ("Soybean", soybean),
    ]
    # The largest observed fraction is below 0.7; this shared range gives
    # small crop fractions more visible contrast without clipping values.
    norm = Normalize(0, 0.7)
    for index, (ax, (label, df)) in enumerate(zip(axes.flat, sources)):
        mesh = plot_panel(ax, df, "crop_fraction", norm, "viridis", geometries)
        style_axis(ax, geometries, show_y=index % 2 == 0, show_x=index >= 2)
        ax.set_title(f"{label}  ·  {len(df):,} cells", loc="left", fontsize=12, pad=8)
    cax = fig.add_axes([0.925, 0.20, 0.018, 0.57])
    colorbar = fig.colorbar(mesh, cax=cax, ticks=np.arange(0, 0.71, 0.1))
    colorbar.set_label("Crop fraction of 0.5° cell", fontsize=10)
    colorbar.ax.tick_params(labelsize=9)
    fig.suptitle("China crop distribution fractions, 2015", x=0.055, y=0.95,
                 ha="left", fontsize=18, fontweight="semibold")
    fig.text(0.055, 0.905,
             "Each crop has its own 0.5° fraction layer; rice seasons share a rice fraction layer.",
             fontsize=10, color="#52616a")
    fig.text(0.055, 0.035,
             "Positive crop-fraction cells shown. Soybean source retains fractions ≥0.005; "
             "its grid is offset by 0.25° from the other crop layers. Boundaries: Natural Earth 1:110m.",
             fontsize=9, color="#52616a")
    save_figure(fig, output_dir, "china_2015_crop_fractions")


def main() -> None:
    args = parse_args()
    args.output_dir.mkdir(parents=True, exist_ok=True)
    tables = load_gdd_tables(args.input_dir)
    soybean = load_soybean(args.soybean_rdata, args.output_dir, args.rscript)
    geometries = get_boundary(args.output_dir)
    scales = {
        variable: {
            "min": value_range(tables, variable)[0],
            "max": value_range(tables, variable)[1],
            "units": VARIABLES[variable][1],
        }
        for variable in VARIABLES
    }
    for variable in VARIABLES:
        if variable not in args.maps:
            continue
        map_variable(variable, tables, geometries, args.output_dir)
        print(f"Wrote {args.output_dir / (map_stem(variable) + '.png')}")
    if "crop_fractions" in args.maps:
        map_fraction(tables, soybean, geometries, args.output_dir)
        print(f"Wrote {args.output_dir / 'china_2015_crop_fractions.png'}")
    summary = {
        "year": 2015,
        "input_dir": str(args.input_dir),
        "soybean_rdata": str(args.soybean_rdata),
        "scales": scales,
        "cells": {
            crop: {"present": len(df), "plotted": int(df.status.eq("ok").sum())}
            for crop, df in tables.items()
        },
        "soybean_fraction_cells": len(soybean),
    }
    (args.output_dir / "map_summary.json").write_text(
        json.dumps(summary, indent=2) + "\n", encoding="utf-8"
    )


if __name__ == "__main__":
    main()
