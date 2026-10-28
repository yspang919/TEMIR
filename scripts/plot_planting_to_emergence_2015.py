"""Map the 2015 interval from inferred planting to observed emergence proxy.

Usage: python scripts/plot_planting_to_emergence_2015.py
Reads TEMIR_ready_input/planting_date_backtracked_2015_draft.csv and writes
PNG, PDF, derived CSV, and a summary directly to phenology_map.
"""

from __future__ import annotations

import argparse
import json
from pathlib import Path

import cartopy.crs as ccrs
import cartopy.io.shapereader as shpreader
import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.colors import ListedColormap, Normalize
import numpy as np
import pandas as pd


ROOT = Path(r"G:\My Drive\Angus_FYP\data\crop_phenology")
LAYERS = {
    1: ("Maize", 80.0),
    2: ("Spring wheat", 35.0),
    5: ("Rice 1 · single / early", 50.0),
    6: ("Rice 2 · late", 50.0),
}
LON = np.arange(73.5, 135.0 + 0.01, 0.5)
LAT = np.arange(18.0, 53.5 + 0.01, 0.5)
LON_EDGES = np.arange(73.25, 135.25 + 0.01, 0.5)
LAT_EDGES = np.arange(17.75, 53.75 + 0.01, 0.5)
BIN_LIMITS = [3, 5, 7, 10, 15, 25, 50]
BIN_LABELS = ["1–3", "4–5", "6–7", "8–10", "11–15", "16–25", "26–50", ">50"]
COLORS = ["#edf8e9", "#c7e9c0", "#a1d99b", "#74c476",
          "#41ab5d", "#238b45", "#006d2c", "#00441b"]


def load_records(path: Path) -> pd.DataFrame:
    df = pd.read_csv(path)
    required = {"layer", "lon", "lat", "DOE", "GDDemer", "planting_date", "status"}
    if not required.issubset(df):
        raise ValueError(f"Missing columns in {path}: {required - set(df)}")
    if df.duplicated(["layer", "lon", "lat"]).any():
        raise ValueError("Duplicate crop-layer grid cells")
    if set(df.layer) != set(LAYERS):
        raise ValueError(f"Unexpected crop layers: {sorted(set(df.layer))}")
    for layer, (_, expected_gdd) in LAYERS.items():
        values = df.loc[df.layer.eq(layer), "GDDemer"]
        if not np.isfinite(values).all() or not np.allclose(values, expected_gdd):
            raise ValueError(f"GDDemer is not constant at {expected_gdd} in layer {layer}")
    if not df.DOE.between(1, 365).all():
        raise ValueError("Invalid 2015 emergence DOY")
    valid = df.planting_date.notna()
    if not df.loc[valid, "planting_date"].between(1, 365).all():
        raise ValueError("Invalid planting DOY")
    df["planting_to_emergence_days"] = df.DOE - df.planting_date
    if not (df.loc[valid, "planting_to_emergence_days"] > 0).all():
        raise ValueError("Planting must precede emergence in 2015")
    return df


def china_geometries(map_root: Path):
    shape = (map_root / "natural_earth_110m" /
             "ne_110m_admin_0_countries.shp")
    if not shape.is_file():
        raise FileNotFoundError(f"Expected existing Natural Earth boundary: {shape}")
    geometries = [record.geometry for record in shpreader.Reader(str(shape)).records()
                  if record.attributes.get("ADMIN") in {"China", "Taiwan"}]
    if not geometries:
        raise ValueError("No China or Taiwan geometries in Natural Earth boundary")
    return geometries


def panel_grid(df: pd.DataFrame) -> tuple[np.ndarray, np.ndarray]:
    x = np.rint((df.lon.to_numpy() - LON[0]) / 0.5).astype(int)
    y = np.rint((df.lat.to_numpy() - LAT[0]) / 0.5).astype(int)
    if ((x < 0) | (x >= len(LON)) | (y < 0) | (y >= len(LAT))).any():
        raise ValueError("Crop records outside ERA5 China grid")
    if not (np.allclose(LON[x], df.lon) and np.allclose(LAT[y], df.lat)):
        raise ValueError("Crop records are not on the ERA5 half-degree grid")
    codes = np.full((len(LAT), len(LON)), np.nan)
    missing = np.full_like(codes, np.nan)
    available = df.planting_to_emergence_days.notna().to_numpy()
    codes[y[available], x[available]] = np.digitize(
        df.loc[available, "planting_to_emergence_days"], BIN_LIMITS,
        right=True)
    missing[y[~available], x[~available]] = 1.0
    return codes, missing


def style_axis(ax, geometries, index: int) -> None:
    ax.set_extent((73, 135, 18, 54), crs=ccrs.PlateCarree())
    ax.set_facecolor("#edf3f6")
    ax.add_geometries(geometries, ccrs.PlateCarree(),
                      facecolor="#f4f5f1", edgecolor="none", zorder=0)
    ax.add_geometries(geometries, ccrs.PlateCarree(), facecolor="none",
                      edgecolor="#34434a", linewidth=0.55, zorder=4)
    xs, ys = [80, 90, 100, 110, 120, 130], [20, 30, 40, 50]
    ax.set_xticks(xs, crs=ccrs.PlateCarree())
    ax.set_yticks(ys, crs=ccrs.PlateCarree())
    ax.set_xticklabels([f"{value}°E" for value in xs])
    ax.set_yticklabels([f"{value}°N" for value in ys])
    ax.tick_params(axis="both", labelsize=8, length=2, color="#65737b",
                   labelbottom=index >= 2, labelleft=index % 2 == 0)
    for spine in ax.spines.values():
        spine.set_edgecolor("#9aa6ac")
        spine.set_linewidth(0.55)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--source", type=Path,
                        default=ROOT / "TEMIR_ready_input" /
                        "planting_date_backtracked_2015_draft.csv")
    parser.add_argument("--output-dir", type=Path,
                        default=ROOT / "phenology_map")
    args = parser.parse_args()
    df = load_records(args.source)
    geometries = china_geometries(args.output_dir)
    stem = args.output_dir / "china_2015_planting_to_emergence_days"
    csv_path = args.output_dir / "planting_to_emergence_days_2015.csv"
    summary_path = args.output_dir / "planting_to_emergence_2015_summary.json"
    outputs = [stem.with_suffix(".png"), stem.with_suffix(".pdf"),
               csv_path, summary_path]
    existing = [path for path in outputs if path.exists()]
    if existing:
        raise FileExistsError(f"Refusing to overwrite existing output: {existing[0]}")
    args.output_dir.mkdir(parents=True, exist_ok=True)
    df.to_csv(csv_path, index=False)

    fig, axes = plt.subplots(2, 2, figsize=(15.2, 10.0),
                             subplot_kw={"projection": ccrs.PlateCarree()})
    fig.subplots_adjust(left=0.055, right=0.90, bottom=0.12, top=0.85,
                        wspace=0.06, hspace=0.18)
    cmap = ListedColormap(COLORS)
    norm = Normalize(vmin=-0.5, vmax=7.5)
    summary = {}
    for index, (ax, (layer, (label, gdd))) in enumerate(zip(axes.flat, LAYERS.items())):
        subset = df.loc[df.layer.eq(layer)]
        codes, missing = panel_grid(subset)
        style_axis(ax, geometries, index)
        ax.pcolormesh(LON_EDGES, LAT_EDGES, np.ma.masked_invalid(missing),
                      cmap=ListedColormap(["#afb5bb"]), vmin=0, vmax=2,
                      shading="flat", transform=ccrs.PlateCarree(), zorder=1)
        mesh = ax.pcolormesh(LON_EDGES, LAT_EDGES, np.ma.masked_invalid(codes),
                             cmap=cmap, norm=norm, shading="flat",
                             transform=ccrs.PlateCarree(), zorder=2)
        days = subset.planting_to_emergence_days.dropna()
        ax.set_title(f"{label}  ·  GDDemer {gdd:g} °C d  ·  {len(days):,}/{len(subset):,} cells",
                     loc="left", fontsize=11.5, pad=8)
        summary[label] = {
            "layer": layer, "GDDemer_C_days": gdd, "source_cells": len(subset),
            "mapped_cells": len(days), "missing_planting_cells": int(len(subset) - len(days)),
            "median_days": float(days.median()),
            "p05_days": float(days.quantile(0.05)),
            "p95_days": float(days.quantile(0.95)),
            "maximum_days": float(days.max()),
        }
    cax = fig.add_axes([0.925, 0.20, 0.018, 0.57])
    cbar = fig.colorbar(mesh, cax=cax, ticks=range(8), boundaries=np.arange(-.5, 8.5))
    cbar.ax.set_yticklabels(BIN_LABELS)
    cbar.set_label("Days from planting to emergence proxy", fontsize=10)
    fig.suptitle("China crops, 2015  |  Planting to emergence interval",
                 x=0.055, y=0.95, ha="left", fontsize=18, fontweight="semibold")
    fig.text(0.055, 0.905,
             "DOE − inferred planting DOY. Shared day bins across crops; gray = GDD cell with no accepted planting date.",
             fontsize=10, color="#52616a")
    fig.text(0.055, 0.035,
             "Planting inferred from ERA5 surface-soil GDD; threshold crossing within ±3 days of recorded DOE. "
             "For rice, DOE is transplanting (proxy), not biological emergence. "
             "Boundaries: Natural Earth 1:110m.",
             fontsize=9, color="#52616a")
    fig.savefig(stem.with_suffix(".png"), dpi=210, facecolor="white")
    fig.savefig(stem.with_suffix(".pdf"), facecolor="white")
    plt.close(fig)
    summary_path.write_text(
        json.dumps(summary, indent=2) + "\n", encoding="utf-8")
    print(json.dumps(summary, indent=2))
    print(f"Wrote {stem}.png and .pdf")


if __name__ == "__main__":
    main()
