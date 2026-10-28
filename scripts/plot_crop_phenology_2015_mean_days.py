"""Plot review maps for the fixed-day 2015 China phenology draft.

    python scripts/plot_crop_phenology_2015_mean_days.py

All DOY figures use 1..365, as requested for the earlier review maps.
Gray crop cells have a status other than ``ok`` in the candidate CSV.
"""

from __future__ import annotations

import argparse
import math
from pathlib import Path

import cartopy.crs as ccrs
import cartopy.io.shapereader as shpreader
import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.colors import BoundaryNorm, ListedColormap, Normalize
from matplotlib.patches import Patch
import numpy as np
import pandas as pd

from plot_crop_phenology_maps_2015 import grid_values, plot_panel, save_figure, style_axis


ROOT = Path(r"G:\My Drive\Angus_FYP\data\crop_phenology")
CROPS = {
    "maize": "Maize",
    "spring_wheat": "Spring wheat",
    "soybean": "Soybean",
    "rice_1": "Rice 1 (early or single)",
    "rice_2": "Rice 2 (late)",
}
VARIABLES = {
    "planting_date": ("Inferred planting date", "DOY", "viridis"),
    "GDDemer": ("GDD to emergence", "°C d", "YlGnBu"),
    "GDDrepr": ("GDD to flowering/heading proxy", "°C d", "YlGnBu"),
    "GDDmat": ("GDD to maturity", "°C d", "YlOrRd"),
    "DOE": ("Emergence date", "DOY", "viridis"),
    "DOF": ("Flowering/heading proxy", "DOY", "viridis"),
    "DOH": ("Maturity/harvest date", "DOY", "viridis"),
}


def country_boundary(root: Path):
    path = (root / "phenology_map" / "season_labels_corrected_2015"
            / "natural_earth_110m" / "ne_110m_admin_0_countries.shp")
    if not path.is_file():
        raise FileNotFoundError(path)
    return [rec.geometry for rec in shpreader.Reader(str(path)).records()
            if rec.attributes.get("ADMIN") in {"China", "Taiwan"}]


def load_tables(input_dir: Path) -> dict[str, pd.DataFrame]:
    tables = {}
    for name in CROPS:
        path = input_dir / f"{name}_phenology_2015_mean_days.csv"
        df = pd.read_csv(path)
        if df.duplicated(["lon", "lat"]).any():
            raise ValueError(f"Duplicate crop cells in {path}")
        for col in VARIABLES:
            if col not in df:
                raise ValueError(f"Missing {col} in {path}")
        tables[name] = df
    return tables


def plot_variable(tables, geometries, variable: str, output_dir: Path):
    title, unit, cmap = VARIABLES[variable]
    if unit == "DOY":
        low, high = 1, 365
    else:
        values = pd.concat([df.loc[df.status.eq("ok"), variable]
                            for df in tables.values()], ignore_index=True)
        low, high = 0, int(math.ceil(values.max() / 100) * 100)
    norm = Normalize(vmin=low, vmax=high)
    fig, axes = plt.subplots(2, 3, figsize=(16, 9),
                             subplot_kw={"projection": ccrs.PlateCarree()})
    fig.subplots_adjust(left=0.045, right=0.95, top=0.89, bottom=0.13,
                        wspace=0.045, hspace=0.13)
    mesh = None
    for ax, (name, label) in zip(axes.flat, CROPS.items()):
        df = tables[name]
        style_axis(ax, geometries, show_y=True, show_x=True)
        mesh = plot_panel(ax, df, variable, norm, cmap, geometries, missing=True)
        count = int(df.status.eq("ok").sum())
        ax.set_title(f"{label} · {count:,}/{len(df):,} usable cells",
                     loc="left", fontsize=10, pad=3)
    axes.flat[-1].axis("off")
    bar = fig.colorbar(mesh, ax=axes.flat[:5].tolist(), orientation="horizontal",
                       fraction=0.045, pad=0.07, shrink=0.53)
    bar.set_label(unit)
    fig.suptitle(f"China 2015 · {title} · fixed-day draft", x=0.05,
                 y=0.965, ha="left", fontsize=17, fontweight="semibold")
    fig.text(0.05, 0.025,
             "Gray: crop present, timing unavailable or model threshold fails. "
             "Soybean dates are regional book means; rice nursery dates are inferred from TR.",
             fontsize=9, color="#52616a")
    save_figure(fig, output_dir, f"china_2015_mean_days_{variable}")


def plot_soybean_groups(df: pd.DataFrame, geometries, output_dir: Path):
    groups = ["northern_spring", "central_summer", "southern_spring"]
    colors = ["#417e9a", "#e99d34", "#72ae73"]
    labels = ["Northern spring", "Central summer", "Southern spring"]
    framed = df.copy()
    framed["group_code"] = framed.crop_group.map(dict(zip(groups, range(3))))
    fig, ax = plt.subplots(figsize=(13, 8),
                           subplot_kw={"projection": ccrs.PlateCarree()})
    fig.subplots_adjust(left=0.07, right=0.96, bottom=0.13, top=0.87)
    style_axis(ax, geometries, show_y=True, show_x=True)
    xedges, yedges, values, _ = grid_values(framed, "group_code")
    cmap = ListedColormap(colors)
    ax.pcolormesh(xedges, yedges, np.ma.masked_invalid(values), cmap=cmap,
                  norm=BoundaryNorm([-0.5, 0.5, 1.5, 2.5], cmap.N),
                  shading="flat", transform=ccrs.PlateCarree(), zorder=2)
    handles = [Patch(facecolor=color,
                     label=f"{label} ({(df.crop_group == group).sum():,} cells)")
               for group, color, label in zip(groups, colors, labels)]
    ax.legend(handles=handles, loc="lower left", framealpha=0.95, fontsize=10)
    fig.suptitle("Soybean 2015 · provisional phenology regions",
                 x=0.07, y=0.95, ha="left", fontsize=18,
                 fontweight="semibold")
    fig.text(0.07, 0.905,
             "Book Fig. 5.1 interpreted using province boundaries and latitude cuts; "
             "not a mapped 2015 cultivar survey.", fontsize=10, color="#52616a")
    fig.text(0.07, 0.055,
             "The source 0.5° soybean fraction grid was overlap-averaged to ERA5 "
             "and retained where fraction ≥0.5%.", fontsize=9, color="#52616a")
    save_figure(fig, output_dir, "soybean_2015_mean_days_region_assignment")


def plot_rice1_groups(df: pd.DataFrame, geometries, output_dir: Path):
    groups = ["early", "middle_single"]
    colors = ["#d7832c", "#4f88aa"]
    framed = df.copy()
    framed["group_code"] = framed.crop_group.map(dict(zip(groups, range(2))))
    fig, ax = plt.subplots(figsize=(13, 8),
                           subplot_kw={"projection": ccrs.PlateCarree()})
    fig.subplots_adjust(left=0.07, right=0.96, bottom=0.13, top=0.87)
    style_axis(ax, geometries, show_y=True, show_x=True)
    xedges, yedges, values, _ = grid_values(framed, "group_code")
    cmap = ListedColormap(colors)
    ax.pcolormesh(xedges, yedges, np.ma.masked_invalid(values), cmap=cmap,
                  norm=BoundaryNorm([-0.5, 0.5, 1.5], cmap.N),
                  shading="flat", transform=ccrs.PlateCarree(), zorder=2)
    ax.legend(handles=[
        Patch(facecolor=colors[0], label=f"Early ({(df.crop_group == groups[0]).sum():,} cells)"),
        Patch(facecolor=colors[1], label=f"Middle/single ({(df.crop_group == groups[1]).sum():,} cells)"),
    ], loc="lower left", framealpha=0.95, fontsize=10)
    fig.suptitle("Rice 1 · provisional early versus middle/single assignment",
                 x=0.07, y=0.95, ha="left", fontsize=17,
                 fontweight="semibold")
    fig.text(0.07, 0.905,
             "Early if first harvest precedes rice 2 transplanting, or TR ≤125 south of 31°N; "
             "otherwise middle/single.", fontsize=10, color="#52616a")
    fig.text(0.07, 0.055,
             "Review the CSV group_reason field before using these nursery offsets for simulation.",
             fontsize=9, color="#52616a")
    save_figure(fig, output_dir, "rice_1_2015_mean_days_group_assignment")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=ROOT)
    parser.add_argument("--input-dir", type=Path)
    parser.add_argument("--output-dir", type=Path)
    args = parser.parse_args()
    input_dir = args.input_dir or args.root / "TEMIR_ready_input" / "mean_days_2015_draft"
    output_dir = args.output_dir or args.root / "phenology_map" / "mean_days_2015_draft"
    output_dir.mkdir(parents=True, exist_ok=True)
    tables = load_tables(input_dir)
    boundary = country_boundary(args.root)
    for variable in VARIABLES:
        plot_variable(tables, boundary, variable, output_dir)
        print("Wrote", output_dir / f"china_2015_mean_days_{variable}.png", flush=True)
    plot_soybean_groups(tables["soybean"], boundary, output_dir)
    print("Wrote", output_dir / "soybean_2015_mean_days_region_assignment.png")
    plot_rice1_groups(tables["rice_1"], boundary, output_dir)
    print("Wrote", output_dir / "rice_1_2015_mean_days_group_assignment.png")


if __name__ == "__main__":
    main()
