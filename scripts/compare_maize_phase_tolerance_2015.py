"""Compare 2015 maize GDD maps under 2-day and 10-day phase tolerances."""

from __future__ import annotations

import sys
from pathlib import Path

import cartopy.crs as ccrs
import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import pandas as pd
from matplotlib.colors import Normalize

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))
from plot_crop_phenology_maps_2015 import plot_panel, style_axis, get_boundary, save_figure


BASE = Path(r"G:\My Drive\Angus_FYP\data\crop_phenology")
ORIGINAL = BASE / "gdd_requirements_2015" / "maize_gdd_requirements_2015.csv"
TRIAL = (
    BASE / "gdd_requirements_2015_maize_tolerance10_draft"
    / "maize_gdd_requirements_2015.csv"
)
OUTPUT = BASE / "phenology_map" / "maize_tolerance10_draft"


def main() -> None:
    original = pd.read_csv(ORIGINAL)
    trial = pd.read_csv(TRIAL)
    merged = original.merge(trial, on=["lat", "lon"], suffixes=("_old", "_new"), validate="one_to_one")
    newly_valid = merged.loc[
        merged.status_old.eq("maize_phase_conflict") & merged.status_new.eq("ok")
    ]
    if len(newly_valid) != 85:
        raise ValueError(f"Expected 85 newly valid maize cells; found {len(newly_valid)}")
    OUTPUT.mkdir(parents=True, exist_ok=True)
    boundary = get_boundary(OUTPUT)

    fig, axes = plt.subplots(
        2, 2, figsize=(15.2, 10.0), subplot_kw={"projection": ccrs.PlateCarree()}
    )
    fig.subplots_adjust(left=0.055, right=0.885, bottom=0.115, top=0.86,
                        wspace=0.06, hspace=0.18)
    rows = [
        ("GDDrepr", "YlGnBu", Normalize(0, 1800)),
        ("GDDmat", "YlOrRd", Normalize(0, 2700)),
    ]
    for row, (variable, cmap, norm) in enumerate(rows):
        for col, (label, data) in enumerate((("2-day tolerance", original),
                                              ("10-day tolerance", trial))):
            ax = axes[row, col]
            mesh = plot_panel(ax, data, variable, norm, cmap, boundary, missing=True)
            style_axis(ax, boundary, show_y=col == 0, show_x=row == 1)
            ax.scatter(
                newly_valid.lon, newly_valid.lat, s=17, marker="s",
                facecolors="none", edgecolors="#d81b60", linewidths=0.8,
                transform=ccrs.PlateCarree(), zorder=5,
            )
            ax.set_title(
                f"{variable} · {label} · {data.status.eq('ok').sum():,}/{len(data):,} cells",
                loc="left", fontsize=11.5, pad=8,
            )
        y0 = 0.54 if row == 0 else 0.17
        cax = fig.add_axes([0.91, y0, 0.016, 0.24])
        colorbar = fig.colorbar(mesh, cax=cax)
        colorbar.set_label("°C d", fontsize=10)
        colorbar.ax.tick_params(labelsize=8)
    fig.suptitle("Maize phase-tolerance sensitivity, China 2015", x=0.055,
                 y=0.95, ha="left", fontsize=18, fontweight="semibold")
    fig.text(0.055, 0.905,
             "Magenta outlines: 85 cells newly accepted when |SOS+VGS − (EOS−RGS)| ≤ 10 days.",
             fontsize=10, color="#52616a")
    fig.text(0.055, 0.035,
             "Gray = crop present but GDD excluded or unavailable. "
             "Both columns use the same GDD scale within each row. "
             "GDD accumulation and ERA5 T2M are otherwise unchanged.",
             fontsize=9, color="#52616a")
    save_figure(fig, OUTPUT, "maize_2015_gdd_phase_tolerance_2_vs_10")
    print(OUTPUT / "maize_2015_gdd_phase_tolerance_2_vs_10.png")


if __name__ == "__main__":
    main()
