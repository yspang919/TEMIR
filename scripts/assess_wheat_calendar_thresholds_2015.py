"""Map a provisional 75/105-DOY winter/spring wheat classification for 2015."""

from __future__ import annotations

import json
import sys
from pathlib import Path

import cartopy.crs as ccrs
import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.colors import BoundaryNorm, ListedColormap
from matplotlib.patches import Patch
import numpy as np
import pandas as pd

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))
from plot_crop_phenology_maps_2015 import grid_values, get_boundary, save_figure, style_axis


BASE = Path(r"G:\My Drive\Angus_FYP\data\crop_phenology")
SOURCE = BASE / "gdd_requirements_2015" / "spring_wheat_gdd_requirements_2015.csv"
OUTPUT = BASE / "phenology_map" / "wheat_75_105_review"


def main() -> None:
    df = pd.read_csv(SOURCE)
    if df.DOE.isna().any():
        raise ValueError("Wheat table has missing GR&EM dates")
    df["provisional_class"] = np.select(
        [df.DOE <= 75, df.DOE >= 105],
        ["early_greenup_likely_winter", "late_emergence_likely_spring"],
        default="uncertain_76_104",
    )
    codes = {
        "early_greenup_likely_winter": 0,
        "uncertain_76_104": 1,
        "late_emergence_likely_spring": 2,
    }
    df["class_code"] = df.provisional_class.map(codes)
    df["maturity_crosscheck"] = np.select(
        [df.DOH <= 180, df.DOH > 210],
        ["maturity_by_DOY180", "maturity_after_DOY210"],
        default="maturity_DOY181_210",
    )
    OUTPUT.mkdir(parents=True, exist_ok=True)
    df[["lat", "lon", "crop_fraction", "DOE", "DOF", "DOH", "status",
        "provisional_class", "maturity_crosscheck"]].to_csv(
        OUTPUT / "wheat_2015_threshold_review.csv", index=False
    )
    summary = {
        "rules": {"winter_candidate": "DOE <= 75", "uncertain": "76 <= DOE <= 104",
                  "spring_candidate": "DOE >= 105"},
        "class_counts": df.provisional_class.value_counts().to_dict(),
        "maturity_crosscheck_by_class": pd.crosstab(
            df.provisional_class, df.maturity_crosscheck
        ).to_dict(orient="index"),
        "note": "DOE is the source GR&EM (green-up or emergence); class is provisional."
    }
    (OUTPUT / "wheat_2015_threshold_review.json").write_text(
        json.dumps(summary, indent=2) + "\n", encoding="utf-8"
    )

    boundary = get_boundary(OUTPUT)
    fig, ax = plt.subplots(figsize=(13, 8), subplot_kw={"projection": ccrs.PlateCarree()})
    fig.subplots_adjust(left=0.07, right=0.96, bottom=0.15, top=0.83)
    xedges, yedges, values, _ = grid_values(df, "class_code")
    colors = ["#3366a8", "#d9a632", "#59a56a"]
    cmap = ListedColormap(colors)
    ax.pcolormesh(
        xedges, yedges, np.ma.masked_invalid(values), cmap=cmap,
        norm=BoundaryNorm([-0.5, 0.5, 1.5, 2.5], cmap.N),
        transform=ccrs.PlateCarree(), shading="flat", zorder=2,
    )
    style_axis(ax, boundary, show_y=True, show_x=True)
    ax.legend(
        handles=[
            Patch(facecolor=colors[0], label="Early GR&EM ≤75: likely winter (836)"),
            Patch(facecolor=colors[1], label="DOY 76–104: uncertain (134)"),
            Patch(facecolor=colors[2], label="Late GR&EM ≥105: likely spring (628)"),
        ],
        loc="lower left", bbox_to_anchor=(0.01, 0.01), frameon=True,
        framealpha=0.95, fontsize=10,
    )
    fig.suptitle("Provisional wheat calendar groups, China 2015", x=0.07,
                 y=0.94, ha="left", fontsize=18, fontweight="semibold")
    fig.text(0.07, 0.89,
             "Based on source GR&EM day of year; not a validated winter/spring wheat map.",
             fontsize=11, color="#52616a")
    fig.text(0.07, 0.055,
             "White = no wheat fraction in the source table. "
             "Maturity-date cross-checks and the original status are in the companion CSV.",
             fontsize=9, color="#52616a")
    save_figure(fig, OUTPUT, "wheat_2015_provisional_75_105_groups")
    print(OUTPUT / "wheat_2015_provisional_75_105_groups.png")


if __name__ == "__main__":
    main()
