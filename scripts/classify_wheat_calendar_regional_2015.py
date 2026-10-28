"""Draft spring/winter wheat labels for the 2015 GR&EM transition band.

Rules are a simulation-oriented interpretation, not observed cultivar labels:
DOE <= 75: winter; DOE >= 105: spring. In the 76–104 band, assign Xinjiang,
the broad Tibetan Plateau (Xizang, Qinghai, western Sichuan), and western
Gansu/Hexi to spring. Assign other cells by nearby definite-date wheat cells.
"""

from __future__ import annotations

import json
import shutil
import sys
from pathlib import Path

import cartopy.crs as ccrs
import cartopy.io.shapereader as shpreader
import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.colors import BoundaryNorm, ListedColormap
from matplotlib.patches import Patch
import numpy as np
import pandas as pd
from scipy.spatial import cKDTree
from shapely.geometry import Point

sys.path.insert(0, str(Path(__file__).resolve().parent))
from plot_crop_phenology_maps_2015 import grid_values, get_boundary, save_figure, style_axis


BASE = Path(r"G:\My Drive\Angus_FYP\data\crop_phenology")
SOURCE = BASE / "gdd_requirements_2015" / "spring_wheat_gdd_requirements_2015.csv"
OUTPUT = BASE / "phenology_map" / "wheat_regional_classification_draft"


def province_geometries(output_dir: Path):
    root = output_dir / "natural_earth_50m_admin1"
    root.mkdir(parents=True, exist_ok=True)
    stem = "ne_50m_admin_1_states_provinces"
    local = root / f"{stem}.shp"
    if not local.exists():
        source = Path(shpreader.natural_earth("50m", "cultural", "admin_1_states_provinces"))
        for ext in (".shp", ".shx", ".dbf", ".prj", ".cpg"):
            item = source.with_suffix(ext)
            if item.exists():
                shutil.copy2(item, root / item.name)
    return [
        (record.attributes["name"], record.geometry)
        for record in shpreader.Reader(str(local)).records()
        if record.attributes.get("admin") == "China"
    ]


def vote_nearby(tree: cKDTree, labels: np.ndarray, lon: float, lat: float):
    point = [lon, lat]
    for radius in (1.01, 2.01):
        near = tree.query_ball_point(point, radius)
        n_spring = sum(labels[i] == "spring" for i in near)
        n_winter = sum(labels[i] == "winter" for i in near)
        if n_spring != n_winter:
            calendar = "spring" if n_spring > n_winter else "winter"
            return calendar, f"nearby_{radius:g}deg_majority", n_spring, n_winter
    distances, indices = tree.query(point, k=8)
    weights = 1 / (np.asarray(distances) + 0.25)
    spring_weight = weights[np.asarray(labels)[indices] == "spring"].sum()
    winter_weight = weights[np.asarray(labels)[indices] == "winter"].sum()
    if spring_weight == winter_weight:
        raise ValueError(f"Unresolved exact nearest-neighbor tie at {lon}, {lat}")
    calendar = "spring" if spring_weight > winter_weight else "winter"
    return calendar, "nearest_8_weighted", int(sum(labels[i] == "spring" for i in indices)), int(sum(labels[i] == "winter" for i in indices))


def main() -> None:
    OUTPUT.mkdir(parents=True, exist_ok=True)
    df = pd.read_csv(SOURCE)
    if df.DOE.isna().any() or df.duplicated(["lat", "lon"]).any():
        raise ValueError("Source wheat table needs complete, unique DOE cells")
    provinces = province_geometries(OUTPUT)
    df["province"] = [
        next((name for name, shape in provinces if shape.covers(Point(lon, lat))), "unassigned")
        for lon, lat in zip(df.lon, df.lat)
    ]
    df["final_calendar"] = np.where(df.DOE <= 75, "winter",
                                    np.where(df.DOE >= 105, "spring", "unassigned"))
    df["assignment_reason"] = np.where(df.DOE <= 75, "DOE_le_75",
                                        np.where(df.DOE >= 105, "DOE_ge_105", ""))
    df["nearby_spring"] = 0
    df["nearby_winter"] = 0
    fixed = df.loc[df.final_calendar.ne("unassigned")]
    tree = cKDTree(fixed[["lon", "lat"]].to_numpy())
    fixed_labels = fixed.final_calendar.to_numpy()

    for index in df.index[df.final_calendar.eq("unassigned")]:
        row = df.loc[index]
        province = row.province
        if province == "Xinjiang":
            calendar, reason = "spring", "Xinjiang"
        elif province in {"Xizang", "Qinghai"}:
            calendar, reason = "spring", "Tibetan_Plateau_province"
        elif province == "Sichuan" and row.lon <= 104.5 and row.lat >= 31:
            calendar, reason = "spring", "western_Sichuan_plateau"
        elif province == "Gansu" and row.lon <= 103.5:
            calendar, reason = "spring", "western_Gansu_Hexi_or_plateau"
        else:
            calendar, reason, n_spring, n_winter = vote_nearby(
                tree, fixed_labels, row.lon, row.lat
            )
            df.at[index, "nearby_spring"] = n_spring
            df.at[index, "nearby_winter"] = n_winter
        df.at[index, "final_calendar"] = calendar
        df.at[index, "assignment_reason"] = reason

    if set(df.final_calendar) != {"winter", "spring"}:
        raise ValueError("Every wheat cell must have a spring or winter label")
    df["newly_assigned"] = df.DOE.between(76, 104)
    cols = ["lat", "lon", "crop_fraction", "DOE", "DOF", "DOH", "status",
            "province", "final_calendar", "assignment_reason", "newly_assigned",
            "nearby_spring", "nearby_winter"]
    csv_path = OUTPUT / "wheat_2015_regional_calendar.csv"
    df[cols].to_csv(csv_path, index=False)
    summary = {
        "date_rules": {"winter": "DOE <= 75", "uncertain": "76 <= DOE <= 104",
                       "spring": "DOE >= 105"},
        "broad_plateau": "Xizang and Qinghai provinces plus western Sichuan (lon<=104.5, lat>=31)",
        "western_Gansu": "Gansu province, lon<=103.5",
        "newly_assigned_counts": df.loc[df.newly_assigned, "final_calendar"].value_counts().to_dict(),
        "final_counts": df.final_calendar.value_counts().to_dict(),
        "assignment_reasons": df.assignment_reason.value_counts().to_dict(),
        "note": "Provisional simulation labels; source GR&EM is green-up or emergence."
    }
    (OUTPUT / "wheat_2015_regional_calendar_summary.json").write_text(
        json.dumps(summary, indent=2) + "\n", encoding="utf-8"
    )

    df["calendar_code"] = df.final_calendar.map({"winter": 0, "spring": 1})
    boundary = get_boundary(OUTPUT)
    fig, ax = plt.subplots(figsize=(13, 8), subplot_kw={"projection": ccrs.PlateCarree()})
    fig.subplots_adjust(left=0.07, right=0.96, bottom=0.15, top=0.83)
    xedges, yedges, values, _ = grid_values(df, "calendar_code")
    colors = ["#366fa8", "#54a66c"]
    cmap = ListedColormap(colors)
    ax.pcolormesh(xedges, yedges, np.ma.masked_invalid(values), cmap=cmap,
                  norm=BoundaryNorm([-0.5, 0.5, 1.5], cmap.N),
                  transform=ccrs.PlateCarree(), shading="flat", zorder=2)
    changed = df.loc[df.newly_assigned]
    ax.scatter(changed.lon, changed.lat, s=18, marker="s", facecolors="none",
               edgecolors="#de6a16", linewidths=0.8,
               transform=ccrs.PlateCarree(), zorder=5)
    style_axis(ax, boundary, show_y=True, show_x=True)
    ax.legend(handles=[
        Patch(facecolor=colors[0], label=f"Winter ({(df.final_calendar == 'winter').sum():,})"),
        Patch(facecolor=colors[1], label=f"Spring ({(df.final_calendar == 'spring').sum():,})"),
        Patch(facecolor="none", edgecolor="#de6a16", label="Originally uncertain (orange outline)"),
    ], loc="lower left", bbox_to_anchor=(0.01, 0.01), frameon=True,
       framealpha=0.95, fontsize=10)
    fig.suptitle("Draft spring/winter wheat calendar, China 2015", x=0.07,
                 y=0.94, ha="left", fontsize=18, fontweight="semibold")
    fig.text(0.07, 0.89,
             "DOY 76–104 cells assigned by western region or nearby definite-date wheat cells.",
             fontsize=11, color="#52616a")
    fig.text(0.07, 0.055,
             "Simulation-oriented classification; not a field-verified wheat variety map. "
             "Exact per-cell reasons are in the CSV.",
             fontsize=9, color="#52616a")
    save_figure(fig, OUTPUT, "wheat_2015_regional_calendar_draft")
    print(json.dumps(summary["final_counts"]))
    print(csv_path)


if __name__ == "__main__":
    main()
