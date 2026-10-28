"""Map original, newly usable, and still unavailable cells from the latest CSVs."""

from pathlib import Path

import cartopy.crs as ccrs
import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.patches import Patch, Rectangle
from matplotlib.collections import PatchCollection
import pandas as pd

from plot_crop_phenology_2015_mean_days import country_boundary
from plot_crop_phenology_maps_2015 import save_figure, style_axis


ROOT = Path(r"G:\My Drive\Angus_FYP\data\crop_phenology")
CROPS = ("maize", "soybean", "rice_1")
COLORS = {"original": "#277da8", "new": "#2a9d5b", "missing": "#9a9a9a"}
ORIGINAL_SOURCE = {"maize": "ok", "soybean": "book_region_calendar",
                   "rice_1": "ok"}


def main() -> None:
    new_dir = ROOT / "TEMIR_ready_input" / "mean_days_gapfill_2015_draft"
    fig, axes = plt.subplots(1, 3, figsize=(18, 6.4),
                             subplot_kw={"projection": ccrs.PlateCarree()})
    fig.subplots_adjust(left=0.035, right=0.975, top=0.83, bottom=0.19,
                        wspace=0.08)
    boundary = country_boundary(ROOT)
    for ax, name in zip(axes, CROPS):
        key = f"{name}_phenology_2015_mean_days.csv"
        df = pd.read_csv(new_dir / key,
                         usecols=["lon", "lat", "status", "source_status"])
        if df.duplicated(["lon", "lat"]).any():
            raise ValueError(f"Duplicate crop cell for {name}")
        df["class"] = "missing"
        df.loc[df.status.eq("ok") &
               df.source_status.eq(ORIGINAL_SOURCE[name]), "class"] = "original"
        df.loc[df.status.eq("ok") &
               df.source_status.ne(ORIGINAL_SOURCE[name]), "class"] = "new"
        style_axis(ax, boundary, show_y=True, show_x=True)
        for category in ("missing", "original", "new"):
            subset = df.loc[df["class"].eq(category)]
            patches = [Rectangle((lon-0.25, lat-0.25), 0.5, 0.5)
                       for lon, lat in zip(subset.lon, subset.lat)]
            ax.add_collection(PatchCollection(patches, facecolor=COLORS[category],
                                             edgecolor="none", zorder=2,
                                             transform=ccrs.PlateCarree()))
        counts = df["class"].value_counts()
        ax.set_title(f"{name.replace('_', ' ').title()} · +{counts.get('new', 0)} cells",
                     loc="left", fontsize=13)
    fig.suptitle("2015 phenology coverage after targeted gap fills",
                 x=0.035, y=0.96, ha="left", fontsize=18, fontweight="semibold")
    fig.legend(handles=[Patch(facecolor=COLORS[k], label=label)
                        for k, label in (("original", "Original usable"),
                                         ("new", "Newly usable"),
                                         ("missing", "Still unavailable"))],
               loc="lower center", ncol=3, frameon=False,
               bbox_to_anchor=(0.5, 0.10), fontsize=11)
    fig.text(0.035, 0.035,
             "Newly usable cells are provisional; source_status in the revised CSV records each gap-fill method.",
             fontsize=10, color="#52616a")
    save_figure(fig, ROOT / "phenology_map", "crop_phenology_2015_gapfill_coverage")


if __name__ == "__main__":
    main()
