"""Build a separate 2015 phenology draft from fixed calendar intervals.

Run from the TEMIR repository with::

    python scripts/derive_crop_phenology_2015_mean_days.py --build

The existing fixed-GDD backtracking outputs are not changed. All GDD sums use
the TEMIR JULES effective-temperature response, ERA5 daily-mean TSOIL1 for
emergence, and ERA5 daily-mean T2M for reproduction and maturity. TEMIR starts
accumulating on the day after planting, hence intervals are (planting, stage].

Source stage semantics:
* maize SOS, SOS+VGS, EOS are emergence, reproductive, maturity proxies;
* spring wheat GR&EM, HE, MA are emergence, reproductive, maturity proxies;
* rice TR, HE, MA are transplant, reproductive, maturity proxies. Nursery
  sowing and true emergence are inferred from book mean differences;
* soybean sowing, emergence, flowering, maturity are book region means.

The book is 中国农业物候, Tables 2.6, 3.3, 4.2, and 5.3. Maize uses the
station-weighted mean of spring, summer, and relay sow-to-emergence durations
(14 days); spring wheat uses the rounded mean of the two spring-wheat rows
(20 days). Rice uses early/middle/late sow-to-transplant and
emergence-to-transplant differences. Soybean uses the three regional stage
calendars directly. These are provisional simulation dates, not 2015 field
observations. Winter wheat is excluded because cross-year ERA5 is unavailable.
"""

from __future__ import annotations

import argparse
import json
import sys
from collections import Counter
from pathlib import Path

import cartopy.io.shapereader as shpreader
import numpy as np
import pandas as pd
from shapely.geometry import Point

from prepare_crop_gdd_requirements_2015 import (
    CARDINAL_TEMPERATURES, find_rscript, grid_index, interval,
    read_daily_temperatures, read_era_grid, read_rdata_table, thermal_prefix,
)


ROOT = Path(r"G:\My Drive\Angus_FYP\data\crop_phenology")
BOOK = ROOT.parent / "publications" / "中国农业物候.pdf"
MAIZE_DAYS = 14
SPRING_WHEAT_DAYS = 20
SOY_CALENDAR = {
    "northern_spring": (130, 146, 194, 268),
    "central_summer": (167, 174, 212, 270),
    "southern_spring": (88, 96, 139, 184),
}
RICE_OFFSETS = {
    "early": (33, 27),       # TR - sowing, TR - emergence
    "middle_single": (44, 37),
    "late": (28, 25),
}
CARDINALS = {**CARDINAL_TEMPERATURES, "soybean": (10.0, 30.0, 40.0)}
SOURCE_CSV = {
    "maize": ROOT / "gdd_requirements_2015_maize_tolerance10_draft"
             / "maize_gdd_requirements_2015.csv",
    "spring_wheat": ROOT / "gdd_requirements_2015_wheat_regional_draft"
                    / "spring_wheat_gdd_requirements_2015.csv",
    "rice_1": ROOT / "gdd_requirements_2015"
              / "rice_sr_er_gdd_requirements_2015.csv",
    "rice_2": ROOT / "gdd_requirements_2015"
              / "rice_lr_gdd_requirements_2015.csv",
}


def read_stage_tables(root: Path) -> dict[str, pd.DataFrame]:
    tables = {}
    for name, default_path in SOURCE_CSV.items():
        path = root / default_path.relative_to(ROOT)
        df = pd.read_csv(path)
        needed = {"lon", "lat", "crop_fraction", "DOE", "DOF", "DOH", "status"}
        if not needed.issubset(df):
            raise ValueError(f"Missing columns in {path}: {needed - set(df)}")
        if df.duplicated(["lon", "lat"]).any():
            raise ValueError(f"Duplicate stage cells in {path}")
        df = df[["lon", "lat", "crop_fraction", "DOE", "DOF", "DOH", "status"]].copy()
        df.rename(columns={"status": "source_status"}, inplace=True)
        tables[name] = df
    return tables


def transfer_soybean_fraction(root: Path, rscript: str, lat: np.ndarray,
                              lon: np.ndarray) -> tuple[pd.DataFrame, dict]:
    """Overlap-average offset 0.5-degree fraction cells onto ERA centers.

    Each source cell with center *.25 or *.75 overlaps four ERA cells by one
    quarter. Unlisted source cells are zero because the saved source already
    excludes fractions <0.005. The target uses the same >=0.005 cutoff.
    """
    source = read_rdata_table(
        root.parent / "crop_distribution" / "soybean" / "soybean_distribution.RData",
        "df_soybean", rscript,
    )
    if source.duplicated(["lon", "lat"]).any():
        raise ValueError("Duplicate soybean source grid cells")
    if not source.soybean_fraction.between(0, 1).all():
        raise ValueError("Soybean fractions must lie in [0, 1]")
    if (np.abs(source.lon.to_numpy() * 2 % 1 - 0.5) > 1e-6).any() or \
       (np.abs(source.lat.to_numpy() * 2 % 1 - 0.5) > 1e-6).any():
        raise ValueError("Unexpected soybean source grid centers")
    parts = []
    for dx in (0.0, 0.5):
        for dy in (0.0, 0.5):
            part = source.copy()
            part["lon"] = np.floor(part.lon.to_numpy() * 2) / 2 + dx
            part["lat"] = np.floor(part.lat.to_numpy() * 2) / 2 + dy
            part["soybean_fraction"] /= 4
            parts.append(part)
    target = pd.concat(parts, ignore_index=True).groupby(
        ["lon", "lat"], as_index=False).soybean_fraction.sum()
    target = target[target.lon.isin(lon) & target.lat.isin(lat)].copy()
    total_before_cutoff = float(target.soybean_fraction.sum())
    target = target[target.soybean_fraction >= 0.005].copy()
    target.rename(columns={"soybean_fraction": "crop_fraction"}, inplace=True)
    return target, {
        "source_cells": len(source), "target_cells": len(target),
        "source_fraction_sum": float(source.soybean_fraction.sum()),
        "fraction_sum_before_target_cutoff": total_before_cutoff,
        "target_fraction_sum": float(target.crop_fraction.sum()),
        "target_cutoff": 0.005,
        "method": "four quarter-cell overlaps per source cell; absent source cells treated as zero",
    }


def admin1_provinces(root: Path):
    path = (root / "phenology_map" / "wheat_regional_classification_draft"
            / "natural_earth_50m_admin1"
            / "ne_50m_admin_1_states_provinces.shp")
    if not path.is_file():
        raise FileNotFoundError(f"Cached province boundary missing: {path}")
    records = [(rec.attributes["name"], rec.geometry)
               for rec in shpreader.Reader(str(path)).records()
               if rec.attributes.get("admin") == "China"]
    if not records:
        raise ValueError("No China province geometries in cached shapefile")
    return records, path


def soybean_region(province: str, latitude: float) -> tuple[str, str]:
    """Approximate Fig. 5.1 with province boundaries and named latitude cuts."""
    if province in {"Heilongjiang", "Jilin", "Liaoning", "Inner Mongol",
                    "Xinjiang", "Ningxia"}:
        return "northern_spring", "province_north"
    if province == "Hebei":
        return ("northern_spring", "hebei_north") if latitude >= 40 else \
               ("central_summer", "hebei_south")
    if province == "Shanxi":
        return ("northern_spring", "shanxi_north") if latitude >= 38 else \
               ("central_summer", "shanxi_south")
    if province == "Shaanxi":
        if latitude >= 36:
            return "northern_spring", "shaanxi_north"
        return ("central_summer", "shaanxi_central") if latitude >= 33.5 else \
               ("southern_spring", "shaanxi_south")
    if province == "Gansu":
        if latitude >= 37:
            return "northern_spring", "gansu_north"
        return ("central_summer", "gansu_south") if latitude >= 33.5 else \
               ("southern_spring", "gansu_far_south")
    if province in {"Beijing", "Tianjin", "Shandong"}:
        return "central_summer", "province_summer"
    if province == "Henan":
        return ("central_summer", "henan_north") if latitude >= 32.5 else \
               ("southern_spring", "henan_south")
    if province in {"Anhui", "Jiangsu"}:
        return ("central_summer", "huaihe_north") if latitude >= 33 else \
               ("southern_spring", "huaihe_south")
    # Book region III covers the country south of Qinling-Huaihe and east of
    # the Tibetan Plateau. Qinghai/Xizang cells are explicit extrapolations.
    return "southern_spring", "southern_or_extrapolated_province"


def add_soybean_regions(df: pd.DataFrame, provinces) -> pd.DataFrame:
    names, geoms = zip(*provinces)
    assigned = []
    for lon, lat in zip(df.lon, df.lat):
        point = Point(float(lon), float(lat))
        found = next((name for name, geom in provinces if geom.covers(point)), None)
        if found is None:
            nearest = int(np.argmin([geom.distance(point) for geom in geoms]))
            found = names[nearest]
            method = "nearest_province_boundary"
        else:
            method = "province_contains_center"
        group, reason = soybean_region(found, float(lat))
        assigned.append((found, group, method + ":" + reason))
    df[["province", "crop_group", "group_reason"]] = pd.DataFrame(
        assigned, index=df.index)
    return df


def stage_table(name: str, source: pd.DataFrame,
                rice2: pd.DataFrame | None = None) -> pd.DataFrame:
    df = source.copy()
    df.insert(0, "crop", name)
    df["crop_group"] = ""
    df["group_reason"] = ""
    if name == "maize":
        df["TR"] = np.nan
        df["crop_group"] = "maize_all"
        df["group_reason"] = "station_weighted_mean_14_days"
        df["planting_date"] = df.DOE - MAIZE_DAYS
    elif name == "spring_wheat":
        df["TR"] = np.nan
        df["crop_group"] = "spring_wheat_all"
        df["group_reason"] = "two_spring_wheat_rows_mean_20_days"
        df["planting_date"] = df.DOE - SPRING_WHEAT_DAYS
    elif name == "rice_1":
        assert rice2 is not None
        df.rename(columns={"DOE": "TR"}, inplace=True)
        later = rice2.loc[rice2.source_status.eq("ok"),
                           ["lon", "lat", "DOE"]].rename(columns={"DOE": "TR2"})
        df = df.merge(later, on=["lon", "lat"], how="left", validate="one_to_one")
        double_sequence = (df.DOH < df.TR2) & df.TR2.notna()
        early_date_south = (df.TR <= 125) & (df.lat <= 31)
        early = double_sequence | early_date_south
        df["crop_group"] = np.where(early, "early", "middle_single")
        df["group_reason"] = np.where(double_sequence, "harvest_before_late_transplant",
                                      np.where(early_date_south,
                                               "southern_early_TR_le_125",
                                               "single_or_middle_default"))
        df["planting_date"] = df.TR - np.where(early, 33, 44)
        df["DOE"] = df.TR - np.where(early, 27, 37)
        df.drop(columns="TR2", inplace=True)
    elif name == "rice_2":
        df.rename(columns={"DOE": "TR"}, inplace=True)
        df["crop_group"] = "late"
        df["group_reason"] = "rice_2_late_layer"
        df["planting_date"] = df.TR - 28
        df["DOE"] = df.TR - 25
    else:
        raise ValueError(name)
    return df


def soybean_table(fractions: pd.DataFrame, provinces) -> pd.DataFrame:
    df = add_soybean_regions(fractions.copy(), provinces)
    df.insert(0, "crop", "soybean")
    for column in ("planting_date", "DOE", "DOF", "DOH"):
        df[column] = df.crop_group.map({
            group: dates[("planting_date", "DOE", "DOF", "DOH").index(column)]
            for group, dates in SOY_CALENDAR.items()
        })
    df["TR"] = np.nan
    df["source_status"] = "book_region_calendar"
    return df


def attach_grid_and_check(df: pd.DataFrame, lat: np.ndarray,
                          lon: np.ndarray) -> pd.DataFrame:
    df["j"], lat_ok = grid_index(df.lat, lat)
    df["i"], lon_ok = grid_index(df.lon, lon)
    df["status"] = "ok"
    df.loc[~(lat_ok & lon_ok), "status"] = "outside_era_grid"
    df.loc[(df.status == "ok") &
           (~df.source_status.isin(["ok", "book_region_calendar"])),
           "status"] = df.source_status
    missing = df[["planting_date", "DOE", "DOF", "DOH"]].isna().any(axis=1)
    df.loc[(df.status == "ok") & missing, "status"] = "missing_stage"
    ordered = ((df.planting_date >= 1) & (df.planting_date < df.DOE) &
               (df.DOE < df.DOF) & (df.DOF < df.DOH) & (df.DOH <= 365))
    df.loc[(df.status == "ok") & ~ordered, "status"] = "invalid_or_cross_year_stage"
    for col in ("GDDemer", "GDDrepr", "GDDmat", "air_GDD_at_DOE",
                "soil_GDD_on_DOE"):
        df[col] = np.nan
    return df


def calculate(df: pd.DataFrame, air_prefix, soil_prefix,
              soil_daily: np.ndarray) -> None:
    rows = df.index[df.status.eq("ok")]
    if not len(rows):
        return
    p, e, f, h = (df.loc[rows, col].to_numpy(dtype=int)
                  for col in ("planting_date", "DOE", "DOF", "DOH"))
    j, i = (df.loc[rows, col].to_numpy(dtype=int) for col in ("j", "i"))
    soil_e, soil_ok = interval(soil_prefix, p, e, j, i)
    air_e, air_e_ok = interval(air_prefix, p, e, j, i)
    air_f, air_f_ok = interval(air_prefix, p, f, j, i)
    air_h, air_h_ok = interval(air_prefix, p, h, j, i)
    df.loc[rows, "GDDemer"] = soil_e
    df.loc[rows, "GDDrepr"] = air_f
    df.loc[rows, "GDDmat"] = air_h
    df.loc[rows, "air_GDD_at_DOE"] = air_e
    df.loc[rows, "soil_GDD_on_DOE"] = soil_daily[e, j, i]
    good = soil_ok & air_e_ok & air_f_ok & air_h_ok
    df.loc[rows[~good], "status"] = "missing_era_temperature"
    flat = good & ((soil_e <= 0) | (soil_daily[e, j, i] <= 0))
    df.loc[rows[flat], "status"] = "no_soil_heat_on_emergence_day"
    invalid = good & ~flat & ((air_f <= soil_e) | (air_h <= air_f))
    df.loc[rows[invalid], "status"] = "nonmonotonic_gdd"


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=ROOT)
    parser.add_argument("--output-dir", type=Path)
    parser.add_argument("--rscript")
    parser.add_argument("--build", action="store_true")
    args = parser.parse_args()
    root = args.root
    output = args.output_dir or root / "TEMIR_ready_input" / "mean_days_2015_draft"
    sample = (root.parent / "ERA5_data" / "merged_file" / "2015" / "01"
              / "ERA5_hourly_20150101.nc")
    lat, lon = read_era_grid(sample)
    stage_sources = read_stage_tables(root)
    soybean, transfer_info = transfer_soybean_fraction(
        root, find_rscript(args.rscript), lat, lon)
    provinces, province_path = admin1_provinces(root)
    tables = {name: stage_table(name, source, stage_sources.get("rice_2"))
              for name, source in stage_sources.items()}
    tables["soybean"] = soybean_table(soybean, provinces)
    for name, df in tables.items():
        attach_grid_and_check(df, lat, lon)
        print(name, len(df), dict(Counter(df.status)),
              dict(Counter(df.crop_group)), flush=True)
    if not args.build:
        print("Inspection only. Use --build to read ERA5 and save a new draft.")
        return
    paths = {name: output / f"{name}_phenology_2015_mean_days.csv" for name in tables}
    summary_path = output / "phenology_2015_mean_days_summary.json"
    if any(path.exists() for path in [*paths.values(), summary_path]):
        raise FileExistsError("Draft output already exists; choose a new --output-dir")
    last_day = max(int(df.loc[df.status.eq("ok"), "DOH"].max())
                   for df in tables.values() if df.status.eq("ok").any())
    era_dir = root.parent / "ERA5_data" / "merged_file" / "2015"
    air, soil = read_daily_temperatures(era_dir, lat, lon, last_day)
    cache = {}
    for name, df in tables.items():
        kind = "rice" if name.startswith("rice") else name
        if kind not in cache:
            cardinals = CARDINALS[kind]
            air_prefix = thermal_prefix(air, cardinals)
            soil_prefix = thermal_prefix(soil, cardinals)
            from prepare_crop_gdd_requirements_2015 import effective_temperature
            soil_daily = effective_temperature(soil, cardinals)
            cache[kind] = (air_prefix, soil_prefix, soil_daily)
        calculate(df, *cache[kind])
    output.mkdir(parents=True, exist_ok=True)
    summary = {
        "year": 2015, "reference_book": str(BOOK),
        "source_period": {"soybean": "1992-2011", "other_crops": "1981-2010"},
        "formula": "GDDemer=soil(planting,DOE]; GDDrepr=air(planting,DOF]; GDDmat=air(planting,DOH]",
        "temperature_method": "TEMIR JULES; ERA5 daily-mean TSOIL1 and T2M",
        "fixed_days": {"maize": MAIZE_DAYS, "spring_wheat": SPRING_WHEAT_DAYS},
        "rice_offsets": RICE_OFFSETS, "soybean_calendar": SOY_CALENDAR,
        "soybean_grid_transfer": transfer_info,
        "soybean_region_boundary": str(province_path),
        "rice_1_group_rule": "early if rice_1 harvest < rice_2 transplant, or TR<=125 and latitude<=31; otherwise middle/single",
        "soybean_region_rule": "Book Fig 5.1 approximated using province and explicit latitude cuts; see script and group_reason",
        "notes": [
            "Winter wheat is excluded because cross-year ERA5 data are unavailable.",
            "Rice sowing/emergence are inferred before observed transplanting; TEMIR has no nursery/transplant process yet.",
            "Soybean R1 is flowering; the book's final stage is maturity, not R6.",
            "Mean stage dates represent regional climatology, not observations at each 2015 grid cell.",
        ],
        "crops": {},
    }
    columns = ["crop", "lat", "lon", "crop_fraction", "province", "crop_group",
               "group_reason", "planting_date", "DOE", "TR", "DOF", "DOH",
               "GDDemer", "GDDrepr", "GDDmat", "air_GDD_at_DOE",
               "soil_GDD_on_DOE", "source_status", "status"]
    for name, df in tables.items():
        for col in columns:
            if col not in df:
                df[col] = np.nan
        df[columns].to_csv(paths[name], index=False, float_format="%.8f")
        accepted = df.loc[df.status.eq("ok")]
        summary["crops"][name] = {
            "cells": len(df), "status_counts": dict(Counter(df.status)),
            "group_counts": dict(Counter(df.crop_group)),
            "accepted_GDDemer_quantiles": accepted.GDDemer.quantile(
                [0.05, 0.5, 0.95]).to_dict() if len(accepted) else {},
            "source_file": (str(root / SOURCE_CSV[name].relative_to(ROOT))
                            if name in SOURCE_CSV else
                            str(root.parent / "crop_distribution" / "soybean"
                                / "soybean_distribution.RData")),
        }
        print("Wrote", paths[name], summary["crops"][name]["status_counts"], flush=True)
    summary_path.write_text(json.dumps(summary, indent=2) + "\n", encoding="utf-8")
    print("Wrote", summary_path, flush=True)


if __name__ == "__main__":
    main()
