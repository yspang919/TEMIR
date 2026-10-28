"""Create a separately versioned, provenance-marked revision of the 2015 draft.

The first mean-days tables are never overwritten. Maize missing VGS is
recovered from EOS-RGS; cells lacking all stage data receive a nearby stage
estimate only when at least three direct-observation donors lie within 3 deg.
For cold southern-soybean cells, the book's summer calendar is tested as an
explicit alternate. Cold single-season rice uses TR as field establishment,
with a provisional seven-day pre-TR soil-GDD interval; nursery dates remain
in the original draft. All changes remain identified in source_status.
"""

from __future__ import annotations

import argparse
import json
import sys
from collections import Counter
from pathlib import Path

import numpy as np
import pandas as pd
from scipy.spatial import cKDTree

from derive_crop_phenology_2015_mean_days import (CARDINALS, ROOT, SOY_CALENDAR,
                                                   calculate)
from prepare_crop_gdd_requirements_2015 import (effective_temperature,
                                                  read_daily_temperatures,
                                                  read_era_grid,
                                                  thermal_prefix)


CROPS = ("maize", "spring_wheat", "soybean", "rice_1", "rice_2")
SUFFIX = "2015_mean_days"


def recover_maize(df: pd.DataFrame, root: Path) -> tuple[pd.DataFrame, dict]:
    raw = pd.read_pickle(root / "maize" / "output" / "maize_phenology.pkl")
    raw = raw.rename(columns={"Lon": "lon", "Lat": "lat"})
    raw = raw[["lon", "lat", "SOS", "VGS", "RGS", "EOS"]]
    df = df.merge(raw, on=["lon", "lat"], how="left", validate="one_to_one")
    absent_vgs = (df.status.eq("missing_stage") & df.DOE.notna() &
                  df.DOF.isna() & df.DOH.notna() & df.RGS.notna())
    df.loc[absent_vgs, "DOF"] = df.loc[absent_vgs, "EOS"] - df.loc[absent_vgs, "RGS"]
    df.loc[absent_vgs, "source_status"] = "recovered_EOS_minus_RGS"
    df.loc[absent_vgs, "group_reason"] = "recovered_EOS_minus_RGS"
    df.loc[absent_vgs, "status"] = "ok"

    # Direct, ordered stage records are the only donors. Interpolate dates and
    # phase lengths separately so estimated stage ordering remains plausible.
    donors = df.loc[df.source_status.eq("ok") & df.status.eq("ok")].copy()
    target = df.index[df.status.eq("missing_stage") & df.DOE.isna() &
                      df.DOF.isna() & df.DOH.isna()]
    tree = cKDTree(donors[["lon", "lat"]].to_numpy(float))
    distance, index = tree.query(df.loc[target, ["lon", "lat"]].to_numpy(float), k=8)
    filled = 0
    for row, dist, ix in zip(target, distance, index):
        within = dist <= 3.0
        if np.count_nonzero(within) < 3:
            continue
        neighbors = donors.iloc[ix[within]]
        weights = 1 / np.maximum(dist[within], 0.25) ** 2
        doy = int(np.rint(np.average(neighbors.DOE, weights=weights)))
        vegetative = int(np.rint(np.average(neighbors.DOF - neighbors.DOE,
                                           weights=weights)))
        reproductive = int(np.rint(np.average(neighbors.DOH - neighbors.DOF,
                                             weights=weights)))
        if not (15 <= vegetative <= 180 and 15 <= reproductive <= 180 and
                1 <= doy - 14 < doy < doy + vegetative <
                doy + vegetative + reproductive <= 365):
            continue
        df.loc[row, ["DOE", "DOF", "DOH", "planting_date"]] = (
            doy, doy + vegetative, doy + vegetative + reproductive, doy - 14)
        df.loc[row, "source_status"] = "nearby_stage_interpolation"
        df.loc[row, "group_reason"] = "eight_nearest_direct_stages_within_3deg"
        df.loc[row, "status"] = "ok"
        filled += 1
    df.drop(columns=["SOS", "VGS", "RGS", "EOS"], inplace=True)
    return df, {"maize_recovered_EOS_minus_RGS": int(absent_vgs.sum()),
                "maize_interpolated_nearby_stages": filled,
                "maize_unfilled_no_stage": int((df.status.eq("missing_stage") &
                                               df.DOE.isna()).sum())}


def alternate_dates(df: pd.DataFrame) -> dict:
    soy = df["soybean"]
    candidates = soy.index[soy.crop_group.eq("southern_spring") &
                           soy.status.eq("no_soil_heat_on_emergence_day")]
    planting, emergence, flowering, maturity = SOY_CALENDAR["central_summer"]
    soy.loc[candidates, ["planting_date", "DOE", "DOF", "DOH"]] = (
        planting, emergence, flowering, maturity)
    soy.loc[candidates, "crop_group"] = "summer_calendar_climate_override"
    soy.loc[candidates, "group_reason"] = "southern_book_region_spring_calendar_cold"
    soy.loc[candidates, "source_status"] = "book_summer_calendar_test"
    soy.loc[candidates, "status"] = "ok"

    rice = df["rice_1"]
    candidates_rice = rice.index[rice.crop_group.eq("middle_single") &
                                 rice.status.eq("no_soil_heat_on_emergence_day") &
                                 rice.lat.ge(38)]
    rice.loc[candidates_rice, "DOE"] = rice.loc[candidates_rice, "TR"]
    rice.loc[candidates_rice, "planting_date"] = rice.loc[candidates_rice, "TR"] - 7
    rice.loc[candidates_rice, "group_reason"] = "cold_nursery_field_establishment_TR"
    rice.loc[candidates_rice, "source_status"] = "field_establishment_proxy_TR"
    rice.loc[candidates_rice, "status"] = "ok"
    return {"soybean_summer_calendar_tested": len(candidates),
            "rice_1_field_establishment_tested": len(candidates_rice)}


def recalculate(name: str, df: pd.DataFrame, air: np.ndarray,
                soil: np.ndarray) -> None:
    changed = df.source_status.isin({"recovered_EOS_minus_RGS",
                                    "nearby_stage_interpolation",
                                    "book_summer_calendar_test",
                                    "field_establishment_proxy_TR"})
    rows = df.index[changed & df.status.eq("ok")]
    if not len(rows):
        return
    kind = "rice" if name.startswith("rice") else name
    cardinals = CARDINALS[kind]
    sample = df.loc[rows].copy().reset_index(drop=True)
    air_prefix = thermal_prefix(air, cardinals)
    soil_prefix = thermal_prefix(soil, cardinals)
    calculate(sample, air_prefix, soil_prefix,
              effective_temperature(soil, cardinals))
    for column in ("GDDemer", "GDDrepr", "GDDmat", "air_GDD_at_DOE",
                   "soil_GDD_on_DOE", "status"):
        df.loc[rows, column] = sample[column].to_numpy()


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=ROOT)
    parser.add_argument("--output-dir", type=Path)
    args = parser.parse_args()
    root = args.root
    src = root / "TEMIR_ready_input" / "mean_days_2015_draft"
    output = args.output_dir or root / "TEMIR_ready_input" / "mean_days_gapfill_2015_draft"
    paths = {name: output / f"{name}_phenology_{SUFFIX}.csv" for name in CROPS}
    summary_path = output / "phenology_2015_mean_days_gapfill_summary.json"
    if any(p.exists() for p in [*paths.values(), summary_path]):
        raise FileExistsError("Revision output exists; choose a new --output-dir")
    tables = {name: pd.read_csv(src / f"{name}_phenology_{SUFFIX}.csv")
              for name in CROPS}
    tables["maize"], changes = recover_maize(tables["maize"], root)
    changes.update(alternate_dates(tables))
    sample = root.parent / "ERA5_data" / "merged_file" / "2015" / "01" / "ERA5_hourly_20150101.nc"
    lat, lon = read_era_grid(sample)
    for name in ("maize", "soybean", "rice_1"):
        df = tables[name]
        df["j"] = np.searchsorted(lat, df.lat.to_numpy(float))
        df["i"] = np.searchsorted(lon, df.lon.to_numpy(float))
        if (df.j.ge(len(lat)) | df.i.ge(len(lon)) |
            (np.abs(lat[df.j.clip(upper=len(lat)-1)] - df.lat) > 1e-6) |
            (np.abs(lon[df.i.clip(upper=len(lon)-1)] - df.lon) > 1e-6)).any():
            raise ValueError(f"Off-grid coordinates in {name}")
    last_day = max(int(df.DOH.max()) for df in tables.values())
    air, soil = read_daily_temperatures(root.parent / "ERA5_data" / "merged_file" / "2015",
                                        lat, lon, last_day)
    for name in ("maize", "soybean", "rice_1"):
        recalculate(name, tables[name], air, soil)
    output.mkdir(parents=True, exist_ok=True)
    summary = {"revision": "gapfill 2015 draft", "changes": changes,
               "crops": {name: dict(Counter(df.status)) for name, df in tables.items()},
               "notes": ["Original mean-days tables remain unchanged.",
                         "Maize spatial fills require >=3 direct donors within 3 degrees.",
                         "Soybean alternate uses central summer book calendar in geographically southern cells only when early spring fails thermal checks.",
                         "Cold middle/single rice approximates field establishment using TR; nursery emergence is not modeled."]}
    for name, df in tables.items():
        df.drop(columns=["i", "j"], errors="ignore").to_csv(
            paths[name], index=False, float_format="%.8f")
        print(name, summary["crops"][name], flush=True)
    summary_path.write_text(json.dumps(summary, indent=2) + "\n", encoding="utf-8")
    print("Wrote", summary_path, flush=True)


if __name__ == "__main__":
    main()
