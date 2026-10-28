"""Infer provisional 2015 planting DOY from DOE and ERA5 surface-soil GDD.

Usage: python scripts/backtrack_planting_dates_2015.py [crop_phenology_dir]
The CSV is an input to build_custom_phenology_map_2015.R. TEMIR starts
accumulating soil GDD on the day after planting. An inferred date is kept
only if the assumed GDDemer threshold first crosses within 3 days of DOE.
"""

from __future__ import annotations

import sys
from collections import Counter
from datetime import date, timedelta
from pathlib import Path

import numpy as np
import pandas as pd
from netCDF4 import Dataset

from prepare_crop_gdd_requirements_2015 import (
    CARDINAL_TEMPERATURES, daily_mean_kelvin, effective_temperature,
    read_era_grid,
)


def source_files(root: Path) -> dict[int, tuple[str, Path]]:
    return {
        1: ("maize", root / "gdd_requirements_2015_maize_tolerance10_draft"
            / "maize_gdd_requirements_2015.csv"),
        2: ("spring_wheat", root / "gdd_requirements_2015_wheat_regional_draft"
            / "spring_wheat_gdd_requirements_2015.csv"),
        5: ("rice", root / "gdd_requirements_2015"
            / "rice_sr_er_gdd_requirements_2015.csv"),
        6: ("rice", root / "gdd_requirements_2015"
            / "rice_lr_gdd_requirements_2015.csv"),
    }


def read_soil(root: Path, lat: np.ndarray, lon: np.ndarray,
              last_day: int) -> np.ndarray:
    soil = np.full((last_day + 1, len(lat), len(lon)), np.nan, dtype=np.float32)
    for doy in range(1, last_day + 1):
        day = date(2015, 1, 1) + timedelta(days=doy - 1)
        path = (root.parent / "ERA5_data" / "merged_file" / "2015"
                / f"{day.month:02d}" / f"ERA5_hourly_{day:%Y%m%d}.nc")
        if not path.is_file():
            raise FileNotFoundError(path)
        with Dataset(path) as nc:
            if (not np.array_equal(nc.variables["latitude"][:], lat)
                    or not np.array_equal(nc.variables["longitude"][:], lon)):
                raise ValueError(f"ERA5 grid changed: {path}")
            soil[doy] = daily_mean_kelvin(
                nc.variables["TSOIL1"], (len(lat), len(lon)))
        if doy % 30 == 0 or doy == last_day:
            print(f"Read soil temperature day {doy}/{last_day}", flush=True)
    return soil


def infer_date(prefix: np.ndarray, missing: np.ndarray, doe: int,
               threshold: float, last_day: int) -> tuple[float, str, float]:
    if doe <= 1 or not np.isfinite(threshold) or threshold <= 0:
        return np.nan, "invalid_stage_or_threshold", np.nan
    options = []
    for planting in range(1, doe):
        if missing[doe] != missing[planting]:
            continue
        target = prefix[planting] + threshold
        # The target is first reached on or after planting + 1.
        crossing = int(np.searchsorted(prefix[planting + 1:last_day + 1],
                                        target, side="left")) + planting + 1
        if crossing > last_day or missing[crossing] != missing[planting]:
            continue
        offset = crossing - doe
        options.append((abs(offset), abs(prefix[doe] - target),
                        -planting, planting, offset))
    if not options:
        return np.nan, "insufficient_soil_gdd_or_weather", np.nan
    best = min(options)
    if best[0] > 3:
        return np.nan, "emergence_mismatch_over_3_days", float(best[4])
    return float(best[3]), ("exact" if best[4] == 0 else "within_3_days"), float(best[4])


def main() -> None:
    root = (Path(sys.argv[1]) if len(sys.argv) > 1 else
            Path(r"G:\My Drive\Angus_FYP\data\crop_phenology"))
    files = source_files(root)
    tables = {}
    for layer, (crop, path) in files.items():
        df = pd.read_csv(path)
        needed = {"lon", "lat", "DOE", "GDDemer", "status"}
        if not needed.issubset(df):
            raise ValueError(f"Missing {needed - set(df)} in {path}")
        tables[layer] = (crop, df.loc[df.status.eq("ok")].copy())
    sample = (root.parent / "ERA5_data" / "merged_file" / "2015"
              / "01" / "ERA5_hourly_20150101.nc")
    lat, lon = read_era_grid(sample)
    last_day = min(365, max(int(df.DOE.max()) for _, df in tables.values()) + 7)
    soil = read_soil(root, lat, lon, last_day)
    output = []
    cache = {}
    for layer, (crop, df) in tables.items():
        if crop not in cache:
            daily = effective_temperature(soil, CARDINAL_TEMPERATURES[crop])
            missing = ~np.isfinite(daily)
            daily[missing] = 0.0
            cache[crop] = (np.cumsum(daily, axis=0, dtype=np.float64),
                           np.cumsum(missing, axis=0, dtype=np.uint16))
        prefix, missing = cache[crop]
        for row in df.itertuples(index=False):
            i = int(np.searchsorted(lon, row.lon))
            j = int(np.searchsorted(lat, row.lat))
            if (i >= len(lon) or j >= len(lat) or
                    abs(lon[i] - row.lon) > 1e-6 or
                    abs(lat[j] - row.lat) > 1e-6):
                raise ValueError(f"Off-grid crop cell: {row.lon}, {row.lat}")
            planting, status, error = infer_date(
                prefix[:, j, i], missing[:, j, i], int(row.DOE),
                float(row.GDDemer), last_day)
            output.append((layer, row.lon, row.lat, row.DOE, row.GDDemer,
                           planting, status, error))
        print(f"Layer {layer}: {dict(Counter(item[6] for item in output if item[0] == layer))}",
              flush=True)
    path = root / "TEMIR_ready_input" / "planting_date_backtracked_2015_draft.csv"
    if path.exists():
        raise FileExistsError(path)
    pd.DataFrame(output, columns=["layer", "lon", "lat", "DOE", "GDDemer",
                                  "planting_date", "status",
                                  "emergence_error_days"]).to_csv(path, index=False)
    print(f"Wrote {path}")


if __name__ == "__main__":
    main()
