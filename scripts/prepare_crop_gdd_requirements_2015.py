"""Calculate provisional 2015 crop GDD requirements on the TEMIR ERA5 grid.

Run from PowerShell::

    python scripts/prepare_crop_gdd_requirements_2015.py
    python scripts/prepare_crop_gdd_requirements_2015.py --build

The first command inspects stage and crop-fraction coverage. ``--build`` reads
daily ERA5 T2M and TSOIL1 and writes CSV tables and a JSON summary to a new
``gdd_requirements_2015`` folder under ``data/crop_phenology``. Existing draft
NetCDF files are never modified.

TEMIR's JULES GDD update is reproduced: take the daily mean of the available
24 hourly temperatures (K), convert to Celsius, then apply the crop-specific
piecewise effective-temperature response. T2M supplies the flowering and
maturity thermal times; TSOIL1 (ERA5-Land 0-7 cm) is separately accumulated as
a diagnostic for the later planting-date / emergence calculation.

Since planting dates are unknown, GDDemer is assumed to be the published
JULES-crop constant: maize 80, spring wheat 35, and calibrated rice 50 degree
days. We approximate GDDrepr = GDDemer + sum of T2M effective temperature on
days (DOE, DOF], and GDDmat = GDDrepr + the sum on (DOF, DOH]. This implicitly
assumes T2M accumulation at DOE is GDDemer, although TEMIR actually triggers
emergence using TSOIL1. Backtracking planting dates later will allow that
assumption to be checked. DOE is excluded because emergence is observed on
that day and its unknown pre-/post-emergence split cannot be reconstructed.

Stage proxies: maize SOS / SOS+VGS / EOS; spring wheat GR&EM / HE / MA; rice
TR / HE / MA. TR is transplanting, not biological emergence, and heading is
only a flowering proxy. Likely winter wheat is excluded by a conservative
within-year rule; no cross-year thermal accumulation is attempted.

References:
  Osborne et al. 2015, Table 4: https://gmd.copernicus.org/articles/8/1139/2015/
  Xu et al. 2025, Table 3: https://gmd.copernicus.org/articles/18/7257/2025/
  TEMIR: extension_crop/plant_phenology.R, f_crop_GDD_update
"""

from __future__ import annotations

import argparse
import io
import json
import shutil
import subprocess
from collections import Counter
from datetime import date, timedelta
from pathlib import Path

import numpy as np
import pandas as pd
from netCDF4 import Dataset


YEAR = 2015
DEFAULT_ROOT = Path(r"G:\My Drive\Angus_FYP\data\crop_phenology")
CARDINAL_TEMPERATURES = {
    "maize": (8.0, 30.0, 42.0),
    "spring_wheat": (0.0, 20.0, 30.0),
    "rice": (8.0, 30.0, 42.0),
}
DEFAULT_EMERGENCE_GDD = {
    "maize": 80.0,  # Osborne et al. (2015), Table 4
    "spring_wheat": 35.0,  # Osborne et al. (2015), Table 4
    "rice_lr": 50.0,  # Xu et al. (2025), Table 3, calibrated rice
    "rice_sr_er": 50.0,
}
CROPS = tuple(DEFAULT_EMERGENCE_GDD)


def find_rscript(explicit: str | None) -> str:
    candidates = [explicit, shutil.which("Rscript"),
                  r"C:\Program Files\R\R-4.5.1\bin\Rscript.exe"]
    for executable in candidates:
        if executable and Path(executable).is_file():
            return str(executable)
    raise FileNotFoundError("Rscript is required to read .RData; pass --rscript PATH")


def read_rdata_table(path: Path, object_name: str, rscript: str) -> pd.DataFrame:
    """Export one saved R data.frame to stdout, avoiding an intermediate file."""
    if not path.is_file():
        raise FileNotFoundError(path)
    code = (
        "a<-commandArgs(TRUE); e<-new.env(); load(a[1],envir=e); "
        "if(!exists(a[2],envir=e)) stop('RData object missing'); "
        "x<-get(a[2],envir=e); "
        "write.csv(as.data.frame(x),file=stdout(),row.names=FALSE,na='')"
    )
    result = subprocess.run([rscript, "--vanilla", "-e", code, str(path), object_name],
                            capture_output=True, text=True, check=False)
    if result.returncode:
        raise RuntimeError(f"Rscript failed for {path}: {result.stderr.strip()}")
    return pd.read_csv(io.StringIO(result.stdout))


def read_era_grid(path: Path) -> tuple[np.ndarray, np.ndarray]:
    if not path.is_file():
        raise FileNotFoundError(path)
    with Dataset(path) as nc:
        lat = np.asarray(nc.variables["latitude"][:], dtype=float)
        lon = np.asarray(nc.variables["longitude"][:], dtype=float)
        for name in ("T2M", "TSOIL1"):
            var = nc.variables[name]
            if var.dimensions != ("time", "latitude", "longitude"):
                raise ValueError(f"Unexpected {name} dimensions in {path}: {var.dimensions}")
            if var.shape != (24, len(lat), len(lon)) or getattr(var, "units", None) != "K":
                raise ValueError(f"Unexpected {name} shape or units in {path}")
    if not (np.all(np.diff(lat) > 0) and np.all(np.diff(lon) > 0)):
        raise ValueError("ERA5 latitude/longitude coordinates must ascend")
    return lat, lon


def grid_index(values: pd.Series, axis: np.ndarray) -> tuple[np.ndarray, np.ndarray]:
    numeric = pd.to_numeric(values, errors="coerce").to_numpy(float)
    indices = np.searchsorted(axis, np.nan_to_num(numeric, nan=axis[0] - 1))
    clipped = np.clip(indices, 0, len(axis) - 1)
    on_grid = (np.isfinite(numeric) & (indices < len(axis))
               & (np.abs(axis[clipped] - numeric) < 1e-6))
    return clipped, on_grid


def load_fraction(root: Path, crop: str, rscript: str) -> pd.DataFrame:
    distribution_root = root.parent / "crop_distribution"
    kind = "rice" if crop.startswith("rice") else (
        "wheat" if crop == "spring_wheat" else crop)
    path = distribution_root / kind / f"{kind}_distribution_fraction_2015.RData"
    table = read_rdata_table(path, f"{kind}_distribution_df", rscript)
    fraction_name = f"{kind}_fraction"
    required = {"lon", "lat", fraction_name}
    if not required.issubset(table):
        raise ValueError(f"Missing columns in {path}: {required - set(table)}")
    table = table[["lon", "lat", fraction_name]].rename(
        columns={fraction_name: "crop_fraction"})
    if table[["lon", "lat"]].duplicated().any():
        raise ValueError(f"Duplicate distribution coordinates in {path}")
    if ((table.crop_fraction < 0) | (table.crop_fraction > 1)).any():
        raise ValueError(f"Crop fractions outside [0, 1] in {path}")
    return table.loc[table.crop_fraction > 0].copy()


def load_stages(root: Path, crop: str, rscript: str) -> pd.DataFrame:
    if crop == "maize":
        path = root / "maize" / "output" / "maize_phenology.pkl"
        table = pd.read_pickle(path).rename(columns={"Lon": "lon", "Lat": "lat"})
        table = table.assign(DOE=table["SOS"], DOF=table["SOS"] + table["VGS"],
                             DOH=table["EOS"],
                             phase_disagreement_days=(table["SOS"] + table["VGS"]
                                                      - (table["EOS"] - table["RGS"])))
    elif crop == "spring_wheat":
        table = read_rdata_table(root / "wheat" / "wheat_2015_phenology.RData",
                                 "wheat_phenology_2015_df", rscript)
        table = table.rename(columns={"Emergence": "DOE", "Heading": "DOF",
                                      "Harvest": "DOH"})
    else:
        filename, object_name = (
            ("Rice_LR_2015_phenology.RData", "Rice_LR_phenology_2015_df")
            if crop == "rice_lr" else
            ("Rice_SR_ER_2015_phenology.RData", "Rice_SR_ER_phenology_2015_df")
        )
        table = read_rdata_table(root / "rice" / filename, object_name, rscript)
        table = table.rename(columns={"Emergence": "DOE", "Heading": "DOF",
                                      "Harvest": "DOH"})
    required = {"lon", "lat", "DOE", "DOF", "DOH"}
    if not required.issubset(table):
        raise ValueError(f"Missing stage columns for {crop}: {required - set(table)}")
    if table[["lon", "lat"]].duplicated().any():
        raise ValueError(f"Duplicate stage coordinates for {crop}")
    fields = ["lon", "lat", "DOE", "DOF", "DOH"]
    if crop == "maize":
        fields.append("phase_disagreement_days")
    return table[fields]


def prepare_records(root: Path, crop: str, rscript: str, lat: np.ndarray,
                    lon: np.ndarray, winter_doe_cutoff: int,
                    winter_doh_cutoff: int, phase_tolerance: float,
                    emergence_gdd: float) -> pd.DataFrame:
    fractions = load_fraction(root, crop, rscript)
    stages = load_stages(root, crop, rscript)
    df = fractions.merge(stages, on=["lon", "lat"], how="left", validate="one_to_one")
    df.insert(0, "crop", crop)
    df["GDDemer"] = float(emergence_gdd)
    for name in ("DOE", "DOF", "DOH"):
        df[name] = np.rint(pd.to_numeric(df[name], errors="coerce"))
    df["j"], valid_lat = grid_index(df.lat, lat)
    df["i"], valid_lon = grid_index(df.lon, lon)
    df["status"] = "ok"
    df.loc[~(valid_lat & valid_lon), "status"] = "outside_era_grid"
    missing_stage = df[["DOE", "DOF", "DOH"]].isna().any(axis=1)
    df.loc[(df.status == "ok") & missing_stage, "status"] = "missing_stage"
    if crop == "spring_wheat":
        winter = ((df.DOE < winter_doe_cutoff) | (df.DOH < winter_doh_cutoff)
                  | (df.DOE > df.DOH)).fillna(False)
        df.loc[(df.status == "ok") & winter, "status"] = "likely_winter_wheat"
    valid_order = ((df.DOE >= 1) & (df.DOE < df.DOF) & (df.DOF < df.DOH)
                   & (df.DOH <= 365)).fillna(False)
    df.loc[(df.status == "ok") & ~valid_order, "status"] = "invalid_stage_order"
    if crop == "maize":
        conflict = df.phase_disagreement_days.abs() > phase_tolerance
        df.loc[(df.status == "ok") & conflict, "status"] = "maize_phase_conflict"
    for name in ("GDDT2m_DOE_DOF", "GDDT2m_DOF_DOH", "GDDTsoil_DOE_DOF",
                 "GDDTsoil_DOF_DOH", "GDDrepr", "GDDmat"):
        df[name] = np.nan
    df["soil_weather_complete"] = False
    return df


def daily_mean_kelvin(var, shape: tuple[int, int]) -> np.ndarray:
    if var.dimensions != ("time", "latitude", "longitude") or var.shape != (24, *shape):
        raise ValueError(f"Unexpected daily temperature dimensions: {var.name}")
    if getattr(var, "units", None) != "K":
        raise ValueError(f"Expected kelvin units for {var.name}")
    hourly = np.ma.filled(var[:], np.nan).astype(np.float32)
    valid = np.isfinite(hourly)
    count = valid.sum(axis=0)
    summed = np.where(valid, hourly, 0.0).sum(axis=0, dtype=np.float64)
    return np.divide(summed, count, out=np.full(shape, np.nan), where=count > 0)


def read_daily_temperatures(era_dir: Path, lat: np.ndarray, lon: np.ndarray,
                            last_day: int) -> tuple[np.ndarray, np.ndarray]:
    shape = (len(lat), len(lon))
    t2m = np.full((last_day + 1, *shape), np.nan, dtype=np.float32)
    tsoil = np.full_like(t2m, np.nan)
    for doy in range(1, last_day + 1):
        day = date(YEAR, 1, 1) + timedelta(days=doy - 1)
        path = era_dir / f"{day.month:02d}" / f"ERA5_hourly_{day:%Y%m%d}.nc"
        if not path.is_file():
            raise FileNotFoundError(f"ERA5 day missing: {path}")
        with Dataset(path) as nc:
            if not (np.array_equal(nc.variables["latitude"][:], lat)
                    and np.array_equal(nc.variables["longitude"][:], lon)):
                raise ValueError(f"ERA5 grid changed in {path}")
            t2m[doy] = daily_mean_kelvin(nc.variables["T2M"], shape)
            tsoil[doy] = daily_mean_kelvin(nc.variables["TSOIL1"], shape)
        if doy % 30 == 0 or doy == last_day:
            print(f"Read ERA5 day {doy}/{last_day}", flush=True)
    return t2m, tsoil


def effective_temperature(temperature_kelvin: np.ndarray,
                          cardinals: tuple[float, float, float]) -> np.ndarray:
    base, optimum, maximum = cardinals
    temp = temperature_kelvin.astype(np.float64) - 273.15
    return np.where(
        (temp < base) | (temp >= maximum), 0.0,
        np.where(temp <= optimum, temp - base,
                 (optimum - base) * (1.0 - (temp - optimum) / (maximum - optimum))),
    )


def thermal_prefix(temperature_kelvin: np.ndarray,
                   cardinals: tuple[float, float, float]) -> tuple[np.ndarray, np.ndarray]:
    # Prefix differences give (start_day, end_day] sums without requiring
    # weather before DOE. A separate count prevents missing days being treated
    # as zero thermal time.
    daily = effective_temperature(temperature_kelvin, cardinals)
    missing = ~np.isfinite(daily)
    daily[missing] = 0.0
    cumulative = np.cumsum(daily, axis=0, dtype=np.float64)
    missing_count = np.cumsum(missing, axis=0, dtype=np.uint16)
    return cumulative, missing_count


def interval(prefix: tuple[np.ndarray, np.ndarray], start: np.ndarray,
             end: np.ndarray, j: np.ndarray, i: np.ndarray) -> tuple[np.ndarray, np.ndarray]:
    cumulative, missing_count = prefix
    values = cumulative[end, j, i] - cumulative[start, j, i]
    complete = missing_count[end, j, i] == missing_count[start, j, i]
    values[~complete] = np.nan
    return values, complete


def calculate_records(df: pd.DataFrame, air_prefix: tuple[np.ndarray, np.ndarray],
                      soil_prefix: tuple[np.ndarray, np.ndarray]) -> pd.DataFrame:
    selected = df.status.eq("ok")
    if not selected.any():
        return df
    rows = df.index[selected]
    e, f, h = (df.loc[rows, name].to_numpy(dtype=int) for name in ("DOE", "DOF", "DOH"))
    j, i = (df.loc[rows, name].to_numpy(dtype=int) for name in ("j", "i"))
    air_ef, air_ef_ok = interval(air_prefix, e, f, j, i)
    air_fh, air_fh_ok = interval(air_prefix, f, h, j, i)
    soil_ef, soil_ef_ok = interval(soil_prefix, e, f, j, i)
    soil_fh, soil_fh_ok = interval(soil_prefix, f, h, j, i)
    air_ok = air_ef_ok & air_fh_ok
    soil_ok = soil_ef_ok & soil_fh_ok
    df.loc[rows, "GDDT2m_DOE_DOF"] = air_ef
    df.loc[rows, "GDDT2m_DOF_DOH"] = air_fh
    df.loc[rows, "GDDTsoil_DOE_DOF"] = soil_ef
    df.loc[rows, "GDDTsoil_DOF_DOH"] = soil_fh
    df.loc[rows, "soil_weather_complete"] = soil_ok
    df.loc[rows[~air_ok], "status"] = "missing_t2m_weather"
    good = rows[air_ok]
    df.loc[good, "GDDrepr"] = df.loc[good, "GDDemer"].to_numpy() + air_ef[air_ok]
    df.loc[good, "GDDmat"] = df.loc[good, "GDDrepr"].to_numpy() + air_fh[air_ok]
    # Stage dates alone are insufficient when TEMIR's thermal response is zero
    # over a whole phase: equal thresholds would make DVI's denominator zero.
    no_phase_heat = (air_ef[air_ok] <= 0) | (air_fh[air_ok] <= 0)
    df.loc[good[no_phase_heat], "status"] = "zero_phase_gdd"
    return df


def apply_wheat_calendar(df: pd.DataFrame, calendar_csv: Path) -> pd.DataFrame:
    """Replace the provisional date-only wheat filter with reviewed cell labels."""
    labels = pd.read_csv(calendar_csv, usecols=["lat", "lon", "final_calendar"])
    if labels.duplicated(["lat", "lon"]).any():
        raise ValueError("Wheat calendar has duplicate grid cells")
    if not labels.final_calendar.isin(["spring", "winter"]).all():
        raise ValueError("Wheat calendar contains an unknown label")
    result = df.merge(labels, on=["lat", "lon"], how="left", validate="one_to_one")
    if result.final_calendar.notna().sum() != len(labels):
        raise ValueError("Wheat calendar has cells absent from the crop table")
    complete = result[["DOE", "DOF", "DOH"]].notna().all(axis=1)
    if (complete & result.final_calendar.isna()).any():
        raise ValueError("Wheat calendar is missing cells with complete stage dates")
    eligible = result.final_calendar.notna() & result.status.ne("outside_era_grid")
    ordered = ((result.DOE >= 1) & (result.DOE < result.DOF)
               & (result.DOF < result.DOH) & (result.DOH <= 365))
    result.loc[eligible & ~ordered, "status"] = "invalid_stage_order"
    result.loc[eligible & ordered & result.final_calendar.eq("winter"), "status"] = "likely_winter_wheat"
    result.loc[eligible & ordered & result.final_calendar.eq("spring"), "status"] = "ok"
    return result


def write_results(output_dir: Path, tables: dict[str, pd.DataFrame],
                  emergence_gdd: dict[str, float], overwrite: bool,
                  wheat_classification_csv: Path | None = None) -> None:
    paths = {crop: output_dir / f"{crop}_gdd_requirements_2015.csv" for crop in tables}
    summary_path = output_dir / "gdd_requirements_2015_summary.json"
    existing = [path for path in [*paths.values(), summary_path] if path.exists()]
    if existing and not overwrite:
        raise FileExistsError(f"Outputs exist; use --overwrite: {existing}")
    output_dir.mkdir(parents=True, exist_ok=True)
    summary = {
        "year": YEAR,
        "method": "TEMIR JULES daily mean T2M / TSOIL1 effective temperature",
        "stage_interval": "(DOE, DOF] and (DOF, DOH]",
        "formula": "GDDrepr=GDDemer+GDDT2m_DOE_DOF; GDDmat=GDDrepr+GDDT2m_DOF_DOH",
        "emergence_gdd": emergence_gdd,
        "crops": {},
    }
    if wheat_classification_csv is not None:
        summary["wheat_classification_csv"] = str(wheat_classification_csv.resolve())
    columns = ["crop", "lat", "lon", "crop_fraction", "GDDemer", "GDDrepr", "GDDmat",
               "DOE", "DOF", "DOH", "GDDT2m_DOE_DOF", "GDDT2m_DOF_DOH",
               "GDDTsoil_DOE_DOF", "GDDTsoil_DOF_DOH", "soil_weather_complete",
               "phase_disagreement_days", "status"]
    for crop, df in tables.items():
        for column in columns:
            if column not in df:
                df[column] = np.nan
        output_columns = columns + (["final_calendar"] if "final_calendar" in df else [])
        df[output_columns].to_csv(paths[crop], index=False, float_format="%.8f")
        counts = dict(Counter(df.status))
        summary["crops"][crop] = {"crop_cells": len(df), "status_counts": counts}
        print(f"Wrote {paths[crop]} ({counts})", flush=True)
    summary_path.write_text(json.dumps(summary, indent=2) + "\n", encoding="utf-8")
    print(f"Wrote {summary_path}", flush=True)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--root", type=Path, default=DEFAULT_ROOT)
    parser.add_argument("--era-dir", type=Path)
    parser.add_argument("--output-dir", type=Path)
    parser.add_argument("--rscript")
    parser.add_argument("--build", action="store_true", help="Read ERA5 and write CSV/JSON")
    parser.add_argument("--overwrite", action="store_true")
    parser.add_argument("--crops", nargs="+", choices=CROPS, default=list(CROPS))
    parser.add_argument("--maize-gddemer", type=float, default=80.0)
    parser.add_argument("--wheat-gddemer", type=float, default=35.0)
    parser.add_argument("--rice-gddemer", type=float, default=50.0)
    parser.add_argument("--winter-doe-before", type=int, default=100)
    parser.add_argument("--winter-doh-before", type=int, default=121)
    parser.add_argument("--wheat-classification-csv", type=Path,
                        help="Reviewed lat/lon/final_calendar labels for spring wheat selection")
    parser.add_argument("--maize-phase-tolerance", type=float, default=2.0)
    args = parser.parse_args()
    emergence_gdd = {
        "maize": args.maize_gddemer, "spring_wheat": args.wheat_gddemer,
        "rice_lr": args.rice_gddemer, "rice_sr_er": args.rice_gddemer,
    }
    if any(not np.isfinite(emergence_gdd[c]) or emergence_gdd[c] <= 0 for c in args.crops):
        parser.error("Every selected GDDemer must be positive and finite")
    era_dir = args.era_dir or args.root.parent / "ERA5_data" / "merged_file" / str(YEAR)
    output_dir = args.output_dir or args.root / "gdd_requirements_2015"
    sample = era_dir / "01" / "ERA5_hourly_20150101.nc"
    lat, lon = read_era_grid(sample)
    rscript = find_rscript(args.rscript)
    tables = {
        crop: prepare_records(args.root, crop, rscript, lat, lon,
                              args.winter_doe_before, args.winter_doh_before,
                              args.maize_phase_tolerance, emergence_gdd[crop])
        for crop in args.crops
    }
    if args.wheat_classification_csv:
        if "spring_wheat" not in tables:
            parser.error("--wheat-classification-csv requires --crops spring_wheat")
        tables["spring_wheat"] = apply_wheat_calendar(
            tables["spring_wheat"], args.wheat_classification_csv)
    for crop, df in tables.items():
        print(json.dumps({"crop": crop, "crop_cells": len(df),
                          "pre_weather_status": dict(Counter(df.status))}), flush=True)
    if not args.build:
        print("Inspection only. Use --build to calculate and write requirements.")
        return
    if not any(df.status.eq("ok").any() for df in tables.values()):
        raise ValueError("No crop cells have valid stage dates")
    last_day = max(int(df.loc[df.status.eq("ok"), "DOH"].max())
                   for df in tables.values() if df.status.eq("ok").any())
    air, soil = read_daily_temperatures(era_dir, lat, lon, last_day)
    prefix_cache = {}
    for crop, df in tables.items():
        kind = "rice" if crop.startswith("rice") else crop
        if kind not in prefix_cache:
            temps = CARDINAL_TEMPERATURES[kind]
            prefix_cache[kind] = (thermal_prefix(air, temps),
                                  thermal_prefix(soil, temps))
        calculate_records(df, *prefix_cache[kind])
    write_results(output_dir, tables, {crop: emergence_gdd[crop] for crop in args.crops},
                  args.overwrite, args.wheat_classification_csv)


if __name__ == "__main__":
    main()
