"""
================================================================================
ERA5 & ERA5-Land to Hourly-Daily NetCDF Processing Pipeline
Memory-Optimized Day-by-Day Streaming Architecture
================================================================================
"""

import os
os.environ["HDF5_USE_FILE_LOCKING"] = "FALSE"

import gc
import glob
import calendar
from datetime import date, timedelta
import shutil
import tempfile
import numpy as np
import xarray as xr

# ==============================================================================
# 1. USER CONFIGURATION
# ==============================================================================

# Base directory on Google Drive
BASE_DIR = r"G:\My Drive\Angus_FYP\data\ERA5_data"
# BASE_DIR = '/Users/User/Google Drive/我的雲端硬碟/Angus_FYP/data/ERA5_data'

# Time range to process
YEARS = [2015]          # e.g., [2015] or range(2015, 2021)
MONTHS = range(12, 13)   # Change to [8] or range(8, 13) for a partial run.
DAYS = None               # None processes every day; e.g. [1, 2] for a check.
# Report cells with this many missing output hours, without filling them.
SIGNIFICANT_MISSING_HOURS = 3
MAX_REPORTED_CELLS_PER_VARIABLE = 20

# Subdirectories and expected file naming prefix / pattern
FOLDERS = ["ERA5_land1", "ERA5_land2", "ERA5_land3", "ERA5_land4", "ERA5_singlelayer"]
FOLDER_PATTERNS = {
    "ERA5_land1": "era5_land_1*{year}{month:02d}*.nc",
    "ERA5_land2": "era5_land_2*{year}{month:02d}*.nc",
    "ERA5_land3": "era5_land_3*{year}{month:02d}*.nc",
    "ERA5_land4": "era5_land_4*{year}{month:02d}*.nc",
    "ERA5_singlelayer": "era5_singlelayer_*{year}{month:02d}*.nc",
}

# ERA5-Land values are totals since 00 UTC; ERA5 single-level accumulations
# already describe the hour ending at their validity time.
LAND_ACCUMULATIONS = {"ssrd", "sshf", "slhf", "tp", "sf", "e"}
# No 2016-01 source is available for the last hour of 2015-12-31.
ESTIMATED_FINAL_HOUR_DATE = date(2015, 12, 31)

# Input Caching Setting:
# False streams directly from Google Drive without copying 4-6 GB to C: drive
CACHE_INPUTS_LOCALLY = False

# Target Spatial Grid (China / East Asia domain by default)
GRID_STEP = 0.5
LAT_MIN, LAT_MAX = 18.0, 53.5
LON_MIN, LON_MAX = 73.5, 135.0

TARGET_LAT = np.arange(LAT_MIN, LAT_MAX + GRID_STEP / 2, GRID_STEP)
TARGET_LON = np.arange(LON_MIN, LON_MAX + GRID_STEP / 2, GRID_STEP)

# Output directory on Google Drive
OUTPUT_BASE = os.path.join(BASE_DIR, "merged_file")

# Compression settings for daily output files
COMPRESS_OUTPUT = True
COMPRESSION_LEVEL = 4

# Soil saturation thresholds for GWETTOP and GWETROOT
THETA_SAT_TOP = 0.45
THETA_SAT_ROOT = 0.45

# Optional xesmf
try:
    import xesmf as xe
    USE_XESMF = True
except ImportError:
    USE_XESMF = False


# ==============================================================================
# 2. COORDINATE & REGRIDDING HELPERS
# ==============================================================================

def clean_and_standardize_dataset(ds: xr.Dataset) -> xr.Dataset:
    """Standardizes coordinate names, dims, and sorts latitude ascending."""
    if "expver" in ds.dims:
        if ds.sizes["expver"] > 1:
            try:
                ds = ds.sel(expver=1).combine_first(ds.sel(expver=5))
            except Exception:
                ds = ds.isel(expver=0)
        else:
            ds = ds.squeeze("expver")

    if "number" in ds.dims and ds.sizes["number"] == 1:
        ds = ds.squeeze("number")

    if "valid_time" in ds.dims:
        ds = ds.rename({"valid_time": "time"})
    elif "valid_time" in ds.coords and "time" not in ds.dims:
        ds = ds.rename({"valid_time": "time"})

    coord_renames = {}
    if "lat" in ds.dims and "latitude" not in ds.dims:
        coord_renames["lat"] = "latitude"
    if "lon" in ds.dims and "longitude" not in ds.dims:
        coord_renames["lon"] = "longitude"
    if coord_renames:
        ds = ds.rename(coord_renames)

    if "latitude" in ds.coords:
        if ds.latitude[0] > ds.latitude[-1]:
            ds = ds.sortby("latitude")

    if "longitude" in ds.coords:
        if (ds.longitude < 0).any():
            ds = ds.assign_coords(longitude=(ds.longitude % 360)).sortby("longitude")

    return ds


def crop_dataset(ds: xr.Dataset, lat_min: float, lat_max: float,
                 lon_min: float, lon_max: float, buffer: float = 1.0) -> xr.Dataset:
    """Pre-crops dataset around target domain before regridding."""
    lat_slice = slice(lat_min - buffer, lat_max + buffer)
    lon_slice = slice(lon_min - buffer, lon_max + buffer)
    return ds.sel(latitude=lat_slice, longitude=lon_slice)


def regrid_daily_dataset(ds_day: xr.Dataset, target_lat: np.ndarray,
                         target_lon: np.ndarray) -> xr.Dataset:
    """Select source pixels on the nested 0.1/0.25 to 0.5 degree grid.

    Source coordinates differ from exact half degrees by floating-point noise.
    Linear interpolation can spread a neighboring NaN into an otherwise valid
    target pixel; nearest selection keeps the actual coincident source value.
    """
    return ds_day.interp(latitude=target_lat, longitude=target_lon,
                         method="nearest")


def find_monthly_files(folder: str, year: int, month: int) -> list[str]:
    """Find monthly files, including unzipped ERA5 single-level subfolders."""
    pattern = FOLDER_PATTERNS[folder].format(year=year, month=month)
    matches = glob.glob(os.path.join(BASE_DIR, folder, pattern))
    if folder == "ERA5_singlelayer":
        nested = os.path.join(BASE_DIR, folder, f"era5_singlelayer_{year}{month:02d}")
        matches.extend(glob.glob(os.path.join(nested, "*.nc")))
    if not matches:
        standard = os.path.join(BASE_DIR, folder, f"{folder.lower()}_{year}{month:02d}.nc")
        if os.path.exists(standard):
            matches = [standard]
    return sorted(set(matches))


def next_midnight_slice(folder: str, next_day: date) -> xr.Dataset | None:
    """Read only 00 UTC from next month's source files; never load the month."""
    files = find_monthly_files(folder, next_day.year, next_day.month)
    if not files:
        return None

    timestamp = np.datetime64(next_day.isoformat())
    midnight_parts = []
    for path in files:
        with xr.open_dataset(path) as opened:
            source = crop_dataset(clean_and_standardize_dataset(opened),
                                  LAT_MIN, LAT_MAX, LON_MIN, LON_MAX, buffer=1.0)
            part = source.sel(time=[timestamp])
            # Materialize this single hour before closing the file handle.
            part = regrid_daily_dataset(part, TARGET_LAT, TARGET_LON).load()
            midnight_parts.append(part)
    midnight = xr.merge(midnight_parts, compat="override")
    if folder == "ERA5_singlelayer" and "ssrd" in midnight:
        midnight = midnight.rename({"ssrd": "ssrd_single"})
    return midnight


def estimate_last_midnight(day_hours: xr.Dataset, folder: str,
                           next_day: date) -> xr.Dataset:
    """Repeat hour 23 for the one permitted missing year-end hour."""
    estimated = day_hours.isel(time=-1, drop=True).copy(deep=False)
    if folder.startswith("ERA5_land"):
        for name in LAND_ACCUMULATIONS.intersection(estimated.data_vars):
            last = day_hours[name].isel(time=-1, drop=True)
            previous = day_hours[name].isel(time=-2, drop=True)
            # This synthetic cumulative value makes its increment equal hour 23.
            estimated[name] = last + (last - previous)
    return estimated.expand_dims(time=[np.datetime64(next_day.isoformat())])


def deaccumulate_era5_land(day_hours: xr.Dataset) -> xr.Dataset:
    """Turn 01..next-day-00 running totals into 24 one-hour amounts."""
    for name in LAND_ACCUMULATIONS.intersection(day_hours.data_vars):
        running_total = day_hours[name]
        hourly = xr.concat(
            [running_total.isel(time=slice(0, 1)), running_total.diff("time")],
            dim="time",
        ).assign_coords(time=day_hours.time)
        day_hours = day_hours.assign({name: hourly})
    return day_hours


def repair_land_accumulation_gaps(day_hours: xr.Dataset,
                                  source: xr.Dataset) -> tuple[xr.Dataset, list[str]]:
    """Repair isolated source pixels before differencing running totals.

    Only accumulated fields with a missing target pixel and a valid reference
    meteorological pixel are considered. A missing source pixel is estimated
    from the surrounding source pixels at the same timestamp. This avoids
    temporal interpolation across ERA5-Land's daily accumulation reset.
    """
    reference_name = "t2m" if "t2m" in day_hours else "sp" if "sp" in day_hours else None
    if reference_name is None:
        return day_hours, []
    reference_valid = np.isfinite(day_hours[reference_name].values)
    repairs = []

    for name in LAND_ACCUMULATIONS.intersection(day_hours.data_vars):
        values = day_hours[name].values.copy()
        missing = ~np.isfinite(values) & reference_valid
        if not missing.any():
            continue
        # A wider gap should be investigated in the source, not silently filled.
        missing_hours_per_cell = missing.sum(axis=0)
        if np.any(missing_hours_per_cell > 2):
            lat_idx, lon_idx = np.argwhere(missing_hours_per_cell > 2)[0]
            hours = day_hours.time.values[missing[:, lat_idx, lon_idx]]
            lon = float(day_hours.longitude.values[lon_idx])
            lat = float(day_hours.latitude.values[lat_idx])
            raise ValueError(
                f"{name}: {len(hours)} missing accumulated hours at "
                f"{lon:g}E, {lat:g}N: {', '.join(map(str, hours))}. "
                "Inspect the source file; this gap exceeds the two-hour repair limit."
            )

        source_field = source[name]
        source_lon = source.longitude.values
        source_lat = source.latitude.values
        for time_idx, lat_idx, lon_idx in np.argwhere(missing):
            lon = float(day_hours.longitude.values[lon_idx])
            lat = float(day_hours.latitude.values[lat_idx])
            source_i = int(np.argmin(np.abs(source_lon - lon)))
            source_j = int(np.argmin(np.abs(source_lat - lat)))
            i0, i1 = max(0, source_i - 1), min(len(source_lon), source_i + 2)
            j0, j1 = max(0, source_j - 1), min(len(source_lat), source_j + 2)
            patch = source_field.sel(time=day_hours.time.values[time_idx]).isel(
                longitude=slice(i0, i1), latitude=slice(j0, j1)).values
            center = patch[source_j - j0, source_i - i0]
            if np.isfinite(center):
                estimate = float(center)
                method = "coincident source pixel"
            else:
                x = source_lon[i0:i1]
                y = source_lat[j0:j1]
                distance = np.hypot((x[None, :] - lon) * np.cos(np.deg2rad(lat)),
                                    y[:, None] - lat)
                valid = np.isfinite(patch) & (distance > 0)
                if np.count_nonzero(valid) < 3:
                    raise ValueError(f"{name}: insufficient source neighbors at "
                                     f"{lon} E, {lat} N, "
                                     f"{day_hours.time.values[time_idx]}")
                weights = 1.0 / distance[valid]
                estimate = float(np.average(patch[valid], weights=weights))
                method = "same-hour surrounding source pixels"
            values[time_idx, lat_idx, lon_idx] = estimate
            repairs.append(f"{name} {lon}E {lat}N "
                           f"{day_hours.time.values[time_idx]}: {method}")
        day_hours = day_hours.assign({name: day_hours[name].copy(data=values)})

    return day_hours, repairs


def copy_with_progress(src: str, dst: str, buffer_size: int = 16 * 1024 * 1024):
    """Copies large files with periodic progress updates."""
    total_size = os.path.getsize(src)
    copied = 0
    with open(src, "rb") as fsrc, open(dst, "wb") as fdst:
        while True:
            buf = fsrc.read(buffer_size)
            if not buf:
                break
            fdst.write(buf)
            copied += len(buf)
            pct = (copied / total_size) * 100
            print(f"\r    -> {copied / (1024*1024):.1f} / {total_size / (1024*1024):.1f} MB ({pct:.0f}%)", end="", flush=True)
    print()


# ==============================================================================
# 3. BUILD CLEAN SEPARATED TEMIR OUTPUT (NO DEEP COPY)
# ==============================================================================

def build_temir_dataset(raw: xr.Dataset) -> xr.Dataset:
    """
    Builds a brand new, clean output dataset containing the TEMIR / MERRA-2
    variables with standardized units and CF attributes.
    Includes surface pressure (sp) to directly supply P_surf in TEMIR.
    """
    out = xr.Dataset(coords={
        "time": raw["time"],
        "latitude": raw["latitude"],
        "longitude": raw["longitude"]
    })
    out = out.assign_coords(hour=("time", np.arange(1, raw.sizes["time"] + 1)))
    out["hour"].attrs = {"long_name": "UTC hour-ending interval within output day"}

    # 1. Total Cloud Fraction (CLDTOT) [0 - 1]
    if "tcc" in raw:
        out["CLDTOT"] = raw["tcc"].clip(0.0, 1.0)
        out["CLDTOT"].attrs = {"units": "1", "long_name": "Total Cloud Fraction"}

    # 2. Mean Sea-Level Pressure (SLP) [Pa]
    if "msl" in raw:
        out["SLP"] = raw["msl"]
        out["SLP"].attrs = {"units": "Pa", "long_name": "Mean Sea Level Pressure"}

    # 2b. Surface Pressure (sp) [Pa] - supplies P_surf in TEMIR
    if "sp" in raw:
        out["sp"] = raw["sp"]
        out["sp"].attrs = {"units": "Pa", "long_name": "Surface Pressure"}

    # 3. 2m Air Temperature (T2M) & 10m Air Temperature (T10M) [K]
    if "t2m" in raw:
        out["T2M"] = raw["t2m"]
        out["T2M"].attrs = {"units": "K", "long_name": "2-meter Air Temperature"}
        out["T10M"] = raw["t2m"].copy()
        out["T10M"].attrs = {"units": "K", "long_name": "10-meter Air Temperature (T2M approx)"}

    # 4. Wind Components (U10M, V10M) [m s-1]
    if "u10" in raw:
        out["U10M"] = raw["u10"]
        out["U10M"].attrs = {"units": "m s-1", "long_name": "10-meter Eastward Wind Component"}
    if "v10" in raw:
        out["V10M"] = raw["v10"]
        out["V10M"].attrs = {"units": "m s-1", "long_name": "10-meter Northward Wind Component"}

    # 5. Snow Depth (SNODP) [m]
    if "sde" in raw:
        out["SNODP"] = raw["sde"].clip(min=0.0)
        out["SNODP"].attrs = {"units": "m", "long_name": "Physical Snow Depth"}

    # 6. Radiation (SWGDN, PARDR, PARDF) [W m-2]
    if "ssrd" in raw:
        swgdn = (raw["ssrd"] / 3600.0).clip(min=0.0)
        out["SWGDN"] = swgdn
        out["SWGDN"].attrs = {"units": "W m-2", "long_name": "Surface Downward Solar Radiation"}

        par_total = 0.45 * swgdn
        if "fdir" in raw and "ssrd_single" in raw:
            # Both ERA5 single-level fields are one-hour amounts. Never divide
            # the hourly fdir by ERA5-Land's running-total ssrd.
            direct_frac = xr.where(
                raw["ssrd_single"] > 1.0,
                raw["fdir"] / raw["ssrd_single"],
                0.0,
            ).clip(0.0, 1.0)
            out["PARDR"] = (direct_frac * par_total).clip(min=0.0)
            out["PARDF"] = ((1.0 - direct_frac) * par_total).clip(min=0.0)
        else:
            # Explicit fallback when the ERA5 single-level source is absent.
            out["PARDR"] = 0.5 * par_total
            out["PARDF"] = 0.5 * par_total

        out["PARDR"].attrs = {"units": "W m-2", "long_name": "Direct-Beam PAR"}
        out["PARDF"].attrs = {"units": "W m-2", "long_name": "Diffuse PAR"}

    # 7. Turbulent Heat Fluxes (HFLUX, EFLUX) [W m-2, upward positive]
    if "sshf" in raw:
        out["HFLUX"] = -raw["sshf"] / 3600.0
        out["HFLUX"].attrs = {"units": "W m-2", "long_name": "Sensible Heat Flux (Upward Positive)"}
    if "slhf" in raw:
        out["EFLUX"] = -raw["slhf"] / 3600.0
        out["EFLUX"].attrs = {"units": "W m-2", "long_name": "Latent Heat Flux (Upward Positive)"}

    # 8. Precipitation & Snowfall Rates (PRECTOT, PRECSNO) [kg m-2 s-1]
    if "tp" in raw:
        out["PRECTOT"] = (1000.0 * raw["tp"] / 3600.0).clip(min=0.0)
        out["PRECTOT"].attrs = {"units": "kg m-2 s-1", "long_name": "Total Precipitation Rate"}
    if "sf" in raw:
        out["PRECSNO"] = (1000.0 * raw["sf"] / 3600.0).clip(min=0.0)
        out["PRECSNO"].attrs = {"units": "kg m-2 s-1", "long_name": "Snowfall Water-Equivalent Rate"}

    # 9. Surface Evaporation (EVAP) [kg m-2 s-1, upward positive]
    if "e" in raw:
        out["EVAP"] = -1000.0 * raw["e"] / 3600.0
        out["EVAP"].attrs = {"units": "kg m-2 s-1", "long_name": "Surface Evaporation"}

    # 10. Specific Humidity at 2m (QV2M) [kg kg-1]
    if "d2m" in raw and "sp" in raw:
        td_c = raw["d2m"] - 273.15
        e_vap = 611.2 * np.exp((17.67 * td_c) / (raw["d2m"] - 29.65))
        p_surf = raw["sp"]
        out["QV2M"] = ((0.622 * e_vap) / (p_surf - 0.378 * e_vap)).clip(min=1e-7)
        out["QV2M"].attrs = {"units": "kg kg-1", "long_name": "2-meter Specific Humidity"}

    # 11. Friction Velocity (USTAR) [m s-1]
    if "ewss" in raw and "nsss" in raw:
        tau_x = raw["ewss"] / 3600.0
        tau_y = raw["nsss"] / 3600.0
        tau = np.sqrt(tau_x**2 + tau_y**2)
        rho_air = raw["sp"] / (287.058 * raw["t2m"]) if ("sp" in raw and "t2m" in raw) else 1.225
        out["USTAR"] = np.sqrt(tau / rho_air).clip(min=1e-4)
        out["USTAR"].attrs = {"units": "m s-1", "long_name": "Surface Friction Velocity"}

    # 12. Soil Moisture Saturation (GWETTOP, GWETROOT) [0 - 1]
    if "swvl1" in raw:
        out["GWETTOP"] = (raw["swvl1"] / THETA_SAT_TOP).clip(0.0, 1.0)
        out["GWETTOP"].attrs = {"units": "1", "long_name": "Relative Soil Saturation in Upper Soil (0-7 cm)"}
        if "swvl2" in raw and "swvl3" in raw:
            theta_root = 0.07 * raw["swvl1"] + 0.21 * raw["swvl2"] + 0.72 * raw["swvl3"]
            out["GWETROOT"] = (theta_root / THETA_SAT_ROOT).clip(0.0, 1.0)
            out["GWETROOT"].attrs = {"units": "1", "long_name": "Relative Soil Saturation in Root Zone (0-100 cm)"}

    # 13. Soil Temperatures (TSOIL1 - TSOIL4) [K]
    soil_depths_cm = {1: "0-7", 2: "7-28", 3: "28-100", 4: "100-289"}
    for layer, depth_cm in soil_depths_cm.items():
        src_name = f"stl{layer}"
        if src_name in raw:
            target_name = f"TSOIL{layer}"
            out[target_name] = raw[src_name]
            out[target_name].attrs = {
                "units": "K",
                "long_name": f"Soil Temperature Layer {layer} ({depth_cm} cm)",
                "depth_range_cm": depth_cm,
            }

    # 14. Surface Roughness Length for Momentum (Z0M) [m]
    if "fsr" in raw:
        out["Z0M"] = raw["fsr"].clip(min=1e-5)
        out["Z0M"].attrs = {"units": "m", "long_name": "Surface Roughness Length for Momentum"}
    elif "sr" in raw:
        out["Z0M"] = raw["sr"].clip(min=1e-5)
        out["Z0M"].attrs = {"units": "m", "long_name": "Surface Roughness Length for Momentum"}

    return out


def report_missing_output_fields(ds_day: xr.Dataset) -> None:
    """Log unfilled gaps over cells with ERA5-Land coverage on this day.

    A static daily land-coverage mask avoids reporting the expected missing
    ocean values, while still detecting a full-day outage of one variable.
    """
    references = [name for name in ("T2M", "sp") if name in ds_day]
    if not references:
        print("  [Warning] Cannot assess output missingness: no T2M or sp reference")
        return
    covered = np.zeros((ds_day.sizes["latitude"], ds_day.sizes["longitude"]),
                       dtype=bool)
    for name in references:
        covered |= np.isfinite(ds_day[name].values).any(axis=0)

    found_missing = False
    for name, field in ds_day.data_vars.items():
        if set(field.dims) != {"time", "latitude", "longitude"}:
            continue
        values = field.transpose("time", "latitude", "longitude").values
        missing = ~np.isfinite(values) & covered[None, :, :]
        if not missing.any():
            continue
        found_missing = True
        hours_per_cell = missing.sum(axis=0)
        affected = int(np.count_nonzero(hours_per_cell))
        significant = np.argwhere(hours_per_cell >= SIGNIFICANT_MISSING_HOURS)
        print(f"  [Warning] {name}: {int(missing.sum())} missing hourly values "
              f"in {affected} covered grid cell(s); "
              f"{len(significant)} cell(s) have >= {SIGNIFICANT_MISSING_HOURS} missing hours")
        for lat_idx, lon_idx in significant[:MAX_REPORTED_CELLS_PER_VARIABLE]:
            lon = float(ds_day.longitude.values[lon_idx])
            lat = float(ds_day.latitude.values[lat_idx])
            hours = ds_day.time.values[missing[:, lat_idx, lon_idx]]
            print(f"    {name} at {lon:g}E, {lat:g}N: {len(hours)} missing hours; "
                  f"UTC hour endings: {', '.join(map(str, hours))}")
        if len(significant) > MAX_REPORTED_CELLS_PER_VARIABLE:
            print(f"    ... {len(significant) - MAX_REPORTED_CELLS_PER_VARIABLE} "
                  "additional cells omitted from console output")
    if not found_missing:
        print("  -> Missing-value check: no gaps in covered grid cells")


# ==============================================================================
# 4. MAIN STREAMING PROCESSING ENGINE
# ==============================================================================

def process_era5():
    local_scratch = os.path.join(tempfile.gettempdir(), "era5_scratch")
    local_output_base = os.path.join(tempfile.gettempdir(), "era5_output")
    os.makedirs(local_scratch, exist_ok=True)
    os.makedirs(local_output_base, exist_ok=True)

    print("================================================================")
    print("ERA5 / ERA5-Land to TEMIR Streaming Processing Engine")
    print(f"Base Directory   : {BASE_DIR}")
    print(f"Output Directory : {OUTPUT_BASE}/<YEAR>/<MM>/")
    print(f"Target Lat Range : [{LAT_MIN}, {LAT_MAX}], step={GRID_STEP}")
    print(f"Target Lon Range : [{LON_MIN}, {LON_MAX}], step={GRID_STEP}")
    print(f"Local Cache Mode : {'Enabled' if CACHE_INPUTS_LOCALLY else 'Disabled (Direct Drive Streaming)'}")
    print("================================================================")

    for year in YEARS:
        for month in MONTHS:
            month_str = f"{month:02d}"
            print(f"\n>>> Processing Period: Year {year}, Month {month_str} <<<")

            monthly_output_dir = os.path.join(OUTPUT_BASE, str(year), month_str)
            os.makedirs(monthly_output_dir, exist_ok=True)

            open_monthly_datasets = {}
            open_source_handles = []
            local_copied_files = []

            # ------------------------------------------------------------------
            # Step A: Open monthly files LAZILY (metadata only)
            # ------------------------------------------------------------------
            for folder in FOLDERS:
                matched_files = find_monthly_files(folder, year, month)
                if not matched_files:
                    print(f"  [Warning] No files found for {folder} in {year}-{month_str}. Skipping.")
                    continue

                folder_opened_ds = []
                for src_file in matched_files:
                    fname = os.path.basename(src_file)

                    if CACHE_INPUTS_LOCALLY:
                        local_file = os.path.join(local_scratch, fname)
                        if not os.path.exists(local_file) or os.path.getsize(local_file) != os.path.getsize(src_file):
                            print(f"  Caching {fname} -> local scratch ...")
                            copy_with_progress(src_file, local_file)
                        target_file = local_file
                        local_copied_files.append(local_file)
                    else:
                        target_file = src_file

                    ds = xr.open_dataset(target_file)
                    open_source_handles.append(ds)
                    ds = clean_and_standardize_dataset(ds)
                    folder_opened_ds.append(ds)

                if len(folder_opened_ds) == 1:
                    ds_folder = folder_opened_ds[0]
                else:
                    ds_folder = xr.merge(folder_opened_ds, compat="override")

                ds_folder = crop_dataset(ds_folder, LAT_MIN, LAT_MAX, LON_MIN, LON_MAX, buffer=1.0)
                open_monthly_datasets[folder] = ds_folder

            if not open_monthly_datasets:
                print(f"  [Notice] No datasets loaded for {year}-{month_str}. Skipping month.")
                continue

            # ------------------------------------------------------------------
            # Step B: Stream Day-by-Day (Slice 24 hrs -> Regrid -> Save -> Free)
            # ------------------------------------------------------------------
            num_days = calendar.monthrange(year, month)[1]

            for day in (DAYS if DAYS is not None else range(1, num_days + 1)):
                if not 1 <= day <= num_days:
                    raise ValueError(f"Invalid day {day} for {year}-{month:02d}")
                day_str = f"{day:02d}"
                date_str = f"{year}-{month_str}-{day_str}"
                next_day = date(year, month, day) + timedelta(days=1)

                final_output_file = os.path.join(
                    monthly_output_dir, f"ERA5_hourly_{year}{month_str}{day_str}.nc"
                )

                if os.path.exists(final_output_file):
                    print(f"  -> Day {date_str} already exists. Preserving it; skipping.")
                    continue

                # 1. Hours 1..23 end at this day's 01..23 UTC. Hour 24 ends
                #    at the following day's 00 UTC, possibly in next month.
                daily_regridded_list = []
                estimated_hour24 = False
                gap_repairs = []
                for folder_name, ds_folder in open_monthly_datasets.items():
                    hour_endings = np.datetime64(date_str) + np.arange(1, 24).astype("timedelta64[h]")
                    ds_day_raw = ds_folder.sel(time=hour_endings)
                    ds_day_regrid = regrid_daily_dataset(ds_day_raw, TARGET_LAT, TARGET_LON)

                    if folder_name == "ERA5_singlelayer" and "ssrd" in ds_day_regrid:
                        ds_day_regrid = ds_day_regrid.rename({"ssrd": "ssrd_single"})

                    if next_day.month == month:
                        midnight = ds_folder.sel(time=[np.datetime64(next_day.isoformat())])
                        midnight = regrid_daily_dataset(midnight, TARGET_LAT, TARGET_LON)
                        if folder_name == "ERA5_singlelayer" and "ssrd" in midnight:
                            midnight = midnight.rename({"ssrd": "ssrd_single"})
                    else:
                        midnight = next_midnight_slice(folder_name, next_day)

                    if midnight is None:
                        if date(year, month, day) != ESTIMATED_FINAL_HOUR_DATE:
                            raise FileNotFoundError(
                                f"Missing {folder_name} at {next_day} 00 UTC; "
                                "only the configured final hour may be estimated."
                            )
                        midnight = estimate_last_midnight(ds_day_regrid, folder_name, next_day)
                        estimated_hour24 = True

                    ds_day_24 = xr.concat([ds_day_regrid, midnight], dim="time")
                    if ds_day_24.sizes["time"] != 24:
                        raise ValueError(f"Expected 24 hours for {folder_name} on {date_str}")
                    if folder_name.startswith("ERA5_land"):
                        ds_day_24, repairs = repair_land_accumulation_gaps(
                            ds_day_24, ds_folder)
                        gap_repairs.extend(repairs)
                        ds_day_24 = deaccumulate_era5_land(ds_day_24)
                    daily_regridded_list.append(ds_day_24)

                # 2. Merge 24 hours across folders
                ds_day_merged = xr.merge(daily_regridded_list, compat="override")

                # 3. Build fresh TEMIR dataset (including sp)
                ds_day_out = build_temir_dataset(ds_day_merged)
                ds_day_out.attrs["hour_intervals_utc"] = (
                    f"{date_str} 00:00 through {next_day.isoformat()} 00:00; "
                    "hour=1..24, time is interval end"
                )
                ds_day_out.attrs["estimated_hour24"] = int(estimated_hour24)
                ds_day_out.attrs["accumulation_gap_repairs"] = (
                    "; ".join(gap_repairs) if gap_repairs else "none")
                for repair in gap_repairs:
                    print(f"  -> Repaired source gap: {repair}")
                if estimated_hour24:
                    ds_day_out.attrs["estimated_hour24_method"] = (
                        "Repeated hour 23 because next-year 00 UTC source is unavailable"
                    )

                report_missing_output_fields(ds_day_out)

                # 4. Save daily file with NetCDF4 compression
                encoding = {}
                if COMPRESS_OUTPUT:
                    for var_name in ds_day_out.data_vars:
                        encoding[var_name] = {"zlib": True, "complevel": COMPRESSION_LEVEL}

                local_out_file = os.path.join(
                    local_output_base, f"ERA5_hourly_{year}{month_str}{day_str}.nc"
                )
                ds_day_out.to_netcdf(
                    local_out_file,
                    format="NETCDF4",
                    engine="netcdf4",
                    encoding=encoding if COMPRESS_OUTPUT else None
                )

                # Publish only after the local NetCDF is complete. A stopped
                # copy leaves a .part file, not a seemingly valid daily file.
                staged_output_file = final_output_file + ".part"
                shutil.copy2(local_out_file, staged_output_file)
                os.replace(staged_output_file, final_output_file)

                try:
                    os.remove(local_out_file)
                except OSError:
                    pass

                n_steps = ds_day_out.sizes.get("time", 0)
                n_vars = len(ds_day_out.data_vars)
                print(f"  -> Generated: {date_str} ({n_steps} hrs, {n_vars} vars)")

                # 5. Clean up memory for this day immediately
                del ds_day_merged, ds_day_out, daily_regridded_list
                gc.collect()

            # ------------------------------------------------------------------
            # Step C: Close monthly file handles and cleanup
            # ------------------------------------------------------------------
            for ds in open_monthly_datasets.values():
                ds.close()
            for ds in open_source_handles:
                ds.close()

            for lf in local_copied_files:
                try:
                    os.remove(lf)
                except OSError:
                    pass

            del open_monthly_datasets, open_source_handles, local_copied_files
            gc.collect()
            print(f"Finished {year}-{month_str}. Cleaned.\n")

    print("=== Processing pipeline finished successfully! ===")


if __name__ == "__main__":
    process_era5()
