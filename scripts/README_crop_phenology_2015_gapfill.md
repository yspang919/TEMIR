# 2015 crop phenology: targeted gap-fill revision

The current review maps are in
`G:/My Drive/Angus_FYP/data/crop_phenology/phenology_map/`.
Files ending in `_2015_mean_days_gapfill` are this revision. The older
`_2015_mean_days` files remain for comparison.

## Why the original maps had gray cells

- **Rice 1 in Northeast China:** Its `TR` record is transplanting. The original
  draft inferred nursery emergence 37 days earlier, often before the modeled
  field soil exceeded the 8 °C rice base temperature. In the northeast
  (`lat >= 38°N`), affected middle/single rice cells now use `DOE = TR` as a
  provisional **field-establishment proxy**, with model planting seven days
  before TR. This is a field timing workaround, not a claim that biological
  emergence occurred at transplanting. It adds 265 usable cells.
- **Soybean:** All distribution cells were already assigned to a book region.
  The gray cells mostly had no effective soil heat on the early spring
  emergence day. For 89 southern cells that failed this check, this revision
  tests the book's central summer calendar (planting/DOE/DOF/DOH =
  167/174/212/270) as a timing alternative; all 89 pass the GDD checks.
  `crop_group = summer_calendar_climate_override` and
  `source_status = book_summer_calendar_test` identify them. This is a
  **calendar extrapolation**, not a changed geographic region boundary.
- **Maize:** Forty cells had SOS/EOS/RGS but no VGS. Their reproductive date is
  recovered from `EOS−RGS`. Of 133 distribution cells with no stage record,
  119 receive inverse-distance estimates from at least three direct stage
  records within 3°. Fourteen have insufficient nearby observations. Fifty-three
  of the 119 interpolated cells still fail the soil-heat check, and one fails
  GDD ordering; the net gain is 105 usable maize cells. The source CSV records
  `recovered_EOS_minus_RGS` or `nearby_stage_interpolation` per cell.

## Coverage

| Layer | Original usable | Revised usable | Change |
| --- | ---: | ---: | ---: |
| Maize | 1,698 / 2,002 | 1,803 / 2,002 | +105 |
| Spring wheat | 657 / 1,598 | 657 / 1,598 | 0 |
| Soybean | 1,036 / 1,149 | 1,125 / 1,149 | +89 |
| Rice 1 | 1,039 / 1,416 | 1,304 / 1,416 | +265 |
| Rice 2 | 638 / 1,416 | 638 / 1,416 | 0 |

`crop_phenology_2015_gapfill_coverage.png/.pdf` shows original usable, newly
usable, and still unavailable crop cells. Gray cells are listed with a `status`
in the revised per-crop CSV tables.

## Files

For each of `maize`, `spring_wheat`, `soybean`, `rice_1`, `rice_2`, the folder
contains both PNG and PDF versions of:

```text
<crop>_phenology_dates_2015_mean_days_gapfill
<crop>_phenology_gdd_2015_mean_days_gapfill
```

Date maps show planting, DOE, DOF, and DOH on a common DOY 1–365 scale. GDD
maps show GDDemer, GDDrepr, and GDDmat. GDDemer uses ERA5 TSOIL1; the later
thresholds use ERA5 T2M and TEMIR's JULES effective-temperature response.

The five revised CSV tables used to build the RDS are together in
`TEMIR_ready_input/mean_days_gapfill_2015_draft/`. The gap-fill summary JSON
is in `phenology_map/`. The six-layer test RDS is
`TEMIR_ready_input/custom_phenology_map_2015_mean_days_gapfill_draft.rds`.
Layer 3 (winter wheat) is empty pending cross-year ERA5; rice PFT process
parameters are also still pending.

This remains a provisional simulation input. In particular, regional soybean
dates and the rice field-establishment proxy require review against local
phenology observations. Tiny crop fractions in high, cold terrain can retain
implausibly low GDD totals even when the strict ordering checks pass. Such
cells should be screened before production simulations.

## Rebuild

From the TEMIR repository:

```powershell
& 'C:\Program Files\R\R-4.5.1\bin\Rscript.exe' --vanilla scripts/build_custom_phenology_map_2015_mean_days.R
```

The RDS builder reads only the five CSVs in
`TEMIR_ready_input/mean_days_gapfill_2015_draft/`. It verifies a staged RDS
before replacing the existing latest RDS. The earlier
`mean_days_2015_draft/` folder is an upstream intermediate, not an RDS
dependency. To recalculate the gap-filled CSVs from raw phenology and ERA5,
run `derive_crop_phenology_2015_mean_days.py` and then
`refine_crop_phenology_2015_gapfill.py` into new output directories.
