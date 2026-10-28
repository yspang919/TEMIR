# Draft China custom phenology input (2015)

The current TEMIR input is
`G:/My Drive/Angus_FYP/data/crop_phenology/TEMIR_ready_input/custom_phenology_map_2015_v2_draft.rds`.
It is one R list. Inspect it with `x <- readRDS(path); str(x, max.level = 1)`.
The `planting_date`, `GDDmat`, `GDDrepr`, and `GDDemer` arrays are indexed
`[lon, lat, layer]` on the 124 × 72 ERA5 grid. The six layer names and current
TEMIR PFT mapping are in `layer_info` and `pft_map`. Rainfed and irrigated
maize, spring wheat, winter wheat, and soybean use PFT numbers 17–24. Rice is
not mapped to a TEMIR PFT yet because its model parameters are still missing.

| Layer | Crop | GDDmat cells | Planting-date cells |
| ---: | --- | ---: | ---: |
| 1 | Maize | 1,781 | 1,557 |
| 2 | Spring wheat | 726 | 610 |
| 3 | Winter wheat | 0 | 0 |
| 4 | Soybean | 0 | 0 |
| 5 | `rice_1`, combined single/early | 1,334 | 1,263 |
| 6 | `rice_2`, late | 638 | 638 |

GDD thresholds come from the 2015 stage records and ERA5 T2M. Planting dates
are backtracked from the observed emergence proxy using daily ERA5 TSOIL1 and
TEMIR's JULES soil GDD response. The modeled soil GDD emergence crossing must
be within three days of the recorded date. Cells outside that tolerance retain
`NA` planting dates. The source CSV
`TEMIR_ready_input/planting_date_backtracked_2015_draft.csv` contains the
status and error in days for every accepted GDD cell. Rice transplanting is
treated as an emergence proxy; this is provisional. The GDD thresholds still
use the prior T2M calculation that assumes GDDemer at the recorded emergence
date, so backtracking soil GDD does not make the two temperatures identical.

To run a map driven crop simulation, set `crop_planting_date_method`,
`GDDmat_method`, `GDDrepr_method`, and `GDDemer_method` to `prescribed-map` in
the simulation copy of `input_TEMIR_crop_extension.R`. Initialization copies
`settings_custom_crop_phenology.R` into the simulation directory. That file
specifies the RDS path, the editable PFT-to-layer mapping, and CLM fallbacks
for absent `GDDrepr` and `GDDemer` arrays or cells. Set the mapping to `NULL`
only when the RDS itself contains the intended `pft_map`. The settings script is
sourced only when any of the four methods uses `prescribed-map`. The RDS is
aligned to the simulation lon/lat grid by coordinate matching. It does not
regrid. Selecting an unmapped PFT or a cell lacking a required map value stops
with a location specific error. These gaps mean the draft supports test runs
on selected cells, but not full coverage of every crop cell.

Rebuild in order:

```powershell
python scripts/backtrack_planting_dates_2015.py
& 'C:\Program Files\R\R-4.5.1\bin\Rscript.exe' --vanilla scripts/build_custom_phenology_map_2015.R
```

Both scripts refuse to overwrite existing outputs. Give the R script a new
output path as its second argument for another draft. The original v1 RDS is
retained separately and does not include planting dates.
