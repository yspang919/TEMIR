# Per-crop 2015 phenology maps

These figures are the per-crop view of the `mean_days_2015_draft` phenology
calculation. They are saved directly in `phenology_map` as PNG and PDF.

| Crop | Dates map | GDD map | Usable cells |
| --- | --- | --- | ---: |
| Maize | `maize_phenology_dates_2015_mean_days` | `maize_phenology_gdd_2015_mean_days` | 1,698 / 2,002 |
| Spring wheat | `spring_wheat_phenology_dates_2015_mean_days` | `spring_wheat_phenology_gdd_2015_mean_days` | 657 / 1,598 |
| Soybean | `soybean_phenology_dates_2015_mean_days` | `soybean_phenology_gdd_2015_mean_days` | 1,036 / 1,149 |
| Rice 1 (early or middle/single) | `rice_1_phenology_dates_2015_mean_days` | `rice_1_phenology_gdd_2015_mean_days` | 1,039 / 1,416 |
| Rice 2 (late) | `rice_2_phenology_dates_2015_mean_days` | `rice_2_phenology_gdd_2015_mean_days` | 638 / 1,416 |

Each dates map has planting/nursery sowing, emergence, flowering/heading proxy,
and maturity/harvest panels, all colored DOY 1–365. Each GDD map has GDDemer,
GDDrepr, and GDDmat panels with labeled °C d scales. Gray squares indicate
crop presence but no usable phenology estimate. The per-cell `status` column
in the source CSV gives the reason.

The companion maps `soybean_2015_mean_days_region_assignment` and
`rice_1_2015_mean_days_group_assignment` show the provisional group labels.
Winter wheat has no map because the cross-year 2015 weather and phenology
calculation is still pending.

Underlying CSVs and summary:
`G:/My Drive/Angus_FYP/data/crop_phenology/TEMIR_ready_input/mean_days_2015_draft`.
TEMIR-ready RDS:
`G:/My Drive/Angus_FYP/data/crop_phenology/TEMIR_ready_input/custom_phenology_map_2015_mean_days_draft.rds`.
The full method and its limitations are documented in
`phenology_map/mean_days_2015_draft/README.md`.

Regenerate from the TEMIR repository:

```powershell
python scripts/plot_crop_phenology_per_crop_2015.py --overwrite
```
