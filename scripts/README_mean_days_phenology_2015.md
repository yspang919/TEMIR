# China crop phenology: fixed-day 2015 draft

This is a **new, provisional candidate**. It does not modify the earlier
`custom_phenology_map_2015_v2_draft.rds` made by backtracking a fixed GDDemer.
The source calendar means are from *中国农业物候* (Tables 2.6, 3.3, 4.2, 5.3;
`G:/My Drive/Angus_FYP/data/publications/中国农业物候.pdf`). Wheat, maize, and
rice means cover 1981–2010; soybean means cover 1992–2011. They are not 2015
grid-cell observations.

## Method

| Crop | Planting and stage dates |
| --- | --- |
| Maize | Planting = 2015 SOS − 14 days; SOS is the emergence proxy; SOS+VGS and EOS are reproductive and maturity proxies. The 14-day national interval is the station-weighted rounded mean of 15-day spring, 6-day summer, and 14-day relay maize. |
| Spring wheat | Planting = 2015 GR&EM − 20 days; GR&EM, heading, and maturity are emergence, reproductive, and harvest proxies. The 20-day interval is the rounded mean of the book's two spring-wheat rows (18.4 and 21.4 days). Reviewed winter-wheat cells remain empty. |
| Rice 1 | The 2015 first-season `TR` map is **transplanting**, not emergence. A provisional early/single classification assigns an early-rice sowing date `TR−33` and emergence `TR−27`, or a middle/single sowing `TR−44` and emergence `TR−37`. Heading and maturity remain 2015 observations/proxies. |
| Rice 2 | All cells use the late-rice offsets: sowing `TR−28`, emergence `TR−25`; heading and maturity remain 2015 observations/proxies. |
| Soybean | The 2015 distribution is assigned to the book's northern spring, central summer, and southern spring regions. Each region receives the book's mean sowing/emergence/flowering/maturity DOYs: 130/146/194/268, 167/174/212/270, or 88/96/139/184. |

Soybean's saved 0.5° fraction grid has centers at `.25/.75` degrees, whereas
ERA5 has `.0/.5` centers. Each source fraction is transferred to its four
overlapping ERA5 cells with one-quarter weight. The target keeps cells with
fraction at least 0.005, yielding 1,149 cells from 1,125 source cells and
retaining 96.1% of the source fraction sum. The soybean region map is an
**approximation** of book Fig. 5.1 using province polygons and explicit
latitude cuts in the Python script. `province`, `crop_group`, and
`group_reason` are preserved per cell for review. Sparse cells in or near
Qinghai/Xizang are extrapolated beyond the stations used for the southern
calendar.

Rice 1 is assigned early if its first harvest precedes a rice-2 transplant
record at the same cell, **or** if `TR<=125` and latitude `<=31°N`; other
cells are middle/single. This is a draft interpretation of combined source
layers, not an observed variety label. The early/middle map and per-cell
`group_reason` CSV should be reviewed before rice simulations.

For each usable cell, 2015 ERA5 daily-mean temperatures are passed through
TEMIR's JULES effective-temperature response:

```text
GDDemer = sum(TSOIL1 effective temperature, days (planting, emergence])
GDDrepr = sum(T2M effective temperature, days (planting, flowering/heading])
GDDmat  = sum(T2M effective temperature, days (planting, maturity])
```

The interval excludes planting day, matching TEMIR's present daily crop-GDD
update. A cell enters the RDS only if stages are within 2015, ERA5 is present,
`0 < GDDemer < GDDrepr < GDDmat`, and soil GDD increases on emergence day.
Otherwise its CSV keeps a status and its RDS numeric values are `NA`.

## Outputs

This section describes the earlier intermediate version. Its CSV input folder
and RDS were removed from `TEMIR_ready_input` after the gap-filled revision
became the model input. Run the derivation script again if that intermediate
version is needed for a new refinement.

- `TEMIR_ready_input/custom_phenology_map_2015_mean_days_draft.rds`: format 2
  with six `[lon,lat,layer]` arrays, the existing PFT map, source status,
  group assignment, and stage-date audit arrays.
- `TEMIR_ready_input/mean_days_2015_draft/`: five per-crop CSV tables and a
  JSON summary.
- `phenology_map/mean_days_2015_draft/`: planting, GDDemer, GDDrepr, GDDmat,
  DOE, DOF, and DOH review maps (PNG and PDF), plus soybean and rice-1 group
  maps. All calendar color scales run from DOY 1 to 365. A frozen copy of the
  build and plot scripts is in its `code/` subfolder; maintained scripts are
  in the TEMIR repository's `scripts/` folder.

| Layer | Usable cells | Main unresolved cells |
| --- | ---: | --- |
| Maize | 1,698 | 173 missing stage; 83 zero soil heat on DOE; 31 missing T2M; 16 phase conflicts |
| Spring wheat | 657 | 803 likely winter; 69 zero soil heat on DOE; 64 invalid stage order |
| Winter wheat | 0 | Cross-year ERA5 and winter phenology are not yet available |
| Soybean | 1,036 | 100 zero soil heat on DOE; 13 missing ERA temperature |
| Rice 1 | 1,039 | 294 zero soil heat on inferred DOE; 74 missing T2M |
| Rice 2 | 638 | 727 missing second-season stage records; 51 missing T2M |

The fixed-day method avoids very long planting-to-emergence intervals, but
raises median GDDemer to about 143 °C d for maize and 151 °C d for spring
wheat, versus the old fixed 80 and 35 °C d assumptions. This trade-off needs
phenological calibration before production simulations. A zero-heat DOE cannot
be reproduced exactly by a soil-GDD threshold, and rice nursery soil
temperature is unlikely to be represented faithfully by field ERA5 TSOIL1.

The book's final soybean stage is **maturity** (`成熟期`), not R6. Rice PFT
biophysical parameters and a nursery/transplanting process are still absent
from TEMIR, so the rice layers are timing drafts rather than runnable rice
simulation inputs.

## Rebuild

From the TEMIR repository:

```powershell
python scripts/derive_crop_phenology_2015_mean_days.py --build
python scripts/refine_crop_phenology_2015_gapfill.py --output-dir <new_gapfill_csv_dir>
& 'C:\Program Files\R\R-4.5.1\bin\Rscript.exe' --vanilla scripts/build_custom_phenology_map_2015_mean_days.R <root> <new_rds> <new_gapfill_csv_dir>
```

This is the full upstream workflow; its first step recreates the earlier
intermediate folder for the gap-fill calculation. For an RDS rebuild from the
retained latest CSVs, run only the R builder. It now defaults to the gap-filled
CSV folder and latest RDS, verifying a staged replacement before updating the
existing file.
