# China crop phenology maps, 2015

Run in PowerShell:

```powershell
python "G:\My Drive\Angus_FYP\data\crop_phenology\phenology_map\plot_crop_phenology_maps_2015.py"
```

To regenerate only the day-of-year maps, add `--maps DOE DOF DOH`.

The script reads the four CSV tables in `gdd_requirements_2015`, extracts the
existing soybean fraction table from `crop_distribution/soybean/soybean_distribution.RData`,
and writes six map sets in `season_labels_corrected_2015` below this folder.
Each set has a high-resolution PNG and a vector PDF:

| Map | Four panels |
| --- | --- |
| `china_2015_gddrepr` | GDD to the reproductive-stage proxy |
| `china_2015_gddmat` | GDD to maturity/harvest |
| `china_2015_doe_doy1-365` | Emergence/transplanting day of year |
| `china_2015_dof_doy1-365` | Reproductive-stage day of year |
| `china_2015_doh_doy1-365` | Maturity/harvest day of year |
| `china_2015_crop_fractions` | Maize, wheat, rice, soybean crop fractions |

The phenology panels show maize, spring wheat, late rice (LR), and combined
single/early rice (SR&ER) on a shared scale **within each map**. LR means
**late rice**, not long-season rice; SR&ER combines **single rice and early
rice**, not a single short-season class. Earlier top-level maps used the wrong
rice labels; use the figures in `season_labels_corrected_2015`.
DOE, DOF, and DOH all use the same
day-of-year color scale, 1–365, so their colors can be compared directly.
Only `status == "ok"` cells are
colored. Gray cells have a positive crop fraction but no usable estimate.
Earlier unsuffixed DOE/DOF/DOH PDFs may remain because another process kept
them open; use the `doy1-365` files inside `season_labels_corrected_2015`.
The crop-fraction map shows all positive-fraction cells, regardless of
phenology status. Soybean has no phenology map yet.

## Why some crop cells are gray

All five phenology maps color only rows with `status == "ok"`. Consequently,
a gray cell can still have observed DOE, DOF, and DOH values if its GDD
calculation or quality check failed. White land means no positive fraction
for that crop in the input layer. The 2015 status counts are:

| Crop | Colored | Gray: missing stage | Gray: other reasons |
| --- | ---: | ---: | --- |
| Maize | 1,696 | 173 | 101 conflicting phase formulas, 31 missing ERA5 T2M, 1 zero phase GDD |
| Spring wheat | 632 | 0 | 963 likely winter wheat, 2 zero phase GDD, 1 invalid stage order |
| Late rice (LR) | 638 | 727 | 51 missing ERA5 T2M |
| Single/early rice (SR&ER) | 1,334 | 0 | 74 missing ERA5 T2M, 8 invalid stage order |

For maize, a phase conflict means `|SOS + VGS - (EOS - RGS)| > 2 days`.
For wheat, "likely winter" is a timing heuristic: `GR&EM < DOY 100`,
`MA < DOY 121`, or `GR&EM > MA`; it is not a field-observed cultivar label.
For late rice, all 427 generic-rice-fraction cells at or north of 35°N
have no LR stage records. The generic rice fraction applies to both rice
tables, so gray late-rice cells in the north do not mean rice is absent.

## Sensitivity reviews

`maize_tolerance10_draft/` compares the original 2-day maize phase tolerance
with a 10-day tolerance. The trial GDD table is in the sibling folder
`gdd_requirements_2015_maize_tolerance10_draft`. It adds 85 valid cells
(1,696 to 1,781) while leaving existing valid GDD values unchanged. The
original GDD tables and corrected-label crop maps above remain the baseline.

`wheat_75_105_review/` contains a provisional wheat timing map and per-cell
CSV: GR&EM ≤75 is likely winter (836 cells), 76–104 is uncertain (134),
and ≥105 is likely spring (628). This is a sensitivity assessment, not an
observed cultivar classification or a replacement for the spring-wheat GDD
table.

The upstream draft GDD tables use daily ERA5 T2M and the TEMIR/JULES effective
temperature response. They assume `GDDemer = 80` for maize, `35` for spring
wheat, and `50 °C d` for rice. `GDDrepr = GDDemer + GDD(DOE, DOF]` and
`GDDmat = GDDrepr + GDD(DOF, DOH]`. These are **requirements inferred from 2015
stage observations**, not a forward simulation. The values are provisional
because planting dates are unknown.

Stage proxies: maize `SOS / SOS+VGS / EOS`, wheat `GR&EM / HE / MA`, and rice
`TR / HE / MA` for `DOE / DOF / DOH`. Rice transplanting stands in for
emergence, and heading stands in for the reproductive-stage marker. Wheat
heading is a proxy rather than a direct flowering observation. Wheat cells
classified as likely winter wheat are excluded from the spring wheat maps.
Long-season and short/early rice use the same rice crop-fraction layer.

The Natural Earth 1:110m country boundary files are copied into
`natural_earth_110m` for offline reruns. The soybean fractions are exported
once to `soybean_fraction_2015.csv` from the original RData table; the source
uses a 0.5° grid offset by 0.25° from the other crops and retains fractions
of at least 0.005. `map_summary.json` records the plotted cell counts and
color scales.
