# Custom 2015 crop fractions for TEMIR

`china2015_staplecrops_distribution.rds` is a crop-presence and area
weight map on the same 124 × 72 grid as the 2015 phenology RDS (longitude
73.5–135°E, latitude 18–53.5°N, both at 0.5° spacing).

The RDS has three `[lon, lat, layer]` fraction layers: maize, spring wheat,
and soybean. It uses `crop_fraction` from the gap-filled 2015 phenology CSVs,
but sets a fraction to zero where phenology `status != "ok"`. Thus the active
cell counts are 1,803 maize, 657 spring wheat, and 1,125 soybean. Winter
wheat is not assigned to the spring-wheat PFTs.

The example mapping in `code_v2.0/input_TEMIR.R` copies each crop fraction to
both rainfed and irrigated PFTs. `ipft` is the 1-based index into TEMIR's
`PFT_frac` array; `pft_number = ipft - 1` for the current CLM PFT list:

| Layer | `ipft` | PFT numbers |
| --- | --- | --- |
| Maize | 18, 19 | 17, 18 |
| Spring wheat | 20, 21 | 19, 20 |
| Soybean | 24, 25 | 23, 24 |

The RDS also contains a `pft_map` for provenance. TEMIR uses the mapping in
`input_TEMIR.R`; changing the input mapping can assign a layer to any existing
PFT, including trees. A general-purpose RDS may instead call its array
`custom_PFT_frac` and needs only that array plus `lon` and `lat`.

These paired fractions are alternatives for per-area simulations. They are
**not** two distinct land areas, and multiple crops can share a cell in
different seasons. Do not sum these PFT fractions to estimate cultivated
area or total yield. Calculate total production later with a separately
chosen crop area and rainfed/irrigated split.

To use the map, set `PFT_frac_method = 'custom-map'` in
`code_v2.0/input_TEMIR.R` (or the initialized simulation's copy). The
configured path points to this RDS. TEMIR loads/regrids the CLM surface data
first, then replaces only the mapped PFT fraction layers. The example sets
`custom_PFT_frac_surface_method = 'CLM'` because its paired crop fractions
are simulation masks. For a full land-cover replacement, set this to
`'custom-map'` so the replacement fractions also enter aerodynamic geometry
and user-defined total-LAI scaling. Current `LAI_data_flag = FALSE` does not
invoke the latter scaling.

Every mapped PFT cell with a positive fraction passes the fraction gate, with
or without biogeochemistry. Zero fraction cells are skipped. Unmapped PFTs
retain TEMIR's existing minimum-fraction gate.
When biogeochemistry is disabled, TEMIR also requires nonzero LAI to simulate
a PFT; supplying a fraction alone does not create LAI or missing PFT parameters.

The full model `lon`/`lat` vectors must match the custom meteorology and be
contained in this crop map. Changing only `lon_sim`/`lat_sim` does not regrid
the global model to 0.5°. A China-grid run will generate its own grid-specific
CLM surface cache; the existing 144 × 91 global cache is not reused.

Build from the checked phenology CSVs:

```powershell
& 'C:\Program Files\R\R-4.5.1\bin\Rscript.exe' --vanilla scripts/build_custom_crop_fraction_2015.R
```

The builder refuses to overwrite an existing RDS. The regression check is
`tests/test_custom_PFT_frac_map.R`.
