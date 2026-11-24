# Custom PFT parameter import

Initialization copies `user_custom_PFT.R` into each simulation directory, for
both prescribed-LAI and BGC cases. Set `import_new_PFT = TRUE` in the case's
`input_TEMIR.R` and edit that local copy. The input script prefers the local
file and falls back to the repository template for older cases without it.
You can also point `custom_PFT_config_file` to another script that defines
`custom_PFTs`. Import helpers stay in the shared code directory. Definitions
are loaded only when import is enabled.

The supplied example reads CLM5 rice (source number 61) as TEMIR PFT number 25,
at R array index 26. Existing PFT numbers 0–24 retain their values. `PFT_df`,
`pftname`, and `pftnum` are extended automatically. Append further definitions
with consecutive TEMIR numbers 26, 27, etc.; source numbers can differ.
Declare `crop_type = 'rice'` for each rice variety. The importer stores it in
`PFT_df$crop_type`; simulation flags derive from that identity, not PFT numbers.
Other accepted types are `none`, `maize`, `springwheat`, `winterwheat`, and
`soybean`. Omitting the field gives `none`; legacy crop names retain their type.

The importer reads common traits in both modes and additional crop traits when
`biogeochem_flag = TRUE`. It applies explicit overrides, validates all active
parameters, and only then returns the extended parameter list. Derived leaf
nitrogen and Vcmax are calculated afterward. The original fixed-index TEMIR
corrections apply to the original PFTs; imported rows use source values plus
their overrides. CLM5's Medlyn slope is mapped to `g1_med_table`, and its MMDD
planting windows are converted to day of year on a non-leap calendar.

Rice's missing `arootf` is explicitly set to zero in the example, following a
wheat-family allocation assumption that needs review before rice BGC work.
The two-dimensional CLM5 `rootprof_beta` requires an explicit variant selection;
TEMIR currently does not use this trait. Canopy height and ozone plant group
are TEMIR settings supplied in the example. There is no automatic replacement
of missing source values with traits from another crop.

For manual input, omit `source` and supply all required parameters in
`overrides`. Source imports can also override individual values. Inspect
`f_custom_PFT_schema(FALSE)` or `f_custom_PFT_schema(TRUE)` for the required
numeric and character parameter names. Unknown parameter names are rejected.

## Surface inputs and simulation status

Import adds zero coverage for every new PFT, preserving the original surface
layers. To select rice for a prescribed-LAI simulation:

1. Supply twelve nonnegative values each for `monthly_LAI` and `monthly_SAI` in
   the rice definition. These are spatially uniform monthly profiles, subject
   to the existing LAI rescaling when that option is enabled.
2. Use `PFT_frac_method = 'custom-map'` and assign rice's map layer to
   `ipft = 26` through `custom_PFT_frac_layer_to_ipft`. Coverage follows the
   existing fraction-map and surface-fraction options in `input_TEMIR.R`.
3. Set `sim_PFT = 25` (TEMIR number, not R index).

Surface and daily canopy arrays are extended to match the catalog. Custom
configurations use separate cache directories, keyed by the imported traits,
definitions, and BGC mode, so legacy caches cannot restore the old PFT count.

For a new BGC crop, explicitly set `initial_state = 'preplant'` and its
`crop_type` in the definition (already set for the rice example). This starts
with zero canopy/carbon and inactive crop flags. Begin on or before planting;
an active midseason crop or a custom-PFT restart is not initialized by this
option. The existing `emergence_carbon_to_leaf` setting supplies seed carbon
at emergence. If only imported crops are selected, no legacy CLM initial-state
file is needed. Mixed runs retain legacy initial maps and append zero states
for the imported crops. Parameter import and the crop process functions have
automated checks; the external-data single-site simulation is the next test.
Importing CLM5 traits does not implement CLM5 rice algorithms or paddy flooding.

## Rice JULES sources and optional flags

Use [Xu et al. (2025)](https://doi.org/10.5194/gmd-18-7257-2025) as the primary
source for rice JULES parameters. Where a value is absent, use the **rice**
defaults in [Osborne et al. (2015)](https://doi.org/10.5194/gmd-8-1139-2015).
Document the equation/table and units for each parameter, including sign or
unit conversions required by TEMIR. A value absent from both studies requires
an explicit documented assumption. Existing CLM5 traits are a separate source.

Rice arguments now sit immediately after the existing crop arguments:
`rice_flag = FALSE` in `f_crop_GDD_update()`, and `is_rice = FALSE` in the
phenology, allocation, retranslocation, carbon budget, and structure functions.
Named callers can omit them, including simulations without an imported rice
PFT. Positional callers must include the new rice slot before non-crop inputs.
For a rice call, pass `TRUE` explicitly. Passing an undefined caller variable
such as `rice_flag = rice_flag` still causes an R error: the caller must define
that variable even though the function has a default.

The rice GDD branch uses 8, 30, and 42 degrees Celsius for base, optimum, and
maximum development temperatures. Xu et al., Sect. 3.1.2, explicitly retains
these values from Osborne et al. The CLM5 `baset` trait remains unchanged.
Study-specific thermal-time
thresholds should not be applied globally without local phenology assessment.

The cited numerical profile is in `extension_crop/rice_JULES_parameters.R`:

| Group | Rice values | Source |
| --- | --- | --- |
| Daily development temperatures | Base 8, optimum 30, maximum 42 degrees Celsius | Xu Sect. 3.1.2, retained from Osborne |
| Allocation intercepts (root/stem/leaf) | 17.4 / 17.4 / 17.9 | Xu Table 3 |
| Allocation slopes (root/stem/leaf) | -20 / -16.7 / -18.5 | Xu Table 3 |
| Stem reserve fraction | 0.12 | Xu Table 3 |
| Senescence | DVI onset 1.25; mu 2; nu 6 | Xu Table 3 and Sect. 3.1.3 |
| Stem retranslocation | Allocation below 0.01; 10% of reserves per day | Xu Sect. 3.1.3; Osborne Eq. (7) |
| Optional custom SLA | gamma 24.5; delta -0.145; leaf carbon fraction 0.399 | Xu Table 3 |
| Optional custom height | kappa 1.27; lambda 0.24; stem carbon fraction 0.404 | Xu Table 3 |

TEMIR keeps the positive-intercept allocation formula `exp(a + b*DVI)`,
matching Osborne Eq. (6). Xu's displayed minus-alpha differs from that formula;
its Table 3 coefficients are mapped to the original convention explicitly.

Rice stage thresholds use the existing `'CLM4.5'` option, which also covers
imported CLM5 traits. The rice maturity rule is `min(GDD0_20yr, hybgdd)`, following
[CLM5 Eq. (2.26.2)](https://escomp.github.io/CTSM/release-clm5.0/tech_note/Crop_Irrigation/CLM50_Tech_Note_Crop_Irrigation.html).
`hybgdd = 2100` is the imported cap, not an unconditional maturity threshold.
Reproduction is `grnfill * GDDmat` and emergence is `lfemerg * GDDmat`; the
downloaded rice rows supply approximately 0.4 and 0.01. At the cap these give
GDDmat/GDDrepr/GDDemer of 2100/840/21. A smaller climate GDD0 lowers all three.
The current climate map supplies GDD0; no rice-specific threshold option is
needed. Prescribed site/map methods retain their supplied values; optional map
fallbacks set to `'CLM4.5'` use the same imported fractions.

Daily GDD accumulation remains a separate choice: `'CLM4.5'` uses imported
`baset` and `mxtmp`; `'JULES'` uses the rice cardinal temperatures above.
Allocation and retranslocation use their
`'JULES'` scheme options. Rice's DVI-dependent senescence is selected with
`crop_leaf_sen_scheme = 'custom'`. Custom SLA/height use the existing `'custom'`
options.

Xu's site-calibrated stage intervals can be supplied through the existing
prescribed-site/map options if needed, with a matching daily GDD definition.

Cold starts use TEMIR's seed-to-leaf establishment setting, rather than the
paper's `initial_carbon_io`/`initial_c_dvi_io` initialization. Economic yield
conversion is not wired yet. TEMIR's `grainC` represents the
harvest pool here, including retranslocated leaf carbon; it must not be assumed
to equal the paper's economic yield without applying its yield definition.

## Rice Sitch parameter reference

Xu Table 5 reports the following parameters for the Sitch photosynthesis factor
`F = max(0, 1 - a * max(O3_flux - O3_flux_th, 0))`:

| Rice profile | O3_flux_th (nmol m-2 s-1) | a |
| --- | --- | --- |
| Osborne defaults reported by Xu | 5 | 0.25 |
| Xu high sensitivity | 7 | 1.2 |
| Xu low sensitivity | 8 | 0.7 |

For dimensional consistency in this formula, `a` multiplies a flux and thus
has reciprocal-flux units. TEMIR's current C3-grass Sitch branch in
`code_v2.0/Farquhar_Ball_Berry.R` uses threshold 5 for both sensitivities,
with `a = 1.40` (high) and `a = 0.25` (low). The rice import still assigns
`plantgroup = 'C3_grass'`. The user-added `sensitivity = 'custom-rice'` branch
uses threshold 7.5 and `a = 0.95` (means of Xu's high and low parameter values).
That selection is separate from the generic high/low settings.

## Verification

For the first single-site BGC test:

- Initialize a new BGC case; edit the case-local `user_custom_PFT.R`.
- Set `import_new_PFT = TRUE`, `sim_PFT = 25`, and your single-site location.
- Assign a positive rice fraction through the custom map at `ipft = 26`.
  The default three-crop fraction map does not automatically add rice.
- Start before planting, use a known prescribed-site planting date, and choose
  either prescribed site thresholds or the existing `CLM4.5` threshold methods
  (the latter need the legacy GDD0 climate map). A phenology map must explicitly
  include the new PFT if prescribed-map methods are used.
- Use JULES allocation/retranslocation and `crop_leaf_sen_scheme = 'custom'`;
  enable custom SLA/height if you want those calibrated rice relations.
- Keep `drydep_scheme = NULL`: custom-PFT deposition mappings are not supplied.
- For a first growth check, leave ozone damage off. To test your mean Sitch
  coefficients afterward, choose `O3_damage_scheme = 'Sitch'` and
  `O3_sensitivity = 'custom-rice'` with the intended ozone forcing.
- The example is rainfed CLM5 rice 61. To model irrigated rice, select CLM5
  source 62 and its matching source name; water stress follows `irrigated`.

From the repository root, with `ncdf4` installed:

```r
source('tests/test_custom_PFT_import.R')
source('tests/test_rice_JULES.R')
```

Tests use a synthetic NetCDF with unordered PFT numbers and two root variants.
They also check the downloaded CLM5 file when it is present at the example
path. A full TEMIR simulation requires the external surface and forcing data.
