# Winter-wheat phenology, cold damage and carbon accounting

`plant_phenology.R` contains `f_crop_phenology()` (the dispatcher), the unchanged
summer workflow `f_summer_crop_phenology()`, and `f_winter_wheat_phenology()`
with its winter helpers. The dispatcher branches before summer GDD updates or
calendar resets. Existing winter crop flags identify the applicable PFTs.

The implementation follows [Lu et al. (2017), Sect. 2.3, Eqs. (1)-(12),
Tables 2-4](https://doi.org/10.5194/gmd-10-1873-2017), with the TEMIR adaptations
listed below. It is not a site-calibrated reproduction of the complete CLM
carbon/nitrogen/soil model.

## Temperature inputs

**Every winter-wheat crown temperature is shallow soil temperature.** The
former air-plus-snow crown reconstruction and separately overridable
`T_crown` argument have been removed.

- `T_soil` is the daily mean shallow soil temperature in K; the crown helper
  returns degrees C for vernalization and cold calculations.
- `T_soil_subdaily`, when supplied, contains equally spaced samples spanning
  the same model day in K. Its mean must match `T_soil`. The driver passes
  `TSOIL1[i,j,]` and its daily mean, including site replacements.
- Daily mean crown temperature drives the vernalization response. Subdaily
  samples drive hardening and damage. A direct caller without subdaily samples
  holds its daily mean constant for the cold calculation; this misses cold
  nights and is less suitable for frost damage.
- `T2m` still drives raw air GDD, while shallow soil drives emergence GDD.
- Use temperatures representative of crown depth, approximately 3-5 cm.
  A 0-7 or 0-10 cm layer average is a practical approximation, not an exact
  crown-depth measurement. The temperature product should already represent
  soil/snow heat transfer; never apply another snow-insulation correction.

Snow depth no longer determines crown temperature. Optional `snow_depth` in m
is used only for stress due to respiration under snow (`RATER`). It can be a
scalar or a vector aligned with the subdaily soil samples. `NULL` means
unavailable and disables **only RATER**, recorded by
`cold_snow_stress_available` / `winter_snow_stress_available`. Known zero snow
is distinct from unavailable snow. FLUXNET driving supplies unavailable snow
rather than inventing a measured zero. Other TEMIR meteorological/deposition
components retain their existing snow-input requirements; this does not make
`SNODP` optional in every meteorological reader.

## Seasonal timing

1. Restore the previous day's state independently for each PFT/grid cell.
   Living crops persist across January 1.
2. Update five daily minimum air temperatures, including today. Plant on a
   prescribed site/map date or use the winter cooling/climate trigger.
3. Initialize the new crop's raw GDD, VD and cold state at planting. Preserve
   litter and harvested-carbon accounting from earlier seasons.
4. Starting the day after planting, accumulate raw air/soil GDD with base
   0 C and maximum increment 26 degree days/day.
5. Trigger soil-driven emergence and the existing seed-to-leaf carbon input
   exactly once. Emergence is irreversible.
6. Starting the day after emergence, accumulate effective vernalization days
   from daily mean shallow soil temperature. Stop VD at the reproductive
   transition. Use `VF = VD^5 / (22.5^5 + VD^5)`.
7. After emergence, adjusted cumulative air GDD is raw cumulative air GDD
   multiplied by current VF. Do not compound adjustment of yesterday's GDD.
8. Update reproductive/maturity events. Harvest at maturity or when elapsed
   growing days exceed the maximum duration. Harvest takes priority over
   frost loss, allocation, and retranslocation.
9. For established vegetative crops, update cold tolerance and request frost
   leaf loss. Apply all C transfers once in the budget, update canopy
   structure, and save the final winter/cold/carbon state.

The legacy `CLM4.5` threshold options select Lu winter defaults: maturity
1700 degree days, emergence at 3%, reproduction at 40%, planting cooling
threshold 283.15 K and maximum duration 330 days. These modified temperature
and duration values were calibrated at US-ARM by Lu, not calibrated in TEMIR.
Prescribed site/map thresholds retain precedence. Missing optional map
thresholds use winter fractions only when the configured fallback allows it.
Thresholds must satisfy `0 < GDDemer < GDDrepr < GDDmat`.

Winter thermal time is independent of the summer `GDD_calculation_method`.
Before emergence, DVI uses soil GDD from -1 to 0. After emergence DVI is at
least zero, even when VF puts adjusted air GDD below the emergence threshold.
Reproduction is latched at DVI 1 and thermal maturity is DVI 2. A forced
harvest can occur below DVI 2. Lu's reproductive event is grain fill; TEMIR's
existing JULES convention labels DVI 1 flowering. This remains a shared model
transition rather than a separately calibrated anthesis prediction.

## Hardening, dehardening and frost damage

Cold processing applies during the established vegetative phase, not before
emergence or after reproduction. Available shallow-soil samples are split
into substeps of at most 30 minutes; rates use fractions of a day.

- `LT50` begins at `-0.6 + 0.142 * LT50_cold_limit`; the default cold limit
  is -23 C and the initial tolerance is -3.866 C.
- Hardening uses coefficient 0.0093 and `10 - max(Tcrown,0)` below 10 C,
  following Lu Eq. (5).
- Dehardening uses coefficient 2.7e-5, with a 10 C gate before vernalization
  completion and a -4 C gate after completion.
- Near-lethal freezing stress follows Lu Eq. (8). The exponential location
  was checked against the archived [Boas implementation of the Lu routines](https://doi.org/10.5281/zenodo.3978092).
  That research source has an inactive cold-tolerance call, so copying it is
  not considered validation of TEMIR's active cold module.
- Optional respiration-under-snow stress uses coefficient 0.54, with the snow
  response saturated at 12.5 cm.
- Survival is 1 at nonfreezing crown temperatures; freezing survival uses
  the Lu/Vico response with shape exponent 4.
- WDD integrates freezing temperature weighted by the non-survival fraction.
  The running survival average is time weighted over the exposure window.

**Early damage (`VF < 0.9`):** remove up to 5 gC/m2 per half hour times the
non-survival fraction, stopping at 10 gC/m2 leaf carbon. Scale the amount by
substep duration so a different input timestep does not multiply the dose.

**Later damage (`VF >= 0.9`):** when WDD exceeds 1 weighted degree day,
remove leaf carbon times one minus the window-mean survival. Reset WDD and
its survival-average window. This can reduce leaves below the early floor.
Losses never exceed available pre-allocation leaf carbon.

Numerical/interpretive TEMIR choices are explicit:

- VF approaches 1 asymptotically. Dehardening completion therefore uses a
  configurable default of 50 effective vernalization days, rather than the
  unreachable literal comparison `VF == 1`. This gate does not force VF to 1
  and is separate from the VF=0.9 damage switch.
- Project LT50 after each small update into [cold limit, unacclimated LT50].
  Bound the freezing-stress exponential against overflow. Treat freezing and
  under-snow terms as nonnegative stresses. These guards avoid nonphysical
  positive LT50, stress-induced hardening, and infinite rates at extremes.
- Use actual exposure averages without a fictitious initial survival sample.
- Cold processing occurs within the existing end-of-day vegetation update.
  Damage changes the canopy used the following day; it does not rerun that
  day's photosynthesis. Substeps resolve exposure, not subdaily canopy C.

## Allocation and carbon destinations

For `crop_C_partit_scheme = 'CLM4.5'`, winter wheat uses Lu Table 3's
reproductive decline anchored to the final vegetative leaf/stem coefficients,
not iterative multiplication of yesterday's coefficients. The initial
leaf/root fractions default to 0.6/0.3; final root allocation is zero, the
LAI decline factor is 1.05, and leaf/stem decline powers are 3/1. Existing
PFT final leaf/stem floors are retained. No maximum-LAI trigger or allocation
cap is applied to this winter path.

Grain allocation is multiplied by VF. To conserve assimilated C, the rejected
grain allocation is redistributed to leaf/stem in their current ratio
(or stem if neither has a sink). This destination is an explicit TEMIR
closure choice: Lu states the grain penalty but does not define this
redistribution. Fractions remain nonnegative and sum to one for emerged,
living crops. JULES allocation retains its existing wheat parameters and
DVI curve; no extra Lu grain multiplier is applied to that distinct scheme.

`f_Cpool_budgets()` dispatches winter crops to a dedicated accounting path:

- Seed input and allocated C enter the eight existing vegetation pools.
- Frost loss is separate from ordinary senescence and always enters litter.
  It is never passed into JULES leaf-to-grain retranslocation.
- Senescence is capped to available leaf C after frost. JULES retranslocation
  uses only the actual senescence loss; its reserve-to-grain transfer remains
  internal to the vegetation budget.
- At harvest, all remaining vegetation is removed once: grain enters the
  cumulative harvested export and other tissue enters litter. Forced harvest
  cannot retranslocate removed leaves back into a new grain pool.
- Litter and harvest ledgers persist across seasons. `frost_litter_C` is a
  cumulative subset of total litter, not another pool to add to total C.
- Each daily budget checks vegetation + litter + harvested C against the
  previous total plus allocation and seed input. Loss capping cannot create
  carbon through negative-pool clipping.

Litter here is a nondecomposing C accounting reservoir, not an implemented
soil biogeochemistry model. Nitrogen removal/routing, soil decomposition,
respiratory reserve drawdown during negative NPP, and winter-specific JULES
calibration remain outside this change. Allocation still uses TEMIR's
existing nonnegative-NPP convention. The balance diagnostic checks the
supplied vegetation fluxes, not a complete ecosystem C/N balance.

## Configuration, climate and checkpoints

Fresh winter-wheat runs start explicitly **preplant**: canopy and vegetation
carbon maps are initialized to zero for winter PFTs, rather than inheriting
active biomass from the legacy CLM initialization. When only winter wheat
and/or imported preplant crops are selected, no legacy initialization NetCDF
file is needed. In mixed runs, other PFTs retain their initialization maps.
Start before the intended planting date; midseason cold starts are not supported.

Optional overrides belong in `input_TEMIR_crop_extension.R`, for example:

```r
winter_wheat_parameters = list(
    max_growing_days = 330,
    vernalization_half_days = 22.5,
    vernalization_complete_days = 50,
    LT50_cold_limit = -23
)
```

Calculated planting (`CLM4.5`) requires five daily minima with mean below the
planting threshold, an eligible date, and explicitly supplied
`winter_wheat_parameters$GDD0_20yr > 50`. That scalar must represent Sep-Jun
(NH) or Apr-Sep (SH); the summer/annual GDD0 map is not silently substituted.
A spatial winter-climate reader and online 20-year calculation remain future
work. Use prescribed dates when winter climatology is unavailable.
Default calculated windows are after Sep 1 through Dec 31 (NH), or after
Apr 1 through Jun 30 (SH). Window ends are TEMIR safeguards. Override the
`planting_start_NH`, `planting_end_NH`, `planting_start_SH`, `planting_end_SH`
MMDD strings if needed. Prescribed dates bypass those windows.

The checkpoint schema is now **version 2**. It stores dates, raw GDD, VD,
the emergence latch and five-day history, LT50, WDD, time-integrated survival
and its duration, and `carbon = list(litter_C, harvested_C, frost_litter_C)`.
The driver saves final states independently per PFT/grid cell under
`winter_wheat_state_PFT_lasthd_ijd`. Ledgers survive replanting. Dates must
advance by one day; elapsed duration includes leap days/year boundaries.
Old version-1 states are rejected rather than inventing cold histories or
mixing prior air/snow crown calculations with the soil-based scheme. Start
this version before planting. One planting per planting year is supported.

## Diagnostics and validation

The following optional daily history variables are registered in the input
template; add them to a case's `output_variables` as needed:

`winter_Tcrown`, `winter_LT50`, `winter_VD`, `winter_VF`, `winter_WDD`,
`winter_survival`, `winter_snow_stress_available`, `leafC_frost_to_litter`,
`winter_litter_C`, `winter_harvested_C`, `winter_C_balance_error`.

Crown/LT50 are degC, frost flux is gC/m2/s, C ledgers/residual are gC/m2.
`winter_WDD` is the remaining window after any damage resets, not lifetime
exposure. Survival is an indicator, not an independent daily mortality
probability to multiply across days.

Run `tests/test_winter_wheat_phenology.R` and
`tests/test_winter_wheat_cold_budget.R`. They check response curves, planting
and stage timing, soil-only crown inputs, analytical hardening/dehardening,
subdaily damage and dose scaling, snow-stress handling, separate C
transfers, harvest precedence, conservation, and serialized coupled restart.
Site validation and frost/phenology calibration remain required.

## Suggested first single-site test

Use the updated crop input template with PFT 21 (rainfed winter wheat) or 22
(irrigated winter wheat), positive PFT coverage, and a fresh run starting
before autumn planting. For an initial CLM-style test, set these options in
the case's `input_TEMIR_crop_extension.R`:

```r
crop_planting_date_method = 'prescribed-site'
prescribed_planting_date = 274  # Oct 1 in a non-leap year; 275 in a leap year
GDDmat_method = GDDrepr_method = GDDemer_method = 'CLM4.5'
crop_C_partit_scheme = 'CLM4.5'
crop_transloc_scheme = 'CLM4.5'
crop_leaf_sen_scheme = 'CLM4.5'
LAI_scheme = SAI_scheme = canopy_h_scheme = 'CLM4.5'
```

Keep these choices at the corresponding existing settings in the template;
setting a prescribed date after a `prescribed-map` settings block has already
run does not undo that earlier map loading. Prescribed planting avoids the
additional winter-climatology input for this first test. Supply complete,
finite shallow soil temperatures in K. The existing custom canopy-structure
branches lack wheat parameters, and the custom wheat senescence rate is zero;
use the CLM4.5 structural and senescence settings above for this pre-test.

Add the winter diagnostic variables listed above to the output selection.
Check that raw GDD, VD/VF and DVI progress consistently, frost losses equal
their litter inputs, the carbon residual stays near zero, and the crop and
litter state persist through January 1. The litter reservoir intentionally
remains available for a future soil C-N model.

`tests/test_winter_wheat_simulation_wiring.R` executes the actual daily
vegetation, history and checkpoint blocks with synthetic weather/GPP for
both allocation schemes, including a cold-spell restart and replanting.
It also checks winter-only and mixed-PFT preplant initialization. It does
not replace validation of the full meteorology/photosynthesis/site workflow.
