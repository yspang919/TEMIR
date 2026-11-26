# Run from the repository root; no NetCDF or external meteorology required.
source('extension_crop/plant_phenology.R')

close = function(x, y) stopifnot(isTRUE(all.equal(x, y, tolerance = 1e-10)))
fails = function(expr, pattern) {
    error = tryCatch({force(expr); ''}, error = conditionMessage)
    stopifnot(nzchar(error), grepl(pattern, error, fixed = TRUE))
}
crop_planting_date_method = 'prescribed-site'
GDDmat_method = GDDrepr_method = GDDemer_method = 'CLM4.5'
GDD_calculation_method = 'JULES' # Winter uses Lu thermal time independently.
crop_leaf_sen_scheme = 'custom'
emergence_carbon_to_leaf = 1
leap = FALSE
args = list(T_10_d = 280, T_min_10_d = 275, T_soil = 278.15, T2m = 278.15,
    is_maize = FALSE, is_springwheat = FALSE, is_winterwheat = 1, is_soybean = FALSE,
    leafC = 20, livestemC = 10, livestemresvC = 5, finerootC = 8, grainC = 2,
    LAI = 1, SAI = .2, GDD_T2m = 0, GDD_Tsoil = 0,
    GDDmat = 0, GDDrepr = 0, GDDemer = 0,
    crop_living_flag = FALSE, crop_planting_flag = FALSE, leaf_emer_flag = FALSE,
    grain_filling_flag = FALSE, harvesting_flag = FALSE,
    prescribed_planting_date_readin = 274, planting_jday = NA_real_,
    grain_filling_jday = NA_real_, harvest_jday = NA_real_, day_since_crop_senescence = NA_real_,
    hybgdd = 9999, GDD_baseT = 10, GDD_maxIncrease = 30, max_growing_season_length = 20,
    leaf_longevity = 1, T_plant_req = 295, T_min_plant_req = 290,
    repr_GDDfrac = .6, emer_GDDfrac = .01,
    prescribed_min_plant_jday = 1, prescribed_max_plant_jday = 365, at_NH_flag = TRUE,
    T_min_daily = 274.15)
advance = function(date, previous = NULL, overrides = list()) {
    force(previous)
    current_date <<- date
    a = args
    if (!is.null(previous)) {
        mapping = c(GDD_T2m = 'GDD_T2m', GDD_Tsoil = 'GDD_Tsoil', GDDmat = 'GDDmat',
            GDDrepr = 'GDDrepr', GDDemer = 'GDDemer', crop_living_flag = 'croplive_flag',
            crop_planting_flag = 'cropplant_flag', leaf_emer_flag = 'leafemergence_flag',
            grain_filling_flag = 'grainfill_flag', harvesting_flag = 'har_flag',
            planting_jday = 'planting_julianday', grain_filling_jday = 'grain_filling_julianday',
            harvest_jday = 'harvesting_julianday', day_since_crop_senescence = 'day_since_crop_senescence',
            winter_state = 'winter_state')
        for (name in names(mapping)) a[[name]] = previous[[mapping[[name]]]]
    }
    a[names(overrides)] = overrides
    do.call(f_crop_phenology, a)
}

# Published cardinal temperatures and half-response point; crown is shallow soil.
# Out-of-range crown
# temperatures contribute no vernalization, never negative/complex responses.
p = f_winter_wheat_parameters()
close(f_winter_wheat_vernalization_rate(4.9), 1)
close(f_winter_wheat_vernalization_rate(-1.3), 0)
close(f_winter_wheat_vernalization_rate(15.7), 0)
close(f_winter_wheat_vernalization_rate(-15), 0)
close(f_winter_wheat_vernalization_rate(25), 0)
close(f_winter_wheat_crown_temperature(263.15), -10)
close(f_winter_wheat_crown_temperature(c(263.15, 273.15, 283.15)), c(-10, 0, 10))
fails(f_winter_wheat_crown_temperature(NA_real_), 'shallow soil')
fails(f_winter_wheat_parameters(list(vernalization_Topt = 30)), 'ranges')
fails(f_winter_wheat_parameters(list(unknown = 1)), 'named list')

# Planting has no GDD/seed input; winter-specific Table 4 defaults override
# the unrelated summer-PFT traits supplied above.
r = advance(20151001)
stopifnot(r$planted_today, r$croplive_flag, !r$emerged_today,
          r$GDD_T2m == 0, r$seedC_to_leafC_flux == 0)
close(c(r$GDDmat, r$GDDrepr, r$GDDemer), c(1700, 680, 51))
total_seed = 0
for (date in seq(as.Date('2015-10-02'), as.Date('2015-10-12'), by = 'day')) {
    r = advance(as.Date(date, origin = '1970-01-01'), r)
    total_seed = total_seed + r$seedC_to_leafC_flux*86400
}
close(total_seed, 1)
stopifnot(r$winter_state$seed_transferred, r$emerged_today, r$DVI == 0,
          r$vernalization_days == 0, r$GDD_T2m == 0)

# Warm days do not vernalize. The seed latch prevents a second emergence even
# when vernalization weighting moves adjusted GDD back below GDDemer.
warm = advance(20151013, r, list(T_soil = 298.15))
stopifnot(warm$vernalization_days == 0, warm$seedC_to_leafC_flux == 0,
          warm$DVI == 0, warm$leafemergence_flag)
for (day in 14:28) {
    warm = advance(as.integer(sprintf('201510%02d', day)), warm, list(T_soil = 278.05))
}
close(warm$vernalization_days, 15)
close(warm$GDD_T2m, warm$winter_state$GDD_T2m_raw * warm$vernalization_factor)
close(warm$vernalization_factor, 15^5/(22.5^5 + 15^5))
stopifnot(warm$seedC_to_leafC_flux == 0)
# Thermal increments are capped at 26, independently of the JULES option.
hot = advance(20151002, advance(20151001), list(T2m = 313.15, T_soil = 313.15))
close(hot$winter_state$GDD_T2m_raw, 26)
close(hot$winter_state$GDD_Tsoil_raw, 26)
# Once reproductive, VD must remain frozen even under optimum crown T.
reproductive = warm
reproductive$grainfill_flag = TRUE
reproductive$leafemergence_flag = FALSE
reproductive$winter_state$GDD_T2m_raw = 1000
reproductive$winter_state$vernalization_days = 40
after_reproduction = advance(20151029, reproductive, list(T_soil = 278.05))
close(after_reproduction$vernalization_days, 40)
stopifnot(after_reproduction$grainfill_flag, after_reproduction$DVI >= 1)

# Prescribed site/map thresholds retain priority; missing map fields use Lu
# fractions only when their existing optional fallback explicitly allows it.
GDDmat_method = GDDrepr_method = GDDemer_method = 'prescribed-site'
prescribed_GDDmat = 2000; prescribed_GDDrepr = 900; prescribed_GDDemer = 40
r_site = advance(20151001)
close(c(r_site$GDDmat, r_site$GDDrepr, r_site$GDDemer), c(2000, 900, 40))
GDDmat_method = GDDrepr_method = GDDemer_method = 'prescribed-map'
custom_phenology_optional_fallback = c(GDDrepr = 'CLM4.5', GDDemer = 'CLM4.5')
r_map = advance(20151001, overrides = list(GDDmat_map_value = 2000))
close(c(r_map$GDDmat, r_map$GDDrepr, r_map$GDDemer), c(2000, 800, 60))
custom_phenology_optional_fallback['GDDemer'] = 'error'
fails(advance(20151001, overrides = list(GDDmat_map_value = 2000)), 'Missing prescribed-map GDDemer')
GDDmat_method = GDDrepr_method = GDDemer_method = 'CLM4.5'

# Multi-month crop, crossing New Year and leap day, with serialized restart.
# Run this twice: full continuity, then save/load state on Dec 31.
run_season = function(restart = FALSE, soil = 278.05) {
    result = advance(20151001)
    events = c(planting = 1, emergence = 0, reproduction = 0)
    seed = 0; jan1 = NULL; feb29 = NULL
    for (date in seq(as.Date('2015-10-02'), as.Date('2016-06-30'), by = 'day')) {
        date = as.Date(date, origin = '1970-01-01')
        result = advance(date, result, list(T_soil = soil,
            T2m = if (date >= as.Date('2016-03-01')) 288.15 else 278.15))
        events = events + c(planting = result$planted_today, emergence = result$emerged_today,
                            reproduction = result$reproductive_today)
        seed = seed + result$seedC_to_leafC_flux*86400
        if (date == as.Date('2015-12-31') && restart) {
            file = tempfile(fileext = '.RData')
            winter_wheat_state_PFT_lasthd_ijd = list(NULL, result$winter_state)
            save(winter_wheat_state_PFT_lasthd_ijd, file = file)
            restored = new.env(); load(file, envir = restored); unlink(file)
            result$winter_state = restored$winter_wheat_state_PFT_lasthd_ijd[[2L]]
        }
        if (date == as.Date('2016-01-01')) jan1 = result
        if (date == as.Date('2016-02-29')) feb29 = result
        if (result$har_flag) break
    }
    list(result = result, events = events, seed = seed, jan1 = jan1, feb29 = feb29)
}
continuous = run_season()
restarted = run_season(TRUE)
stopifnot(identical(continuous, restarted), all(continuous$events == 1),
          continuous$seed == 1, continuous$jan1$croplive_flag,
          continuous$jan1$winter_state$GDD_T2m_raw == 460,
          continuous$feb29$croplive_flag,
          continuous$result$harvest_reason == 'maturity')
# Reproduction freezes VD/VF; cold exposure and C accounting persist in v2 state.
stopifnot(continuous$result$vernalization_factor < 1,
          continuous$result$DVI == 2, continuous$result$leafC_loss_flux*86400 == 20)

# The maximum-duration safeguard uses actual elapsed dates (366-day year),
# even without vernalization; forced harvest retains the existing flux contract.
short = advance(20151001, overrides = list(winter_parameters = list(max_growing_days = 1)))
short = advance(20151002, short, list(winter_parameters = list(max_growing_days = 1)))
warning_message = NULL
short = withCallingHandlers(advance(20151003, short,
    list(winter_parameters = list(max_growing_days = 1))), warning = function(w) {
        warning_message <<- conditionMessage(w); invokeRestart('muffleWarning')
    })
stopifnot(short$har_flag, short$harvest_reason == 'maximum_growing_days',
          grepl('after 2 days', warning_message), short$seedC_to_leafC_flux == 0)

# Exact five-day cooling trigger, winter climate requirement, NH/SH windows.
crop_planting_date_method = 'CLM4.5'
fails(advance(20151001), 'winter_parameters$GDD0_20yr')
auto = NULL
for (day in 1:5) {
    auto = advance(as.integer(sprintf('201510%02d', day)), auto,
        list(winter_parameters = list(GDD0_20yr = 1800)))
    stopifnot(auto$croplive_flag == (day == 5))
}
stopifnot(auto$planting_julianday == 278, auto$Tmin_5d == 274.15)
auto_SH = NULL
for (day in 1:5) auto_SH = advance(as.integer(sprintf('201604%02d', day)), auto_SH,
    list(at_NH_flag = FALSE, winter_parameters = list(GDD0_20yr = 1800)))
stopifnot(auto_SH$planted_today, auto_SH$planting_julianday == 96)
for (case in list(list(T_min_daily = 293.15),
                  list(winter_parameters = list(GDD0_20yr = 50)))) {
    settings = list(winter_parameters = list(GDD0_20yr = 1800))
    settings[names(case)] = case
    blocked = NULL
    for (day in 1:5) blocked = advance(as.integer(sprintf('201510%02d', day)), blocked, settings)
    stopifnot(!blocked$croplive_flag)
}

# Preserve per-PFT state isolation and permit a clean new season after harvest.
# A second PFT can advance without mutating the first PFT's state object.
crop_planting_date_method = 'prescribed-site'
first = advance(20151001)
second = advance(20151001, overrides = list(winter_parameters = list(GDDmat = 2000)))
second = advance(20151002, second)
stopifnot(first$winter_state$GDD_T2m_raw == 0, second$winter_state$GDD_T2m_raw == 5)
crop_planting_date_method = 'prescribed-site'
last = continuous$result
start_after_harvest = as.Date(last$winter_state$last_date) + 1
for (date in seq(start_after_harvest, as.Date('2016-09-30'), by = 'day')) {
    last = advance(as.Date(date, origin = '1970-01-01'), last,
                   list(prescribed_planting_date_readin = 275))
    stopifnot(!last$croplive_flag, last$seedC_to_leafC_flux == 0, !last$planted_today)
}
next_season = advance(20161001, last, list(prescribed_planting_date_readin = 275))
stopifnot(next_season$planted_today, next_season$vernalization_days == 0,
          !next_season$winter_state$seed_transferred, next_season$GDD_T2m == 0,
          !next_season$har_flag)

# Active old checkpoints and skipped dates must fail rather than silently
# rebuilding raw thermal time from adjusted GDD or erasing winter memory.
crop_planting_date_method = 'prescribed-site'
fails(advance(20151001, overrides = list(crop_living_flag = TRUE)), 'saved winter_state')
fails(advance(20151003, advance(20151001)), 'immediately preceding')

cat('Winter-wheat phenology: vernalization, stages, calendars, dispatch and restart OK\n')
