source('extension_crop/read_custom_crop_phenology.R')

fixture <- list(
    lon = c(100, 100.5), lat = c(30, 30.5),
    layer_info = data.frame(layer = 1:2, layer_name = c('maize', 'wheat')),
    pft_map = data.frame(pft_number = c(17L, 18L, 19L), layer = c(1L, 1L, 2L)),
    planting_date = array(c(90, NA, 91, 92, 100, 101, 102, 103), c(2, 2, 2)),
    GDDmat = array(c(1100, 1200, 1300, 1400, 800, 850, 900, 950), c(2, 2, 2))
)
path <- tempfile(fileext = '.rds')
saveRDS(fixture, path)
map <- f_read_custom_crop_phenology(path, lon = 100.5, lat = c(30, 30.5),
                                   required_fields = c('planting_date', 'GDDmat'))
stopifnot(identical(dim(map$GDDmat), c(1L, 2L, 2L)))
cell <- f_custom_crop_phenology_cell(map, 1L, 2L, 18L,
                                    required_fields = c('planting_date', 'GDDmat'))
stopifnot(identical(cell$planting_date, 92), identical(cell$GDDmat, 1400),
          is.na(cell$GDDrepr), is.na(cell$GDDemer))
missing_cell <- try(f_custom_crop_phenology_cell(
    map, 1L, 1L, 17L, required_fields = 'planting_date'), silent = TRUE)
stopifnot(inherits(missing_cell, 'try-error'))
unmapped <- try(f_custom_crop_phenology_cell(map, 1L, 1L, 25L), silent = TRUE)
stopifnot(inherits(unmapped, 'try-error'))
unlink(path)

source('extension_crop/plant_phenology.R')
source('extension_crop/rice_JULES_parameters.R')
leap <- FALSE
current_date <- '2015-04-10'
date.to.day <- function(x) 100L
GDD_calculation_method <- 'JULES'
crop_planting_date_method <- 'prescribed-map'
GDDmat_method <- GDDrepr_method <- GDDemer_method <- 'prescribed-map'
crop_leaf_sen_scheme <- 'CLM4.5'
emergence_carbon_to_leaf <- 1
custom_phenology_optional_fallback <- c(GDDrepr = 'CLM4.5', GDDemer = 'CLM4.5')
run_planting <- function(repr = NA_real_, emer = NA_real_,
                         t2m = 293, already_planted = FALSE,
                         planting_day = NA_real_, season_limit = 200,
                         initial_gdd = 0, location = NULL, rice = FALSE) {
    f_crop_phenology(
        T_10_d = 20, T_min_10_d = 10, T_soil = 290, T2m = t2m,
        is_maize = !rice, is_rice = rice, is_springwheat = FALSE, is_winterwheat = FALSE,
        is_soybean = FALSE, leafC = 0, livestemC = 0, livestemresvC = 0,
        finerootC = 0, grainC = 0, LAI = 0, SAI = 0,
        GDD_T2m = initial_gdd, GDD_Tsoil = 0,
        GDDmat = if (already_planted) 1100 else NA_real_,
        GDDrepr = if (already_planted) 600 else NA_real_,
        GDDemer = if (already_planted) 80 else NA_real_,
        crop_living_flag = already_planted, crop_planting_flag = already_planted,
        leaf_emer_flag = FALSE, grain_filling_flag = FALSE,
        harvesting_flag = FALSE, prescribed_planting_date_readin = 100,
        planting_jday = planting_day, grain_filling_jday = NA_real_,
        harvest_jday = NA_real_, day_since_crop_senescence = NA_real_,
        GDDmat_map_value = 1100, GDDrepr_map_value = repr,
        GDDemer_map_value = emer, hybgdd = 2000, GDD_baseT = 8,
        GDD_maxIncrease = 30, max_growing_season_length = season_limit,
        leaf_longevity = 1, T_plant_req = 10, T_min_plant_req = 0,
        repr_GDDfrac = 0.6, emer_GDDfrac = 0.04,
        prescribed_min_plant_jday = 1, prescribed_max_plant_jday = 365,
        at_NH_flag = TRUE, diagnostic_location = location)
}
mapped <- run_planting(repr = 600, emer = 80)
stopifnot(mapped$planting_julianday == 100, mapped$GDDmat == 1100,
          mapped$GDDrepr == 600, mapped$GDDemer == 80)
fallback <- run_planting()
stopifnot(fallback$GDDmat == 1100, fallback$GDDemer == 44,
          fallback$GDDrepr > fallback$GDDemer,
          fallback$GDDrepr < fallback$GDDmat)
custom_phenology_optional_fallback['GDDrepr'] <- 'error'
stopifnot(inherits(try(run_planting(), silent = TRUE), 'try-error'))
capture_warning <- function(expr) {
    warning_text <- character()
    value <- withCallingHandlers(expr, warning = function(w) {
        warning_text <<- c(warning_text, conditionMessage(w))
        invokeRestart('muffleWarning')
    })
    list(value = value, warnings = warning_text)
}
cold <- capture_warning(run_planting(
    repr = 600, emer = 80, t2m = 265.5,
    already_planted = TRUE, planting_day = 40,
    location = 'PFT 20 at 100.5 E, 35.0 N'))
stopifnot(length(cold$warnings) == 0L,
          is.na(cold$value$harvesting_julianday))
overlong <- capture_warning(run_planting(
    already_planted = TRUE, planting_day = 40, season_limit = 50))
stopifnot(length(overlong$warnings) == 1L,
          grepl('after 60 day\\(s\\) since planting', overlong$warnings),
          grepl('growing season exceeded', overlong$warnings),
          overlong$value$harvesting_julianday == 100)
mature <- capture_warning(run_planting(
    already_planted = TRUE, planting_day = 40, initial_gdd = 1100,
    t2m = 265.5))
stopifnot(length(mature$warnings) == 0L,
          mature$value$harvesting_julianday == 100)
# Rice is optional for named callers; positional calls include its crop slot.
# Expected increments follow Xu (2025), Sect. 3.1.2 (8/30/42 degC).
gdd = function(temperature, rice = FALSE, maize = FALSE, soybean = FALSE,
               spring = FALSE, winter = FALSE, alive = TRUE) {
    f_crop_GDD_update(maize, soybean, spring, winter, rice,
        100, 50, temperature + 273.15, temperature + 273.15, 8, 30, alive)
}
for (temperature in c(5, 8, 20, 30, 36, 42, 45)) {
    expected = c(0, 0, 12, 22, 11, 0, 0)[match(temperature, c(5, 8, 20, 30, 36, 42, 45))]
    rice_gdd = gdd(temperature, rice = TRUE)
    stopifnot(abs(rice_gdd$GDD_T_2m - 100 - expected) < 1e-10,
              abs(rice_gdd$GDD_T_soil - 50 - expected) < 1e-10)
}
stopifnot(gdd(30, rice = TRUE, alive = FALSE)$GDD_T_2m == 0,
          gdd(25, maize = TRUE)$GDD_T_2m == 117,
          gdd(25, soybean = TRUE)$GDD_T_2m == 115,
          gdd(25, spring = TRUE)$GDD_T_2m == 110,
          gdd(25, winter = TRUE)$GDD_T_2m == 110)
legacy_call = f_crop_GDD_update(maize_flag = TRUE, soybean_flag = FALSE,
    springwheat_flag = FALSE, winterwheat_flag = FALSE,
    GDD_T_2m_prev = 100, GDD_T_soil_prev = 50, today_T_soil = 298.15,
    today_T_2m = 298.15, GDD_base_T = 8, GDD_max_increase = 30, crop_live_flag = TRUE)
stopifnot(legacy_call$GDD_T_2m == 117)
rice_day = run_planting(repr = 600, emer = 80, already_planted = TRUE,
    planting_day = 90, initial_gdd = 100, rice = TRUE)
stopifnot(abs(rice_day$GDD_T2m - (100 + 293 - 273.15 - 8)) < 1e-10)
cat('Custom crop phenology maps, planting thresholds and optional rice GDD OK\n')
