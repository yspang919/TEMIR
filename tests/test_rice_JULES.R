# Run from the repository root. No external simulation data are required.
source('extension_crop/rice_JULES_parameters.R')
source('extension_crop/plant_phenology.R')
source('extension_crop/biomass_partitioning.R')
source('extension_crop/crop_retranslocation.R')
source('extension_crop/Cpool_budget.R')
source('extension_crop/plant_physiology.R')

rice = f_rice_JULES_parameters()
flags = list(is_maize = FALSE, is_springwheat = FALSE,
             is_winterwheat = FALSE, is_soybean = FALSE, is_rice = TRUE)
close = function(actual, expected) stopifnot(isTRUE(all.equal(actual, expected, tolerance = 1e-10)))
allocate = function(dvi, living = TRUE) do.call(f_get_JULES_crop_allocation_coefficients,
    c(flags, list(DVI_crop = dvi, croplive_flag = living)))
early = unlist(allocate(0)); late = unlist(allocate(1.9))
close(sum(early), 1); close(sum(late), 1)
close(unname(early['p_leaf'] / early['p_root']), exp(.5))
stopifnot(all(early >= 0), all(late >= 0), late['p_harv'] > early['p_harv'],
          late['p_root'] < early['p_root'], all(unlist(allocate(-.5)) == 0),
          all(unlist(allocate(1, living = FALSE)) == 0))
# Existing wheat/maize/soybean coefficients and no-rice defaults remain intact.
for (type in c('is_maize', 'is_soybean', 'is_springwheat', 'is_winterwheat')) {
    legacy = flags[names(flags) != 'is_rice']; legacy[[type]] = TRUE
    result = do.call(f_get_JULES_crop_allocation_coefficients,
        c(legacy, list(DVI_crop = 1, croplive_flag = TRUE)))
    intercept = switch(type, is_maize = c(13.5, 12.1, 13.1),
        is_soybean = c(19.8, 18.5, 19.2), c(18.5, 16, 18))
    slope = switch(type, is_maize = c(-15, -12.1, -14.1),
        is_soybean = c(-15.4, -13.2, -14.3), c(-20, -15, -18.5))
    expected = c(exp(intercept + slope), 1)
    close(unname(unlist(result[c('p_root', 'p_stem', 'p_leaf', 'p_harv')])), expected / sum(expected))
}

crop_C_partit_scheme = 'JULES'
partition = do.call(f_crop_allocation_fluxes, c(flags, list(
    A_can_umolm2s1 = 20, mr_total = 0, gr_fraction = .25,
    GDDmat = 2100, GDD_T2m = 840, GDD_Tsoil = 840, GDDemer = 21, GDDrepr = 840,
    DVI_JULES = 1, crop_living_flag = TRUE, peak_lai_flag = FALSE, grain_filling_flag = TRUE,
    astem_leafem = 0, aleaf_leafem = 0, aleaf = 0, astem = 0,
    bfact = .1, arooti = .1, arootf = 0, astemf = .05, declfact = 1.05,
    allconss = 1, aleaff = 0, allconsl = 3, lfemerg = .01, fleafi = .75)))
fluxes = unlist(partition[c('leaf_carbon_partitioning_flux_gCm2s1',
    'fineroot_carbon_partitioning_flux_gCm2s1', 'grain_carbon_partitioning_flux_gCm2s1',
    'livestem_carbon_partitioning_flux_gCm2s1', 'livestem_resv_carbon_partitioning_flux_gCm2s1')])
close(sum(fluxes), partition$daily_mean_NPP)
close(partition$livestem_resv_carbon_partitioning_flux_gCm2s1 /
    (partition$livestem_resv_carbon_partitioning_flux_gCm2s1 +
     partition$livestem_carbon_partitioning_flux_gCm2s1), .12)

retranslocate = function(dvi, astem = .009) do.call(f_crop_retranslocation_JULES,
    c(flags, list(grainC = 5, leafC = 100, leafC_senesc_flux = 2/86400,
        livestem_resvC = 10, astem = astem, DVI = dvi)))
transfer = retranslocate(1.5)
close(transfer$grain_C, 8); close(transfer$livestem_resv_C, 9)
close(transfer$grain_C + transfer$livestem_resv_C - 15, 2)
stopifnot(is.na(retranslocate(1.2)$leaf2grain_flx),
          is.na(retranslocate(1.5, astem = .01)$stem2grain_flx))
close(retranslocate(1.25, astem = .02)$leaf2grain_flx, 2)

leap = FALSE; current_date = '2015-04-10'; date.to.day = function(x) 100L
GDD_calculation_method = 'JULES'
crop_planting_date_method = 'prescribed-site'
GDDmat_method = GDDrepr_method = GDDemer_method = 'CLM4.5'
crop_leaf_sen_scheme = 'custom'; emergence_carbon_to_leaf = 1
phenology = function(gdd = 0, planted = FALSE, leaf = 100, climate_gdd = 2400,
                     repr_map = NA_real_, emer_map = NA_real_, emerged = TRUE) {
    do.call(f_crop_phenology, c(flags, list(
        T_10_d = 20, T_min_10_d = 10, T_soil = 281.15, T2m = 281.15,
        leafC = leaf, livestemC = if (leaf == 0) 0 else 20,
        livestemresvC = if (leaf == 0) 0 else 10, finerootC = if (leaf == 0) 0 else 10,
        grainC = if (leaf == 0) 0 else 5,
        LAI = 3, SAI = .5, GDD_T2m = gdd, GDD_Tsoil = gdd,
        GDDmat = 2100, GDDrepr = 840, GDDemer = 21, GDD0_20yr = climate_gdd,
        GDDrepr_map_value = repr_map, GDDemer_map_value = emer_map,
        crop_living_flag = planted, crop_planting_flag = planted,
        leaf_emer_flag = emerged, grain_filling_flag = FALSE, harvesting_flag = FALSE,
        prescribed_planting_date_readin = 100, planting_jday = if (planted) 90 else NA,
        grain_filling_jday = NA, harvest_jday = NA, day_since_crop_senescence = NA,
        hybgdd = 2100, GDD_baseT = 10, GDD_maxIncrease = 30,
        max_growing_season_length = 200, leaf_longevity = 1,
        T_plant_req = 294.15, T_min_plant_req = 283.15, repr_GDDfrac = .4,
        emer_GDDfrac = .01, prescribed_min_plant_jday = 1,
        prescribed_max_plant_jday = 365, at_NH_flag = TRUE)))
}
planted = phenology()
close(c(planted$GDDemer, planted$GDDrepr, planted$GDDmat), c(21, 840, 2100))
# hybgdd is a cap, not an unconditional maturity requirement.
uncapped = phenology(climate_gdd = 1800)
close(c(uncapped$GDDemer, uncapped$GDDrepr, uncapped$GDDmat), c(18, 720, 1800))
missing_climate = tryCatch({phenology(climate_gdd = NULL); NA_character_}, error = conditionMessage)
stopifnot(!is.na(missing_climate), grepl('GDD0_20yr', missing_climate, fixed = TRUE))
for (stage in c(0, 1, 1.25, 1.5, 1.9)) {
    state = phenology(21 + if (stage < 1) 819*stage else 819 + 1260*(stage-1), planted = TRUE)
    close(state$DVI, stage)
    expected = if (stage <= 1.25) 0 else 100 * min(1, 2*(stage-1.25)^6) / 86400
    close(state$leafC_loss_flux, expected)
    stopifnot(state$leafC_loss_flux * 86400 <= 100)
}
# Missing optional map values use the same imported CLM fractions.
GDDrepr_method = GDDemer_method = 'prescribed-map'
custom_phenology_optional_fallback = c(GDDrepr = 'CLM4.5', GDDemer = 'CLM4.5')
state = phenology(climate_gdd = 1800)
close(c(state$GDDemer, state$GDDrepr, state$GDDmat), c(18, 720, 1800))
# Explicit map and site thresholds take priority over those fractions.
state = phenology(climate_gdd = 1800, repr_map = 700, emer_map = 20)
close(c(state$GDDemer, state$GDDrepr, state$GDDmat), c(20, 700, 1800))
GDDmat_method = GDDrepr_method = GDDemer_method = 'prescribed-site'
prescribed_GDDmat = 1800; prescribed_GDDrepr = 1000; prescribed_GDDemer = 40
state = phenology()
close(c(state$GDDemer, state$GDDrepr, state$GDDmat), c(40, 1000, 1800))
cold = phenology(leaf = 0, emerged = FALSE)
stopifnot(cold$croplive_flag, cold$leafC == 0, cold$seedC_to_leafC_flux == 0)
emergent = phenology(gdd = 40, planted = TRUE, leaf = 0, emerged = FALSE)
close(emergent$seedC_to_leafC_flux * 86400, emergence_carbon_to_leaf)
stopifnot(emergent$leafemergence_flag,
    phenology(gdd = 50, planted = TRUE, leaf = 1, emerged = TRUE)$seedC_to_leafC_flux == 0)

# Verify that leaf loss and stem transfer conserve carbon through the budget.
crop_transloc_scheme = 'JULES'
budget_args = list(is_crop = TRUE, is_evergreen = FALSE, is_stress_decid = FALSE,
    is_season_decid = FALSE, grain_filling_flag = TRUE, harvesting_flag = FALSE,
    DVI = 1.5, astem = .009, seedC_to_leafC_flux_gCm2s1 = 0)
for (pool in c('leaf', 'livestem', 'livestem_resv', 'deadstem', 'livecoarseroot',
               'deadcoarseroot', 'fineroot', 'grain')) {
    budget_args[[paste0(pool, '_C_prev')]] = switch(pool, leaf = 100, livestem_resv = 10, grain = 5, 0)
    budget_args[[paste0(pool, '_C_biomass_partitioning_flux_gCm2s1')]] = 0
    budget_args[[paste0(pool, '_C_loss_flux_gCm2s1')]] = if (pool == 'leaf') 2/86400 else 0
}
budget = do.call(f_Cpool_budgets, c(flags, budget_args))
close(budget$leaf_C_new, 98); close(budget$grain_C_new, 8)
close(budget$livestem_resv_C_new, 9)
close(sum(unlist(budget[c('leaf_C_new', 'grain_C_new', 'livestem_resv_C_new')])), 115)
# Propagate preplant emergence into the budget: seed creates the first leaf
# carbon exactly once, allowing photosynthesis to start on the following day.
seed_args = budget_args
for (name in grep('_C_prev$|_C_loss_flux_gCm2s1$', names(seed_args), value = TRUE)) seed_args[[name]] = 0
seed_args$DVI = 0; seed_args$astem = .5
seed_args$seedC_to_leafC_flux_gCm2s1 = emergent$seedC_to_leafC_flux
seed_budget = do.call(f_Cpool_budgets, c(flags, seed_args))
close(seed_budget$leaf_C_new, emergence_carbon_to_leaf)
close(seed_budget$grain_C_new, 0)

LAI_scheme = 'custom'; SAI_scheme = 'CLM4.5'; canopy_h_scheme = 'custom'; dt = 3600
structure = f_vegetation_structure(is_crop = TRUE, is_shrub = FALSE, is_evergreen = FALSE,
    is_stress_decid = FALSE, is_season_decid = FALSE, is_woody = FALSE,
    is_managed_crop = TRUE, is_maize = FALSE, is_soybean = FALSE, is_rice = TRUE,
    slatop = .035, dsladlai = 0, laimx = 7, ztopmx = 1.8, leafC = 100,
    livestemC = 200, deadstemC = 0, h_top = .1, tlai = 3, tsai = .5,
    peak_lai_flag = FALSE, harvesting_flag = FALSE, crop_living_flag = TRUE, crop_DVI = 1.5)
close(structure$tlai, 100/1000/.399 * 24.5*(1.5+.06)^(-.145))
close(structure$canopy_top, 1.27*(200/1000/.404)^.24)

# All rice arguments immediately follow the existing crop flags.
for (fun in list(f_crop_phenology, f_crop_allocation_fluxes,
                f_get_JULES_crop_allocation_coefficients, f_crop_retranslocation_JULES)) {
    arguments = names(formals(fun))
    stopifnot(match('is_rice', arguments) == match('is_soybean', arguments) + 1L)
}
cat('Rice JULES development, allocation, senescence, retranslocation and structure OK\n')
