################################################################################
### Module for calculating the phenology of plants
################################################################################

# TODO
# Reading in some of the CLM-planting parameters when the scheme is selected, possible? 
# Add a flag indicates crop is harvested due to the growing season is too long? When this happen, LAI and crop yield are very strange... This could help diagnosis

f_crop_phenology = function(T_10_d, T_min_10_d, T_soil, T2m,
                            is_maize, is_springwheat, is_winterwheat, is_soybean, is_rice = FALSE,
                            leafC, livestemC, livestemresvC, finerootC, grainC, LAI, SAI,
                            GDD_T2m, GDD_Tsoil, GDDmat, GDDrepr, GDDemer,
                            crop_living_flag, crop_planting_flag, leaf_emer_flag, grain_filling_flag, harvesting_flag,
                            prescribed_planting_date_readin = NULL, planting_jday, grain_filling_jday, harvest_jday, day_since_crop_senescence,
                            GDD0_20yr = NULL, GDD8_20yr = NULL, GDD10_20yr = NULL,
                            GDDmat_map_value = NA_real_, GDDrepr_map_value = NA_real_, GDDemer_map_value = NA_real_,
                            hybgdd, GDD_baseT, GDD_maxIncrease, max_growing_season_length, leaf_longevity, T_plant_req, T_min_plant_req, repr_GDDfrac, emer_GDDfrac,
                            prescribed_min_plant_jday, prescribed_max_plant_jday, at_NH_flag,
                            diagnostic_location = NULL,
                            winter_state = NULL, T_min_daily = NULL, snow_depth = NULL,
                            winter_parameters = list(), T_soil_subdaily = NULL){

    # Dispatch before any summer GDD update or calendar reset. Keep the public
    # argument order for existing named and positional callers.
    if (length(is_winterwheat) != 1L || is.na(is_winterwheat))
        stop('is_winterwheat must be one non-missing crop flag')
    call = match.call()
    if (as.logical(is_winterwheat)) {
        call[[1L]] = f_winter_wheat_phenology
    } else {
        call[[1L]] = f_summer_crop_phenology
        for (name in c('winter_state', 'T_min_daily', 'snow_depth',
                       'winter_parameters', 'T_soil_subdaily')) {
            if (name %in% names(call)) call[[name]] = NULL
        }
    }
    eval(call, envir = parent.frame())
}


# Existing summer-crop workflow; equations and output fields are unchanged.
f_summer_crop_phenology = function(T_10_d, T_min_10_d, T_soil, T2m,
                            is_maize, is_springwheat, is_winterwheat, is_soybean, is_rice = FALSE,
                            leafC, livestemC, livestemresvC, finerootC, grainC, LAI, SAI,
                            GDD_T2m, GDD_Tsoil, GDDmat, GDDrepr, GDDemer, 
                            crop_living_flag, crop_planting_flag, leaf_emer_flag, grain_filling_flag, harvesting_flag,
                            prescribed_planting_date_readin = NULL, planting_jday, grain_filling_jday, harvest_jday, day_since_crop_senescence, 
                            GDD0_20yr = NULL, GDD8_20yr = NULL, GDD10_20yr = NULL,
                            GDDmat_map_value = NA_real_, GDDrepr_map_value = NA_real_, GDDemer_map_value = NA_real_,
                            hybgdd, GDD_baseT, GDD_maxIncrease, max_growing_season_length, leaf_longevity, T_plant_req, T_min_plant_req, repr_GDDfrac, emer_GDDfrac,
                            prescribed_min_plant_jday, prescribed_max_plant_jday, at_NH_flag,
                            diagnostic_location = NULL){
    
    day_per_year = if (leap) 366 else 365 
    
    current_jday = date.to.day(current_date)
    if (is_rice) rice_parameters = f_rice_JULES_parameters()
    GDD_update = f_crop_GDD_update(maize_flag = is_maize, soybean_flag = is_soybean, springwheat_flag = is_springwheat, winterwheat_flag = is_winterwheat,
                                   GDD_T_2m_prev = GDD_T2m, GDD_T_soil_prev = GDD_Tsoil, today_T_soil = T_soil, today_T_2m = T2m, 
                                   GDD_base_T = GDD_baseT, GDD_max_increase = GDD_maxIncrease, crop_live_flag = crop_living_flag,
                                   rice_flag = is_rice)
    GDD_T2m = GDD_update$GDD_T_2m
    GDD_Tsoil = GDD_update$GDD_T_soil
    
    # Initializing variables for a potential growing season
    if (crop_planting_date_method == 'CLM4.5') {
        # For calculated planting date in CLM4.5, initialize the growing season at 1st Jan and 1st June in the Northern- and Southern-hemisphere, respectively.
        # How about at the tropics in CLM 5? (Pang, May 2024)
        crop_calendar_start_jday = if (at_NH_flag) 1 else if (!leap) 183 else 184
        if (current_jday == crop_calendar_start_jday) {
            if (!crop_living_flag) {
                crop_planting_flag = FALSE; leaf_emer_flag = FALSE; grain_filling_flag = FALSE; harvesting_flag = FALSE
                planting_jday = NA; harvest_jday = NA; day_since_crop_senescence = NA
                LAI = 0; SAI = 0; leafC = 0; livestemC = 0; livestemresvC = 0; grainC = 0; finerootC = 0; CUO_grain_filling = NA
            }
        }
    } else if (any(crop_planting_date_method == c('prescribed-map', 'prescribed-site'))) {
        # For prescribed planting date, initialize the growing season 1 day before planting
        if (current_jday == prescribed_planting_date_readin - 1) {
            if (!crop_living_flag) {
                crop_planting_flag = FALSE; leaf_emer_flag = FALSE; grain_filling_flag = FALSE; harvesting_flag = FALSE
                planting_jday = NA; harvest_jday = NA; day_since_crop_senescence = NA
                LAI = 0; SAI = 0; leafC = 0; livestemC = 0; livestemresvC = 0; grainC = 0; finerootC = 0; CUO_grain_filling = NA 
            }
        }
    }
    
    
    # Crop planting
    if (!crop_living_flag && !crop_planting_flag) {
        if (any(crop_planting_date_method == c('prescribed-map','prescribed-site'))) {
            if (current_jday == prescribed_planting_date_readin) {
                crop_planting_flag = T; crop_living_flag = T; planting_jday = current_jday 
                seed_C_to_leaf_C = emergence_carbon_to_leaf
                # print(paste0("[crop_phenology] Crop is planted on ", current_date))
            }
        } else if (crop_planting_date_method == 'CLM4.5') { 
            # Using climate and weather conditions to calculate planting date
            # 4 conditions:
            # 1. 10-days running mean daily mean T_2m > T_plant_req  (weather constraint)
            # 2. 10-days running mean daily min. T_2m > T_min_plant_req (weather constraint)
            # 3. GDD8_20yr != 0 dday (climate constraint),this is modified a bit from the original CLM4.5 which is GDD8_20yr >= 50 dday
            # 4. Between 'prescribed_min_plant_jday' and 'prescribed_max_plant_jday' (range of date where planting is possible)
            if (leap) {
                prescribed_min_plant_jday = prescribed_min_plant_jday + 1 
                prescribed_max_plant_jday = prescribed_max_plant_jday + 1
            }
            
            if (T_10_d > T_plant_req && 
                T_min_10_d > T_min_plant_req &&
                GDD8_20yr > 0 &&
                current_jday >= prescribed_min_plant_jday && 
                current_jday <= prescribed_max_plant_jday) {
                
                crop_planting_flag = T; crop_living_flag = T; planting_jday = current_jday 
                seed_C_to_leaf_C = emergence_carbon_to_leaf
                # print(paste0("[crop_phenology] Crop is planted on ", current_date))
            }
        } # else (!prescribed_planting_date_flag)
        
        # Determining the growing degree day (GDD) requirements, once crop are planted 
        if (crop_living_flag) {
            
            if (GDDmat_method == 'prescribed-site') {
                # 'prescribed_GDD_mat' is an input at 'input_TEMIR_crop_extension.R'
                GDDmat = prescribed_GDDmat  # prescribed_GDD_mat??
            } else if (GDDmat_method == 'prescribed-map') {
                GDDmat = GDDmat_map_value
            } else if (GDDmat_method == 'CLM4.5') {
                # The legacy option name covers imported CLM5 crop traits too.
                # CLM5 Tech Note, Eq. (2.26.2): rice uses GDD0, capped by
                # hybgdd (2100 in the downloaded CLM5 rice parameter rows).
                # https://escomp.github.io/CTSM/release-clm5.0/tech_note/Crop_Irrigation/CLM50_Tech_Note_Crop_Irrigation.html
                if (is_rice) {
                    if (length(GDD0_20yr) != 1L || !is.finite(GDD0_20yr))
                        stop('CLM rice maturity requires a finite GDD0_20yr climate value')
                    GDDmat = min(GDD0_20yr, hybgdd)
                }
                if (is_maize) GDDmat = max(950, min(GDD8_20yr * 0.85, hybgdd))
                if (is_springwheat) GDDmat = min(GDD0_20yr, hybgdd)
                if (is_soybean) GDDmat = min(GDD10_20yr, hybgdd)
                # There is no winter wheat model in CLM4.5
            }

            if (GDDrepr_method == 'prescribed-site') {
                # It is an input at 'input_TEMIR_crop_extension.R'
                GDDrepr = prescribed_GDDrepr # prescribed_GDD_repr??
            } else if (GDDrepr_method == 'prescribed-map' &&
                       is.finite(GDDrepr_map_value)) {
                GDDrepr = GDDrepr_map_value
            } else if (GDDrepr_method == "CLM4.5" ||
                       (GDDrepr_method == 'prescribed-map' &&
                        custom_phenology_optional_fallback[['GDDrepr']] == 'CLM4.5')) {
                if (is_maize) {
                    maize_maturity_rating = max(73, min(135, (GDDmat + 53.683)/13.882))          # details of this formula can be found in Kucharik (2003), Earth Interactions No.7
                    GDD_repr_factor = -0.002 * (maize_maturity_rating - 73) + 0.65
                    GDD_repr_factor = min(max(GDD_repr_factor, 0.55), 0.65)
                    GDDrepr = GDD_repr_factor * GDDmat
                } else {
                    # Includes rice grnfill imported from CLM5 (approximately .4).
                    GDDrepr = repr_GDDfrac * GDDmat
                }
            } else {
                stop('Missing prescribed-map GDDrepr and fallback is set to error')
            }
            
            if (GDDemer_method == 'prescribed-site') {
                GDDemer = prescribed_GDDemer # prescribed_GDD_emer??
            } else if (GDDemer_method == 'prescribed-map' &&
                       is.finite(GDDemer_map_value)) {
                GDDemer = GDDemer_map_value
            } else if (GDDemer_method == "CLM4.5" ||
                       (GDDemer_method == 'prescribed-map' &&
                        custom_phenology_optional_fallback[['GDDemer']] == 'CLM4.5')) {
                # Includes rice lfemerg imported from CLM5 (approximately .01).
                GDDemer = emer_GDDfrac * GDDmat
            } else {
                stop('Missing prescribed-map GDDemer and fallback is set to error')
            }
            
            # Checking the value of GDDmat, GDDrepr, GDDemer
            if (!all(is.finite(c(GDDmat, GDDrepr, GDDemer))) ||
                GDDemer <= 0 || GDDrepr <= GDDemer || GDDmat <= GDDrepr) {
                message = 'Crop GDD thresholds must satisfy 0 < GDDemer < GDDrepr < GDDmat'
                if (any(c(GDDmat_method, GDDrepr_method, GDDemer_method) ==
                        'prescribed-map')) stop(message) else warning(message)
            }
        } else {
            # Crop is not planted (yet). No need to determine GDD requirements.
        }
        
    } else {
        # Crop is planted previously and GDD requirements are already determined. NO need to determine planting date and GDD requirements. 
        # Section is left blank here deliberately
    }
    
    # Determining when crop emerges, reaching reproductive stage, and mature (if it is planted).
    if (crop_living_flag) {
        
        days_since_planting = if (current_jday >= planting_jday) {current_jday - planting_jday} else {current_jday - planting_jday + day_per_year}
        
        # Development index (DVI) follows the definition in JULES-crop.
        if (GDD_T2m < GDDemer) {
            DVI = -1 + GDD_T2m / GDDemer
        } else if (GDD_T2m >= GDDemer && GDD_T2m < GDDrepr) {
            DVI = 0 + (GDD_T2m - GDDemer) / (GDDrepr - GDDemer)
        } else if (GDD_T2m >= GDDrepr && GDD_T2m < GDDmat) {
            DVI = 1 + (GDD_T2m - GDDrepr) / (GDDmat - GDDrepr)
        } else if (GDD_T2m >= GDDmat) {
            DVI = 2
        }
        
        # Declaring vegetation carbon fluxes (except allocation), values will be modified when crop reach specific growing stages determined below
        seed_C_to_leaf_C = 0 
        seed_C_to_leaf_C_flux = 0
        leaf_C_loss_flux = 0
        grain_C_loss_flux = 0
        livestem_C_loss_flux = 0
        fineroot_C_loss_flux = 0
        livestem_resv_C_loss_flux = 0
        
        
        # print(paste0("[crop_phenology] GDDemer = ", signif(GDDemer, 5),"___GDDrepr = ",signif(GDDrepr, 5), "__ GDDmat = ", signif(GDDmat, digits = 4)))
        
        # Harvesting conditions
        # 1. GDDT2m >= GDDmat   OR
        # 2. Growing season is too long (prevent crops stay indefinitely)
        if (GDD_T2m >= GDDmat || days_since_planting > max_growing_season_length) {

            # Maturity takes precedence when more than one condition is met.
            # The dates remain in daily output, while this warning records why
            # a crop was harvested before reaching its GDD requirement.
            if (GDD_T2m < GDDmat) {
                reasons = c(
                    if (days_since_planting > max_growing_season_length)
                        sprintf('growing season exceeded the %g-day limit', max_growing_season_length)
                )
                warning(sprintf(
                    'Crop%s forced to harvest on %s after %g day(s) since planting (DOY %g to %g): %s; GDDT2m %.2f < GDDmat %.2f.',
                    if (is.null(diagnostic_location)) '' else paste0(' ', diagnostic_location),
                    current_date, days_since_planting, planting_jday, current_jday,
                    paste(reasons, collapse = '; '), GDD_T2m, GDDmat
                ), call. = FALSE)
            }

            crop_living_flag = FALSE
            grain_filling_flag = FALSE; leaf_emer_flag = FALSE; harvesting_flag = TRUE
            harvest_jday = current_jday
            day_since_crop_senescence = NA
            
            # Remove all carbon from the vegetation at harvest
            leaf_C_loss_flux = leafC / 86400
            grain_C_loss_flux = grainC / 86400
            livestem_C_loss_flux = livestemC / 86400
            fineroot_C_loss_flux = finerootC / 86400
            livestem_resv_C_loss_flux = livestemresvC / 86400
            
        } else if (GDD_T2m >= GDDrepr && GDD_T2m < GDDmat) {
            # At the reproductive stage
            # NOTE: Reproductive stage in CLM begins during grain fill (R3), whereas in JULES it begins at flowering (R1). In TEMIR, we follows the JULES definition which is adopted elsewhere.
            # TODO: at same point, the variable name should be changed for better readability. e.g., grain_filling_flag to crop_repr_flag
            
            # First time fulfill the conditions
            if (!grain_filling_flag) {
                # print(paste0("[Crop model] Reproductive stage starts on ", current_date))
                grain_filling_jday = current_jday
                grain_filling_flag = TRUE
                leaf_emer_flag = FALSE
                
                # # SoyFACE project          
                # if (O3_damage_flag & O3_damage_scheme == 'Lombardozzi' & O3_sensitivity == 'custom') {
                #   CUO_grain_filling = CUO
                # }
            }
            
        } else if (GDD_Tsoil >= GDDemer && GDD_T2m < GDDrepr) {  
            
            # Crop emerges and before reaching the reproductive stage
            # 1. GDDTsoil >= GDDemer and GDDT2m < GDDrepr, this is equivalent to 0 <= DVI < 1.0 in JULES.
            
            # First day when crop reaches GDDemer
            if (!leaf_emer_flag) { 
                # print(paste0("[Crop model] Crop emerges on ", current_date))
                leaf_emer_flag = TRUE
                # Transfers C from seed to leaf at emergence (unit: gC m^-2 s^-1)
                seed_C_to_leaf_C_flux = emergence_carbon_to_leaf / 86400
            } else {
                # No C fluxes except for the allocation of asslimated carbon during the vegetative stage
            }
            
        } else {
            # After planting but before emergence
            # No C fluxes
            # This block is intentionally left empty
        } 
        
        # Foliage loss during the late growing stage
        # In CLM4.5, reaching reproductive stage associates with foliage loss, but this is not the case in JULES (also from field observations).
        # In JULES, foliage loss starts at the midway between reaching reproductive stage and maturity GDD-wise (i.e., DVI >= 1.5).
        
        # Add new condition '!harvesting_flag' to prevent overwriting leaf loss flux during harvest
        if (!harvesting_flag) {
            if (crop_leaf_sen_scheme == 'CLM4.5' && GDD_T2m >= GDDrepr && GDD_T2m < GDDmat) {
                
                leaf_C_loss_flux = 1/(leaf_longevity * 86400 * 365) * leafC  
                
                # not used in CLM4.5 leaf senescence scheme, only for tracking purpose
                if (is.na(day_since_crop_senescence)) {
                    day_since_crop_senescence = 0
                } else {
                    day_since_crop_senescence = day_since_crop_senescence + 1   
                }
                
            } else if (crop_leaf_sen_scheme == 'custom' && !is.na(DVI) && GDD_T2m < GDDmat) {

                if (is_maize) {leaf_senescence_DVI = 1.0} # calibrated value = 1.0 ... now try 0.4
                if (is_soybean) {leaf_senescence_DVI = 1.2}  # calibrated value = 1.2 ... now try 1.0
                if (is_winterwheat || is_springwheat) {leaf_senescence_DVI = 1.5}  # not calibrated
                if (is_rice) leaf_senescence_DVI = rice_parameters$senescence$DVI_onset
                
                if (DVI >= leaf_senescence_DVI) {
                    
                    if (is.na(day_since_crop_senescence)) {
                        day_since_crop_senescence = 0
                    } else {
                        day_since_crop_senescence = day_since_crop_senescence + 1   
                    }
                    
                    # senescence rate per LAI derived from field data
                    # Maize ~ -2E-4*d^1.05
                    # Soybean ~ -2E-3*d^0.6
                    leaf_sens_rate_day = 0
                    if (is_maize) {leaf_sens_rate_day = 2E-4 * day_since_crop_senescence^1.05}
                    if (is_soybean) {leaf_sens_rate_day = 2E-3 * day_since_crop_senescence^0.6}
                    if (is_rice) {
                        # Xu (2025), Table 3 / Sect. 3.1.3; cap daily loss to
                        # available leaf carbon (preserves nonnegative pools).
                        leaf_sens_rate_day = min(1, rice_parameters$senescence$mu *
                            (DVI - leaf_senescence_DVI)^rice_parameters$senescence$nu)
                    }
                    
                    leaf_C_loss_flux = (leaf_sens_rate_day/86400) * leafC
                }
            }
        }

    } else {
        # Crops not planted
        days_since_planting = NA; day_since_crop_senescence = NA
        seed_C_to_leaf_C_flux = 0; leaf_C_loss_flux = 0; grain_C_loss_flux = 0; livestem_C_loss_flux = 0; livestem_resv_C_loss_flux = 0; fineroot_C_loss_flux = 0
        DVI = NA
    }
    
    # List of outputs
    # Fluxes: seedC_to_leafC, leafC_loss, grainC_loss, livestemC_loss, finerootC_loss
    # Phenology flags: crop_living_flag, crop_planting_flag, leaf_emer_flag, grain_filling_flag, harvest_flag
    # Phenology date:  planting_jday, harvest_jday
    # Phenology GDD: GDD_T_2m, GDD_T_soil
    # C pools and other physiology variables: leaf_C, fineroot_C, grain_C, livestem_C, LAI, SAI
    output = list(seedC_to_leafC_flux = seed_C_to_leaf_C_flux, leafC_loss_flux = leaf_C_loss_flux, grainC_loss_flux = grain_C_loss_flux, livestemC_loss_flux = livestem_C_loss_flux, livestem_resvC_loss_flux = livestem_resv_C_loss_flux, finerootC_loss_flux = fineroot_C_loss_flux,
                  croplive_flag = crop_living_flag, cropplant_flag = crop_planting_flag, leafemergence_flag = leaf_emer_flag, grainfill_flag = grain_filling_flag, har_flag = harvesting_flag, day_since_crop_senescence = day_since_crop_senescence,
                  planting_julianday = planting_jday, grain_filling_julianday = grain_filling_jday, harvesting_julianday = harvest_jday,
                  GDD_T2m = GDD_T2m, GDD_Tsoil = GDD_Tsoil, GDDmat = GDDmat, GDDrepr = GDDrepr, GDDemer = GDDemer, DVI = DVI,
                  leafC = leafC, grainC = grainC, finerootC = finerootC, livestemC = livestemC, livestemresvC = livestemresvC,
                  LAI_out = LAI, SAI_out = SAI)
    
    return(output)
    
}


################################################################################
# Winter-wheat phenology: daily TEMIR adaptation of Lu et al. (2017), Sect. 2.3,
# Eqs. (1)-(3), Tables 2 and 4. https://doi.org/10.5194/gmd-10-1873-2017
# Crown temperature is supplied shallow soil temperature (TEMIR adaptation).
# Hardening and damage follow Lu Eqs. (4)-(12), with bounded half-hour substeps.
# Carbon transfers are applied separately in Cpool_budget.R.
################################################################################

f_winter_wheat_parameters = function(overrides = list()) {
    # Table 4's modified planting temperature and maximum duration were fitted
    # at US-ARM; these are starting values, not a TEMIR calibration.
    p = list(planting_temperature = 283.15, max_growing_days = 330,
             GDDmat = 1700, emergence_fraction = .03, reproductive_fraction = .4,
             base_temperature = 0, maximum_daily_GDD = 26,
             vernalization_Tmin = -1.3, vernalization_Topt = 4.9,
             vernalization_Tmax = 15.7, vernalization_half_days = 22.5,
             vernalization_exponent = 5,
             vernalization_complete_days = 50,
             LT50_cold_limit = -23, hardening_coefficient = .0093,
             dehardening_coefficient = 2.7e-5, freezing_stress_coefficient = 1.9,
             snow_stress_coefficient = .54, survival_exponent = 4,
             frost_stage_VF = .9, frost_WDD_threshold = 1,
             frost_instant_C = 5, frost_leaf_C_floor = 10,
             initial_leaf_allocation = .6, initial_root_allocation = .3,
             final_root_allocation = 0, allocation_decline_factor = 1.05,
             leaf_allocation_decline = 3, stem_allocation_decline = 1,
             planting_start_NH = '0901', planting_end_NH = '1231',
             planting_start_SH = '0401', planting_end_SH = '0630',
             GDD0_20yr = NA_real_)
    # Cardinal temperatures: Streck et al. (2003), as used with Lu Eq. (1).
    # https://doi.org/10.2134/agronj2003.1550a
    # Planting start follows Lu Table 2 (NH) / winter climate season (SH).
    # End dates are TEMIR annual planting-window safeguards, not Lu calibration.
    # GDD0_20yr must describe the WINTER season (Sep-Jun NH / Apr-Sep SH).
    # Do not silently substitute TEMIR's annual summer-crop GDD0 map.
    if (!is.list(overrides) || (length(overrides) &&
        (is.null(names(overrides)) || anyDuplicated(names(overrides)) ||
         any(!names(overrides) %in% names(p)))))
        stop('winter_parameters must be a named list of winter-wheat parameter overrides')
    p[names(overrides)] = overrides
    numeric_names = setdiff(names(p), c('planting_start_NH', 'planting_end_NH',
                                      'planting_start_SH', 'planting_end_SH', 'GDD0_20yr'))
    if (any(!vapply(p[numeric_names], function(x)
        is.numeric(x) && length(x) == 1L && is.finite(x), logical(1))))
        stop('Winter-wheat numeric parameters must be finite scalars')
    if (!(p$vernalization_Tmin < p$vernalization_Topt &&
          p$vernalization_Topt < p$vernalization_Tmax) ||
        any(unlist(p[c('max_growing_days', 'GDDmat', 'maximum_daily_GDD',
                       'vernalization_half_days', 'vernalization_exponent')]) <= 0) ||
        !(0 < p$emergence_fraction && p$emergence_fraction < p$reproductive_fraction &&
          p$reproductive_fraction < 1)) stop('Invalid winter-wheat parameter ranges')
    if (p$LT50_cold_limit >= 0 || -.6+.142*p$LT50_cold_limit <= p$LT50_cold_limit ||
        p$vernalization_complete_days <= 0 ||
        p$frost_stage_VF <= 0 || p$frost_stage_VF >= 1 ||
        p$frost_WDD_threshold <= 0 || p$survival_exponent <= 0 ||
        any(unlist(p[c('hardening_coefficient', 'dehardening_coefficient',
                       'freezing_stress_coefficient', 'snow_stress_coefficient',
                       'frost_instant_C', 'frost_leaf_C_floor')]) < 0))
        stop('Invalid winter-wheat cold/damage parameter ranges')
    if (p$initial_leaf_allocation < 0 || p$initial_leaf_allocation > 1 ||
        p$initial_root_allocation < 0 || p$initial_root_allocation > 1 ||
        p$final_root_allocation < 0 || p$final_root_allocation > 1 ||
        p$allocation_decline_factor < 1 ||
        p$leaf_allocation_decline <= 0 || p$stem_allocation_decline <= 0)
        stop('Invalid winter-wheat allocation parameters')
    for (name in c('planting_start_NH', 'planting_end_NH', 'planting_start_SH', 'planting_end_SH')) {
        x = p[[name]]
        if (!is.character(x) || length(x) != 1L || is.na(x) ||
            !grepl('^[0-9]{4}$', x) || is.na(as.Date(paste0('2001', x), '%Y%m%d')))
            stop('Winter planting-window dates must be valid MMDD strings')
    }
    if (p$planting_start_NH >= p$planting_end_NH || p$planting_start_SH >= p$planting_end_SH)
        stop('Winter planting-window start must precede its end within the planting year')
    if (!is.numeric(p$GDD0_20yr) || length(p$GDD0_20yr) != 1L ||
        (!is.na(p$GDD0_20yr) && (!is.finite(p$GDD0_20yr) || p$GDD0_20yr < 0)))
        stop('winter_parameters$GDD0_20yr must be nonnegative or NA')
    p
}

f_winter_wheat_vernalization_rate = function(T_crown_C, parameters = f_winter_wheat_parameters()) {
    # Lu Eq. (1), dimensionless daily response, evaluated at daily mean crown T.
    p = parameters
    if (length(T_crown_C) != 1L || !is.finite(T_crown_C))
        stop('Winter-wheat crown temperature must be finite')
    if (T_crown_C <= p$vernalization_Tmin || T_crown_C >= p$vernalization_Tmax) return(0)
    alpha = log(2) / log((p$vernalization_Tmax - p$vernalization_Tmin) /
                        (p$vernalization_Topt - p$vernalization_Tmin))
    x = ((T_crown_C - p$vernalization_Tmin) /
         (p$vernalization_Topt - p$vernalization_Tmin))^alpha
    min(1, max(0, 2*x - x*x))
}

f_winter_wheat_crown_temperature = function(T_soil) {
    # Every winter simulation uses shallow soil temperature, in K. Snow effects
    # are already embedded in the supplied land-surface soil temperature.
    # Output degC; no air-temperature or snow-depth reconstruction/fallback.
    if (!is.numeric(T_soil) || !length(T_soil) || any(!is.finite(T_soil)))
        stop('Winter-wheat shallow soil temperature must be finite (K)')
    T_soil - 273.15
}

f_winter_wheat_cold_update = function(state, T_soil, VF, vernalization_days,
                                     leafC, snow_depth = NULL,
                                     parameters = f_winter_wheat_parameters()) {
    # Lu (2017), Eqs. (4)-(12). Use actual subdaily shallow-soil samples when
    # supplied; otherwise hold the daily mean constant. Each sample represents
    # an equal fraction of a day and is subdivided to at most half an hour.
    # The 5 gC damage amount is PER HALF HOUR, not once per arbitrary timestep.
    p = parameters
    temperatures = f_winter_wheat_crown_temperature(T_soil)
    if (is.null(snow_depth)) {
        snow = rep(NA_real_, length(temperatures))
    } else {
        if (!is.numeric(snow_depth) || !length(snow_depth) ||
            !length(snow_depth) %in% c(1L, length(temperatures)) ||
            any(!is.finite(snow_depth)) || any(snow_depth < 0))
            stop('Cold-stress snow depth must be nonnegative m, scalar or aligned with soil samples; NULL means unavailable')
        snow = rep(snow_depth, length.out = length(temperatures))
    }
    for (x in c('LT50', 'WDD', 'survival_sum', 'survival_days')) {
        if (length(state[[x]]) != 1L || !is.finite(state[[x]])) stop('Invalid winter-wheat cold state')
    }
    if (length(leafC) != 1L || !is.finite(leafC) || leafC < 0 ||
        length(VF) != 1L || !is.finite(VF) || VF < 0 || VF > 1 ||
        !is.finite(vernalization_days) || vernalization_days < 0)
        stop('Invalid winter-wheat frost-damage inputs')
    initial_LT50 = -.6 + .142*p$LT50_cold_limit
    if (state$LT50 < p$LT50_cold_limit || state$LT50 > initial_LT50 ||
        any(c(state$WDD, state$survival_sum, state$survival_days) < 0))
        stop('Winter-wheat cold state outside physical bounds')
    leaf_remaining = leafC
    # VF approaches 1 asymptotically. Use an explicit 50-effective-day default
    # for the completed-vernalization dehardening gate (TEMIR convention),
    # separately from Lu's VF=.9 switch between the two damage regimes.
    complete = vernalization_days >= p$vernalization_complete_days
    split = ceiling(48/length(temperatures))
    dt_days = 1/(length(temperatures)*split)
    rates = c(hardening = 0, dehardening = 0, freezing_stress = 0, snow_stress = 0)
    survival_integral = 0
    damage_events = 0L
    for (k in seq_along(temperatures)) for (step in seq_len(split)) {
        tc = temperatures[k]
        lt = state$LT50
        hard = if (tc < 10) p$hardening_coefficient*(10-max(tc, 0))*(lt-p$LT50_cold_limit) else 0
        dehard = if (tc >= if (complete) -4 else 10)
            p$dehardening_coefficient*(initial_LT50-lt)*(tc+4)^3 else 0
        # Positive exposure stress only. The printed exponential can overflow
        # at lethal extremes; bound its exponent and project LT50 to the
        # interval [cold limit, unacclimated LT50] after each small update.
        delta = lt-tc
        freeze = if (delta > 0) delta*exp(min(700, p$freezing_stress_coefficient*delta+3.74)) else 0
        snow_stress = if (is.na(snow[k])) 0 else
            p$snow_stress_coefficient*max(0, (exp(min(700, .84+.051*tc))-2)/1.85)*min(snow[k]*100, 12.5)/12.5
        state$LT50 = min(initial_LT50, max(p$LT50_cold_limit,
            lt + (dehard+freeze+snow_stress-hard)*dt_days))
        survival = if (tc >= 0) 1 else 2^(-((abs(tc)/abs(state$LT50))^p$survival_exponent))
        state$WDD = state$WDD + max(-tc, 0)*(1-survival)*dt_days
        if (state$WDD > 0) {
            state$survival_sum = state$survival_sum + survival*dt_days
            state$survival_days = state$survival_days + dt_days
        }
        loss = 0
        if (VF < p$frost_stage_VF && state$WDD > 0 && survival < 1) {
            loss = min(max(leaf_remaining-p$frost_leaf_C_floor, 0),
                       p$frost_instant_C*(1-survival)*dt_days*48)
        } else if (VF >= p$frost_stage_VF && state$WDD > p$frost_WDD_threshold) {
            average_survival = state$survival_sum/state$survival_days
            loss = leaf_remaining*(1-min(1, max(0, average_survival)))
            state$WDD = state$survival_sum = state$survival_days = 0
            damage_events = damage_events + 1L
        }
        leaf_remaining = max(0, leaf_remaining-loss)
        rates = rates + c(hardening = hard, dehardening = dehard,
                          freezing_stress = freeze, snow_stress = snow_stress)*dt_days
        survival_integral = survival_integral + survival*dt_days
    }
    list(state = state, leaf_frost_loss_flux = (leafC-leaf_remaining)/86400,
         survival_probability = min(1, max(0, survival_integral)), rates = rates,
         damage_events = damage_events, snow_stress_available = !is.null(snow_depth))
}

f_winter_wheat_date = function(x) {
    # TEMIR normally supplies numeric YYYYMMDD; accept ISO dates in unit tests
    # and direct function use too. Use Date arithmetic for cross-year/leap days.
    if (inherits(x, 'Date')) return(x)
    x = as.character(x)
    out = if (grepl('^[0-9]{8}$', x)) as.Date(x, '%Y%m%d') else as.Date(x)
    if (length(out) != 1L || is.na(out)) stop('Invalid winter-wheat simulation date')
    out
}

f_winter_wheat_phenology = function(T_10_d, T_min_10_d, T_soil, T2m,
                            is_maize, is_springwheat, is_winterwheat, is_soybean, is_rice = FALSE,
                            leafC, livestemC, livestemresvC, finerootC, grainC, LAI, SAI,
                            GDD_T2m, GDD_Tsoil, GDDmat, GDDrepr, GDDemer,
                            crop_living_flag, crop_planting_flag, leaf_emer_flag, grain_filling_flag, harvesting_flag,
                            prescribed_planting_date_readin = NULL, planting_jday, grain_filling_jday, harvest_jday, day_since_crop_senescence,
                            GDD0_20yr = NULL, GDD8_20yr = NULL, GDD10_20yr = NULL,
                            GDDmat_map_value = NA_real_, GDDrepr_map_value = NA_real_, GDDemer_map_value = NA_real_,
                            hybgdd, GDD_baseT, GDD_maxIncrease, max_growing_season_length, leaf_longevity, T_plant_req, T_min_plant_req, repr_GDDfrac, emer_GDDfrac,
                            prescribed_min_plant_jday, prescribed_max_plant_jday, at_NH_flag,
                            diagnostic_location = NULL,
                            winter_state = NULL, T_min_daily = NULL, snow_depth = NULL,
                            winter_parameters = list(), T_soil_subdaily = NULL) {
    p = f_winter_wheat_parameters(winter_parameters)
    today = f_winter_wheat_date(current_date)
    current_jday = as.integer(format(today, '%j'))
    year = as.integer(format(today, '%Y'))
    # No generic January/July reset: the active crop persists across New Year.
    if (is.null(winter_state)) {
        if (crop_living_flag || leaf_emer_flag || grain_filling_flag)
            stop('Active winter wheat requires saved winter_state; restart from a preplant date or a winter-aware checkpoint')
        winter_state = list(version = 2L, last_date = NA_character_,
            planting_date = NA_character_, last_planting_year = NA_integer_,
            GDD_T2m_raw = 0, GDD_Tsoil_raw = 0, vernalization_days = 0,
            seed_transferred = FALSE, Tmin_history = numeric(),
            LT50 = -.6 + .142*p$LT50_cold_limit, WDD = 0,
            survival_sum = 0, survival_days = 0,
            carbon = list(litter_C = 0, harvested_C = 0, frost_litter_C = 0))
    }
    s = winter_state
    required = c('version', 'last_date', 'planting_date', 'last_planting_year',
                 'GDD_T2m_raw', 'GDD_Tsoil_raw', 'vernalization_days', 'seed_transferred', 'Tmin_history',
                 'LT50', 'WDD', 'survival_sum', 'survival_days', 'carbon')
    if (!is.list(s) || !all(required %in% names(s)) || !identical(s$version, 2L))
        stop('Invalid winter_state schema/version; start the soil-temperature/cold scheme before planting')
    if (any(!vapply(s[c('GDD_T2m_raw', 'GDD_Tsoil_raw', 'vernalization_days')],
                   function(x) is.numeric(x) && length(x) == 1L && is.finite(x) && x >= 0,
                   logical(1))))
        stop('Invalid winter-wheat thermal/vernalization state')
    if (!is.logical(s$seed_transferred) || length(s$seed_transferred) != 1L ||
        is.na(s$seed_transferred) || !is.numeric(s$Tmin_history) ||
        length(s$Tmin_history) > 5L || any(!is.finite(s$Tmin_history)))
        stop('Invalid winter-wheat emergence/temperature-history state')
    if (!is.na(s$last_date) && as.integer(today - as.Date(s$last_date)) != 1L)
        stop('winter_state must come from the immediately preceding model day')
    if (crop_living_flag && (is.na(s$planting_date) || !crop_planting_flag))
        stop('Active winter wheat has inconsistent planting state')
    if (crop_living_flag && (grain_filling_flag || leaf_emer_flag) && !s$seed_transferred)
        stop('Active winter wheat has inconsistent emergence state')
    if (length(T_soil) != 1L || !is.finite(T_soil))
        stop('Winter-wheat daily soil temperature must be finite (K)')
    crown_C = f_winter_wheat_crown_temperature(T_soil)
    cold_temperatures = if (is.null(T_soil_subdaily)) T_soil else T_soil_subdaily
    f_winter_wheat_crown_temperature(cold_temperatures) # validate, never fall back to air
    if (!is.null(T_soil_subdaily) && abs(mean(T_soil_subdaily)-T_soil) > 1e-6)
        stop('T_soil must equal the mean of its subdaily shallow-soil samples')
    if (length(T2m) != 1L || !is.finite(T2m)) stop('Winter-wheat daily air temperature must be finite (K)')
    # Five daily minima, including today. Do not substitute the summer 10-day
    # statistic. Wait for a full window after a cold start before auto-planting.
    if (!is.null(T_min_daily)) {
        if (length(T_min_daily) != 1L || !is.finite(T_min_daily)) stop('T_min_daily must be finite (K)')
        s$Tmin_history = tail(c(s$Tmin_history, T_min_daily), 5L)
    }
    Tmin_5d = if (length(s$Tmin_history) == 5L) mean(s$Tmin_history) else NA_real_
    seed_flux = leaf_loss = stem_loss = reserve_loss = root_loss = grain_loss = 0
    planted_today = emerged_today = reproductive_today = FALSE
    harvest_reason = NA_character_
    frost_loss = 0; survival_probability = 1; cold_rates = rep(0, 4)
    cold_damage_events = 0L
    eligible_year = is.na(s$last_planting_year) || s$last_planting_year != year

    if (!crop_living_flag && eligible_year) {
        plant_now = FALSE
        if (crop_planting_date_method %in% c('prescribed-site', 'prescribed-map')) {
            days_this_year = as.integer(as.Date(sprintf('%d-12-31', year)) - as.Date(sprintf('%d-01-01', year))) + 1L
            if (length(prescribed_planting_date_readin) != 1L ||
                !is.finite(prescribed_planting_date_readin) ||
                prescribed_planting_date_readin != round(prescribed_planting_date_readin) ||
                prescribed_planting_date_readin < 1 || prescribed_planting_date_readin > days_this_year)
                stop('Winter-wheat prescribed planting date must be a valid day of year')
            plant_now = current_jday == prescribed_planting_date_readin
        } else if (crop_planting_date_method == 'CLM4.5') {
            if (is.null(T_min_daily)) stop('Calculated winter-wheat planting requires T_min_daily (K)')
            if (!is.finite(p$GDD0_20yr))
                stop('Calculated winter-wheat planting requires winter_parameters$GDD0_20yr for Sep-Jun (NH) or Apr-Sep (SH); alternatively prescribe the planting date')
            start = as.Date(paste0(year, if (at_NH_flag) p$planting_start_NH else p$planting_start_SH), '%Y%m%d')
            end = as.Date(paste0(year, if (at_NH_flag) p$planting_end_NH else p$planting_end_SH), '%Y%m%d')
            plant_now = today > start && today <= end && is.finite(Tmin_5d) &&
                Tmin_5d < p$planting_temperature && p$GDD0_20yr > 50
        } else stop('Unsupported winter-wheat planting date method')

        if (plant_now) {
            # Existing prescribed site/map thresholds retain precedence. The
            # legacy CLM4.5 option selects Lu Table 4's winter defaults here.
            threshold = function(method, map_value, site_name, fallback, field) {
                if (method == 'prescribed-site') return(get(site_name, inherits = TRUE))
                if (method == 'prescribed-map') {
                    if (is.finite(map_value)) return(map_value)
                    if (field == 'GDDmat' || !exists('custom_phenology_optional_fallback', inherits = TRUE) ||
                        !identical(unname(custom_phenology_optional_fallback[[field]]), 'CLM4.5'))
                        stop(paste('Missing prescribed-map', field, 'for winter wheat'))
                } else if (method != 'CLM4.5') stop('Unsupported winter-wheat GDD requirement method')
                fallback
            }
            GDDmat = threshold(GDDmat_method, GDDmat_map_value, 'prescribed_GDDmat', p$GDDmat, 'GDDmat')
            GDDrepr = threshold(GDDrepr_method, GDDrepr_map_value, 'prescribed_GDDrepr', p$reproductive_fraction * GDDmat, 'GDDrepr')
            GDDemer = threshold(GDDemer_method, GDDemer_map_value, 'prescribed_GDDemer', p$emergence_fraction * GDDmat, 'GDDemer')
            if (length(c(GDDmat, GDDrepr, GDDemer)) != 3L ||
                any(!is.finite(c(GDDmat, GDDrepr, GDDemer))) ||
                !(0 < GDDemer && GDDemer < GDDrepr && GDDrepr < GDDmat))
                stop('Winter-wheat GDD thresholds must satisfy 0 < GDDemer < GDDrepr < GDDmat')
            crop_living_flag = crop_planting_flag = TRUE
            leaf_emer_flag = grain_filling_flag = harvesting_flag = FALSE
            planting_jday = current_jday
            grain_filling_jday = harvest_jday = day_since_crop_senescence = NA_real_
            # Normal emergence/harvest transfers remain in the existing C
            # budget. Do not clear carbon pools as a phenology side effect.
            s$planting_date = as.character(today)
            s$last_planting_year = year
            s$GDD_T2m_raw = s$GDD_Tsoil_raw = s$vernalization_days = 0
            s$seed_transferred = FALSE
            s$LT50 = -.6 + .142*p$LT50_cold_limit
            s$WDD = s$survival_sum = s$survival_days = 0
            planted_today = TRUE
        }
    }
    VF = s$vernalization_days^p$vernalization_exponent /
        (p$vernalization_half_days^p$vernalization_exponent + s$vernalization_days^p$vernalization_exponent)
    DVI = NA_real_
    if (crop_living_flag) {
        if (any(!is.finite(c(GDDmat, GDDrepr, GDDemer))) ||
            !(0 < GDDemer && GDDemer < GDDrepr && GDDrepr < GDDmat))
            stop('Invalid saved winter-wheat GDD thresholds')
        # Planting-day GDD is zero, matching the existing summer convention.
        if (!planted_today) {
            s$GDD_T2m_raw = s$GDD_T2m_raw + min(p$maximum_daily_GDD, max(T2m - 273.15 - p$base_temperature, 0))
            s$GDD_Tsoil_raw = s$GDD_Tsoil_raw + min(p$maximum_daily_GDD, max(T_soil - 273.15 - p$base_temperature, 0))
        }
        was_emerged = s$seed_transferred
        if (!s$seed_transferred && s$GDD_Tsoil_raw >= GDDemer) {
            s$seed_transferred = TRUE
            leaf_emer_flag = TRUE
            emerged_today = TRUE
        }
        # Begin vernalization on the day AFTER emergence; stop once the
        # reproductive transition has been latched. Daily approximation to Lu.
        if (was_emerged && !grain_filling_flag) {
            s$vernalization_days = s$vernalization_days + f_winter_wheat_vernalization_rate(crown_C, p)
            VF = s$vernalization_days^p$vernalization_exponent /
                (p$vernalization_half_days^p$vernalization_exponent + s$vernalization_days^p$vernalization_exponent)
        }
        # Lu: adjusted CUMULATIVE GDD = raw cumulative GDD * current VF.
        # Never feed yesterday's adjusted GDD back into the raw accumulator.
        # Emergence is soil-driven and irreversible, so DVI stays >= 0 after
        # emergence even if adjusted air GDD is below the emergence threshold.
        GDD_T2m = if (s$seed_transferred) s$GDD_T2m_raw * VF else s$GDD_T2m_raw
        GDD_Tsoil = s$GDD_Tsoil_raw
        if (!s$seed_transferred) {
            DVI = max(-1, min(0, -1 + GDD_Tsoil / GDDemer))
        } else if (GDD_T2m < GDDrepr && !grain_filling_flag) {
            DVI = max(0, (GDD_T2m - GDDemer) / (GDDrepr - GDDemer))
        } else {
            DVI = min(2, max(1, 1 + (GDD_T2m - GDDrepr) / (GDDmat - GDDrepr)))
        }
        days_since_planting = as.integer(today - as.Date(s$planting_date))
        mature = s$seed_transferred && GDD_T2m >= GDDmat
        if (mature || days_since_planting > p$max_growing_days) {
            harvest_reason = if (mature) 'maturity' else 'maximum_growing_days'
            if (!mature) warning(sprintf(
                'Winter wheat%s forced to harvest on %s after %d days: growing season exceeded the %g-day limit; adjusted GDD %.2f < GDDmat %.2f.',
                if (is.null(diagnostic_location)) '' else paste0(' ', diagnostic_location),
                today, days_since_planting, p$max_growing_days, GDD_T2m, GDDmat), call. = FALSE)
            crop_living_flag = leaf_emer_flag = grain_filling_flag = FALSE
            harvesting_flag = TRUE
            harvest_jday = current_jday
            day_since_crop_senescence = NA_real_
            # Existing harvest flux contract, unchanged (gC m-2 s-1).
            leaf_loss = leafC/86400; stem_loss = livestemC/86400
            reserve_loss = livestemresvC/86400; root_loss = finerootC/86400; grain_loss = grainC/86400
        } else {
            if (emerged_today) seed_flux = emergence_carbon_to_leaf/86400
            # Frost acts on established vegetative leaves. Harvest takes
            # precedence; a newly emerged seedling is first exposed tomorrow.
            if (was_emerged && !grain_filling_flag) {
                cold = f_winter_wheat_cold_update(s, cold_temperatures, VF,
                    s$vernalization_days, leafC, snow_depth, p)
                s = cold$state; frost_loss = cold$leaf_frost_loss_flux
                survival_probability = cold$survival_probability
                cold_rates = cold$rates; cold_damage_events = cold$damage_events
            }
            if (s$seed_transferred && GDD_T2m >= GDDrepr && !grain_filling_flag) {
                grain_filling_flag = TRUE
                leaf_emer_flag = FALSE
                grain_filling_jday = current_jday
                reproductive_today = TRUE
            }
            # Retain the existing wheat senescence parameterization:
            # CLM4.5 turnover after reproduction; custom wheat has zero rate.
            senescing = (crop_leaf_sen_scheme == 'CLM4.5' && grain_filling_flag) ||
                        (crop_leaf_sen_scheme == 'custom' && DVI >= 1.5)
            if (senescing) {
                day_since_crop_senescence = if (is.na(day_since_crop_senescence)) 0 else day_since_crop_senescence + 1
                if (crop_leaf_sen_scheme == 'CLM4.5') leaf_loss = leafC/(leaf_longevity*86400*365)
            }
            leaf_loss = min(leaf_loss, max(0, leafC/86400-frost_loss))
        }
    } else {
        GDD_T2m = GDD_Tsoil = 0
        day_since_crop_senescence = NA_real_
    }
    s$last_date = as.character(today)
    list(seedC_to_leafC_flux = seed_flux, leafC_loss_flux = leaf_loss,
         grainC_loss_flux = grain_loss, livestemC_loss_flux = stem_loss,
         livestem_resvC_loss_flux = reserve_loss, finerootC_loss_flux = root_loss,
         croplive_flag = crop_living_flag, cropplant_flag = crop_planting_flag,
         leafemergence_flag = leaf_emer_flag, grainfill_flag = grain_filling_flag,
         har_flag = harvesting_flag, day_since_crop_senescence = day_since_crop_senescence,
         planting_julianday = planting_jday, grain_filling_julianday = grain_filling_jday,
         harvesting_julianday = harvest_jday, GDD_T2m = GDD_T2m, GDD_Tsoil = GDD_Tsoil,
         GDDmat = GDDmat, GDDrepr = GDDrepr, GDDemer = GDDemer, DVI = DVI,
         leafC = leafC, grainC = grainC, finerootC = finerootC, livestemC = livestemC,
         livestemresvC = livestemresvC, LAI_out = LAI, SAI_out = SAI,
         winter_state = s, vernalization_days = s$vernalization_days,
         vernalization_factor = VF, crown_temperature_C = crown_C, Tmin_5d = Tmin_5d,
         leaf_frost_loss_flux = frost_loss, survival_probability = survival_probability,
         LT50 = s$LT50, WDD = s$WDD, cold_rates = cold_rates,
         cold_damage_events = cold_damage_events, cold_snow_stress_available = !is.null(snow_depth),
         planted_today = planted_today, emerged_today = emerged_today,
         reproductive_today = reproductive_today, harvest_reason = harvest_reason)
}


f_evergreen_phenology = function(){
    # evergreen phenology in CLM4.5
    # d(leaf_C)/dt = -leaf_long * leaf_C
    
}

f_stress_deciduous_phenology = function(){
    
}

f_seasonal_deciduous_phenology = function(){
    
}



f_onset_growth_fluxes = function(){
    
}

f_litter_fall_fluxes = function(){
    
}

f_wood_turnover_fluxes = function(){
    
}

f_litter_to_soil_fluxes = function(){
    
}

f_crop_GDD_update = function(maize_flag, soybean_flag, springwheat_flag, winterwheat_flag, rice_flag = FALSE,
                             GDD_T_2m_prev, GDD_T_soil_prev, today_T_soil, today_T_2m, 
                             GDD_base_T, GDD_max_increase, 
                             crop_live_flag){
    
    # Daily mean temperatures
    T_soil_degC = today_T_soil - 273.15
    T_2m_degC = today_T_2m - 273.15
    
    # print(paste0('[GDD update] GDDTsoil_prev = ', GDD_T_soil_prev,"__GDDT2m_prev = ", GDD_T_2m_prev))
    # print(paste0('[GDD update] T_soil_degC = ', T_soil_degC,"__GDD_base_T = ", GDD_base_T,"__GDD_max_increase = ", GDD_max_increase))
    
    if (GDD_calculation_method == 'CLM4.5') {
        
        GDD_soil_increase = min(max(T_soil_degC - GDD_base_T, 0), GDD_max_increase)
        GDD_T_2m_increase = min(max(T_2m_degC - GDD_base_T, 0), GDD_max_increase)
        
    } else if (GDD_calculation_method == 'JULES') {
        
        if (maize_flag) {T_b = 8; T_o = 30; T_m = 42}
        if (soybean_flag) {T_b = 10; T_o = 30; T_m = 40}  # modified T_b from 5 to 10 for soybean
        if (springwheat_flag | winterwheat_flag) {T_b = 0; T_o = 20; T_m = 30}
        # Optional rice support: FALSE by default, including runs without rice.
        # Rice JULES source policy: use Xu et al. (2025) where reported;
        # otherwise use the rice defaults in Osborne et al. (2015), with
        # parameter-specific attribution (never infer a wheat fallback).
        # Xu et al. (2025), Sect. 3.1.2, explicitly retains these three
        # development temperatures from Osborne et al. (2015); units are degC.
        # https://doi.org/10.5194/gmd-18-7257-2025
        # https://doi.org/10.5194/gmd-8-1139-2015
        # These are GDD cardinal temperatures, separate from CLM5 baset and
        # from photosynthesis temperature parameters in Xu et al.'s Table 2.
        if (rice_flag) {
            rice_development = f_rice_JULES_parameters()$development
            T_b = rice_development$T_b; T_o = rice_development$T_o; T_m = rice_development$T_m
        }
        
        GDD_soil_increase = ifelse(test = T_soil_degC < T_b | T_soil_degC >= T_m, yes = 0, 
                                   no = ifelse(test = T_soil_degC <= T_o, yes = T_soil_degC - T_b, 
                                               no = (T_o - T_b)*(1 - (T_soil_degC - T_o)/(T_m - T_o))))
        
        GDD_T_2m_increase = ifelse(test = T_2m_degC < T_b | T_2m_degC >= T_m, yes = 0,
                                   no = ifelse(test = T_2m_degC <= T_o, yes = T_2m_degC - T_b,
                                               no = (T_o - T_b)*(1 - (T_2m_degC - T_o)/(T_m - T_o))))
        
        
    } else if (GDD_calculation_method == 'custom') {
        # Implement your scheme here...
    }
    
    # GDD accumulates from planting until harvesting
    if (crop_live_flag) {
        GDD_T_soil_output = GDD_T_soil_prev + GDD_soil_increase
        GDD_T_2m_output = GDD_T_2m_prev + GDD_T_2m_increase
    } else {
        # Crops not planted/alive
        GDD_T_2m_output = 0
        GDD_T_soil_output = 0
    }
    
    # print(paste0('[GDD update] GDD_T_soil = ', GDD_T_soil_output,"__GDD_T_2m = ", GDD_T_2m_output))
    
    output = list(GDD_T_2m = GDD_T_2m_output, GDD_T_soil = GDD_T_soil_output)
    
}
