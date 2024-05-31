################################################################################
### Module for calculating the phenology of plants
################################################################################

# TODO
# Reading in some of the CLM-planting parameters when the scheme is selected, possible? 
# Reading in Sack GDDmat in a more elegant way? Having multiple similar function arguments is not a good way.........
# Add a flag indicates crop is harvested due to the growing season is too long? When this happen, LAI and crop yield are very strange... This could help diagnosis

f_crop_phenology = function(T_10_d, T_min_10_d, T_soil, T2m,
                            is_maize, is_springwheat, is_winterwheat, is_soybean, 
                            leafC, livestemC, finerootC, grainC, LAI, SAI,
                            GDD_T2m, GDD_Tsoil, GDDmat, GDDrepr, GDDemer, 
                            crop_living_flag, crop_planting_flag, leaf_emer_flag, grain_filling_flag, harvesting_flag,
                            prescribed_planting_date_readin = NULL, planting_jday, grain_filling_jday, harvest_jday,
                            GDD0_20yr = NULL, GDD8_20yr = NULL, GDD10_20yr = NULL, GDDmat_M = NULL, GDDmat_S = NULL, GDDmat_SW = NULL, GDDmat_WW = NULL,
                            hybgdd, GDD_baseT, GDD_maxIncrease, max_growing_season_length, leaf_longevity, T_plant_req, T_min_plant_req, repr_GDDfrac, emer_GDDfrac,
                            prescribed_min_plant_jday, prescribed_max_plant_jday, at_NH_flag){

  day_per_year = if (leap) 366 else 365 
  
  current_jday = date.to.day(current_date)
  GDD_update = f_crop_GDD_update(GDD_T_2m_prev = GDD_T2m, GDD_T_soil_prev = GDD_Tsoil, today_T_soil = T_soil, today_T_2m = T2m, GDD_base_T = GDD_baseT, GDD_max_increase = GDD_maxIncrease, crop_live_flag = crop_living_flag)
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
              planting_jday = NA; harvest_jday = NA
              LAI = 0; SAI = 0; leafC = 0; livestemC; grainC = 0; finerootC = 0; CUO_grain_filling = NA
          }
      }
  } else if (any(crop_planting_date_method == c('prescribed-map', 'prescribed-site'))) {
      # For prescribed planting date, initialize the growing season 1 day before planting
      if (current_jday == prescribed_planting_date_readin - 1) {
          if (!crop_living_flag) {
              crop_planting_flag = FALSE; leaf_emer_flag = FALSE; grain_filling_flag = FALSE; harvesting_flag = FALSE
              planting_jday = NA; harvest_jday = NA
              LAI = 0; SAI = 0; leafC = 0; livestemC; grainC = 0; finerootC = 0; CUO_grain_filling = NA 
          }
      }
  }

  
  # Planting the crops at the planting date
  if (!crop_living_flag && !crop_planting_flag) {
    if (any(crop_planting_date_method == c('prescribed-map','prescribed-site'))) {
      if (current_jday == if(is.na(prescribed_planting_date)) prescribed_planting_date_readin else prescribed_planting_date) {
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
    
    # Determining the growing degree day (GDD) requirements, once the crop is planted 
    if (crop_living_flag) {
      
        if (GDDmat_method == 'prescribed-site') {
            # 'prescribed_GDD_mat' is an input at 'input_TEMIR_crop_extension.R'
            GDDmat = prescribed_GDD_mat
        } else if (GDDmat_method == 'prescribed-map') {
            if (is_maize)  GDDmat = GDDmat_M
            if (is_springwheat) GDDmat = GDDmat_SW
            if (is_winterwheat) GDDmat = GDDmat_WW
            if (is_soybean) GDDmat = GDDmat_S
        } else if (GDDmat_method == 'CLM4.5') {
            if (is_maize) GDDmat = max(950, min(GDD8_20yr * 0.85, hybgdd))
            if (is_springwheat) GDDmat = min(GDD0_20yr, hybgdd)
            if (is_soybean) GDDmat = min(GDD10_20yr, hybgdd)
            # There is no winter wheat model in CLM4.5
        }
        
        if (GDDrepr_method == 'prescribed-site') {
            # It is an input at 'input_TEMIR_crop_extension.R'
            GDDrepr = prescribed_GDD_repr
        } else if (GDDrepr_method == "CLM4.5") {
            if (is_maize) {
                maize_maturity_rating = max(73, min(135, (GDDmat + 53.683)/13.882))          # details of this formula can be found in Kucharik (2003), Earth Interactions No.7
                GDD_repr_factor = -0.002 * (maize_maturity_rating - 73) + 0.65
                GDD_repr_factor = min(max(GDD_repr_factor, 0.55), 0.65)
                GDDrepr = GDD_repr_factor * GDDmat
            } else {
                GDDrepr = repr_GDDfrac * GDDmat
            }
        }
        
        if (GDDemer_method == 'prescribed-site') {
            GDDemer = prescribed_GDD_emer
        } else if (GDDemer_method == "CLM4.5") {
            GDDemer = emer_GDDfrac * GDDmat
        }
    
      # Checking the value of GDDmat, GDDrepr, GDDemer
      if (GDDmat < GDDrepr || GDDmat < GDDemer || GDDrepr < GDDemer){
        warning("The values of GDDmat or/and GDDrepr or/and GDDemer might have some problems. 'GDDmat should be greather than GDDrepr, and GDDrepr should be greater than GDDemer'")
      }
    } else {
      # Crop is not planted (yet). No need to determine GDD requirements.
    }
    
  } else {
    # Crop is planted previously and GDD requirements are already determined. NO need to determine planting date and GDD requirements. 
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

    
    # print(paste0("[crop_phenology] GDDemer = ", signif(GDDemer, 5),"___GDDrepr = ",signif(GDDrepr, 5), "__ GDDmat = ", signif(GDDmat, digits = 4)))

    # Harvesting conditions
    # 1. GDDT2m > GDDmat   OR
    # 2. Growing season is too long that crops are forced to be harvested 
    if (GDD_T2m >= GDDmat || days_since_planting > max_growing_season_length) { 
      # print(paste0("[Crop model] Harvesting starts on ", current_date))
      crop_living_flag = FALSE
      grain_filling_flag = FALSE; leaf_emer_flag = FALSE; harvesting_flag = TRUE
      harvest_jday = current_jday
      
      # Remove carbon from the vegetation at harvest
      seed_C_to_leaf_C = 0 
      seed_C_to_leaf_C_flux = 0
      leaf_C_loss_flux = leafC / 86400
      grain_C_loss_flux = grainC / 86400
      livestem_C_loss_flux = livestemC / 86400
      fineroot_C_loss_flux = finerootC / 86400
      
    } else if (GDD_T2m >= GDDrepr && GDD_T2m < GDDmat) {
      # At the reproductive stage (grain fill)
      # 1. GDDT2m >= GDDrepr and GDDT2m < GDDmat (CLM4.5), this is equivalent to 1.0 <= DVI < 2.0 in JULES.
      # Leaf senescence is now determined separately below.
      
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
      
      # First time fulfill the conditions
      if (!leaf_emer_flag) { 
        # print(paste0("[Crop model] Crop emerges on ", current_date))
        leaf_emer_flag = TRUE
        # When crop emerges, all the carbon in the seed transfers to leaf (unit of the flux, gC m^-2 s^-1)
        seed_C_to_leaf_C_flux = emergence_carbon_to_leaf / 86400
        leaf_C_loss_flux = 0; grain_C_loss_flux = 0; livestem_C_loss_flux = 0; fineroot_C_loss_flux = 0
      } else {
	# No other C fluxes except for the allocation of asslimated carbon
        seed_C_to_leaf_C_flux = 0; leaf_C_loss_flux = 0; grain_C_loss_flux = 0; livestem_C_loss_flux = 0; fineroot_C_loss_flux = 0
      }
    } else {
      # After planting and before emergence
      # Variables to return
      seed_C_to_leaf_C_flux = 0; leaf_C_loss_flux = 0; grain_C_loss_flux = 0; livestem_C_loss_flux = 0; fineroot_C_loss_flux = 0
    } 
    
    # Foliage loss during the reproductive stage
    # In CLM4.5, reaching reproductive stage associates with foliage loss, but this is not the case in JULES (also from field observations).
    # In JULES, foliage loss starts at the midway between reaching reproductive stage and maturity GDD-wise (i.e., DVI >= 1.5).
    if (crop_leaf_sen_scheme == 'CLM4.5' && GDD_T2m >= GDDrepr && GDD_T2m < GDDmat) {
      # leaf senescence rate for every time step during the reproductive stage (unit: gC m^-2 s^-1) 
      # leaf_longevity (yr) is a PFT-specific variables read in PFT_surf_data.R
      leaf_C_loss_flux = 1/(leaf_longevity * 86400 * 365) * leafC  
      livestem_C_loss_flux = 0
      # Declare other loss fluxes
      seed_C_to_leaf_C_flux = 0; grain_C_loss_flux = 0; fineroot_C_loss_flux = 0
    } else if (crop_leaf_sen_scheme == 'custom' && !is.na(DVI) && DVI >= leaf_senescence_DVI) {
      # 'leaf_senescence_DVI' is an input from input_TEMIR_crop_extension.R
      
      # Implemented your scheme here...
      # leaf_C_loss_flux = ...
      # livestem_C_loss_flux = ...
      leaf_C_loss_flux = 1/(leaf_longevity * 86400 * 365) * leafC
      livestem_C_loss_flux = 0
      
      # Declare other loss fluxes
      seed_C_to_leaf_C_flux = 0; grain_C_loss_flux = 0; fineroot_C_loss_flux = 0
      
      # # soyFACE project
      # # Leaf senescence rate per unit leaf mass accelerates after flowering
      # day_since_grain_filling = current_jday - grain_filling_jday
      # day_since_grain_filling = ifelse(test = day_since_grain_filling < 0, no = day_since_grain_filling,
      #                              yes = day_since_grain_filling + 365)
      # 
      # senescence_acceleration_rate = 0.00165  # the senescence rate per unit leaf mass increases by 'senescence_sensitivity' per day after flowering (unit: g g^-1 d^-1)
      # 
      # if (O3_damage_flag & O3_damage_scheme == 'Lombardozzi' & O3_sensitivity == 'custom') {
      #   senescence_o3_sensitivity = 0.0283
      #   senescence_o3_factor = 1 + CUO_grain_filling * senescence_o3_sensitivity
      # } else {
      #   senescence_o3_factor = 1
      # }
      # 
      # senescence_rate_per_unit_mass = senescence_acceleration_rate * day_since_grain_filling * senescence_o3_factor
      # 
      # leaf_C_loss_flux = (senescence_rate_per_unit_mass / 86400) * leafC
      # 
      # # livestem mass follow leaf senescence in CLM4.5 - a first order ODE
      # # stem turnover rate = 0.00623 d^-1 (derived from soyFACE); or 0.0105 (after DVI >= 1.4)
      # livestem_C_loss_flux = livestemC * (0.00623 / 86400)
    }
    
    
  } else {
      # Crops not planted
      days_since_planting = NA
      seed_C_to_leaf_C_flux = 0; leaf_C_loss_flux = 0; grain_C_loss_flux = 0; livestem_C_loss_flux = 0; fineroot_C_loss_flux = 0
      DVI = NA
  }
  
  if (crop_living_flag) {
      cat(paste("Day of year = ", current_jday, "\nDay since planting = ", days_since_planting, 
                "\nGDDT2m = ", signif(GDD_T2m,4), " (req = ", signif(GDDmat,4),' and ', signif(GDDrepr,4), ")", 
                "\nGDDTsoil = ", signif(GDD_Tsoil,3), " (req = ", signif(GDDemer, 3),")", sep = ""))
      print("")
  }
  
  # List of outputs
  # Fluxes: seedC_to_leafC, leafC_loss, grainC_loss, livestemC_loss, finerootC_loss
  # Phenology flags: crop_living_flag, crop_planting_flag, leaf_emer_flag, grain_filling_flag, harvest_flag
  # Phenology date:  planting_jday, harvest_jday
  # Phenology GDD: GDD_T_2m, GDD_T_soil
  # C pools and other physiology variables: leaf_C, fineroot_C, grain_C, livestem_C, LAI, SAI
  output = list(seedC_to_leafC_flux = seed_C_to_leaf_C_flux, leafC_loss_flux = leaf_C_loss_flux, grainC_loss_flux = grain_C_loss_flux, livestemC_loss_flux = livestem_C_loss_flux, finerootC_loss_flux = fineroot_C_loss_flux,
                croplive_flag = crop_living_flag, cropplant_flag = crop_planting_flag, leafemergence_flag = leaf_emer_flag, grainfill_flag = grain_filling_flag, har_flag = harvesting_flag,
                planting_julianday = planting_jday, grain_filling_julianday = grain_filling_jday, harvesting_julianday = harvest_jday,
                GDD_T2m = GDD_T2m, GDD_Tsoil = GDD_Tsoil, GDDmat = GDDmat, GDDrepr = GDDrepr, GDDemer = GDDemer, DVI = DVI,
                leafC = leafC, grainC = grainC, finerootC = finerootC, livestemC = livestemC, LAI_out = LAI, SAI_out = SAI)
  
  return(output)
  
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

f_crop_GDD_update = function(GDD_T_2m_prev, GDD_T_soil_prev, today_T_soil, today_T_2m, 
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
  } else if (GDD_calculation_method == 'custom') {
    # Implement your scheme here....
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
