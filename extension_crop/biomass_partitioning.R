################################################################################
### Module for calculating the allocation fluxes of assimilated carbon
################################################################################

################################################################################
### Functions:
################################################################################

f_evergreen_allocation_fluxes = function(){
  # reserved for natural vegetation model
}

f_seasonal_deciduous_allocation_fluxes = function(){
  # reserved for natural vegetation model 
}

f_stress_deciduous_allocation_fluxes = function(){
  # reserved for natural vegetation model
}

f_crop_allocation_fluxes = function(A_can_umolm2s1, mr_total, gr_fraction = 0.3,
                                    is_maize, is_springwheat, is_winterwheat, is_soybean,
                                    GDDmat, GDD_T2m, GDD_Tsoil, GDDemer, GDDrepr, DVI_JULES, 
                                    crop_living_flag, peak_lai_flag, grain_filling_flag, 
                                    astem_leafem, aleaf_leafem, aleaf, astem,
                                    bfact, arooti, arootf, astemf, declfact, allconss, aleaff, allconsl, lfemerg, fleafi){
    
    # Convert A_can (GPP) from umol m^-2 s^-1 to gC m^-2 s^-1
    A_can_gCm2s1 = A_can_umolm2s1 * 12.011e-6
    
    # Growth respiration = fraction * (GPP - maintenance respirations)
    growth_respiration = max(0, (A_can_gCm2s1 - mr_total) * gr_fraction)
    NPP_gCm2s1 = A_can_gCm2s1 - growth_respiration - mr_total
    
    # Ensure allocation fluxes (or NPP) are positive
    NPP_gCm2s1 = max(0, NPP_gCm2s1)

      if (crop_C_partit_scheme == 'custom') {
        # Implement your own scheme here....
        
      } else if (crop_C_partit_scheme == 'JULES') {
        
          # Calculate allocation coefficients following the method presented in JULES-crop (Osborne et al. 2015)
          JULES_crop_alloc = f_get_JULES_crop_allocation_coefficients(is_maize, is_springwheat, is_winterwheat, is_soybean, 
                                                                      DVI_crop = DVI_JULES, croplive_flag = crop_living_flag)
          astem = JULES_crop_alloc$p_stem
          aleaf = JULES_crop_alloc$p_leaf
          aroot = JULES_crop_alloc$p_root
          agrain = JULES_crop_alloc$p_harv
          
        } else if (crop_C_partit_scheme == 'CLM4.5'){
          
          # Calculate allocation coefficients following the method in CLM4.5
          CLM45_crop_alloc = f_get_CLM_crop_allocation_coefficients(a_stem_leafem = astem_leafem, a_leaf_leafem = aleaf_leafem, a_leaf = aleaf, a_stem = astem,
                                                                     GDDT2m = GDD_T2m, GDDTsoil = GDD_Tsoil, GDD_mat = GDDmat, GDD_repr = GDDrepr, GDD_emer = GDDemer,
                                                                     crop_live_flag = crop_living_flag, peak_LAI_flag = peak_lai_flag, grain_fill_flag = grain_filling_flag,
                                                                     bfact = bfact, arooti = arooti, arootf = arootf, astemf = astemf, declfact = declfact, allconss = allconss, aleaff = aleaff, allconsl = allconsl, lfemerg = lfemerg, fleafi = fleafi)
          astem = CLM45_crop_alloc$a_stem
          aleaf = CLM45_crop_alloc$a_leaf
          aroot = CLM45_crop_alloc$a_root
          agrain = CLM45_crop_alloc$a_grain
          astem_leafem = CLM45_crop_alloc$a_stem_leafemergence
          aleaf_leafem = CLM45_crop_alloc$a_leaf_leafemergence
        }
      
    # Allocation fluxes to different plant parts
    leaf_carbon_partitioning_flux = NPP_gCm2s1 * aleaf
    fineroot_carbon_partitioning_flux = NPP_gCm2s1 * aroot
    grain_carbon_partitioning_flux = NPP_gCm2s1 * agrain
    # Livestem has a reserved component for retranslocation in JULES crops
    if (crop_C_partit_scheme == 'JULES') {
        stem_resv_frac = 0
        if (is_maize) {stem_resv_frac = 0.12}  # From Willams et al. (2017) recalibrated value, old value in Osborne: 0.35
        if (is_soybean) {stem_resv_frac = 0.26} # From Leung et al. (2020) recalibrated value
        if (is_springwheat || is_winterwheat) {stem_resv_frac = 0.40} # From Osborne et al. (2015) default value
        livestem_carbon_partitioning_flux = NPP_gCm2s1 * astem * (1 - stem_resv_frac)
        livestem_resv_carbon_partitioning_flux = NPP_gCm2s1 * astem * (stem_resv_frac)
    } else {
        livestem_carbon_partitioning_flux = NPP_gCm2s1 * astem
        livestem_resv_carbon_partitioning_flux = NA
    }
    
    # # Allocation fluxes to plant parts that are only available for deciduous PFTs
    # # Carbon is stored in these pools until next growing season 
    # leaf_storage_carbon_partitioning_flux = NPP_gCm2s1 * a_leaf 
    # fineroot_storage_carbon_partitioning_flux = NPP_gCm2s1 * a_root
    # livestem_storage_carbon_partitioning_flux = NPP_gCm2s1 * a_stem
    # grain_storage_carbon_partitioning_flux = NPP_gCm2s1 * a_grain
    
    output = list(daily_mean_GPP = A_can_gCm2s1,
                  daily_mean_NPP = NPP_gCm2s1,
                  leaf_carbon_partitioning_flux_gCm2s1 = leaf_carbon_partitioning_flux, 
                  fineroot_carbon_partitioning_flux_gCm2s1 = fineroot_carbon_partitioning_flux, 
                  livestem_carbon_partitioning_flux_gCm2s1 = livestem_carbon_partitioning_flux, 
                  livestem_resv_carbon_partitioning_flux_gCm2s1 = livestem_resv_carbon_partitioning_flux,
                  deadstem_carbon_partitioning_flux_gCm2s1 = 0, 
                  livecoarseroot_carbon_partitioning_flux_gCm2s1 = 0,
                  deadcoarseroot_carbon_partitioning_flux_gCm2s1 = 0,
                  grain_carbon_partitioning_flux_gCm2s1 = grain_carbon_partitioning_flux,
                  astem = astem,
                  aleaf = aleaf,
                  aroot = aroot,
                  arepr = agrain,
                  astem_em = ifelse(test = exists("astem_leafem"), yes = astem_leafem, no = NA),
                  aleaf_em = ifelse(test = exists("aleaf_leafem"), yes = aleaf_leafem, no = NA)
                  )
    
    return(output)
    
}



f_get_CLM_crop_allocation_coefficients = function(a_stem_leafem, a_leaf_leafem, a_leaf, a_stem,
                                              crop_live_flag, peak_LAI_flag, grain_fill_flag,
                                              GDDT2m, GDDTsoil, GDD_mat, GDD_repr, GDD_emer,
                                              bfact, arooti, arootf, astemf, declfact, allconss, aleaff, allconsl, lfemerg, fleafi,
                                              current_date){
    

    if (crop_live_flag) {
        if (GDDTsoil >= GDD_emer && GDDT2m < GDD_repr) {
            # In the vegetative stage (after emergence and before reproductive stage)
            if (limit_crop_LAI_flag && peak_LAI_flag) {
                # If LAI reaches its cap, stop allocating carbon to leaf (only when the cap is enabled)
                a_leaf = 0; astem = 0; aroot = 1; agrain = 0
            } else {
                # Follow the description in the CLM
                a_root = arooti - (arooti - arootf)*(GDDT2m/GDD_mat)
                a_leaf = (1-a_root)*(fleafi * (exp(-bfact) - exp(-bfact * GDDT2m/GDD_repr)) / (exp(-bfact)-1))
                a_stem = 1-a_root-a_leaf
                a_grain = 0
            }
          # Record values of a_stem and a_leaf, values just before leaving the vegetative stage will be used in the reproductive stage
          a_stem_leafem = a_stem; a_leaf_leafem = a_leaf
        } else if (GDDT2m >= GDD_repr && GDDT2m < GDD_mat) { 
          # In the reproductive stage
          # Root allocation
          a_root = arooti - (arooti - arootf)*(GDDT2m/GDD_mat)
          # Leaf allocation
          if (a_leaf_leafem <= aleaff){
            a_leaf = a_leaf_leafem
          } else {
            a_leaf = max(aleaff, min(1, a_leaf * (1-(GDDT2m-GDD_repr)/(GDD_mat*declfact-GDD_repr))^allconsl))
          }
          # Stem allocation
          if (a_stem_leafem <= astemf){
            a_stem = a_stem_leafem
          } else {
            a_stem = max(astemf,min(1, a_stem * (1-(GDDT2m-GDD_repr)/(GDD_mat*declfact-GDD_repr))^allconss))
          }
          # Grain allocation
          a_grain = 1-a_root-a_leaf-a_stem
        } else {
          # Before emergence, after planting
          a_leaf = 0; a_stem = 0; a_root = 0; a_grain = 0    
        }
    } else {
      # After harvesting, before planting
      a_leaf = 0; a_stem = 0; a_root = 0; a_grain = 0
    }
  output = list(a_leaf = a_leaf, a_stem = a_stem, a_root = a_root, a_grain = a_grain, a_stem_leafemergence = a_stem_leafem, a_leaf_leafemergence = a_leaf_leafem)
  return(output)
}

f_get_JULES_crop_allocation_coefficients = function(is_maize, is_springwheat, is_winterwheat, is_soybean, 
                                                    DVI_crop, croplive_flag){
    
    # PFT specific parameters for allocation coefficients in JULES, Osborne et al. (2015)
    if (is_soybean) {
        # Osborne et al. (2015) default
        # a_root_JULES = 20.0; b_root_JULES = -16.5; a_stem_JULES = 18.5; b_stem_JULES = -14.5; a_leaf_JULES = 19.5; b_leaf_JULES = -15.0
      
        # Leung et al. (2017) calibrated with US-Ne2 Ne3
        a_root_JULES = 19.8; b_root_JULES = -15.4; a_stem_JULES = 18.5; b_stem_JULES = -13.2; a_leaf_JULES = 19.2; b_leaf_JULES = -14.3
        
        # New calibrate parameters with multiple sites
        # a_root_JULES = 11.5; b_root_JULES = -9.6; a_stem_JULES = 10.9; b_stem_JULES = -7.8; a_leaf_JULES = 11.8; b_leaf_JULES = -9.0
        
    } else if (is_springwheat || is_winterwheat) {
        # JULES only has 'wheat', doesn't differentiate spring and winter variety 
        a_root_JULES = 18.5; b_root_JULES = -20.0; a_stem_JULES = 16.0; b_stem_JULES = -15.0; a_leaf_JULES = 18.0; b_leaf_JULES = -18.5
    } else if (is_maize) { 
        # Osborne et al. (2015) default
        # a_root_JULES = 13.5; b_root_JULES = -15.5; a_stem_JULES = 12.5; b_stem_JULES = -12.5; a_leaf_JULES = 13.0; b_leaf_JULES = -14.0
        
        # Willams et al. (2017) calibrated with US-Ne1 Ne2 Ne3
        a_root_JULES = 13.5; b_root_JULES = -15.0; a_stem_JULES = 12.1; b_stem_JULES = -12.1; a_leaf_JULES = 13.1; b_leaf_JULES = -14.1
        
        # New calibrate parameters with multiple sites
        # a_root_JULES = 2.95; b_root_JULES = -4.50; a_stem_JULES = 4.15; b_stem_JULES = -3.99; a_leaf_JULES = 4.11; b_leaf_JULES = -4.50
    }
    
    # Allocation coefficients
    if (croplive_flag) {
        
        if(!is.na(DVI_crop) && DVI_crop >= 0){
            p_root = exp(a_root_JULES + b_root_JULES * DVI_crop) / (exp(a_root_JULES + b_root_JULES * DVI_crop) + exp(a_stem_JULES + b_stem_JULES * DVI_crop) + exp(a_leaf_JULES + b_leaf_JULES * DVI_crop) + 1)
            p_leaf = exp(a_leaf_JULES + b_leaf_JULES * DVI_crop) / (exp(a_root_JULES + b_root_JULES * DVI_crop) + exp(a_stem_JULES + b_stem_JULES * DVI_crop) + exp(a_leaf_JULES + b_leaf_JULES * DVI_crop) + 1)
            p_stem = exp(a_stem_JULES + b_stem_JULES * DVI_crop) / (exp(a_root_JULES + b_root_JULES * DVI_crop) + exp(a_stem_JULES + b_stem_JULES * DVI_crop) + exp(a_leaf_JULES + b_leaf_JULES * DVI_crop) + 1)
            p_harv = 1 / (exp(a_root_JULES + b_root_JULES * DVI_crop) + exp(a_stem_JULES + b_stem_JULES * DVI_crop) + exp(a_leaf_JULES + b_leaf_JULES * DVI_crop) + 1)
        } else {
            # Crops not yet emerged
            p_root = 0; p_leaf = 0; p_stem = 0; p_harv = 0
        }
    } else {
        # Crops not planted
        p_root = 0; p_leaf = 0; p_stem = 0; p_harv = 0
    }
    
    output = list(p_root = p_root, p_leaf = p_leaf, p_stem = p_stem, p_harv = p_harv)
    return(output)
}

f_custom_crop_allocation_coefficients = function(){}
