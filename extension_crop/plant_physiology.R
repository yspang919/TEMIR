################################################################################
### Module for calculating the physical structure of vegetation
################################################################################

################################################################################
### Functions:
################################################################################

f_vegetation_structure = function(is_crop, is_shrub, is_evergreen, is_stress_decid, is_season_decid, is_woody,
                                  is_managed_crop, is_maize, is_soybean, 
                                  slatop, dsladlai, laimx, ztopmx,
                                  leafC, livestemC, deadstemC, 
                                  h_top, tlai = LAI, tsai = SAI, 
                                  peak_lai_flag, harvesting_flag, crop_living_flag, crop_DVI,
                                  CUO = NA) {
    
    # CUO argument in the function call: only for in the soyFACE project. Not include in the published version?
    
    # Constants and parameters:
    taper = 200            # Height:radiius ratio of wood PFTs
    stocking = 0.1         # Number density of vegetation (individual m^-2)
    dwood = 2.5e5          # Wood density (gC m^-2)
    dtsmonth = 2592000     # Seconds in a 30-days month
    peak_lai_flag = FALSE  # Flag indicates that LAI reaches the limit for crops (applicable to simulated crops and CLM4.5 phenlogy only)
    
    # using LAI and SAI of the current time step to update SAI later
    LAI_prev = tlai
    SAI_prev = tsai
    htop_prev = h_top
    

    # Calculation of LAI
    if (LAI_scheme == 'CLM4.5') {
        if (dsladlai > 0) {
            # Specific leaf area increases with LAI (trees, shrubs)
            LAI_new = slatop * exp(leafC * dsladlai - 1) / dsladlai
        } else {
            # Crop, grass PFTs
            LAI_new = slatop * leafC
            
            if (is_managed_crop) {
                # The LAI cap in the CLM4.5 crop model
                peak_lai_flag = if (limit_crop_LAI_flag && LAI_new > laimx) TRUE else FALSE
            }
        }
    } else if (LAI_scheme == 'custom') {
        # Implement your scheme here...
        # New scheme
        # LAI = LAI_base * f_phen * f_CO2 * f_O3
        if (is_managed_crop) {
            if (!is.na(crop_DVI) && crop_DVI >= 0 && crop_DVI <= 2) {
                # SLA (JULES): gamma*(DVI+0.06)^delta
                # SLA_base = gamma; f_phen = (DVI + 0.06)^delta
                if (is_maize) {SLA_base = 17.6; SLA_delta = -0.33}   # Willams et al. 2017 values 
                if (is_soybean) {SLA_base = 24.0; SLA_delta = 0.15}   # Leung et al. 2020 values
                f_CO2 = 1
                f_O3 = 1
                f_phen = (crop_DVI+0.06)^SLA_delta
                LAI_new = leafC * (SLA_base/1000/0.45 * f_phen * f_CO2 * f_O3)   # convert the JULES-based SLA unit (m2 kg-1) to CLM-based SLA unit (m2 gC-1), assume 45% C content

            } else {
                LAI_new = 0
            }
        }
    }
    
    LAI_new = max(0, LAI_new)
    # SAI calculation
    if (SAI_scheme == 'CLM4.5') {
        if (!is_managed_crop) {
            if (is_crop) {
                # generic crops
                tsai_alpha = 1-1*dt/dtsmonth
                tsai_min = 0.1*0.5  # 0.5 is the scale to match MODIS derived value according to CLM
                SAI_new = max(tsai_alpha*SAI_prev+max(LAI_prev-LAI_new,0), tsai_min)
            } else {
                # other PFTs
                tsai_alpha = 1-0.5*dt/dtsmonth
                tsai_min = 1*0.5
                SAI_new = max(tsai_alpha*SAI_prev+max(LAI_prev-LAI_new, 0), tsai_min)
            }
        } else {
            # maize soybean wheats
            if (harvesting_flag && LAI_new < 1e-4) {
                # After harvesting, SAI = 0.25
                SAI_new = 0.25
            } else if (is_maize) { 
                SAI_new = 0.1*LAI_new
            } else {
                # soybean and wheats
                SAI_new = 0.2*LAI_new
            }
            
        } 
    } else if (SAI_scheme == 'custom') {
        # Implement your scheme here...
    }
    
    SAI_new = max(0, SAI_new)
    
    # Canopy height and bottom calculations
    if (canopy_h_scheme == 'CLM4.5') {
        if (!is_managed_crop) {
            if (is_woody) {
                taper =  if (is_shrub) 10 else 200
                h_top = ((3 * deadstemC * taper^2) / (pi * stocking * dwood))^(1/3)
                h_top = max(h_top,0.01)
                h_bottom = max(0,min(3,h_top-1))
            } else { 
                # Grasses and unmanaged crops
                h_top = max(0.25, tlai * 0.25)
                h_top = max(h_top,0.01)
                h_bottom = max(0,min(0.05,h_top-0.2))
            }
        } else { 
            # Simulated crops
            if (crop_living_flag) {
                h_top = ztopmx * min((tlai/(laimx-1))^2, 1)
                h_top = max(0.05, htop_prev, h_top, na.rm = TRUE)    # prevent canopy height decreases during leaf senescence
                h_bottom = 0.02  
            } else {
                h_top = 0
                h_bottom = 0
            }
        }
    } else if (canopy_h_scheme == 'custom') {
        # Implement your scheme here....
        # h_top = ....
        # h_bottom = ...
        if (!is_managed_crop) {
            # natural vegetations
            # temporary... 
            stop('Missing custom canopy height calculation scheme in f_vegetation_structure.R for natural vegetation simulation....')
        } else {
            # using stemC to derive crop height similar to JULES
            # height (in m) = kappa*(stem_mass (in kg m-2))^lambda
            # parameters derived from several FLUXNET site
            
            if (is_soybean) {h_kappa = 1.3; h_lambda = 0.41}    # JULES Leung et al. kappa = 1.9; lambda = 0.47.
            if (is_maize) {h_kappa = 3.2; h_lambda = 0.435}     # JULES Willams et al. kappa = 3.6; lambda = 0.38 at US Mead Fluxnet
            
            if (crop_living_flag) {
                # need to convert stemC (gC m-2) to biomass (kg m-2),  assume 45% C content
                # also ensure canopy height doesn't decrease when stemC decreases during retranslocation (when JULES retranslocation is used)
                h_top = max(0.05, htop_prev, h_kappa*(livestemC/1000/0.45)^h_lambda, na.rm = TRUE)  # minimum height of 0.05m follows CLM 
                h_bottom = 0.02   # follow CLM
            } else {
                h_top = 0
                h_bottom = 0
            }
        }
    }
    
    # prevent negative canopy height/bottom
    h_top = max(0, h_top)
    h_bottom = max(0, h_bottom)
    
    output = list(tlai = LAI_new, tsai = SAI_new, canopy_top = h_top, canopy_bottom = h_bottom, peak_lai_flag = peak_lai_flag)
    
    return(output)
}

