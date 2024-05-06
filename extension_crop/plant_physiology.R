################################################################################
### Module for calculating the physical structure of vegetation
################################################################################

# TODO
# Check if there is a PFT category represents PFT#10 - #12
# Is it possible not to use ipft to select a PFT? Code is less readable.

################################################################################
### Functions:
################################################################################

f_vegetation_structure = function(is_crop, is_evergreen, is_stress_decid, is_season_decid, is_woody, ipft,
                                  slatop, dsladlai, laimx, ztopmx,
                                  leafC, deadstemC, tlai = LAI, tsai = SAI, 
                                  peak_lai_flag, harvesting_flag, crop_living_flag,
                                  CUO = NA) {
   
   # CUO argument in the function call: only for in the soyFACE project. Not include in the published version?
  
   # Constants and parameters:
    taper = 200            # Height:radiius ratio of wood PFTs
    stocking = 0.1         # Number density of vegetation (individual m^-2)
    dwood = 2.5e5          # Wood density (gC m^-2)
    dtsmonth = 2592000     # Seconds in a 30-days month
    peak_lai_flag = FALSE  # Flag indicates that LAI reaches the limit for crops (applicable to simulated crops and CLM4.5 phenlogy only)
    
    # using LAI and SAI of the current time step to update SAI later
    LAI_old = tlai
    SAI_old = tsai

    # print(paste0('[physiology] LAI and SAI = ', signif(LAI_old, digits = 3),'  ', signif(tsai_old, digits = 3)))

    # Calculation of LAI
    if (LAI_scheme == 'CLM4.5') {
      if (dsladlai > 0) {
        # Specific leaf area increases with LAI (trees, shrubs)
        LAI_new = slatop * exp(leafC * dsladlai - 1) / dsladlai
      } else {
        # Crop, grass PFTs
        LAI_new = slatop * leafC
        if (is_crop && !is_stress_decid) {
          # The LAI cap in the CLM4.5 crop model
          peak_lai_flag = if (limit_crop_LAI_flag && LAI_new > laimx) TRUE else FALSE
        }
      }
    } else if (LAI_scheme == 'custom') {
        # Implement your scheme here...
      
        # # soyFACE project:
        # # modeling SLA as function of CO2 concentration and O3 uptake for soybean
        # # SLA = SLA_375ppm + a * ([CO2] - 375)
        # # SLA = SLA_noO3 * f(CUO)
        # sla_375 = 0.055695   # from soyFACE data, unit: m^2 gC^-1 yr 2001 - 2008
        # # sla_375 = 0.048783   # from soyFACE data, unit: m^2 gC^-1 yr 2009 - 2010
        # 
        # sla_CO2_sensitivity = - 3.1611e-5 # derive from soyFACE data, for soybean only!!!
        # # sla_f_O3 = 
        # 
        # sla_co2 = sla_375 + sla_CO2_sensitivity * (CO2_conc - 375)
        # 
        # if (O3_damage_flag & O3_damage_scheme == 'Lombardozzi' & O3_sensitivity == 'custom') {
        #    if (is.na(CUO)) {CUO = 0}
        #    sla_o3_factor = 1 + 0.0028 * CUO
        #    sla_o3_factor = max(0.01, min(1, sla_o3_factor))
        # } else {
        #   sla_o3_factor = 1
        # }
        # LAI_new = sla_co2 * sla_o3_factor * leafC
        # rm(list = c('sla_375', 'sla_CO2_sensitivity', 'sla_co2', 'sla_o3_factor'))
    }
    
    LAI_new = max(0, LAI_new)
    
    # SAI calculation
    if (SAI_scheme == 'CLM4.5') {
      if (ipft == 16 || ipft == 17) { # C3 unmanaged crops
        tsai_alpha = 1-1*dt/dtsmonth
        tsai_min = 0.1*0.5  # 0.5 is the scale to match MODIS derived value according to CLM
        SAI_new = max(tsai_alpha*SAI_old+max(tlai_old-tlai,0), tsai_min)
      } else if (!(ipft >= 18 && ipft <= 25)) { 
        # other PFTs except simulated crops
        tsai_alpha = 1-0.5*dt/dtsmonth
        tsai_min = 1*0.5
        SAI_new = max(tsai_alpha*SAI_old+max(LAI_old-LAI_new, 0), tsai_min)
      } else {
        # Simulated crops
        if (harvesting_flag && LAI_new < 1e-4) { # after harvesting, SAI = 0.25
          # After harvesting, SAI = 0.25
          SAI_new = 0.25
        } else if (ipft == 18 || ipft == 19) { 
          # Maize
          SAI_new = 0.1*LAI_new
        } else {
          # Other crops: soybean, spring and winter cereals
          SAI_new = 0.2*LAI_new
        }
      }
    } else if (SAI_scheme == 'custom') {
      # Implement your scheme here...
      # SAI_new = ...
    }
    SAI_new = max(0, SAI_new)
    
    # Canopy height and bottom calculations
    if (canopy_h_scheme == 'CLM4.5') {
      if (!(is_crop && !is_season_decid)) {
        # All vegetation except simulated crops
        if (is_woody) {
          # PFT #10-12: shurbs, others: trees
          taper =  if (ipft >= 10 && ipft <= 12) 10 else 200
          h_top = ((3 * deadstemC * taper^2) / (pi * stocking * dwood))^(1/3)
          h_top = max(h_top,0.01)
          h_bottom = max(0,min(3,htop-1))
        } else { 
          # Grasses and unmanaged crops
          h_top = max(0.25, tlai * 0.25)
          h_top = max(htop,0.01)
          h_bottom = max(0,min(0.05,h_top-0.2))
        }
      } else { 
        # Simulated crops
        if (crop_living_flag){
          h_top = ztopmx * min(tlai/laimx-1,1)^2
          h_top = max(0.05, h_top)
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
    }
    h_top = max(0, h_top)
    h_bottom = max(0, h_bottom)
    
    output = list(tlai = LAI_new, tsai = SAI_new, canopy_top = h_top, canopy_bottom = h_bottom, peak_lai_flag = peak_lai_flag)
  
    return(output)
}

