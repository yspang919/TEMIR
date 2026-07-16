################################################################################
### Module for calculating the carbon fluxes during retranslocation of crops
################################################################################

################################################################################
### Functions:
################################################################################

f_crop_retranslocation_CLM = function(variables) {
    invisible(NULL)
}


f_crop_retranslocation_JULES = function(is_maize, is_springwheat, is_winterwheat, is_soybean,
                                        # retrans_DVI = 1.5, retrans_astem = 0.01,
                                        grainC, 
                                        leafC = NULL, leafC_senesc_flux, 
                                        livestem_resvC, 
                                        astem, DVI,
                                        second_per_day = 86400) {
    
    # defination in JULES
    harvest_DVI = 2
    
    # NOTE:
    # In JULES, leaf to grain retranslocation and leaf senescenece occur at the same time and with the same rate.
    # Leaf senescenece rate in TEMIR is modified to better match the decay of LAI, but the accruacy of leaf to grain retranslocation scheme is not examinated.....
    
    if (is_maize) {
        retrans_DVI = 1.0  # calibrated 1.0 +- 0.2
        retrans_astem = 0.01  # default JULES
    }
    
    if (is_springwheat) {
        retrans_DVI = 1.5 # default JULES
        retrans_astem = 0.01 # default JULES
    }
    
    if (is_winterwheat) {
        retrans_DVI = 1.5 # default JULES 1.5 +- 0.3
        retrans_astem = 0.01 # default JULES
    }
    
    if (is_soybean) {
        retrans_DVI = 1.2  # calibrated
        retrans_astem = 0.01  # default JULES
    }
    
    # Not all C in senesced leaves is transferred to grain. Need some research.... assume efficiency of 1 at the time being....
    leaf2grain_eff = 1
    
    
    if (!is.na(DVI) && DVI >= retrans_DVI && DVI < harvest_DVI) {
        leaf2grain_flux = leafC_senesc_flux * second_per_day * leaf2grain_eff
        # leafC = leafC - leafC_senesc_flux * second_per_day     # is not needed, leaf senescence is calculated outside the function
        grainC = grainC + leaf2grain_flux
    } else {
        leaf2grain_flux = NA
    }
    
    if (!is.na(astem) && !is.na(livestem_resvC) && astem <= retrans_astem && !is.na(DVI) && DVI < harvest_DVI) {
        # rate: 10% d-1, convert to s-1
        stem2grain_flux = (0.1/86400) * livestem_resvC * second_per_day
        livestem_resvC = livestem_resvC - stem2grain_flux
        grainC = grainC + stem2grain_flux
    } else {
        stem2grain_flux = NA
    }
    
    output = list(grain_C = grainC, livestem_resv_C = livestem_resvC,
                  leaf2grain_flx = leaf2grain_flux, stem2grain_flx =  stem2grain_flux)
    
    return(output)
}
