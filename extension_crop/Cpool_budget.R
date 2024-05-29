################################################################################
### Module for calculating the carbon budgets in the vegetation
################################################################################

# TODO
# Check the mortality rate for natural PFTs in CLM4.5
# Check if generic C3 crops have natural mortality rate
################################################################################
### Functions:
################################################################################

f_Cpool_budgets  = function(is_crop, is_evergreen, is_stress_decid, is_season_decid,
                            grain_filling_flag, harvesting_flag, DVI,
                            leaf_C_prev, leaf_C_biomass_partitioning_flux_gCm2s1, leaf_C_loss_flux_gCm2s1, seedC_to_leafC_flux_gCm2s1 = NULL,
                            livestem_C_prev, livestem_C_biomass_partitioning_flux_gCm2s1, livestem_C_loss_flux_gCm2s1,
                            deadstem_C_prev, deadstem_C_biomass_partitioning_flux_gCm2s1, deadstem_C_loss_flux_gCm2s1,
                            livecoarseroot_C_prev, livecoarseroot_C_biomass_partitioning_flux_gCm2s1, livecoarseroot_C_loss_flux_gCm2s1,
                            deadcoarseroot_C_prev, deadcoarseroot_C_biomass_partitioning_flux_gCm2s1, deadcoarseroot_C_loss_flux_gCm2s1,
                            fineroot_C_prev, fineroot_C_biomass_partitioning_flux_gCm2s1, fineroot_C_loss_flux_gCm2s1,
                            grain_C_prev, grain_C_biomass_partitioning_flux_gCm2s1, grain_C_loss_flux_gCm2s1) {
 
 # Constants and parameters:
 second_per_day = 86400
 natural_PFT_mort_rate_CLM = 0/(365*86400)
 retrans_DVI_JULES = 1.5
 
 # Carbon pools in the previous day (unit: gC m^-2)
 leaf_C = leaf_C_prev
 livestem_C = livestem_C_prev
 deadstem_C = deadstem_C_prev
 livecoarseroot_C = livecoarseroot_C_prev
 deadcoarseroot_C = deadcoarseroot_C_prev
 fineroot_C = fineroot_C_prev
 grain_C = grain_C_prev 
  
 # print(paste0('[Cpool addition before] (unit: gC m-2) leaf_C = ', signif(leaf_C, digits = 3), ' livestem_C = ', signif(livestem_C, digits = 3), ' fineroot_C = ', signif(fineroot_C, digits = 3) , ' grain_C = ', signif(grain_C, digits = 3)))
 
 # Updating carbon pools during the emergence of crops (excluding generic crops)
 if (is_crop) {
    if (!is.na(seedC_to_leafC_flux_gCm2s1)) {leaf_C = leaf_C + seedC_to_leafC_flux_gCm2s1 * second_per_day}
 }
 
 # Updating carbon pools with fluxes (unit: gC m^-2 s^-1) from the allocation of assimilated carbon
 leaf_C = leaf_C + leaf_C_biomass_partitioning_flux_gCm2s1 * second_per_day
 livestem_C = livestem_C + livestem_C_biomass_partitioning_flux_gCm2s1 * second_per_day
 deadstem_C = deadstem_C + deadstem_C_biomass_partitioning_flux_gCm2s1 * second_per_day
 livecoarseroot_C = livecoarseroot_C + livecoarseroot_C_biomass_partitioning_flux_gCm2s1 * second_per_day
 deadcoarseroot_C = deadcoarseroot_C + deadcoarseroot_C_biomass_partitioning_flux_gCm2s1 * second_per_day
 fineroot_C = fineroot_C + fineroot_C_biomass_partitioning_flux_gCm2s1 * second_per_day
 grain_C = grain_C + grain_C_biomass_partitioning_flux_gCm2s1 * second_per_day
  
 # Updating carbon storage pools from the allocation of assimilated carbon (reserved for deciduous PFTs)
 # leaf_C_storage = leaf_C_storage + leaf_C_storage_biomass_partitioning_flux_gCm2s1 * second_per_day
 # ...
 # ...
  
 # Updating carbon pools from losses fluxes ((unit: gC m^-2 s^-1) due to phenology
 leaf_C = leaf_C - leaf_C_loss_flux_gCm2s1 * second_per_day
 livestem_C = livestem_C - livestem_C_loss_flux_gCm2s1 * second_per_day
 deadstem_C = deadstem_C - deadstem_C_loss_flux_gCm2s1 * second_per_day
 livecoarseroot_C = livecoarseroot_C - livecoarseroot_C_loss_flux_gCm2s1 * second_per_day
 deadcoarseroot_C = deadcoarseroot_C - deadcoarseroot_C_loss_flux_gCm2s1 * second_per_day
 fineroot_C = fineroot_C - fineroot_C_loss_flux_gCm2s1 * second_per_day
 grain_C = grain_C - grain_C_loss_flux_gCm2s1 * second_per_day
  
 # Updating carbon pools at the reproductive stage (retranslocation, for crops only)
 if (is_crop) {
    if (crop_transloc_scheme == 'JULES') {
       # Translocation in JULES: 100 % of the leaf carbon loss is going to grain after halfway of the reproductive stage GDDT2m-wise (i.e., DVI >= 1.5)
       # Adding a translocation efficiency term here, (probably?) not all C loss are retranslocated
       if (!is.na(DVI) && DVI >= retrans_DVI_JULES) {
         tranloc_flux = leaf_C_loss_flux_gCm2s1
         tranloc_eff = 1
         grain_C = grain_C + (tranloc_flux * tranloc_flux * second_per_day)
       }
    } else if (crop_transloc_scheme == 'CLM4.5') {
       # Retranslocation scheme in CLM4.5 requires a nitrogen model
       stop('CLM4.5 retranslocation scheme is currently not available ')
    } else if (crop_transloc_scheme == 'custom') {
      # Implement your retranslocation scheme here
    }
 }

  # Mortality loss for natural PFTs
  # Only leaf, fineroot and livestem (display pools) have mortality loss
  if (!is_crop) {
    leaf_C = leaf_C - natural_PFT_mort_rate_CLM * leaf_C * second_per_day
    fineroot_C = fineroot_C - natural_PFT_mort_rate_CLM * fineroot_C * second_per_day
    livestem_C = livestem_C - natural_PFT_mort_rate_CLM * livestem_C * second_per_day
  }
 
  # Plant carbon pools are non negative
  leaf_C = max(0, leaf_C)
  livestem_C = max(0, livestem_C)
  deadstem_C = max(0, deadstem_C)
  livecoarseroot_C = max(0, livecoarseroot_C)
  deadcoarseroot_C = max(0, deadcoarseroot_C)
  fineroot_C = max(0, fineroot_C)
  grain_C = max(0, grain_C)
  
  # print(paste0('[Cpool addition final] leaf_C = ', signif(leaf_C, digits = 3), ' livestem_C = ', signif(livestem_C, digits = 3), ' fineroot_C = ', signif(fineroot_C, digits = 3) , ' grain_C = ', signif(grain_C, digits = 3)))
  
  output = list(leaf_C_new = leaf_C, fineroot_C_new = fineroot_C, livestem_C_new = livestem_C, grain_C_new = grain_C, deadstem_C_new = deadstem_C, livecoarseroot_C_new = livecoarseroot_C, deadcoarseroot_C_new = deadcoarseroot_C)
  return(output)
}

