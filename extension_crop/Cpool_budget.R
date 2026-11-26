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
                            is_maize, is_soybean, is_springwheat, is_winterwheat, is_rice = FALSE,
                            grain_filling_flag, harvesting_flag, DVI, astem,
                            leaf_C_prev, leaf_C_biomass_partitioning_flux_gCm2s1, leaf_C_loss_flux_gCm2s1, seedC_to_leafC_flux_gCm2s1 = NULL,
                            livestem_C_prev, livestem_C_biomass_partitioning_flux_gCm2s1, livestem_C_loss_flux_gCm2s1,
                            livestem_resv_C_prev, livestem_resv_C_biomass_partitioning_flux_gCm2s1, livestem_resv_C_loss_flux_gCm2s1 = NULL, 
                            deadstem_C_prev, deadstem_C_biomass_partitioning_flux_gCm2s1, deadstem_C_loss_flux_gCm2s1,
                            livecoarseroot_C_prev, livecoarseroot_C_biomass_partitioning_flux_gCm2s1, livecoarseroot_C_loss_flux_gCm2s1,
                            deadcoarseroot_C_prev, deadcoarseroot_C_biomass_partitioning_flux_gCm2s1, deadcoarseroot_C_loss_flux_gCm2s1,
                            fineroot_C_prev, fineroot_C_biomass_partitioning_flux_gCm2s1, fineroot_C_loss_flux_gCm2s1,
                            grain_C_prev, grain_C_biomass_partitioning_flux_gCm2s1, grain_C_loss_flux_gCm2s1,
                            leaf_frost_loss_flux_gCm2s1 = 0, winter_carbon_state = NULL) {
  # Winter transfers are accounted explicitly: damaged tissue is litter, never
  # JULES leaf-to-grain retranslocation. Keep the legacy summer budget intact.
  if (is_crop && is_winterwheat) {
    return(f_winter_wheat_Cpool_budget(
      pools = c(leaf = leaf_C_prev, stem = livestem_C_prev, reserve = livestem_resv_C_prev,
                deadstem = deadstem_C_prev, livecoarseroot = livecoarseroot_C_prev,
                deadcoarseroot = deadcoarseroot_C_prev, root = fineroot_C_prev, grain = grain_C_prev),
      allocation = c(leaf = leaf_C_biomass_partitioning_flux_gCm2s1,
                     stem = livestem_C_biomass_partitioning_flux_gCm2s1,
                     reserve = if (is.na(livestem_resv_C_biomass_partitioning_flux_gCm2s1)) 0 else livestem_resv_C_biomass_partitioning_flux_gCm2s1,
                     deadstem = deadstem_C_biomass_partitioning_flux_gCm2s1,
                     livecoarseroot = livecoarseroot_C_biomass_partitioning_flux_gCm2s1,
                     deadcoarseroot = deadcoarseroot_C_biomass_partitioning_flux_gCm2s1,
                     root = fineroot_C_biomass_partitioning_flux_gCm2s1,
                     grain = grain_C_biomass_partitioning_flux_gCm2s1),
      losses = c(leaf = leaf_C_loss_flux_gCm2s1, stem = livestem_C_loss_flux_gCm2s1,
                 reserve = if (is.null(livestem_resv_C_loss_flux_gCm2s1)) 0 else livestem_resv_C_loss_flux_gCm2s1,
                 deadstem = deadstem_C_loss_flux_gCm2s1,
                 livecoarseroot = livecoarseroot_C_loss_flux_gCm2s1,
                 deadcoarseroot = deadcoarseroot_C_loss_flux_gCm2s1,
                 root = fineroot_C_loss_flux_gCm2s1, grain = grain_C_loss_flux_gCm2s1),
      seed_flux = if (is.null(seedC_to_leafC_flux_gCm2s1)) 0 else seedC_to_leafC_flux_gCm2s1,
      frost_flux = leaf_frost_loss_flux_gCm2s1, carbon_state = winter_carbon_state,
      harvesting = harvesting_flag, DVI = DVI, astem = astem))
  }
  
    
  # Constants and parameters:
  second_per_day = 86400
  natural_PFT_mort_rate_CLM = 0/(365*86400)   # currently set to 0...
  retrans_DVI_JULES = 1.5
  
  # Carbon pools in the previous day (unit: gC m^-2)
  leaf_C = leaf_C_prev
  livestem_C = livestem_C_prev
  livestem_resv_C = livestem_resv_C_prev
  deadstem_C = deadstem_C_prev
  livecoarseroot_C = livecoarseroot_C_prev
  deadcoarseroot_C = deadcoarseroot_C_prev
  fineroot_C = fineroot_C_prev
  grain_C = grain_C_prev 
  
  # Updating carbon pools during the emergence of crops (excluding generic crops)
  if (is_crop) {
    if (!is.na(seedC_to_leafC_flux_gCm2s1)) {leaf_C = leaf_C + seedC_to_leafC_flux_gCm2s1 * second_per_day}
  }
  
  # Updating carbon pools with fluxes (unit: gC m^-2 s^-1) from the allocation of assimilated carbon
  leaf_C = leaf_C + leaf_C_biomass_partitioning_flux_gCm2s1 * second_per_day
  livestem_C = livestem_C + livestem_C_biomass_partitioning_flux_gCm2s1 * second_per_day
  livestem_resv_C = livestem_resv_C + livestem_resv_C_biomass_partitioning_flux_gCm2s1 * second_per_day
  deadstem_C = deadstem_C + deadstem_C_biomass_partitioning_flux_gCm2s1 * second_per_day
  livecoarseroot_C = livecoarseroot_C + livecoarseroot_C_biomass_partitioning_flux_gCm2s1 * second_per_day
  deadcoarseroot_C = deadcoarseroot_C + deadcoarseroot_C_biomass_partitioning_flux_gCm2s1 * second_per_day
  fineroot_C = fineroot_C + fineroot_C_biomass_partitioning_flux_gCm2s1 * second_per_day
  grain_C = grain_C + grain_C_biomass_partitioning_flux_gCm2s1 * second_per_day
  
  # Updating carbon storage pools from the allocation of assimilated carbon (reserved for deciduous PFTs)
  # leaf_C_storage = leaf_C_storage + leaf_C_storage_biomass_partitioning_flux_gCm2s1 * second_per_day
  # ...
  # ...
  

  # Updating carbon pools from phenological lost fluxes (unit: gC m^-2 s^-1)
  leaf_C = leaf_C - leaf_C_loss_flux_gCm2s1 * second_per_day
  livestem_C = livestem_C - livestem_C_loss_flux_gCm2s1 * second_per_day
  livestem_resv_C = livestem_resv_C - livestem_resv_C_loss_flux_gCm2s1 * second_per_day
  deadstem_C = deadstem_C - deadstem_C_loss_flux_gCm2s1 * second_per_day
  livecoarseroot_C = livecoarseroot_C - livecoarseroot_C_loss_flux_gCm2s1 * second_per_day
  deadcoarseroot_C = deadcoarseroot_C - deadcoarseroot_C_loss_flux_gCm2s1 * second_per_day
  fineroot_C = fineroot_C - fineroot_C_loss_flux_gCm2s1 * second_per_day
  grain_C = grain_C - grain_C_loss_flux_gCm2s1 * second_per_day
 
  # Crop retranslocation
  if (is_crop) {
    if (crop_transloc_scheme == 'JULES') {
	crop_retrans_result = f_crop_retranslocation_JULES(is_maize = is_maize, is_springwheat = is_springwheat, is_winterwheat = is_winterwheat, is_soybean = is_soybean, is_rice = is_rice,
							   # retrans_DVI = 1.5, retrans_astem = 0.01,
							   grainC = grain_C,
							   leafC = leaf_C, leafC_senesc_flux = leaf_C_loss_flux_gCm2s1,
							   livestem_resvC = livestem_resv_C,
							   astem = astem, DVI = DVI)
  	grain_C = crop_retrans_result$grain_C
	livestem_resv_C = crop_retrans_result$livestem_resv_C

    } else if (crop_transloc_scheme == 'CLM4.5') {
	    # In development, require a nitrogen module...
	    crop_retrans_result = f_crop_retranslocation_CLM()
    } else if (crop_transloc_scheme == 'custom') {
	    # Implement your own scheme...
            crop_retrans_result = f_crop_retranslocation_custom()
    }
  }
    
  # Mortality loss for natural PFTs
  # Only leaf, fineroot and livestem (display pools) have mortality loss
  if (!is_crop) {
    leaf_C = leaf_C - natural_PFT_mort_rate_CLM * leaf_C * second_per_day
    fineroot_C = fineroot_C - natural_PFT_mort_rate_CLM * fineroot_C * second_per_day
    livestem_C = livestem_C - natural_PFT_mort_rate_CLM * livestem_C * second_per_day
  }
  
  # Make sure plant carbon pools are non negative
  leaf_C = max(0, leaf_C)
  livestem_C = max(0, livestem_C)
  livestem_resv_C = max(0, livestem_resv_C)
  deadstem_C = max(0, deadstem_C)
  livecoarseroot_C = max(0, livecoarseroot_C)
  deadcoarseroot_C = max(0, deadcoarseroot_C)
  fineroot_C = max(0, fineroot_C)
  grain_C = max(0, grain_C)
  
  output = list(leaf_C_new = leaf_C, 
                fineroot_C_new = fineroot_C, livecoarseroot_C_new = livecoarseroot_C, deadcoarseroot_C_new = deadcoarseroot_C,
                livestem_C_new = livestem_C, livestem_resv_C_new = livestem_resv_C, deadstem_C_new = deadstem_C,
                grain_C_new = grain_C)
  
  return(output)
}

f_winter_wheat_Cpool_budget = function(pools, allocation, losses, seed_flux,
                                      frost_flux, carbon_state = NULL,
                                      harvesting, DVI, astem) {
  # Persistent litter is an accounting reservoir: TEMIR does not yet simulate
  # soil/litter decomposition. Harvested grain is a cumulative export ledger.
  # All incoming fluxes are gC m-2 s-1; reservoirs are gC m-2.
  if (is.null(carbon_state)) carbon_state = list(litter_C = 0, harvested_C = 0, frost_litter_C = 0)
  fields = c('litter_C', 'harvested_C', 'frost_litter_C')
  if (!is.list(carbon_state) || !all(fields %in% names(carbon_state)) ||
      any(!vapply(carbon_state[fields], function(x)
          is.numeric(x) && length(x) == 1L && is.finite(x) && x >= 0, logical(1))))
    stop('Invalid winter-wheat carbon accounting state')
  if (carbon_state$frost_litter_C > carbon_state$litter_C + 1e-9)
    stop('Winter frost litter must be a subset of total litter carbon')
  if (!crop_transloc_scheme %in% c('CLM4.5', 'JULES'))
    stop('Winter-wheat carbon accounting supports CLM4.5 or JULES retranslocation')
  expected = c('leaf', 'stem', 'reserve', 'deadstem', 'livecoarseroot', 'deadcoarseroot', 'root', 'grain')
  for (x in list(pools, allocation, losses)) {
    if (!identical(names(x), expected) || any(!is.finite(x)) || any(x < 0))
      stop('Winter-wheat carbon pools and fluxes must be finite nonnegative values for all eight pools')
  }
  if (length(seed_flux) != 1L || length(frost_flux) != 1L ||
      any(!is.finite(c(seed_flux, frost_flux))) || min(seed_flux, frost_flux) < 0)
    stop('Winter seed/frost fluxes must be finite and nonnegative')
  if (harvesting && (any(allocation != 0) || seed_flux != 0))
    stop('Winter harvest cannot coincide with positive allocation or seed input')
  total_before = sum(pools) + carbon_state$litter_C + carbon_state$harvested_C
  available = pools + allocation*86400
  available['leaf'] = available['leaf'] + seed_flux*86400
  # Harvest takes precedence, so no tissue is removed by frost twice.
  frost_C = if (harvesting) 0 else min(frost_flux*86400, available['leaf'])
  available['leaf'] = available['leaf'] - frost_C
  actual_loss = if (harvesting) available else pmin(losses*86400, available)
  updated = available - actual_loss
  leaf_to_grain = stem_to_grain = 0
  if (!harvesting && crop_transloc_scheme == 'JULES') {
    transfer = f_crop_retranslocation_JULES(
      is_maize = FALSE, is_springwheat = FALSE, is_winterwheat = TRUE,
      is_soybean = FALSE, grainC = unname(updated['grain']), leafC = unname(updated['leaf']),
      leafC_senesc_flux = unname(actual_loss['leaf'])/86400,
      livestem_resvC = unname(updated['reserve']), astem = astem, DVI = DVI)
    leaf_to_grain = if (is.na(transfer$leaf2grain_flx)) 0 else transfer$leaf2grain_flx
    stem_to_grain = if (is.na(transfer$stem2grain_flx)) 0 else transfer$stem2grain_flx
    updated['grain'] = transfer$grain_C
    updated['reserve'] = transfer$livestem_resv_C
  }
  exported = if (harvesting) unname(actual_loss['grain']) else 0
  litter_gain = unname(frost_C + sum(actual_loss) - leaf_to_grain - exported)
  carbon_state$litter_C = carbon_state$litter_C + litter_gain
  carbon_state$harvested_C = carbon_state$harvested_C + exported
  carbon_state$frost_litter_C = carbon_state$frost_litter_C + unname(frost_C)
  # frost_litter_C is a cumulative SUBSET of litter_C, not another reservoir.
  balance = sum(updated) + carbon_state$litter_C + carbon_state$harvested_C -
            total_before - sum(allocation)*86400 - seed_flux*86400
  if (any(updated < -1e-10) || abs(balance) > 1e-9*max(1, total_before, sum(allocation)*86400))
    stop('Winter-wheat carbon balance failed')
  list(leaf_C_new = unname(updated['leaf']), fineroot_C_new = unname(updated['root']),
       livecoarseroot_C_new = unname(updated['livecoarseroot']),
       deadcoarseroot_C_new = unname(updated['deadcoarseroot']),
       livestem_C_new = unname(updated['stem']), livestem_resv_C_new = unname(updated['reserve']),
       deadstem_C_new = unname(updated['deadstem']), grain_C_new = unname(updated['grain']),
       winter_carbon_state = carbon_state, litter_C_new = carbon_state$litter_C,
       harvested_C_total = carbon_state$harvested_C,
       leaf_frost_to_litter_flux = unname(frost_C)/86400,
       total_litter_input_flux = litter_gain/86400,
       harvest_grain_export_flux = exported/86400,
       leaf_to_grain_C = leaf_to_grain, stem_to_grain_C = stem_to_grain,
       carbon_balance_error = unname(balance))
}

