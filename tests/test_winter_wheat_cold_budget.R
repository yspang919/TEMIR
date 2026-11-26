# Analytical cold response, damage/C conservation and daily coupled restart.
# No external meteorology or packages required.
source('tests/test_winter_wheat_phenology.R')
source('extension_crop/biomass_partitioning.R')
source('extension_crop/Cpool_budget.R')
source('extension_crop/crop_retranslocation.R')
p = f_winter_wheat_parameters()
fails(f_winter_wheat_parameters(list(LT50_cold_limit=-.1)), 'cold/damage')
initial_LT = -.6+.142*p$LT50_cold_limit
cold_state = function(LT = initial_LT) list(LT50 = LT, WDD = 0, survival_sum = 0, survival_days = 0)
update_cold = function(soil, state = cold_state(), vf = 0, vd = 0, leaf = 100, snow = NULL) {
    f_winter_wheat_cold_update(state, soil, vf, vd, leaf, snow, p)
}

# Closed-form discrete response at zero crown temperature, with no freezing
# injury/dehardening/snow stress. Cold tolerance strengthens over the full day.
hardened = update_cold(273.15)
expected = p$LT50_cold_limit + (initial_LT-p$LT50_cold_limit)*(1-.093/48)^48
close(hardened$state$LT50, expected)
stopifnot(hardened$state$LT50 < initial_LT, hardened$leaf_frost_loss_flux == 0,
          hardened$state$WDD == 0)
close(hardened$survival_probability,1)
# Warm dehardening is limited by the unacclimated LT50, without inherited
# hardening rates persisting into the next warm day.
dehardened = update_cold(288.15, cold_state(-20))
coefficient = p$dehardening_coefficient*19^3
close(dehardened$state$LT50, initial_LT+(-20-initial_LT)*(1-coefficient/48)^48)
stopifnot(dehardened$state$LT50 > -20, dehardened$rates['hardening'] == 0,
          dehardened$rates['dehardening'] > 0)
# Completion has an explicit effective-day gate; asymptotic VF==1 is not used.
before_complete = update_cold(273.15, cold_state(-20), .98, 49)
after_complete = update_cold(273.15, cold_state(-20), .98, 50)
stopifnot(before_complete$rates['dehardening'] == 0,
          after_complete$rates['dehardening'] > 0)

# Constant weather gives the same half-hour integration for daily, hourly and
# half-hour input series. The early empirical damage floor is respected.
early = update_cold(258.15)
close(early$leaf_frost_loss_flux*86400, 90)
close(update_cold(rep(258.15, 24)), early)
close(update_cold(rep(258.15, 48)), early)
stopifnot(early$state$LT50 >= p$LT50_cold_limit,
          early$state$LT50 <= initial_LT,
          early$survival_probability >= 0, early$survival_probability <= 1)
close(update_cold(258.15, leaf = 5)$leaf_frost_loss_flux, 0)
# Late accumulated damage can remove more than the early 10 gC floor and
# resets the WDD/average-survival window when a threshold is crossed.
late = update_cold(258.15, vf = .95, vd = 40)
stopifnot(late$damage_events > 0, late$leaf_frost_loss_flux > 0,
          late$leaf_frost_loss_flux*86400 <= 100, late$state$WDD <= 1,
          late$state$survival_sum <= late$state$survival_days)
# Daily means alone miss a cold night: same mean, different injury.
no_night = update_cold(273.15)
cold_night = update_cold(c(rep(263.15, 12), rep(283.15, 12)))
stopifnot(no_night$leaf_frost_loss_flux == 0, cold_night$leaf_frost_loss_flux > 0)
# Snow never reconstructs crown T. Optional respiration stress saturates at
# 12.5 cm; missing snow disables only that stress and is diagnosed explicitly.
no_snow = update_cold(278.15, cold_state(-20))
snow = update_cold(278.15, cold_state(-20), snow = .125)
deep_snow = update_cold(278.15, cold_state(-20), snow = 1)
close(snow, deep_snow)
stopifnot(no_snow$rates['snow_stress'] == 0, snow$rates['snow_stress'] > 0,
          !no_snow$snow_stress_available, snow$snow_stress_available)
fails(update_cold(NA_real_), 'shallow soil')

# Winter phenology uses shallow soil in warm and freezing weather, independent
# of the air temperature. There is no separately overridable crown input.
crop_planting_date_method = 'prescribed-site'
r = advance(20151001)
soil_based = advance(20151002, r, list(T2m = 243.15, T_soil = 278.15, snow_depth = NULL))
close(soil_based$crown_temperature_C, 5)
fails(advance(20151002, r, list(T_soil_subdaily = rep(280,24))), 'must equal the mean')
old = r; old$winter_state$version = 1L
fails(advance(20151002, old), 'schema/version')

pool_names = c('leaf', 'stem', 'reserve', 'deadstem', 'livecoarseroot', 'deadcoarseroot', 'root', 'grain')
pools = setNames(c(100,20,10,0,0,0,30,5), pool_names)
allocation = setNames(c(2,0,0,0,0,0,0,3)/86400, pool_names)
loss = setNames(c(10,0,0,0,0,0,0,0)/86400, pool_names)
crop_transloc_scheme = 'JULES'
budget = f_winter_wheat_Cpool_budget(pools, allocation, loss,
    seed_flux = 1/86400, frost_flux = 20/86400, harvesting = FALSE, DVI = 1.6, astem = .005)
close(budget$leaf_C_new, 73)
close(budget$grain_C_new, 19)
close(budget$livestem_resv_C_new, 9)
close(budget$litter_C_new, 20)
close(budget$winter_carbon_state$frost_litter_C, 20)
close(budget$carbon_balance_error, 0)
without_frost = f_winter_wheat_Cpool_budget(pools, allocation, loss,
    seed_flux = 1/86400, frost_flux = 0, harvesting = FALSE, DVI = 1.6, astem = .005)
close(budget$grain_C_new, without_frost$grain_C_new) # frost never becomes grain
# Harvest takes priority, exports grain, puts other tissue into litter, and
# cannot retranslocate a forced-harvest leaf loss into a new grain pool.
harvest = f_winter_wheat_Cpool_budget(pools, allocation*0, loss,
    seed_flux = 0, frost_flux = 100/86400, harvesting = TRUE, DVI = .5, astem = 0)
close(harvest$grain_C_new, 0); close(harvest$leaf_C_new, 0)
close(harvest$litter_C_new, 160); close(harvest$harvested_C_total, 5)
close(harvest$leaf_frost_to_litter_flux, 0)
close(harvest$carbon_balance_error, 0)
# Overlapping losses are capped to available tissue; carbon is never created
# by max(0,pool) corrections or by excessive retranslocation.
excessive = f_winter_wheat_Cpool_budget(pools, allocation*0, loss*100,
    seed_flux = 0, frost_flux = 20/86400, harvesting = FALSE, DVI = 1.6, astem = .1)
close(excessive$leaf_C_new, 0); close(excessive$litter_C_new, 20)
close(excessive$carbon_balance_error, 0)

# Check the real public budget interface, including CLM's NA reserve flux.
wrapper = function() f_Cpool_budgets(
    is_crop=TRUE,is_evergreen=FALSE,is_stress_decid=FALSE,is_season_decid=FALSE,
    is_maize=FALSE,is_soybean=FALSE,is_springwheat=FALSE,is_winterwheat=TRUE,
    grain_filling_flag=FALSE,harvesting_flag=FALSE,DVI=.5,astem=.5,
    leaf_C_prev=100,leaf_C_biomass_partitioning_flux_gCm2s1=0,
    leaf_C_loss_flux_gCm2s1=0,seedC_to_leafC_flux_gCm2s1=0,
    livestem_C_prev=0,livestem_C_biomass_partitioning_flux_gCm2s1=0,livestem_C_loss_flux_gCm2s1=0,
    livestem_resv_C_prev=0,livestem_resv_C_biomass_partitioning_flux_gCm2s1=NA_real_,livestem_resv_C_loss_flux_gCm2s1=0,
    deadstem_C_prev=0,deadstem_C_biomass_partitioning_flux_gCm2s1=0,deadstem_C_loss_flux_gCm2s1=0,
    livecoarseroot_C_prev=0,livecoarseroot_C_biomass_partitioning_flux_gCm2s1=0,livecoarseroot_C_loss_flux_gCm2s1=0,
    deadcoarseroot_C_prev=0,deadcoarseroot_C_biomass_partitioning_flux_gCm2s1=0,deadcoarseroot_C_loss_flux_gCm2s1=0,
    fineroot_C_prev=0,fineroot_C_biomass_partitioning_flux_gCm2s1=0,fineroot_C_loss_flux_gCm2s1=0,
    grain_C_prev=0,grain_C_biomass_partitioning_flux_gCm2s1=0,grain_C_loss_flux_gCm2s1=0,
    leaf_frost_loss_flux_gCm2s1=10/86400)
crop_transloc_scheme = 'CLM4.5'
wrapped = wrapper()
close(wrapped$leaf_C_new,90); close(wrapped$livestem_resv_C_new,0)
close(wrapped$litter_C_new,10); close(wrapped$carbon_balance_error,0)

# Lu reproductive allocation is anchored, gradually declining, conserves NPP,
# and a lower VF reduces grain allocation without losing rejected carbon.
allocate = function(gdd, vf=1) f_get_winter_wheat_CLM_allocation(gdd, gdd, 1700,680,51,
    TRUE, .5,.15,.05,0,vf)
a = allocate(1000); b = allocate(1000,.5)
close(sum(unlist(a[c('a_leaf','a_stem','a_root','a_grain')])),1)
close(sum(unlist(b[c('a_leaf','a_stem','a_root','a_grain')])),1)
close(b$a_grain,.5*a$a_grain)
close(allocate(1000),a) # repeated calls cannot compound yesterday's decline
stopifnot(allocate(900)$a_stem > allocate(1300)$a_stem,
          allocate(900)$a_grain < allocate(1300)$a_grain)

# Public allocation interface used by simulate_ij: the CLM winter branch
# ignores the summer LAI-cap flag, applies VF, and has a finite reserve flux.
crop_C_partit_scheme = 'CLM4.5'
limit_crop_LAI_flag = TRUE
public_allocate = function(gdd, vf = 1) f_crop_allocation_fluxes(
    A_can_umolm2s1=20,mr_total=0,gr_fraction=.25,
    is_maize=FALSE,is_springwheat=FALSE,is_winterwheat=TRUE,is_soybean=FALSE,
    GDDmat=1700,GDD_T2m=gdd,GDD_Tsoil=1000,GDDemer=51,GDDrepr=680,DVI_JULES=1.5,
    crop_living_flag=TRUE,peak_lai_flag=TRUE,grain_filling_flag=gdd>=680,
    astem_leafem=.5,aleaf_leafem=.15,aleaf=.2,astem=.4,
    bfact=.1,arooti=.3,arootf=0,astemf=.05,declfact=1.05,
    allconss=1,aleaff=0,allconsl=3,lfemerg=.03,fleafi=.425,
    vernalization_factor=vf)
pub = public_allocate(1000,.5)
flux_names = c('leaf_carbon_partitioning_flux_gCm2s1','fineroot_carbon_partitioning_flux_gCm2s1',
    'livestem_carbon_partitioning_flux_gCm2s1','livestem_resv_carbon_partitioning_flux_gCm2s1',
    'grain_carbon_partitioning_flux_gCm2s1')
close(sum(unlist(pub[flux_names])),pub$daily_mean_NPP)
close(pub$livestem_resv_carbon_partitioning_flux_gCm2s1,0)
stopifnot(public_allocate(200)$aleaf>0,public_allocate(200)$aroot<1)
close(pub$arepr,.5*public_allocate(1000)$arepr)
limit_crop_LAI_flag = FALSE

# Coupled phenology + real carbon budget through a cold spell, then harvest.
# Save/load the exact same per-PFT state shape used by the daily driver.
coupled_run = function(restart = FALSE) {
    r = advance(20151001)
    stocks = pools
    crop_transloc_scheme <<- 'JULES'
    for (day in 2:25) {
        soil = if (day < 14) 278.15 else 263.15
        r = suppressWarnings(advance(as.integer(sprintf('201510%02d',day)), r,
            list(T_soil=soil, T_soil_subdaily=rep(soil,24),
                 leafC=unname(stocks['leaf']), livestemC=unname(stocks['stem']),
                 livestemresvC=unname(stocks['reserve']), finerootC=unname(stocks['root']),
                 grainC=unname(stocks['grain']), winter_parameters=list(max_growing_days=23))))
        loss = setNames(c(r$leafC_loss_flux,r$livestemC_loss_flux,r$livestem_resvC_loss_flux,
                         0,0,0,r$finerootC_loss_flux,r$grainC_loss_flux),pool_names)
        budget = f_winter_wheat_Cpool_budget(stocks, allocation*0, loss,
            r$seedC_to_leafC_flux,r$leaf_frost_loss_flux,r$winter_state$carbon,
            r$har_flag,r$DVI,.5)
        stocks = setNames(unlist(budget[c('leaf_C_new','livestem_C_new','livestem_resv_C_new',
            'deadstem_C_new','livecoarseroot_C_new','deadcoarseroot_C_new','fineroot_C_new','grain_C_new')]),pool_names)
        r$winter_state$carbon = budget$winter_carbon_state
        if (day == 18 && restart) {
            path = tempfile(fileext='.RData')
            winter_wheat_state_PFT_lasthd_ijd = list(NULL,r$winter_state)
            save(winter_wheat_state_PFT_lasthd_ijd,file=path)
            e = new.env();load(path,envir=e);unlink(path)
            r$winter_state = e$winter_wheat_state_PFT_lasthd_ijd[[2]]
        }
        close(budget$carbon_balance_error,0)
    }
    list(result=r,budget=budget,stocks=stocks)
}
full = coupled_run(); resumed = coupled_run(TRUE)
stopifnot(identical(full,resumed),full$result$har_flag,all(full$stocks==0),
          full$result$winter_state$carbon$frost_litter_C > 0)
close(full$budget$litter_C_new+full$budget$harvested_C_total,sum(pools)+1)

cat('Winter wheat soil crown, hardening, frost, allocation, C conservation and coupled restart OK\n')
