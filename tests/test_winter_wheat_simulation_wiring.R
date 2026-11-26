# Execute the actual daily vegetation and checkpoint blocks from simulate_ij.R
# with synthetic weather/GPP, without running external radiation/met readers.
# This is an integration smoke test, not site validation.
source('code_v2.0/simulate_ij.R')
walk = function(node, predicate) {
    if (missing(node)) return(list())
    result = list()
    if (predicate(node)) result = list(node)
    if (is.call(node) || is.expression(node)) {
        children = if (is.call(node)) as.list(node)[-1L] else as.list(node)
        for (child in children) result = c(result, walk(child, predicate))
    }
    result
}
is_head = function(x, name) is.call(x) && identical(x[[1L]], as.name(name))
one = function(nodes) {stopifnot(length(nodes)==1L); nodes[[1L]]}
tree = body(f_simulate_ij)
daily = one(walk(tree, function(x) is_head(x,'if') &&
    identical(paste(deparse(x[[2L]]),collapse=''), 'h == 24/dt_hr') &&
    'f_crop_phenology' %in% all.names(x)))
history_init = one(walk(tree, function(x) is_head(x,'if') &&
    identical(x[[2L]],as.name('biogeochem_flag')) &&
    all(c('BGC_temp_var_vec','winter_wheat_state_next') %in% all.names(x))))
history_assign = one(walk(tree, function(x) is_head(x,'=') &&
    identical(x[[2L]],as.name('output_assign_df')) &&
    is_head(x[[3L]],'rbind.data.frame')))
history_update = one(walk(tree, function(x) is_head(x,'if') &&
    identical(paste(deparse(x[[2L]]),collapse=''), 'biogeochem_flag && h == 24/dt_hr')))
checkpoint = one(walk(tree, function(x) is_head(x,'if') &&
    identical(x[[2L]],as.name('biogeochem_flag')) &&
    all(c('temp_env','winter_wheat_state_PFT_lasthd_ijd') %in% all.names(x))))

fixture = function(scheme) {
    e = new.env(parent=globalenv())
    for (file in c('plant_phenology.R','biomass_partitioning.R','Cpool_budget.R',
                   'crop_retranslocation.R','plant_physiology.R'))
        sys.source(file.path('extension_crop',file),e)
    values = list(biogeochem_flag=TRUE, h=24,dt_hr=1,dt=3600,
        evergreen_flag=FALSE,stress_decid_flag=FALSE,season_decid_flag=FALSE,
        shrub_flag=FALSE,woody_flag=FALSE,managed_crop_flag=TRUE,crop_flag=TRUE,
        maize_flag=FALSE,springwheat_flag=FALSE,winterwheat_flag=TRUE,soybean_flag=FALSE,rice_flag=FALSE,
        single_site_flag=TRUE,use_site_obs_flag=FALSE,FLUXNET_flag=FALSE,using_site_LAI_flag=FALSE,
        crop_planting_date_method='prescribed-site',GDDmat_method='CLM4.5',
        GDDrepr_method='CLM4.5',GDDemer_method='CLM4.5',GDD_calculation_method='JULES',
        crop_C_partit_scheme=scheme,crop_transloc_scheme=scheme,crop_leaf_sen_scheme='CLM4.5',
        LAI_scheme='CLM4.5',SAI_scheme='CLM4.5',canopy_h_scheme='CLM4.5',limit_crop_LAI_flag=FALSE,
        winter_wheat_parameters=list(),emergence_carbon_to_leaf=1,
        T_10d=283.15,Tmin_10d=278.15,Tmin_daily=278.15,
        pftnum=0:24,pftname=as.character(0:24),ipft=22L,i=1L,j=1L,lon=100,lat=30,
        leaf_C=0,livestem_C=0,livestem_resv_C=0,fineroot_C=0,grain_C=0,deadstem_C=0,
        GDDT2m=0,GDDTsoil=0,GDD_mat=0,GDD_repr=0,GDD_emer=0,
        crop_live_flag=FALSE,crop_plant_flag=FALSE,leaf_emergence_flag=FALSE,
        grain_fill_flag=FALSE,harvest_flag=FALSE,peak_LAI_flag=FALSE,
        day_of_planting=NA_real_,day_of_grain_filling=NA_real_,day_of_harvest=NA_real_,day_since_crop_senes=NA_real_,
        GDDmat_map_value=NA_real_,GDDrepr_map_value=NA_real_,GDDemer_map_value=NA_real_,
        GDDmat_max=1700,GDD_base_T=0,GDDT2m_change_max=26,crop_season_length_max=330,
        leaflong=1,avg_T_planting_req=283.15,min_T_planting_req=283.15,
        repr_GDDfraction=.4,emer_GDDfraction=.03,earliest_planting_doy_NH=244,
        latest_planting_doy_NH=365,earliest_planting_doy_SH=91,latest_planting_doy_SH=181,at_NH_flag=TRUE,
        a_stem=0,a_leaf=0,a_root=0,a_repr=0,a_stem_leafem=0,a_leaf_leafem=0,
        b_factor=.1,a_root_initial=.3,a_root_final=0,a_stem_final=.05,GDD_decline_factor=1.05,
        a_stem_alloc_power=1,a_leaf_final=0,a_leaf_alloc_power=3,a_leaf_base=.425,
        sla_top=.03,dsla_dlai=0,LAI_max=6,ztopmax=1,htop=0,LAI=0,SAI=0,mr_daily=0)
    list2env(values,e);e$winter_state=NULL
    e
}
restore = function(e,saved) {
    mapping = c(a_stem='astem',a_leaf='aleaf',a_stem_leafem='astem_leafem',a_leaf_leafem='aleaf_leafem',
        GDDT2m='GDDT2m',GDDTsoil='GDDTsoil',GDD_mat='GDDmat',GDD_repr='GDDrepr',GDD_emer='GDDemer',
        crop_live_flag='crop_live_flag',peak_LAI_flag='peak_LAI_flag',crop_plant_flag='crop_plant_flag',
        leaf_emergence_flag='leaf_emergence_flag',grain_fill_flag='grain_fill_flag',harvest_flag='harvest_flag',
        day_of_planting='planting_jday',day_of_grain_filling='grain_filling_jday',day_of_harvest='harvesting_jday',
        day_since_crop_senes='day_since_crop_senescence',leaf_C='leafC',fineroot_C='frootC',
        livestem_C='livestemC',livestem_resv_C='livestemC_resv',deadstem_C='deadstemC',grain_C='grainC',
        LAI='tlai',SAI='tsai',htop='htop')
    # Some legacy flags and date fields omit _PFT in checkpoint names.
    for (name in names(mapping)) {
        prefix=mapping[[name]]
        key=paste0(prefix,if (prefix %in% c('peak_LAI_flag','leaf_emergence_flag',
            'grain_fill_flag','harvest_flag','planting_jday','grain_filling_jday',
            'harvesting_jday','day_since_crop_senescence')) '' else '_PFT','_lasthd_ijd')
        assign(name,get(key,saved)[e$ipft],e)
    }
    e$winter_state=saved$winter_wheat_state_PFT_lasthd_ijd[[e$ipft]]
}
run = function(scheme,restart=FALSE) {
    e=fixture(scheme);saved=NULL;inputs=0;frost=0;events=0L
    dates=seq(as.Date('2015-10-01'),as.Date('2016-10-02'),by='day')
    for (k in seq_along(dates)) {
        if (!is.null(saved)) restore(e,saved)
        date=dates[k];e$current_date=as.integer(format(date,'%Y%m%d'))
        e$prescribed_planting_date=as.integer(format(as.Date(paste0(format(date,'%Y'),'-10-01')),'%j'))
        soil=if (k %in% 40:55) 263.15 else 278.15
        e$T_daily=if (k %in% 40:55) 263.15 else 283.15
        e$TSOIL1=array(soil,c(1,1,24));e$T_soil_daily=soil
        e$An_daily=if (e$leaf_C>0) 15 else 0
        eval(history_init,e)
        suppressWarnings(eval(daily,e))
        stopifnot(e$crop_phenology$crown_temperature_C==soil-273.15,
                  abs(e$plant_Cpool_budgets$carbon_balance_error)<1e-8,
                  all(is.finite(unlist(e$plant_Cpool_budgets[1:8]))))
        events=events+e$crop_phenology$planted_today
        inputs=inputs+86400*(sum(unlist(e$crop_allocation_fluxes[c(
            'leaf_carbon_partitioning_flux_gCm2s1','fineroot_carbon_partitioning_flux_gCm2s1',
            'livestem_carbon_partitioning_flux_gCm2s1','livestem_resv_carbon_partitioning_flux_gCm2s1',
            'grain_carbon_partitioning_flux_gCm2s1')]))+e$crop_emergence_flux)
        frost=frost+e$plant_Cpool_budgets$leaf_frost_to_litter_flux*86400
        e$alloc_prefix='crop_allocation_fluxes'
        e$output_assign_df=data.frame(output_hist_name='unused',assign_string='NA_real_')
        eval(history_assign,e)
        tryCatch(eval(history_update,e),error=function(err)
            stop('Daily checkpoint history ',e$out_var,': ',conditionMessage(err)))
        e$temp_env=new.env();eval(checkpoint,e);saved=e$temp_env
        stopifnot(identical(saved$winter_wheat_state_PFT_lasthd_ijd[[e$ipft]]$carbon,
                            e$plant_Cpool_budgets$winter_carbon_state))
        if (k==48 && restart) {
            file=tempfile(fileext='.RData');save(list=ls(saved),envir=saved,file=file)
            e=fixture(scheme);saved=new.env();load(file,envir=saved);unlink(file)
        }
    }
    total=sum(unlist(e$plant_Cpool_budgets[1:8]))+
        e$plant_Cpool_budgets$litter_C_new+e$plant_Cpool_budgets$harvested_C_total
    stopifnot(abs(total-inputs)<1e-7,frost>0,events==2L,
              e$plant_Cpool_budgets$harvested_C_total>0,
              e$plant_Cpool_budgets$litter_C_new>=frost)
    list(stocks=unlist(e$plant_Cpool_budgets[1:8]),winter=e$winter_state,
         inputs=inputs,frost=frost)
}
# Exercise the real initial-map block without external files. A winter-only
# run needs no legacy active-biomass map; mixed runs zero only winter layers.
initial_tree=parse('code_v2.0/PFT_surf_data.R')
initial_block=tail(Filter(function(x) is_head(x,'if'),as.list(initial_tree)),1)[[1]]
initial_fixture=function(selected) {
    e=new.env(parent=globalenv())
    list2env(list(biogeochem_flag=TRUE,custom_PFT_enabled=FALSE,sim_PFT=selected,
        lon=c(100,101),lat=30,pftnum=0:24,pftname=as.character(0:24),
        initial_data_dir='not-used/',PFT_df=data.frame(PFT_number=0:24,
            PFT_description=replace(rep('other',25),22:23,
                c('winter_temperate_cereal','irrigated_winter_temperate_cereal')))),e)
    e$nc_open=function(...) stop('Winter-only cold start must not read legacy initialization')
    e
}
initial=initial_fixture(21L);eval(initial_block,initial)
stopifnot(all(initial$leafC_initial_map==0),all(initial$LAI_initial_map==0))
mixed=initial_fixture(c(17L,21L))
mixed$nc_open=function(...) NULL
mixed$ncvar_get=function(...) array(5,c(2,1,25))
mixed$nc_close=function(...) invisible(NULL)
eval(initial_block,mixed)
for (key in c('LAI_initial_map','SAI_initial_map','leafC_initial_map','grainC_initial_map')) {
    value=get(key,mixed)
    stopifnot(all(value[,,22:23]==0),all(value[,,c(1:21,24:25)]==5))
}

for (scheme in c('CLM4.5','JULES')) stopifnot(identical(run(scheme),run(scheme,TRUE)))
cat('Actual winter daily simulation/checkpoint blocks: both allocation schemes and cold-spell restart OK\n')
