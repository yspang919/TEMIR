################################################################################
### Terrestrial Ecosystem Model in R (TEMIR)
### Execution module for single-site, regional or global simulation
################################################################################

timestamp()

################################################################################
### Source input scripts for model configuration:
################################################################################

# Set simulation directory:
simulation_dir = paste0(getwd(), '/')

# Source input scripts:
# *** Please make sure the input scripts (e.g., input_TEMIR.R and input_crop_extension.R) are in the same simulation directory as this execution script. ***
source('input_TEMIR.R')

if (file.exists('input_TEMIR_crop_extension.R')) {
    # Simulation with the crop module
    source('input_TEMIR_crop_extension.R')
    biogeochem_flag = TRUE
} else {
    # Simulation without the crop module, biogeochem_flag is FALSE by settings
    biogeochem_flag = FALSE
}

# Loading required libraries
source('library_setup.R')

if (single_site_flag && use_site_obs_flag) {
    # Settings for using single site observations data to drive TEMIR
    source('settings_1site_inputs.R')
} else {
  FLUXNET_site_flag = FALSE
  using_site_met_flag = FALSE
  using_site_o3_flag = FALSE
  using_site_soil_flag = FALSE
  using_site_LAI_flag = FALSE
}

# Check existence of directory paths:
dir_check = ls(pattern = "_dir$")
for (idir in seq_along(dir_check)) {
   if (!dir.exists(paths = get(dir_check[idir]))) stop(paste0(dir_check[idir], ' does not exist!'))
} ; rm(dir_check, idir)

# Print directory paths:
print(paste0('Simulation directory: ', simulation_dir), quote=FALSE)
print('Please make sure this is the simulation that you desire!', quote=FALSE); cat('\n')
print(paste0('Code directory: ', code_dir), quote=FALSE)
print('Please make sure this is the code directory that you desire!', quote=FALSE); cat('\n')

# Get simulation configuration for saving:
model_config_vec = setdiff(ls(), ls(pattern = '_dir$'))

# Turn on default TEMIR aerodynamic conductance scheme if infer_canopy_met_flag=TRUE:
if (infer_canopy_met_flag) ga_scheme = 'CLM4.5'

################################################################################
### Source scripts
################################################################################

# Globally available geophysical constants:
source(paste0(code_dir, 'geophys_const.R'))

# Useful functions:
source(paste0(code_dir, 'tools.R'))

# Functions to compute aerodynamic conductance, temperature and humidity profiles:
source(paste0(code_dir, 'Monin_Obukhov.R'))

# Functions to compute canopy radiative transfer:
source(paste0(code_dir, 'radiative_transfer.R'))

# Functions to compute photosynthesis:
source(paste0(code_dir, 'Farquhar_Ball_Berry.R'))

# Functions to loop simulation over longitudes and latitudes:
# It includes a function to reshape history data into gridded format.
source(paste0(code_dir, 'simulate_ij.R'))

# Functions to compute dry deposition velocities and required parameters:
source(paste0(code_dir, 'drydep_toolbox.R'))

# Functions to incorporate FLUXNET datasets:
if (FLUXNET_site_flag) source(paste0(code_dir, 'FLUXNET_functions.R'))

# Surface and PFT input parameters (prescribed):
source(paste0(code_dir, 'PFT_surf_data.R'))

# Functions used in the crop module (after version 2.0)
if(biogeochem_flag){
   source(paste0(TEMIR_dir, '/extension_crop/biomass_partitioning.R'))
   source(paste0(TEMIR_dir, '/extension_crop/plant_phenology.R'))
   source(paste0(TEMIR_dir, '/extension_crop/plant_physiology.R'))
   source(paste0(TEMIR_dir, '/extension_crop/maintenance_respiration.R'))
   source(paste0(TEMIR_dir, '/extension_crop/Cpool_budget.R'))
   source(paste0(TEMIR_dir, '/extension_crop/crop_retranslocation.R'))
}

# Loading user-provided site-level meteorological data
# This code will be used to read site input data in the future
if (single_site_flag && use_site_obs_flag) {
   source(paste0(code_dir, 'read_1site_inputs.R'))
}

################################################################################
### Define additional dimensional and variable info:
################################################################################

cat('\n')

# Reading FLUXNET info is now in temp_read_site_met.R
# Temporary: just put it here to prevent a missing flag, check to see if it is needed in the future 
FLUXNET_flag = FALSE

# Time step (s):
dt = dt_hr*3600

# Vector of simulation dates:
date_vec = make.date.vec(start.date=start_date, end.date=end_date)

# Number of simulation days:
n_day_sim = length(date_vec)

# Corresponding indices in global lon/lat grid for the simulated region:
if (single_site_flag) {
   ind_lon = find.lon.lat(lonspec=lon_sim, latspec=lat_sim, lon=lon, lat=lat)[1]
   ind_lat = find.lon.lat(lonspec=lon_sim, latspec=lat_sim, lon=lon, lat=lat)[2]
} else {
   ind_lon = which((lon + dlon/2) > lon_sim[1] & (lon - dlon/2) < lon_sim[2])
   ind_lat = which((lat + dlat/2) > lat_sim[1] & (lat - dlat/2) < lat_sim[2])
}

# Define a list of c(i, j), where i and j are indices of global lon/lat grid to be simulated:
ij = list()
n = 0
for (n_i in 1:length(ind_lon)) {
   i = ind_lon[n_i]
   for (n_j in 1:length(ind_lat)) {
      j = ind_lat[n_j]
      if (single_site_flag) {
         # Runs site simulation unless land fraction = 0
         if (FRLAND[i,j] != 0.0) {
            n = n + 1 ; ij[[n]] = c(i, j)
            if (!FLUXNET_site_flag) {
               print(paste0('Single site simulation using ', met_name, ' data :'), quote = FALSE)
               print(paste0('Longitude = ', lon_sim, ', Latitude = ', lat_sim), quote = FALSE)
               print(paste0('Land cover fraction of site = ', signif(FRLAND[i,j])), quote = FALSE)
            } else { 
               if (FLUXNET_flag){
                  print(paste0('Simulation using FLUXNET data of site ', FLUXNET_site_id, ' (gapfilled with ', met_name, '):'), quote = FALSE)
               } else {
                  print(paste0('Simulation using ', met_name, ' data of FLUXNET site ', FLUXNET_site_id, ' :'), quote = FALSE)
               }
               print(paste0('Longitude = ', lon_sim, ', Latitude = ', lat_sim), quote = FALSE)
               print(paste0('Land cover fraction of site ', FLUXNET_site_id, ' = ', signif(FRLAND[i,j])), quote = FALSE)
            }
         } else stop('Grid cell contains no land!!!')
      } else { 
         if (FRLAND[i,j] < 0.05) { 
            # Too little non-glacial land: skip calculations altogether and do not include in list of c(i, j). This prevents calculations for oceanic and permanently glacial grid cells.
            n = n
         } else {
            # n = (n_i - 1)*length(ind_lat) + n_j
            n = n + 1
            ij[[n]] = c(i, j)
         }
      }   
   }
} ; rm(n, n_i)

# Print general simulation information:
if (!single_site_flag) {
   if (lon_sim == c(-180, 180) && lat_sim == c(-90, 90)){
      print(paste0('Global simulation using ', met_name, ' data...'), quote = FALSE)
   } else {
      print(paste0('Simulation using ', met_name, ' data : longitude = ', lon_sim[1],' to ', lon_sim[2], ', latitude = ', lat_sim[1], ' to ', lat_sim[2] ,'...'), quote = FALSE)
   }
}

################################################################################

# Define nc data dimensions and variables if nc archiving is turned on:

# Define variable dimensions:
X = ncdim_def(name='lon', units='degrees_east', vals=lon[ind_lon], longname='Longitude')
Y = ncdim_def(name='lat', units='degrees_north', vals=lat[ind_lat], longname='Latitude')
# D = ncdim_def(name='date', units='YYYYMMDD', vals=date_vec, unlim=TRUE, calendar='standard', longname='Date in YYYYMMDD (defined by 00:00-24:00 UTC)')
P = ncdim_def(name='pft', units='unitless', vals=pftnum, longname='Plant function type (0 = bare land)')
H = ncdim_def(name='hour', units='hours', vals=dt_hr*(1:(24/dt_hr)), longname='Hour on YYYYMMDD (starting from 00:00 UTC)')

# Make list of variables selected to archive:
# If each nc file contains data for a day of outputs, "dim=list(X, Y, P, H)" in "ncvar_def". If it contains all days, "dim=list(X, Y, D, P, H)".
var_name = available_outputs_df[na.omit(match(unique(output_variables), available_outputs_df$variable_name)),]
var_list = list()
var_name[] = lapply(var_name, as.character) # for R version 3.1.1 
for (v in 1:nrow(var_name)) {
   var_dim_list = if (var_name[v,'res_level'] == 'PFT') list(X, Y, P, H) else if (var_name[v,'res_level'] == 'grid') list(X, Y, H) else if (var_name[v,'res_level'] == 'PFT_daily') list(X, Y, P)
   var_list[[v]] = ncvar_def(name=var_name[v,'variable_name'], units=var_name[v,'unit'], dim=var_dim_list, longname=var_name[v,'long_name'], prec='float', compression=4)
}
rm(var_dim_list)

cat('\n')

################################################################################
### Simulation:
################################################################################
print(paste0('### Begin simulation for case: ', basename(simulation_dir), ' ###'), quote=FALSE)

# Number of cores to use:
# If running on a cluster instead of a personal computer, overwrite what is set above with these system-dependent settings:
if (cluster_flag) {
   # These settings are dependent on the cluster environment.
   n_core = as.numeric(Sys.getenv('PBS_NUM_PPN'))
   if (is.na(n_core)) n_core = detectCores()
   n_node = as.numeric(Sys.getenv('PBS_NUM_NODES'))
   if (is.na(n_node)) n_node = 1
   if (n_node > 1) stop('TEMIR cannot be run on multiple nodes!')
}
# For single-site simulation, always use one core only:
if (single_site_flag) n_core = 1
# Print number of cores used:
print(paste0('# of cores used: ', as.character(n_core)), quote=FALSE)

# Continue run:
if (continue_flag) print('Continuing simulation from previous run...', quote=FALSE)

# Check and reshape data arrays:
if (debug_flag) {
   # Data array holding all days of data will be created:
   hist_grid = array(NaN, dim=c(length(ind_lon), length(ind_lat), n_day_sim, length(pftname), 24/dt_hr, nrow(var_name)))
   # Check error for all days:
   err_hist_ij = list()
   err_msg = NULL
}

#### Update: FLUXNET related variable is now put in settings_1site_input.R
# Input (meteorological and vegetation) variables dataframe for simulation from various sources (NA if no corresponding input exists):
# 1st col = TEMIR input variable name; 2nd col = TEMIR input unit; 
# 3rd col = MERRA2 variable name; 4th col = MERRA2 unit conversion needed;
# 5th col = GEOS-FP variable name; 6th col = GEOS-FP unit conversion needed;
input_data_df = `colnames<-`(rbind.data.frame(
  # Cloud fraction (0-1)
  c('CLDTOT', '0-1', 'CLDTOT', FALSE, 'CLDTOT', FALSE),
  # Root zone soil wetness (0-1)
  c('GWETROOT', '0-1', 'GWETROOT', FALSE, 'GWETROOT', FALSE),
  # Top soil wetness (0-1)
  c('GWETTOP','0-1', 'GWETTOP', FALSE, 'GWETTOP', FALSE),
  # Sea-level pressure (Pa)
  c('SLP', 'Pa', 'SLP', FALSE, 'SLP', TRUE),
  # Atmospheric pressure (Pa)
  c('ATMP', 'Pa', NA, FALSE, NA, FALSE), 
  # Temperature at 10 m above displacement height (K)
  c('T10M', 'K', 'T10M', FALSE, 'T10M', FALSE),
  # Temperature at 2 m above displacement height (K)
  c('T2M', 'K', 'T2M', FALSE, 'T2M', FALSE),
  # Sensible heat flux (W m^-2)
  c('HFLUX', 'W m^-2','HFLUX', FALSE,'HFLUX', FALSE),
  # Latent heat flux (W m^-2)
  c('EFLUX', 'W m^-2', 'EFLUX', FALSE, 'EFLUX', FALSE),
  # Surface downward total PAR flux (W m^-2)
  # This variable is not included in MERRA2 or GEOSFP data, but it is usually measured during field experiments.
  # Created for easier merging with single-site observation data frame.
  c('PARTOT','W m^-2', NA, FALSE, NA, FALSE),
  # Surface downward PAR diffuse flux (W m^-2)
  c('PARDF', 'W m^-2', 'PARDF', FALSE,'PARDF', FALSE),
  # Surface downward PAR direct beam flux (W m^-2)
  c('PARDR', 'W m^-2', 'PARDR', FALSE, 'PARDR', FALSE),
  # Surface incident shortwave flux (W m^-2)
  c('SWGDN', 'W m^-2', 'SWGDN', FALSE, 'SWGDN', FALSE),
  # Total Precipitation (kg m-2 s-1)
  c('PRECTOT', 'kg m-2 s-1', 'PRECTOT', FALSE, 'PRECTOT', FALSE),
  # Snowfall (kg m-2 s-1)
  c('PRECSNO', 'kg m-2 s-1', 'PRECSNO', FALSE, 'PRECSNO', FALSE),
  # Snow depth (m)
  c('SNODP', 'm', 'SNODP', FALSE, 'SNODP', FALSE),
  # Surface evaporation (kg m^-2 s^-1)
  c('EVAP', 'kg m^-2 s^-1', 'EVAP', FALSE, 'EVAP', FALSE),
  # Relative humidity at 2 m above displacement height (%)
  c('RH', '%', NA, FALSE, NA, FALSE),
  # Specific humidity at 2 m above displacement height (kg kg^-1)
  c('QV2M', 'kg kg^-1', 'QV2M', FALSE, 'QV2M', FALSE),
  # Eastward wind at 10 m above displacement height (m s^-1)
  c('U10M', 'm s^-1', 'U10M', FALSE, 'U10M', FALSE),
  # Northward wind at 10 m above displacement height (m s^-1)
  c('V10M','m s^-1', 'V10M', FALSE,'V10M', FALSE),
  # Wind speed (m s^-1)
  c('WS','m s^-1', NA, FALSE, NA, FALSE),
  # Friction velocity (m s^-1)
  c('USTAR', 'm s^-1', 'USTAR', FALSE, 'USTAR', FALSE),
  # Roughness length for momentum (m)
  c('Z0M', 'm', 'Z0M', FALSE, 'Z0M', FALSE),
  # Dataframe settings
  stringsAsFactors = FALSE),
  c('TEMIR_var_name', 'TEMIR_unit', 'MERRA2_var_name', 'MERRA2_unit_differ', 
    'GEOSFP_var_name', 'GEOSFP_unit_differ'))

if (FLUXNET_site_flag && FLUXNET_flag) {
  # using FLUXNET meteorological input
  updated_input_df = merge.data.frame(x = input_data_df, y = FLUXNET_input_data_df, by = 'TEMIR_var_name', all = TRUE)
} else {
  updated_input_df = input_data_df
}


# User-provided soil composition at a single site
if (single_site_flag && use_site_obs_flag && using_site_soil_flag) {
  if (site_num_soil_layers == 1) {
    print('Replacing soil hydraulic properties with site soil compostion....')
    theta_sat_bulk[i,j] = site_theta_sat_layer
    theta_sat_bulk_top[i,j] = site_theta_sat_layer
    theta_sat_bulk_bottom[i,j] = site_theta_sat_layer
    
    b_psi_bulk[i,j] = site_bpsi_layer
    b_psi_bulk_top[i,j] = site_bpsi_layer
    b_psi_bulk_bottom[i,j] = site_bpsi_layer
    
    psi_sat_bulk[i,j] = site_psi_sat_layer
    psi_sat_bulk_top[i,j] = site_psi_sat_layer
    psi_sat_bulk_bottom[i,j] = site_psi_sat_layer
  } else if (site_num_soil_layers >= 1) {
    stop('User-provided site soil composition is currently limited to a bulk soil layer.')
  }
}

################################################################################

# Start simulation for each day:

for (d in 1:n_day_sim) {
   
   timestamp()
   
   # Current date:
   current_date = to.yyyymmdd(from.yyyymmdd(start_date) + (d - 1)*24)
   # Strings for year, month and day:
   YYYY = substr(x=as.character(current_date), start=1, stop=4)
   MM = substr(x=as.character(current_date), start=5, stop=6)
   DD = substr(x=as.character(current_date), start=7, stop=8)
   print(paste0('Current simulation date = ', YYYY, '/', MM, '/', DD), quote=FALSE)
   
   # Create temporary data directory for each simulation day:
   # if (length(dir(path=paste0(simulation_dir, 'temp_data/'), pattern=paste0('temp_', YYYY, MM, DD))) == 0) system(command=paste0("mkdir '", simulation_dir, 'temp_data/temp_', YYYY, MM, DD, "'"))
   if (!dir.exists(paste0(simulation_dir, 'temp_data/temp_', YYYY, MM, DD))) dir.create(paste0(simulation_dir, 'temp_data/temp_', YYYY, MM, DD))
   
   # Number of days from 00:00 UTC Jan 1 (whole number not including hours):
   leap = is.leap(yyyy = as.numeric(YYYY))
   n_day_whole = date.to.day(yyyymmdd = current_date, leap = leap)
   
   # This could be refactored once the data pre-processor is created.
   # Time shifting FLUXNET to UTC for current simulation date:
   if (FLUXNET_flag){
      FLUXNET_current = f_FLUXNET_UTC_2_local(FLUXNET.dir = FLUXNET_dir, date = current_date, site.id = FLUXNET_site_id, utc.offset = time_shift, out.utc.offset = FALSE)
      FLUXNET_end = f_FLUXNET_UTC_2_local(FLUXNET.dir = FLUXNET_dir, date = to.yyyymmdd(from.yyyymmdd(current_date) + 24), site.id = FLUXNET_site_id, utc.offset = time_shift, out.utc.offset = FALSE)
      # Time shift obtained daily as daylight saving time is not required as Fluxnet discounts daylight saving time
   }
   
   #############################################################################
   
   # Reload interannually varying inputs on YYYY/01/01 if necessary:
   
   if (paste0(MM, DD) == '0101') {
      
      # Reload LAI data if interannually varying LAI data are used:
      if (LAI_data_flag) {
         if (length(year_vec) > 1) {
            YYYY_LAI = as.character(LAI_used_years[which(year_vec == as.numeric(YYYY))])
            last_YYYY_LAI = as.character(LAI_used_years[which(year_vec == as.numeric(YYYY)) - 1])
            if (length(last_YYYY_LAI) == 0) last_YYYY_LAI = 'no_YYYY_LAI'
            if (YYYY_LAI != last_YYYY_LAI) {
               subfn = paste0('daily_monthly_LAI_data_', YYYY_LAI, '.RData')
               filename = paste0(processed_surf_data_dir, subfn)
               print(paste0('Loading existing daily LAI and SAI data for year ', YYYY_LAI, '...'), quote=FALSE)
               load(filename)
            }
         }
      }
      
      # Ozone field is read every model year
      if (O3_damage_flag & !O3_fixed_flag & !using_site_o3_flag) {
         if (length(year_vec) > 1) {
            YYYY_O3 = as.character(O3_used_years[which(year_vec == as.numeric(YYYY))])
            last_YYYY_O3 = as.character(O3_used_years[which(year_vec == as.numeric(YYYY)) - 1])
            if (length(last_YYYY_O3) == 0) last_YYYY_O3 = 'no_YYYY_O3'
            if (YYYY_O3 != last_YYYY_O3) {
               filename = paste0(O3_data_dir, O3_subn1, YYYY_O3, O3_subn2)
               print(paste0('Loading surface O3 concentrations from ', filename, '...'), quote=FALSE)
               nc = nc_open(filename)
               lon_O3 = ncvar_get(nc, unname(O3_dim_vec['longitude']))
               lat_O3 = ncvar_get(nc, unname(O3_dim_vec['latitude']))
               # Surface O3 concentration (ppbv):
               O3_hourly = ncvar_get(nc, O3_array_name)
               nc_close(nc)
               # Regrid to model resolution if input resolution is not consistent:
               if (sum(lon != lon_O3) > 0 | sum(lat[2:(length(lat)-1)] != lat_O3[2:(length(lat_O3)-1)]) > 0) {
                  # Regrid to model resolution:
                  print(paste0('Regridding hourly O3 concentrations for year ', YYYY_O3, '...'), quote=FALSE)
                  O3_hourly = sp.regrid(spdata=O3_hourly, lon.in=lon_O3, lat.in=lat_O3, lon.out=lon, lat.out=lat)
               }
            }
         }
      }
   }
   
   # Single-site ozone data is read once at the beginning of a simulation.
   # Get the hourly data by subsetting the dataframe based on YYYYMMDD
   if (O3_damage_flag & !O3_fixed_flag & using_site_o3_flag) {
      O3_hourly = field_measurement_o3_df %>% 
         filter(YYYYMMDD == current_date) %>% 
         pull(o3)  # length = 24
      
      if (any(is.na(O3_hourly))) {
         warning(paste0('Site ozone data on ', current_date, ' contains at least one NA values'))
      }
   }
   
   #############################################################################
   
   # Meteorological inputs:
   if (met_name == 'GEOSFP') {
      subdir = 'GEOS_FP/'
      file_ext = 'nc'
   } else if (met_name == 'MERRA2') {
      subdir = 'MERRA2/'
      file_ext = 'nc4'
   } else {
      stop('met_name specified is not available!')
   }
   
   subfn = paste0(subdir, YYYY, '/', MM, '/', met_name, '.', YYYY, MM, DD, '.A1.2x25.', file_ext)
   filename = paste0(met_data_dir, subfn)
   # These met fields are 1-hour average starting from 00:00 UTC of the day.
   
   # Open nc file:
   nc = nc_open(filename)
   
   # Load meteorological data:
   for (imet in 1:nrow(updated_input_df)) {
      
      # Get TEMIR and meteorological variable name:
      TEMIR_variable_name = as.character(updated_input_df$TEMIR_var_name[imet])
      met_variable_name = as.character(updated_input_df[,paste0(met_name, '_var_name')][imet])
      
      # Load particular meteorological data if variable exists in global meteorological field:
      if (!is.na(met_variable_name)) if (is.na(as.logical(met_variable_name))) assign(x = TEMIR_variable_name, value = ncvar_get(nc, met_variable_name)) else next
      
      # Get meteorological field dimension for FLUXNET conformity:
      if (!exists('met_dim') && FLUXNET_flag) met_dim = dim(get(x = TEMIR_variable_name))
      
      # Convert GEOS-FP data unit if required:
      if (as.logical(updated_input_df[,paste0(met_name, '_unit_differ')][imet])) assign(x = TEMIR_variable_name, value = f_met_unit_convert(met.name = met_name, TEMIR.var = TEMIR_variable_name, met.var = met_variable_name))
      
   }
   
   # Close nc file:
   nc_close(nc)
   
   # Replacing MERRA2 meteorological data for one site with FLUXNET / site-measurements, if necessary
   if (single_site_flag && use_site_obs_flag) {
      if (FLUXNET_site_flag && FLUXNET_flag) {
         # Replace with FLUXNET meteorology
         f_replace_metmaps_with_FLUXNET()
      } else if (using_site_met_flag && !FLUXNET_flag) {
         
         # Replace with custom meteorological data
         f_replace_metmaps_with_custom_data(site_i = ind_lon, site_j = ind_lat)
         
         # Obtain current day LAI value from measurements
         if (using_site_LAI_flag) {
            LAI_today = prescriebd_LAI_df %>% filter(YYYYMMDD == current_date) %>% pull(LAI)
         }
      }
   }
   
   #############################################################################
   
   # Loading additional inputs for the crop modules (> v2.0)
   if (biogeochem_flag) {
      
       # Soil temperature input
       if (T_soil_source == 'MERRA2') {

         filename = list.files(path = paste0(soilT_data_dir), pattern = paste0(YYYY,MM,DD), recursive = T)
         filename = paste0(soilT_data_dir, filename)
         
         nc = nc_open(filename)
         # Soil temperature of the surface layer (0 - 9.88cm) (K)
         TSOIL1 = ncvar_get(nc,"TSOIL1")
         # Soil temperature of the 2nd layer (9.88cm - 29.4cm) (K)
         TSOIL2 = ncvar_get(nc,"TSOIL2")
         # Soil temperature of the 3rd layer (29.4cm - 68.0cm) (K)
         TSOIL3 = ncvar_get(nc,"TSOIL3")
         # Soil temperature of the 4th layer (68.0cm - 144cm) (K)
         TSOIL4 = ncvar_get(nc,"TSOIL4")
         # Soil temperature of the 5th layer (144cm - 295cm) (K)
         TSOIL5 = ncvar_get(nc,"TSOIL5")
         nc_close(nc)
         
         # Replace Tsoil from MERRA2 maps with site measurements
         if (single_site_flag && use_site_obs_flag && using_site_met_flag && any(replace_TS_flag_array)) {
            print('Replacing MERRA2 Tsoil map with measurements')
            # Only replacing value on that target grid cell in the MERRA2 map
            f_replace_soilT_with_custom_data(site_i = ind_lon, site_j = ind_lat)
         }
         
       } else if (T_soil_source == 'custom') {
          # Implement your code to read daily soil temperature input date for global regional simulations (in K)
          # The dimension of the input should be [lon, lat, hour].
       }
      
      
      managed_crop_PFT_number = PFT_df$PFT_number[
         PFT_df$PFT_description %in% c(
            'corn',
            'irrigated_corn',
            'spring_temperate_cereal',
            'irrigated_spring_temperate_cereal',
            'winter_temperate_cereal',
            'irrigated_winter_temperate_cereal',
            'soybean',
            'irrigated_soybean'
         )
      ]
   
       # GDD requirements (exclusive for crop simulation)
      if (any(sim_PFT %in% managed_crop_PFT_number)) {
         if (GDDmat_method == "CLM4.5") {
            # For simplicity, we decide not to implement the moving average of GDDx like the one in CLM4.5, as the prediction of the change in planting/harvesting date are not very accurate anyway
            # We only use the GDDx map in yr 2000 and calculate the corresponding GDDmat
            filename = paste0(GDDmat_map_dir, 'MEERA2_year_2000_growing_season_GDDx_map.nc')
            nc = nc_open(filename)
            GDD0_map = ncvar_get(nc,"GDD0")
            GDD8_map = ncvar_get(nc,"GDD8")
            GDD10_map = ncvar_get(nc,"GDD10")
            nc_close(nc)
         } else if (GDDmat_method == 'prescribed-map') {
            # The map is read at PFT_surf_data.R
         } else if (GDDmat_method == 'prescribed-site') {
            # The site GDDmat is an input in 'input_TEMIR_crop_extension.R'
         }
      }
   }

   #############################################################################

   environment(f_simulate_ij) = globalenv()
   environment(f_hist_reshape) = globalenv()
   
   # Simulate for each lon/lat:
   print('Simulating for each lon/lat...', quote=FALSE)
   hist_ij = if (n_core != 1) { 
      if (Sys.info()["sysname"] == 'Windows') {
         cl = makeCluster(getOption("cl.cores", n_core))
         parLapply(cl = cl, X = ij, fun=f_simulate_ij)
         stopCluster(cl)
      } else {
         mclapply(ij, FUN=f_simulate_ij, mc.cores=n_core)
      } 
   } else {
      lapply(ij, FUN=f_simulate_ij)
   }
   print(paste0('Done on ', Sys.time()), quote=FALSE)
   
   if (debug_flag) {
      
      # "hist_grid" is where all the output data are.
      # Its dimensions: hist_grid = array(NaN, dim=c(length(ind_lon), length(ind_lat), n_day_sim, length(pftname), 24/dt_hr, nrow(var_name)))
      
      output = f_hist_reshape(ij=ij, hist_ij=hist_ij)
      hist_grid[,,d,,,] = output$hist_grid
      err_hist_ij[[d]] = output$err_hist_ij
      err_msg = c(err_msg, output$err_msg)
      
   } else {
      
      print('Reshaping and saving history data into gridded data file...', quote=FALSE)
      output = f_hist_reshape(ij=ij, hist_ij=hist_ij)
      hist_grid = output$hist_grid
      err_hist_ij = output$err_hist_ij
      err_msg = output$err_msg
      
      if (archive_format == 'RData') {
         
         # Save output history data in RData:
         filename = paste0(simulation_dir, 'hist_data/hist_grid_', YYYY, MM, DD, '.RData')
         FLUXNET_error_vec = if (FLUXNET_flag) c('FLUXNET_var_err', 'FLUXNET_global_err') else NULL
         save(list=c('hist_grid', 'err_hist_ij', 'err_msg', FLUXNET_error_vec), file=filename)
         
      } else if (archive_format == 'nc') {
         
         # Get history nc file name:
         filename = paste0(simulation_dir, 'hist_data/hist_grid_', YYYY, MM, DD, '.nc')
         
         # Delete previous history nc file:
         if (file.exists(filename)) file.remove(filename)
         
         # Create history nc file:
         nc = nc_create(filename=filename, vars=var_list)
         
         # Put in values of variables:
         for (v in 1:nrow(var_name)) {
            if (var_name[v,4] == 'grid') ncvar_put(nc=nc, varid=var_list[[v]], vals= hist_grid[,,1,,v])
            if (var_name[v,4] == 'PFT') ncvar_put(nc=nc, varid=var_list[[v]], vals=hist_grid[,,,,v])
            if (var_name[v,4] == 'PFT_daily') ncvar_put(nc=nc, varid=var_list[[v]], vals=hist_grid[,,,24,v])
         }
         
         # Put in dimentions and attributes:
         ncatt_put(nc, varid='lon', attname='axis', attval='X')
         ncatt_put(nc, varid='lat', attname='axis', attval='Y')
         ncatt_put(nc, varid='pft', attname='axis', attval='P')
         ncatt_put(nc, varid='hour', attname='axis', attval='T')
         ncatt_put(nc, varid=0, attname='Title', attval=paste0(basename(simulation_dir), ' ', YYYY, MM, DD))
         ncatt_put(nc, varid=0, attname='Conventions', attval='COARDS')
         ncatt_put(nc, varid=0, attname='History', attval=paste0('Generated on ', Sys.time()))
         
         # FLUXNET data warnings:
         if (FALSE && single_site_flag && FLUXNET_flag) {
            ncatt_put(nc, varid=0, attname='FLUXNET Site Warnings', attval=FLUXNET_global_err)
            if (!is.null(FLUXNET_var_err)) {
               ncvar_put(nc, varid=FLUXNET_err_nc_def, vals=FLUXNET_var_err)
               ncatt_put(nc, varid='error_number', attname='axis', attval='n_error')
               ncatt_put(nc, varid='error_replace', attname='axis', attval='error_replace')
               rm(FLUXNET_var_err)
            }
         }

         # Close nc file:
         nc_close(nc)

         # Save error messages:
         if (!is.null(err_msg) || length(err_hist_ij) != 0) {
            filename = paste0(simulation_dir, 'hist_data/hist_err_', YYYY, MM, DD, '.RData')
            save(list=c('err_hist_ij', 'err_msg'), file=filename)
         }
         
      } else stop('Debugging mode is off but data archiving format is not correctly specified.')
      
      print(paste0('Done on ', Sys.time()), quote=FALSE)
      
   }
   
   # Delete temporary data 15 days ago to prevent excessive amount of data:
   if (d > 15) {
      d_prev = 15
      previous_date = to.yyyymmdd(from.yyyymmdd(current_date) - d_prev*24)
      pathname = paste0(simulation_dir, 'temp_data/temp_', as.character(previous_date))
      suppressMessages(dir.remove(pathname))
   }
}

################################################################################

# Save model configuration:

if (!debug_flag) {
   filename = paste0(simulation_dir, 'hist_data/model_config.RData')
   execution_config = c('dt', 'ind_lon', 'ind_lat', 'ij',  'n_day_sim', 'pftname', 'pftnum', 'PFT_frac')
   out_config = c(execution_config, model_config_vec)
   save(list = out_config, file=filename)
}

# End of simulation for all days.
print('### End of Simulation ###', quote=FALSE)
timestamp()

################################################################################
### End of execution
################################################################################
