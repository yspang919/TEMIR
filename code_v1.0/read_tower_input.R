##### Additional options
# If TRUE, soil water stress factor for plant is always 1 (no water stress), see simulate_ij.R variable 'beta_t'
# If FALSE, soil water stress factor for plant will be calculated with soil water content input (may not guarantee free of water stress even if the crop is irrigated....)
force_crop_irrigation = FALSE


# Update soil composition at site (assume signle-layer soil)
# It will replace the default 2.5x2 deg. CLM land surface soil data
# Soil hydraulic properties are handled in PFT_surf_data.R
# soil sand percentage (%)
site_sand_pct = 40
# soil clay percentage (%)
site_clay_pct = 30
# soil organic matter fraction (fraction)
site_om_frac = 0.10

# calculate site soil hydraulic properties 
# saturated volumetric water content
site_theta_satmin = 0.489 - 0.00126*site_sand_pct
site_theta_sat = (1-site_om_frac)*site_theta_satmin + site_om_frac*0.9
# Clapp and Homberger parameter
site_b_psimin = 2.91 + 0.159*site_clay_pct
site_b_psi = (1-site_om_frac)*site_b_psimin + site_om_frac*2.7
# Saturated soil matric potential (mm):
site_psi_satmin = -10.0*10^(1.88 - 0.0131*site_sand_pct)
site_psi_sat = (1-site_om_frac)*site_psi_satmin + site_om_frac*-10.3     


##### Code for reading the measurement data at the flux tower

flux_met_dir = 'G:/My Drive/eCO2eO3_field_data/IN-Hyd/'
flux_met_filename = 'IN-Hyd_2019_met_input_for_Syam.RData'


# dataframe of the varialbes in RData, 
replace_MERRA2_df = `colnames<-`(rbind.data.frame(
   # Column 1: Name in the Rdata dataframe columns
   # Column 2: Name in MERRA2 or TEMIR (will overwrite that variable if it already existed) 
   # Column 3: Replace MERRA2 with flux tower data in simulation? 
   c('TA',      'site_T2M',        TRUE),     # air temperature at 2m (K)
   c('PA',      'site_P_2m',       TRUE),     # air pressure at 2m (Pa)
   c('RH',      'site_RH',         FALSE),     # relative humidity (%)
   c('VPD',     'site_vpd',        TRUE),     # vapor pressure defitcit (kPa) 
   c('WS',      'site_u10m',       TRUE),      # wind speed (m s-1)
   c('LAI',     'LAI',             TRUE),     # leaf area index (m2 m-2)
   c('SWC',     'site_SWC',        TRUE),     # soil water content (fraction)
   c('SW_IN',   'site_SWGDN',      TRUE),     # incoming shortwave radidation (W m-2) 
   # c('LW_IN', , FALSE),                      # longwave radiation (not used)
   # c('NetRad', , FALSE),                     #  net radiation (not used)
   c('PPFD_TOT', 'PAR_total',      TRUE),      # total PAR (W m-2)
   stringsAsFactors = FALSE),
   c('site_met_name', 'MERRA2_TEMIR_name', 'replace_MERRA2_flag'))



# required field-measured variables for the simulation
desired_flux_tower_variables = replace_MERRA2_df$site_met_name[which(replace_MERRA2_df$replace_MERRA2_flag == TRUE)] 

# LAI will be read separately
desired_flux_tower_MET_varaibles = setdiff(desired_flux_tower_variables, 'LAI')

# create temp environment to contain the laoded content
temp_env = new.env()
load(paste0(flux_met_dir, flux_met_filename),  envir = temp_env)

# select desired MET variables that will replace MERRA2 input
full_flux_met_df = temp_env$final_df %>%
      select(UTC_time, UTC_year, UTC_jday, UTC_hr, all_of(desired_flux_tower_MET_varaibles))

# get site LAI separately
if (replace_MERRA2_df$replace_MERRA2_flag[which(replace_MERRA2_df$site_met_name == 'LAI')]) {
   full_flux_LAI_df = temp_env$final_df %>%
      select(UTC_time, UTC_year, UTC_jday, UTC_hr, LAI)
}
