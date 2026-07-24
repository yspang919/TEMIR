################################################################################
### Terrestrial Ecosystem Model in R (TEMIR)
### Input script for using custom inputs to replace input maps in single site simulations
################################################################################

# In this script, users can specify variables that are read from maps and replace them with observation data for single-site simulations.
# It supports input from FLUXNET spreadsheet, and R data frame that is stored in a .RData file.

# Variables that can be replaced include:
# - meteorological inputs (from both .RData or FLUXNET spreadsheet)
# - ozone inputs (only from .RData)
# - soil characteristics (directly specify the value in this script)
# - leaf area index (only from .RData). If "biogeochem_flag == TRUE", the simulated LAI value is replaced with user-provided input.

# Prescribing LAI here is preferred for single-site, single-PFT simulations, as user-provided LAI values are not resolved into different PFTs.


# This script is only executed when "single_site_flag == TRUE"

# Running simulation at a FLUXNET site?

FLUXNET_site_flag = FALSE
if (FLUXNET_site_flag) {
  # Set FLUXNET directory:
  FLUXNET_dir = paste0(TEMIR_dir, 'TEMIR_inputs/FLUXNET/')
  # Running simulation with FLUXNET meteorological/canopy data?
  FLUXNET_flag = FALSE
  # Specify FLUXNET site ID :
  FLUXNET_site_id = "US-Ha1"

  # FLUXNET input dataframe
  # 1st col = TEMIR input variable name
  # 2nd col = FLUXNET variable name
  # 3rd col = FLUXNET conversion needed
  # 4th col = replace the value in meteorological map?
  FLUXNET_input_data_df = `colnames<-`(rbind.data.frame(
    c('GWETTOP', 'SWC_F_MDS_1', FALSE, TRUE),
    c('ATMP', 'PA_F', TRUE, TRUE),
    c('T10M', 'TA_F', FALSE, TRUE),
    c('T2M', 'TA_F', FALSE, TRUE),
    c('HFLUX', 'H_F_MDS', FALSE, TRUE),
    c('EFLUX', 'LE_F_MDS', FALSE, TRUE),
    c('PARDF', 'PPFD_DIF', TRUE, TRUE),
    c('PARDR', 'PPFD_IN', TRUE, TRUE),
    c('SWGDN', 'SW_IN_F', FALSE, TRUE),
    c('PRECTOT', 'P_F', TRUE, TRUE),
    c('QV2M', 'RH', TRUE, TRUE),
    c('WS', 'WS_F', FALSE, TRUE),
    c('USTAR', 'USTAR', FALSE, TRUE),
    # Dataframe settings
    stringsAsFactors = FALSE),
    c('TEMIR_var_name', 'FLUXNET_var_name', 'FLUXNET_unit_differ', 'replace_met_map_flag'))

} else {
  FLUXNET_flag = FALSE
}


#################################### Running simulations with user-provided single-site meteorological inputs?
using_site_met_flag = FALSE
if (using_site_met_flag) {

   # Directory contains the file for single site meteorological input
   field_met_base_dir = paste0(TEMIR_dir, 'TEMIR_inputs/site_measurements_inputs/')
   
   field_met_case = 'example_case001/met_input/'
   field_met_filename = 'example_case001_2010_met_input.RData'
   # name of the dataframe that contains the input
   field_met_df_name = 'met_df'


   # Example: replace T2m, PAR_total, wind speed, soil water content from measurements
   
   # Atmospheric variables
   replace_precip = FALSE
   colname_precip = 'P'

   # Example: 'column_T2m' is the name of the column that contains the values of T2m in the data frame
   replace_T2m = FALSE
   colname_T2m = 'TA'

   replace_partot = FALSE
   colname_partot = 'PAR_tot'
   
   replace_pardir = FALSE
   colname_pardir = NA

   replace_pardif = FALSE
   colname_pardif = NA

   replace_SWR = FALSE
   colname_SWR = 'SW_IN'

   replace_wind = FALSE
   colname_wind = 'WS'

   replace_RH = FALSE
   colname_RH = NA

   replace_P2m = FALSE
   colname_P2m = 'PA'

   replace_VPD = FALSE
   colname_VPD = 'VPD'
   
   # Soil variables
   replace_SWC_L1 = FALSE
   colname_SWC_L1 = 'SWC_30cm'

   replace_SWC_L2 = FALSE
   colname_SWC_L2 = 'SWC_100cm'

   replace_SWC_L3 = FALSE
   colname_SWC_L3 = NA

   replace_SWC_L4 = FALSE
   colname_SWC_L4 = NA

   replace_SWC_L5 = FALSE
   colname_SWC_L5 = NA

   replace_TS_L1 = TRUE
   colname_TS_L1 = 'TS_5cm'
   
   replace_TS_L2 = FALSE
   colname_TS_L2 = NA

   replace_TS_L3 = FALSE
   colname_TS_L3 = NA

   replace_TS_L4 = FALSE
   colname_TS_L4 = NA

   replace_TS_L5 = FALSE
   colname_TS_L5 = NA


   # Specify the variables that have to be replaced with observation data in the data frame
   # 1st column: TEMIR input variable name
   # 2nd column: Variable name in the data source
   # 3rd column: Replace the variable read from MERRA2/GEOSFP map with single site observation data?
   # 4th column: Description of the variable and its unit.
   # 5th column: This variable also appears in met. field (MERRA2/ GEOS-FP)

   site_meas_option_df = `colnames<-`(rbind.data.frame(
    # Atmospheric variables
    c('PRECTOT', colname_precip, replace_precip, 'Total precipitation (kg m^-2 s^-1)', TRUE),
    c('T2M', colname_T2m, replace_T2m, 'Air temperature at 2m height (K)', TRUE),
    c('PARTOT', colname_partot, replace_partot, 'Total PAR (W m^-2)', FALSE),
    c('PARDR', colname_pardir, replace_pardir, 'Direct PAR (W m^-2)', TRUE),
    c('PARDF', colname_pardif, replace_pardif, 'Diffuse PAR (W m^-2)', TRUE),
    c('SWGDN', colname_SWR, replace_SWR, 'Short wave radiation (W m^-2)', TRUE),
    c('WS', colname_wind, replace_wind, 'Wind speed (m s^-1)', FALSE),
    c('RH', colname_RH, replace_RH, 'Relative humidity (%)', FALSE),
    c('ATMP', colname_P2m, replace_P2m, 'Atmospheric pressure at 2m height (Pa)', FALSE),
    c('VPD', colname_VPD, replace_VPD, 'Vapor pressure deficit (kPa)', FALSE),
    # Soil variables
    # Layered variable are from the shallowest layer (L1) to the deepest layer (L5).
    # Please use L1 first if there is only 1 measurement of SWC or TS. Followed by L2, L3...
    c('SWC1', colname_SWC_L1, replace_SWC_L1, 'Volumetric soil water content layer 1 (0-1)', FALSE),   # TEMIR has SWC variables, but they are not shared to same name here
    c('SWC2', colname_SWC_L2, replace_SWC_L2, 'Volumetric soil water content layer 2 (0-1)', FALSE),
    c('SWC3', colname_SWC_L3, replace_SWC_L3, 'Volumetric soil water content layer 3 (0-1)', FALSE),
    c('SWC4', colname_SWC_L4, replace_SWC_L4, 'Volumetric soil water content layer 4 (0-1)', FALSE),
    c('SWC5', colname_SWC_L5, replace_SWC_L5, 'Volumetric soil water content layer 5 (0-1)', FALSE),
    
    c('TSOIL1', colname_TS_L1, replace_TS_L1, 'Soil temperature layer 1 (K)', TRUE),
    c('TSOIL2', colname_TS_L2, replace_TS_L2, 'Soil temperature layer 2 (K)', TRUE),
    c('TSOIL3', colname_TS_L3, replace_TS_L3, 'Soil temperature layer 3 (K)', TRUE),
    c('TSOIL4', colname_TS_L4, replace_TS_L4, 'Soil temperature layer 4 (K)', TRUE),
    c('TSOIL5', colname_TS_L5, replace_TS_L5, 'Soil temperature layer 5 (K)', TRUE),
    
    # Column names of this dataframe
    stringsAsFactors = FALSE),
    c('TEMIR_var_name', 'source_var_name', 'replace_met_map_flag', 'Description', 'var_also_in_metfield'))
    
    replace_SWC_flag_array = c(replace_SWC_L1, replace_SWC_L2, replace_SWC_L3, replace_SWC_L4, replace_SWC_L5)
    replace_TS_flag_array = c(replace_TS_L1, replace_TS_L2, replace_TS_L3, replace_TS_L4, replace_TS_L5)
 
    # Specify the depth of the SWC and TS measurements from L1 to the deepest soil layer (in m)
    site_SWC_meas_depth = c(0.3, 1.0) # example: soil water content measurement at 30 cm
    site_TS_meas_depth = c(0.05) # example: no soil temperature measurement provided
   
    # Rename variables that appear in TEMIR for soil moisture input (special case)
    if (all(replace_SWC_flag_array == c(TRUE, FALSE, FALSE, FALSE, FALSE))) {
      # If one layer of SWC is provided, SWC1 will be use to represent the soil water content of the whole soil content
      site_meas_option_df[which(site_meas_option_df$source_var_name == colname_SWC_L1),]$TEMIR_var_name = 'GWETBULK'
    } else if (all(replace_SWC_flag_array == c(TRUE, TRUE, FALSE, FALSE, FALSE))) {
      # If two layers of SWC is provided, SWC1 and SWC2 will be use to represent the soil water content at the top zone (0 - 5 cm) and root zone (5 - 100 cm).
      site_meas_option_df[which(site_meas_option_df$source_var_name == colname_SWC_L1),]$TEMIR_var_name = 'GWETTOP'
      site_meas_option_df[which(site_meas_option_df$source_var_name == colname_SWC_L2),]$TEMIR_var_name = 'GWETROOT'
      # GWETTOP and GWETROOT also exist in MERRA2
      site_meas_option_df[which(site_meas_option_df$source_var_name == colname_SWC_L1),]$var_also_in_metfield = TRUE
      site_meas_option_df[which(site_meas_option_df$source_var_name == colname_SWC_L2),]$var_also_in_metfield = TRUE
    } else if (all(replace_SWC_flag_array == FALSE)) {
      # print('Not replacing SWC with observation...')
    } else {
      stop("Currently only two SWC replacement configurations are supported: one layer mapped to 'GWETBULK', or two layers mapped to 'GWETTOP' and 'GWETROOT'.")
    }
    
    if (length((which(replace_SWC_flag_array == TRUE))) > 0) {
       # using at least 1 level of site meas. SWC
       if (!is.numeric(site_SWC_meas_depth) || any(is.na(site_SWC_meas_depth))) {
          stop('The specified site soil water content measurement depth must be numeric and cannot contain NA values')
       }
       if (length(site_SWC_meas_depth) != length(which(replace_SWC_flag_array == TRUE))) {
          stop('The number of specified site soil water content measurement depth does not match the number of layer of available site SWC measurement data')
       }
    }
    
    if (length((which(replace_TS_flag_array == TRUE))) > 0) {
       # using at least 1 level of site meas. TS
       if (!is.numeric(site_TS_meas_depth) || any(is.na(site_TS_meas_depth))) {
          stop('The specified site temperature measurement depth must be numeric and cannot contain NA values')
       }
       if (length(site_TS_meas_depth) != length(which(replace_TS_flag_array == TRUE))) {
          stop('The number of specified site temperature measurement depth does not match the number of layer of available site TS measurement data')
       }
    }

}


##################################### Running simulations with single-site ozone input. Using single-site ozone inputs will disable the use of ozone input from .nc map.
using_site_o3_flag = FALSE

if (using_site_o3_flag) {
   # Directory that contains ozone input files for single-site simulations.
   # It will overwrite 'O3_data_dir' for ozone input maps declared in input_TEIMR.R
   O3_data_dir = paste0(TEMIR_dir, 'TEMIR_inputs/site_measurements_inputs/')
   
   # O3 input sub-directory. Enter 'NULL' if a sub-directory is not needed.
   field_o3_case = 'example_case001/o3_input/'
   # name of the .RData
   field_o3_filename = 'example_case001_2010_o3_input.RData'
   # name of the dataframe of the o3 inputs in .RData
   o3_met_df_name = 'ozone_df'
   
}



# Running simulations with user-provided single-site soil composition.
# The soil composition provided will replace the soil compositions and thereby hydraulic properties derived from land surface maps.
using_site_soil_flag = FALSE

if (using_site_soil_flag) {

   # Number of soil layers observations at the site
   # Currently only support single-layer soil for user-provided composition
   site_num_soil_layers = 1
   
   # Reserved for future development...
   if (site_num_soil_layers > 1) {stop('User-provided site soil composition is currently limited to a single layer.')}
   
   # Depth of the soil layers from the shallowest to the deepest layer (m)
   # example: site_depth_soil_layers = c(0.05, 0.1, 0.3, 0.6, 1)
   # for single soil layer, the depth is not relevent here (treated as bulk soil)
   # site_depth_soil_layer = c(0.05, 1)
   # site_depth_soil_layer = c(1)
   site_depth_soil_layer = c(1.5)

   # Specifying soil compositions of different soil layers at the site
   # Organic matter fraction of the soil layers from the shallowest to the deepest layer (0 to 1)
   # Example: site_om_frac_layer = c(0.02, 0.03, 0.25)      # soil organic matter fraction at 3 layers of soil
   site_om_frac_layer = 0.02
   # Percentage of clay presented in the soil layers from the shallowest to the deepest layer (0 to 100)
   site_clay_pct_layer = 30
   # Percentage of sand presented in the soil layers from the shallowest to the deepest layer (0 to 100)
   site_sand_pct_layer = 30
   
   if (any(c(length(site_om_frac_layer), length(site_clay_pct_layer), length(site_sand_pct_layer)) != site_num_soil_layers)) {
      stop('The number of site soil layer does not match the length of specified soil composition propertie(s)!')
   }

   # Calculation of soil hydraulic properties using user-specified soil composition.
   # Saturated volumetric water content from the shallowest to the deepest layer (length =  site_num_soil_layers)
   site_theta_sat_layer = (1-site_om_frac_layer)*(0.489-0.00126*site_sand_pct_layer) + site_om_frac_layer*0.9
   # Clapp and Homberger parameter from the shallowest to the deepest layer
   site_bpsi_layer = (1-site_om_frac_layer)*(2.91+0.159*site_clay_pct_layer) + site_om_frac_layer*2.7
   # Satured soil matric potential (mm)
   site_psi_sat_layer = (1-site_om_frac_layer)*(-10.0*10^(1.88-0.0131*site_sand_pct_layer)) + site_om_frac_layer*(-10.3)
   
}

# Running simulations with user-provided single-site leaf area index measurements.
# Currently it only supports LAI input for a single PFT.
using_site_LAI_flag = FALSE
if (using_site_LAI_flag) {
   
   if (length(sim_PFT) > 1) {
     warning('Providing site-level LAI measurements for a single-site simulation with more than one PFT simulated.')
   }
  
   # Base directory containing the prescribed LAI input
   site_prescribed_LAI_dir = paste0(TEMIR_dir, 'TEMIR_inputs/site_measurements_inputs/')
   
   prescribed_LAI_case = 'example_case001/prescribed_LAI/'
   prescribed_LAI_filename = 'example_case001_LAI_input_2010.RData'
   prescribed_LAI_df_name = 'LAI_df'

}



####################################
# FLUXNET and custom site meteorological input are mutually exclusive overlay sources.
if (FLUXNET_flag && using_site_met_flag) {
  stop("Cannot replace meteorological map input with FLUXNET input and other meteorological/land surface inputs at the same time!")
}


