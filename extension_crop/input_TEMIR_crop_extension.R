################################################################################
### Terrestrial Ecosystem Model in R (TEMIR)
### Input setting script for the carbon-vegetation and crop model 
################################################################################

# Directory for storing starting conditions (initial data)
initial_data_dir = paste0(TEMIR_dir, 'TEMIR_inputs/initial_data/')

# Directory for storing additional R scripts for the crop model
crop_module_code_dir = paste0(TEMIR_dir,"extension_crop/")

# Directory for storing soil temperature maps
# Mandatory, regardless if site-level input is provided
# Default input: MERRA-2 tavg1_2d_lnd_Nx.nc files
soilT_data_dir = paste0(TEMIR_dir,'TEMIR_inputs/soilT_map/')

################################################################################

### Settings for the simulation of all vegetation ###

# Minimum coverage fraction of a PFT for that PFT to be simulated in a grid cell (a number between 0 to 1)
# By default, TEMIR only simulates a PFT if its coverage within a grid cell is greater than 1%.
# To simulate PFTs that are not abundant in a geographical location, this value can be changed to 0. However, be aware that this can greatly increase the computation time for regional/global simulations.
PFT_frac_threshold = 0.01

# Data Source of soil temperature input for the calculation of maintenance respiration ('MERRA2' or 'custom')
# 'MERRA2' (default): using the soil temperature from MERRA-2 (5 layers of soil temperature)
# 'custom': using your own soil temperature data (single- or multilayer), the depth of the bottom of each layer must be provided if there are more than one layer
T_soil_source = 'MERRA2'

# Specify the depth of the bottom of the soil layer of the soil temperature input (in m) (soil T maps)
# For single-layer soil temperature data input, T_soil_depth does not need to be specified as the calculation of root fraction is not needed.
# Only applies to soil temperature map input; soil temperature depth from single-site measurements should be specified in settings_1site_inputs.R.
if (T_soil_source == 'MERRA2') {
   map_TS_meas_depth = c(0.0988, 0.2940, 0.6799, 1.4425, 2.9496)
} else if (T_soil_source == 'custom') {
   # map_TS_meas_depth = c(...)
   # T_soil_depth = NA
}

# How to calculate the maintenance respiration? ('CLM4.5', or 'custom')
# 'CLM4.5' (default): maintenance respiration is function of plant nitrogen content, T2m, Tsoil, and sensitivity coefficient Q10.
# 'custom': implement your own method in maintenance_respiration.R
main_resp_scheme = 'CLM4.5'

# How to calculate LAI, SAI, and canopy height? ('CLM4.5' or 'custom')
# 'CLM4.5' (default): LAI is calculated from leaf carbon pool; SAI is calculated from stem carbon pool.
# 'custom': implement your own methods in plant_physiology.R
LAI_scheme = 'custom'
SAI_scheme = 'CLM4.5'
canopy_h_scheme = 'custom'

################################################################################

### Settings for the simulation of natural vegetation (in development, Jul 2026) ###

# How the assimilated carbon is allocated in natural vegetation? ('CLM4.5' or 'custom')
# Can either be 'CLM4.5' or 'custom'
# 'CLM4.5' (default): follow the description in CLM4.5 (in development)
# 'custom' implement your own scheme in biomass_partitioning.R
natveg_C_partit_method = 'custom'

################################################################################

### Settings for the simulation of crops ###

## How is the planting date of crops determined? ('CLM4.5', 'prescribed-map', 'prescribed-site')
# 'CLM4.5': The planting date is determined by climate (20-year running mean of crop mature GDD requirement) and 10-day running mean T2m. This method assumes farmers adjust the planting date based on past climate. However, planting dates in the corn belt derived with this method are about 30 days earlier when compared to the USDA record. It is recommended to use the Sack et al. 2010 dataset (or another planting calendar) if shifting crop planting under a changing climate is not a concern.
# 'prescribed-map': read in a regional/global crop calendar that contains crop planting date (in day of year). By default, the data compiled by Sack et al. (2010) are used, but it is possible to replace them with another crop calendar. Suitable for regional or global simulations when shifts in planting date are not a concern.
# 'prescribed-site' (default): enter a single value (in day of year) as the planting date. Suitable for single-site/small-scale simulations with a known planting date.
#  In the Sack et al. (2010) dataset, there are primary and secondary crop growing season for maize and rice.
#   - crop_growing_season can either be 'primary' and 'secondary'

crop_planting_date_method = 'prescribed-site'

# Specify the directory name that contains the map of planting date (data is imported at PFT_surf_data.R)
if (crop_planting_date_method == 'prescribed-map') {
    # Set directory contains a map of planting date
    crop_calender_map_dir = paste0(TEMIR_dir,'/TEMIR_inputs/crop_calender/Sack2010/')
    if (exists('crop_growing_season')) {
        warning("'crop_growing_season' for the Sack planting dataset is declared in both 'input_TEMIR_basic_settings.R' and 'input_TEMIR_biogeochem_extension.R'\nIt is overwritten by the one declared in input_TEMIR_biogeochem_extension.R")
    }
    crop_growing_season = 'primary'
}

# Prescribed planting date for single-site simulations
if (crop_planting_date_method == 'prescribed-site') {
    # example case: 15 April
    prescribed_planting_date = 105
}

## How is the growing degree day (GDD) requirement for crop reaching maturity (GDDmat) determined? ('CLM4.5', 'prescribed-map', 'prescribed-site')
# 'CLM4.5': GDDmat are derived from 20-year running mean of GDD0, GDD8 and GDD10 (GDDx) as described in the CLM4.5 technical note. This method assumes farmers will adapt to local climate and use seeds with different maturity requirements.
# 'prescribed-map': read in a regional/global map that contains GDDmat (in growing degree day) for different crops. The default input file contains GDDmat values derived using the planting and harvesting complied by Sack et al (2010). The GDDmat value in the file is the average GDDmat from year 1995 to year 2015 (assuming fixed planting and harvesting date). 
# 'prescribed-site' (default): enter a single GDDmat value (in growing degree day). Suitable for single-site simulations with a pre-determined GDDmat/harvesting day.

# Note: there is a slight difference in the definition of mid-growing season between CLM and TEMIR.
# Mid-growing stage in CLM models is the 'grain-fill' stage (~R3), whereas in this model the mid-growing season in JULES/TEMIR is the 'flowering' stage (~R1).
# The biomass allocation coefficients from different crop models are therefore calibrated following different phenology.

GDDmat_method = 'prescribed-site'

# Specify the directory name that contains the GDDmat files
if (any(GDDmat_method == c('CLM4.5', 'prescribed-map'))) {
    GDDmat_map_dir = paste0(TEMIR_dir, 'TEMIR_inputs/CLM4.5_GDDx_map/')
}

# Specify a GDDmat value for single site and single PFT simulation.
if (GDDmat_method == 'prescribed-site') {
    # example of a European maize growing season
    prescribed_GDDmat = 1650
}

## How is the GDD requirement for crop to reach flowering stage GDDrepr determined? ('CLM4.5', 'custom')
# 'CLM4.5' (default): GDDrepr is a fraction of GDDmat (50% to 70%, depends on crop type)
# 'prescribed-site': enter a single GDDrepr value (in growing degree day). Suitable for single-site simulations with a pre-determined GDDrepr.

GDDrepr_method = 'prescribed-site'
# Fill in the prescribed GDDrepr value here
if (GDDrepr_method == 'prescribed-site') {
    # example of an European maize growing season, flowering on 1 Jun
    prescribed_GDDrepr = 503
}

## How is the GDD requirement for crop emerging from soil GDDemer determined. i.e. when first leaf appears ('CLM4.5', 'custom')
# 'CLM4.5' (default): GDDemer is a fraction of GDDmat (3% to 5%, depends on crop type)
# 'prescribed-site': enter a single GDDemer value (in growing degree day). Suitable for single-site simulations with a pre-determined GDDemer.

GDDemer_method = 'prescribed-site'
# Fill in the prescribed GDDemer value here
if (GDDemer_method == 'prescribed-site') {
    # example of an European maize growing season, emergence on 15 Apr
    prescribed_GDDemer = 36
}

## How is the increase of growing degree day GDD calculated? ('CLM4.5', 'custom')
# 'CLM4.5': the increase of GDD is calculated using the parameters from CLM4.5
# 'JULES' (default): the increase of GDD is calculated using the parameters from JULES-crop
# 'custom': implement your own scheme in phenology.R
GDD_calculation_method = 'JULES'

## How is the assimilated carbon allocated in crops? ('CLM4.5', 'JULES', 'custom') 
# 'CLM4.5': partitioning coefficients are calculated based on the description in CLM4.5, and a new option 'limit_crop_LAI_flag' is added.
# limit_crop_LAI_flag is TRUE: crop LAI is constrained by laimx in PFT_surf.data as implemented in CLM4.5, plant stops carbon partition to leaf when LAI reaches its maximum value.
# limit_crop_LAI_flag is FALSE: the crop LAI constrain is removed.
# 'JULES' (default): partitioning coefficients are calculated based on the description of JULES model described in Osborne et al. (2015)
# 'custom': implement your own scheme in biomass_partitioning.R
crop_C_partit_scheme = 'JULES'
if (crop_C_partit_scheme == 'CLM4.5') limit_crop_LAI_flag = FALSE

## How is carbon translocation parameterized in the model? ('CLM4.5', 'JULES', 'custom')
# Carbon translocation refers to the transport of assimilated carbon within the plant, usually occurs during the reproductive stage as carbon in leaf and stem are transported to grain.
# 'CLM4.5': based on the translocation scheme in CLM4.5, this scheme requires the nitrogen cycle (in development).
# 'JULES' (default): based on the translocation scheme for crops in JULES-crop. This scheme assumes carbon loss during leaf senescence goes to the grain during the reproductive stage. Note that the DVI (phenology) requirement is reduced from 1.5 (in JULES) to 1.0.
# 'custom' implement your own translocation requirement in Cpool_budget.R, or simply leave it blank if the translocation is not wanted.
crop_transloc_scheme = 'JULES'

## How is the leaf senescence rate and the timing of senescence parameterized in the model? ('CLM4.5', 'custom')
# 'CLM4.5' (default): based on the leaf senescence rate described in CLM4.5 Technical Note. Leaves fall at the beginning of the reproductive stage (i.e., GDD >= GDDrepr).
# 'custom': implement your own leaf senescence scheme in plant_phenology.R. Leaf loss does not necessarily start at the beginning of the reproductive stage. Please also modify the variable 'leaf_senescence_DVI' below if needed.
crop_leaf_sen_scheme = 'custom'


## What is the initial leaf carbon per unit area (equivalent to LAI) of crops after emergence?
# The seed or sowing density (which is usually provided in field experiments) determines the initial crop carbon content per unit land area and the potential yield.   
# Here please enter a numeric value for the initial leaf carbon content per unit land area for the simulation (in gC m^-2, default value in CLM4.5 for all crops is 1 gC m^-2).
# Assuming all seed carbon goes to the leaf at emergence, you may use the following formula to estimate the initial leaf carbon content per unit land area.
# leafC_initial ~ sowing_density * average_seed_weight * %_mass_C_in_seed; where average_seed_weight is the average weight of one seed (in g); %_mass_C_in_seed is the percentage mass of carbon in seed which is usually about 40% to 45%.
emergence_carbon_to_leaf = 1.0

################################################################################

# Output/history data archiving:

# Additional output in the biogeochemistry / crop module.

# Recommended output
# The 'output_variables' array here is appended to the 'output_variables' array in input_TEMIR.R.  
output_variables = c(output_variables,
                     'LAI', 'SAI', 'GPP', 'NPP', 'mr_total', 'htop',
                     'GDDT2m', 'GDDTsoil', 'day_of_planting', 'day_of_grain_filling', 'day_of_harvesting', 'day_since_crop_senescence',
                     'leafC', 'finerootC', 'livestemC', 'grainC', 'livestemC_resv',
                     'crop_live_flag', 'crop_plant_flag', 'leaf_emergence_flag', 'grain_fill_flag', 'harvest_flag'
                     # 'CUO_grainfill'
                     )

# Additional available outputs are shown below:
# Outputs except maintenance respiration in the biogeochemistry / crop module have a minimum time resolution of a day, there is an additional option to output variables with a daily resolution.
# 1st col = variable name; 2nd col = unit; 3rd col = long name; 4th col = at which level are the varialbes resolved ('PFT_daily' PFT level daily, 'PFT' PFT level hourly, 'grid' grid level hourly)
available_outputs_df = rbind.data.frame(
    available_outputs_df,
    # Plant physiology variables
    c('LAI', 'm^2 m^-2', 'Total leaf area index', 'PFT_daily'),
    c('SAI', 'm^2 m^-2', 'Total stem area index', 'PFT_daily'),
    c('htop', 'm', 'Canopy height', 'PFT_daily'),
    c('hbot', 'm', 'Canopy bottom', 'PFT_daily'),
    # Plant carbon pool diagnostics
    c('leafC', 'gC m^-2', 'Leaf carbon stock', 'PFT_daily'),
    c('finerootC', 'gC m^-2', 'Fine root carbon stock', 'PFT_daily'),
    c('livestemC', 'gC m^-2', 'Live stem carbon stock', 'PFT_daily'),
    c('livestemC_resv', 'gC m^-2', 'Live stem carbon stock reserved for crop retranslocation', 'PFT_daily'),
    c('deadstemC', 'gC m^-2', 'Dead stem carbon stock', 'PFT_daily'),
    c('livecoarserootC', 'gC m^-2', 'Live coarse root carbon stock', 'PFT_daily'),
    c('deadcoarserootC', 'gC m^-2', 'Dead coarse root carbon stock', 'PFT_daily'),
    c('grainC', 'gC m^-2', 'Grain carbon stock (crop dry yield)', 'PFT_daily'),
    # GPP, NPP, biomass partitioning fluxes and respirations
    c('GPP', 'gC m^-2 s^-1', 'Daily mean GPP', 'PFT_daily'),
    c('NPP', 'gC m^-2 s^-1', 'Daily mean NPP (GPP - growth resp. - main. resp.)', 'PFT_daily'),
    c('leafC_alloc', 'gC m^-2 s^-1', 'Biomass partitioning rate to leafC', 'PFT_daily'),
    c('finerootC_alloc', 'gC m^-2 s^-1', 'Biomass partitioning rate to finerootC', 'PFT_daily'),
    c('livestemC_alloc', 'gC m^-2 s^-1', 'Biomass partitioning rate to livestemC', 'PFT_daily'),
    c('deadstemC_alloc', 'gC m^-2 s^-1', 'Biomass partitioning rate to deadstemC', 'PFT_daily'),
    c('livecoarserootC_alloc', 'gC m^-2 s^-1', 'Biomass partitioning rate to livecoarserootC', 'PFT_daily'),
    c('deadcoarserootC_alloc', 'gC m^-2 s^-1', 'Biomass partitioning rate to deadcoarserootC', 'PFT_daily'),
    c('grainC_alloc', 'gC m^-2 s^-1', 'Biomass partitioning rate to grainC', 'PFT_daily'),
    c('mr_leaf', 'gC m^-2 s^-1', 'Leaf maintenance respiration', 'PFT'),
    c('mr_fineroot', 'gC m^-2 s^-1', 'Fine root maintenance respiration', 'PFT'),
    c('mr_livestem', 'gC m^-2 s^-1', 'Live stem maintenance respiration', 'PFT'),
    c('mr_livecoarseroot', 'gC m^-2 s^-1', 'Live coarse root maintenance respiration', 'PFT'),
    c('mr_grain', 'gC m^-2 s^-1', 'Grain maintenance respiration', 'PFT'),
    c('mr_total', 'gC m^-2 s^-1', 'Total maintenance respiration', 'PFT'),
    # Crop phenology variables
    c('GDDT2m', 'Degree day', 'Growing degree day of T2m', 'PFT_daily'),
    c('GDDTsoil', 'Degree day', 'Growing degree day of Tsoil', 'PFT_daily'),
    c('GDDmat' , 'Degree day', 'Growing degree day requirement to reach crop maturity', 'PFT_daily'),
    c('GDDrepr', 'Degree day', 'Growing degree day requirement to reach grain fill (reproductive stage)', 'PFT_daily'),
    c('GDDemer', 'Degree day', 'Growing degree day requirement to reach leaf emergence (vegetative stage)', 'PFT_daily'),
    c('crop_live_flag', '', 'Flag determines whether crop is live', 'PFT_daily'),
    c('crop_plant_flag', '', 'Flag determines whether crop is planted already', 'PFT_daily'),
    c('leaf_emergence_flag', '', 'Flag determines whether crop is in the vegetative stage', 'PFT_daily'),
    c('grain_fill_flag', '', 'Flag determines whether crop is in the reproductive stage', 'PFT_daily'),
    c('harvest_flag', '', 'Flag determines whether crop is harvested', 'PFT_daily'),
    c('day_of_planting', 'Day of year', 'Crop planting day', 'PFT_daily'),
    c('day_of_grain_filling', 'Day of year', 'Crop flowering day (start of the reproduction stage)', 'PFT_daily'),
    c('day_of_harvesting', 'Day of year', 'Crop harvesting day', 'PFT_daily'),
    c('day_since_crop_senescence', 'Day', 'Number of day passed since leaves start to senesce in crop', 'PFT_daily'),
    # Carbon partitioning coefficients
    c('aleaf','','Biomass allocation fraction to leaf', 'PFT_daily'),
    c('aleaf_leafem', '', 'aleaf during the vegetative stage', 'PFT_daily'),
    c('astem','','Biomass allocation fraction to livestem', 'PFT_daily'),
    c('astem_leafem', '', 'astem during the vegetative stage', 'PFT_daily'),
    c('aroot','','Biomass allocation fraction to fine root', 'PFT_daily'),
    c('arepr','','Biomass allocation fraction to grain (reproductive)', 'PFT_daily')
)

# End of input_TEMIR_biogeochem_extension.R

################################################################################
# Checking for valid inputs and directories

# Declaring missing flags and values

if (!exists("prescribed_planting_date")) prescribed_planting_date = NA
if (!exists("prescribed_GDD_mat")) prescribed_GDD_mat = NA
if (!exists("prescribed_GDD_repr")) prescribed_GDD_mat = NA
if (!exists("prescribed_GDD_emer")) prescribed_GDD_mat = NA
if (!exists("limit_crop_LAI_flag")) limit_crop_LAI_flag = FALSE
if (crop_leaf_sen_scheme != 'custom') leaf_senescence_DVI = NA


# Checking simulation option inputs
if (!any(T_soil_source == c('MERRA2', 'custom'))) stop("Invalid input for 'T_soil_source' in input_TEMIR_crop_extension.R")
if (!any(main_resp_scheme == c('CLM4.5', 'custom'))) stop("Invalid input for 'main_resp_scheme' in input_TEMIR_crop_extension.R")
if (!any(LAI_scheme == c('CLM4.5','custom'))) stop("Invalid input for 'LAI_scheme' in input_TEMIR_crop_extension.R")
if (!any(SAI_scheme == c('CLM4.5','custom'))) stop("Invalid input for 'SAI_scheme' in input_TEMIR_crop_extension.R")
if (!any(canopy_h_scheme == c('CLM4.5','custom'))) stop("Invalid input for 'canopy_h_scheme' in input_TEMIR_crop_extension.R")

if (!any(natveg_C_partit_method == c('CLM4.5','custom'))) stop("Invalid input for 'natveg_C_partit_method' in input_TEMIR_crop_extension.R")
if (!any(crop_planting_date_method == c('CLM4.5','prescribed-map','prescribed-site'))) stop("Invalid input for 'crop_planting_date_method' in input_TEMIR_crop_extension.R")

if (!any(GDDmat_method == c('CLM4.5','prescribed-map','prescribed-site'))) {stop("Invalid input for 'GDDmat_method' in input_TEMIR_crop_extension.R")}
if (!any(GDDrepr_method == c('CLM4.5','prescribed-site'))) {stop("Invalid input for 'GDDrepr_method' in input_TEMIR_crop_extension.R")}
if (!any(GDDemer_method == c('CLM4.5','prescribed-site'))) {stop("Invalid input for 'GDDemer_method' in input_TEMIR_crop_extension.R")}
if (!any(GDD_calculation_method == c('CLM4.5', 'JULES', 'custom'))) {stop("Invalid input for 'GDD_calculation_method' in input_TEMIR_crop_extension.R")}

if (!any(crop_C_partit_scheme == c('CLM4.5','JULES','custom'))) stop("Invalid input for 'crop_C_partit_method' in input_TEMIR_crop_extension.R")
if (!any(crop_transloc_scheme == c('CLM4.5', 'JULES', 'custom'))) stop("Invalid input for 'crop_transloc_method' in input_TEMIR_crop_extension.R")
if (!any(crop_leaf_sen_scheme == c('CLM4.5', 'custom'))) stop("Invalid input for 'crop_leaf_sen_scheme' in input_TEMIR_crop_extension.R")

# Checking if directories are missing
# if (crop_planting_date_method == 'prescribed-map' && !dir.exists(crop_calender_map_dir)) stop('Directory for crop calendar maps does not exist!')
# if (any(GDDmat_method == c('CLM4.5', 'prescribed-map')) && !dir.exists(GDDmat_map_dir)) stop('Directory for crop GDDmat maps does not exist!')


################################################################################
### End of input specification for the biogeochemistry / crop module
################################################################################
