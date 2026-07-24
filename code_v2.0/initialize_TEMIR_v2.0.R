################################################################################
### Terrestrial Ecosystem Model in R (TEMIR) v1.1
### Initialization script for single-site, regional or global simulation
################################################################################

# TEMIR version (Change this and the model will copy the necessary scripts):
# version 1.0 (Aug 2023): simulation of terrestrial plant GPP with satellite phenology (satellite-derived LAI)
# version 2.0 (Jul 2026): simulation of staple crops with a vegetation-carbon model
TEMIR_version = '2.0'

# Set TEMIR directory:
TEMIR_dir = 'C:/Users/Document/TEMIR/'
# Set simulation parent directory:
# Users can specify their customized parent directory for their simulations here.
sim_parent_dir = TEMIR_dir

################################################################################
### TEMIR simulation setup:
################################################################################

# Create a name for this simulation:
simulation_name = 'crop_sim_example'

# Simulation types:

# Simulating biogeochemistry? (default: FALSE)
# If TRUE, leaf area index and stem area index are now simulated instead of prescribed.
# Must be TRUE for crop simulations
biogeochem_flag = TRUE

################################################################################
### Check TEMIR model availability:
################################################################################

# Set simulation parent directory:
sim_dir = paste0(sim_parent_dir, 'TEMIR_run/', simulation_name, '/')

# Check if TEMIR directory exists:
if (!dir.exists(paths = TEMIR_dir)) stop('TEMIR directory does not exist!')

# Check if TEMIR version exists:
if (!dir.exists(paths = paste0(TEMIR_dir, '/code_v', TEMIR_version))) stop('TEMIR version does not exist!')

################################################################################
### Initialize TEMIR:
################################################################################

# Check if simulation directory already exists:
if (dir.exists(paths = sim_dir)) stop(paste0('Simulation directory "', sim_dir,'" already exists!'))

# Create output directory:
dir.create(path = paste0(sim_dir, 'hist_data'), recursive = TRUE)

# Create temporary directory:
dir.create(path = paste0(sim_dir, 'temp_data'))

# Set simulation directory as working directory:
setwd(sim_dir)

# Copy execution script:
file.copy(from = paste0(TEMIR_dir, 'code_v', TEMIR_version, '/execution_v', TEMIR_version, '.R'), to = sim_dir)

# Copy baseline input script:
file.copy(from = paste0(TEMIR_dir, 'code_v', TEMIR_version, '/input_TEMIR.R'), to = sim_dir)

# Copy library setup script:
file.copy(from = paste0(TEMIR_dir, 'code_v', TEMIR_version, '/library_setup.R'), to = sim_dir)

# Copy script that contains functions to analyze outputs:
file.copy(from = paste0(TEMIR_dir, 'code_v', TEMIR_version, '/find_hist_stat.R'), to = sim_dir)

# Copy input script for the crop model (only if biogeochem_flag == TRUE)
if (biogeochem_flag) file.copy(from = paste0(TEMIR_dir, '/extension_crop/', 'input_TEMIR_crop_extension.R'), to = sim_dir)

# Copy input script for using site observation data to drive TEMIR
file.copy(from = paste0(TEMIR_dir, 'code_v', TEMIR_version, '/settings_1site_inputs.R'), to = sim_dir)

print(paste0('Sucessfully initializing TEMIR simulation: ', simulation_name))

################################################################################
### End of initialization
################################################################################
