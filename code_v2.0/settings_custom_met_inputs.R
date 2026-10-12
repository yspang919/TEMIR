################################################################################
### Prepared gridded meteorological input for TEMIR
################################################################################

# Copy this script with the simulation and edit it when met_name = 'CUSTOM'.
# Prepare all fields before running TEMIR. Values must already use the TEMIR
# units in input_data_df; this reader performs no custom-source unit conversion.
# Daily fields must have dimensions [longitude, latitude, hour] in the order
# configured by lon and lat in input_TEMIR.R, with 24 UTC hourly averages.

custom_met_dir = file.path(met_data_dir, 'CUSTOM')
custom_met_file = function(yyyymmdd) {
   file.path(custom_met_dir, paste0('CUSTOM.', yyyymmdd, '.nc'))
}

# TEMIR continues to use the MERRA-2 invariant CN file for land fractions and
# surface geopotential. Its fields are regridded if the model uses another grid.
# Set this path explicitly if the static MERRA-2 file is stored elsewhere.
custom_static_file = file.path(met_data_dir, 'MERRA2', '2015', '01',
                               'MERRA2.20150101.CN.2x25.nc4')

# Select one representation for wind and pressure. The corresponding source
# variable names below may be changed independently of TEMIR's internal names.
custom_wind_input = 'speed'       # 'speed' or 'components'
custom_pressure_input = 'surface' # 'surface' or 'sea_level'

# Source variable names for hourly fields. Keep names on the left unchanged.
# WS is wind speed (m s-1); PS is surface pressure (Pa). PARDR and PARDF
# are direct and diffuse photosynthetically active radiation (W m-2).
custom_met_vars = c(
   CLDTOT = 'CLDTOT', GWETROOT = 'GWETROOT', GWETTOP = 'GWETTOP',
   T10M = 'T10M', T2M = 'T2M', HFLUX = 'HFLUX', EFLUX = 'EFLUX',
   PARDF = 'PARDF', PARDR = 'PARDR', SWGDN = 'SWGDN',
   PRECTOT = 'PRECTOT', PRECSNO = 'PRECSNO', SNODP = 'SNODP',
   EVAP = 'EVAP', QV2M = 'QV2M', USTAR = 'USTAR', Z0M = 'Z0M',
   WS = 'WS', U10M = 'U10M', V10M = 'V10M',
   PS = 'PS', SLP = 'SLP'
)

# The crop / biogeochemistry module additionally needs soil temperatures.
# These may be in the same daily file or in a separate file. Set layer bottom
# depths (m) from shallowest to deepest, matching the mapped variable order.
if (biogeochem_flag) {
   T_soil_source = 'custom'
   custom_soil_temperature_file = custom_met_file
   custom_soil_temperature_vars = c(
      TSOIL1 = 'TSOIL1', TSOIL2 = 'TSOIL2', TSOIL3 = 'TSOIL3',
      TSOIL4 = 'TSOIL4', TSOIL5 = 'TSOIL5'
   )
   map_TS_meas_depth = c(0.0988, 0.2940, 0.6799, 1.4425, 2.9496)
}
