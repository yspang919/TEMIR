# Run with Rscript tests/test_custom_met_map.R from the repository root.
source('code_v2.0/tools.R')

test_dir = tempfile('temir_cache_test_')
dir.create(test_dir)

lon = c(-180, -177.5, -175)
lat = c(-2, 0, 2)
grid_file = f_grid_cache_path(test_dir, 'surf_data_regrid_2000', lon, lat)
shifted_grid_file = f_grid_cache_path(test_dir, 'surf_data_regrid_2000', lon + 0.5, lat)
fine_grid_file = f_grid_cache_path(test_dir, 'surf_data_regrid_2000',
                                   seq(-180, -175, by = 1), lat)
stopifnot(grid_file != shifted_grid_file, grid_file != fine_grid_file)

sample_grid = matrix(seq_len(length(lon) * length(lat)), length(lon), length(lat))
legacy_file = file.path(test_dir, 'surf_data_regrid_2000.RData')
save(lon, lat, sample_grid, file = legacy_file)
stopifnot(isTRUE(f_reuse_legacy_grid_cache(legacy_file, grid_file, lon, lat)))
stopifnot(!isTRUE(f_reuse_legacy_grid_cache(legacy_file, shifted_grid_file,
                                            lon + 0.5, lat)))

loaded = new.env(parent = emptyenv())
f_load_grid_cache(grid_file, lon, lat, envir = loaded)
stopifnot(identical(loaded$sample_grid, sample_grid))
stopifnot(!exists('lon', envir = loaded, inherits = FALSE))
stopifnot(inherits(try(f_load_grid_cache(grid_file, lon + 0.5, lat,
                                       envir = loaded), silent = TRUE), 'try-error'))

config = new.env(parent = globalenv())
config$met_data_dir = test_dir
config$biogeochem_flag = TRUE
sys.source('code_v2.0/settings_custom_met_inputs.R', envir = config)
stopifnot(config$custom_wind_input == 'speed')
stopifnot(config$custom_pressure_input == 'surface')
stopifnot(config$T_soil_source == 'custom')
stopifnot(identical(names(config$custom_soil_temperature_vars),
                    paste0('TSOIL', 1:5)))
stopifnot(length(config$map_TS_meas_depth) == 5)

config_without_crop = new.env(parent = globalenv())
config_without_crop$met_data_dir = test_dir
config_without_crop$biogeochem_flag = FALSE
sys.source('code_v2.0/settings_custom_met_inputs.R', envir = config_without_crop)
stopifnot(!exists('T_soil_source', envir = config_without_crop, inherits = FALSE))

names_to_read = c('CLDTOT', 'WS', 'U10M', 'V10M', 'PS', 'SLP', 'PARDR', 'PARDF')
surface_speed = f_custom_met_columns(names_to_read, config$custom_met_vars,
                                      'speed', 'surface')
sea_level_components = f_custom_met_columns(names_to_read, config$custom_met_vars,
                                             'components', 'sea_level')
stopifnot(identical(unname(surface_speed[c(2, 5)]), c('WS', 'PS')))
stopifnot(all(is.na(surface_speed[c(3, 4, 6)])))
stopifnot(identical(unname(sea_level_components[c(3, 4, 6)]),
                    c('U10M', 'V10M', 'SLP')))
stopifnot(all(is.na(sea_level_components[c(2, 5)])))
stopifnot(identical(unname(surface_speed[c(7, 8)]), c('PARDR', 'PARDF')))

unlink(test_dir, recursive = TRUE)
cat('Custom map settings and grid caches passed.\n')
