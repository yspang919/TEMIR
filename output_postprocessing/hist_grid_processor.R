# Process daily TEMIR history files for a site or regional grid.
# Edit these settings and run this file with Rscript. In an interactive R
# session, source this file and call do.call(f_process_hist_grid, settings).
# PFT numbers are TEMIR's zero-based values (e.g. 17 = rainfed maize).
settings = list(
   run_dir = 'TEMIR_run/China_crop_1day_test',
   start_date = 20150101,
   end_date = 20150102,
   pft_numbers = c(17L, 18L, 19L, 20L, 23L, 24L),
   output_format = 'nc',        # nc, rds, csv, xlsx
   hourly_mode = 'daily_mean', # none, daily_mean, full, both
   output_dir = NULL,          # NULL uses RUN_DIR/postprocessed
   daily_variables = c('LAI', 'GPP', 'NPP', 'GDDT2m', 'leafC',
                       'finerootC', 'livestemC', 'grainC', 'htop'),
   hourly_variables = c('A_can', 'g_can', 'g_s', 'g_ssun', 'g_ssha',
                        'phi_sun', 'phi_sha', 'LAI_sun', 'LAI_sha',
                        'beta_t', 'CUO_can', 'CUO_sun', 'CUO_sha')
)

f_process_regional_hist = function(run_dir, start_date, end_date,
                                    pft_numbers = c(17L, 18L, 19L, 20L, 23L, 24L),
                                    hourly_mode = 'daily_mean', output_path = NULL,
                                    daily_variables = settings$daily_variables,
                                    hourly_variables = settings$hourly_variables) {
   if (!requireNamespace('ncdf4', quietly = TRUE)) stop('Package ncdf4 is required')
   if (!hourly_mode %in% c('daily_mean', 'full', 'none', 'both')) {
      stop('hourly_mode must be daily_mean, full, none, or both')
   }
   start_day = as.Date(as.character(start_date), format = '%Y%m%d')
   end_day = as.Date(as.character(end_date), format = '%Y%m%d')
   if (is.na(start_day) || is.na(end_day) || start_day > end_day) {
      stop('Supply a valid start and end date in YYYYMMDD format')
   }
   days = seq(start_day, end_day, by = 'day')
   day_codes = format(days, '%Y%m%d')
   run_dir = normalizePath(run_dir, winslash = '/', mustWork = TRUE)
   input_files = file.path(run_dir, 'hist_data', paste0('hist_grid_', day_codes, '.nc'))
   missing_files = input_files[!file.exists(input_files)]
   if (length(missing_files)) {
      stop('Missing ', length(missing_files), ' daily history file(s); first: ', missing_files[1L])
   }
   if (is.null(output_path)) {
      output_path = file.path(run_dir, 'postprocessed',
                              paste0('processed_', day_codes[1L], '_', tail(day_codes, 1L),
                                     '_pft', paste(pft_numbers, collapse = '-'), '.nc'))
   }
   dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
   if (file.exists(output_path)) stop('Output already exists: ', output_path)
   partial_path = paste0(output_path, '.partial.nc')
   if (file.exists(partial_path)) stop('Partial output already exists: ', partial_path)
   success = FALSE
   on.exit(if (!success && file.exists(partial_path)) unlink(partial_path), add = TRUE)

   first = ncdf4::nc_open(input_files[1L])
   first_open = TRUE
   on.exit(if (first_open) ncdf4::nc_close(first), add = TRUE)
   lon = ncdf4::ncvar_get(first, 'lon')
   lat = ncdf4::ncvar_get(first, 'lat')
   source_pft = ncdf4::ncvar_get(first, 'pft')
   source_hour = if (hourly_mode == 'none') NULL else
      ncdf4::ncvar_get(first, 'hour')
   pft_numbers = as.integer(pft_numbers)
   if (!length(pft_numbers) || anyNA(pft_numbers) || anyDuplicated(pft_numbers)) {
      stop('pft_numbers must be nonempty, unique integers')
   }
   pft_index = match(pft_numbers, source_pft)
   if (anyNA(pft_index)) {
      stop('PFT number(s) absent from history files: ',
           paste(pft_numbers[is.na(pft_index)], collapse = ', '))
   }
   source_vars = names(first$var)
   daily_candidates = unique(daily_variables)
   hourly_candidates = unique(hourly_variables)
   daily_vars = intersect(daily_candidates, source_vars)
   hourly_vars = if (hourly_mode == 'none') character() else
      intersect(hourly_candidates, source_vars)
   if (!length(c(daily_vars, hourly_vars))) stop('No requested variables in ', input_files[1L])
   if (length(setdiff(daily_candidates, daily_vars))) {
      message('Daily variables unavailable in source: ',
              paste(setdiff(daily_candidates, daily_vars), collapse = ', '))
   }
   for (name in daily_vars) {
      dims = vapply(first$var[[name]]$dim, function(x) x$name, character(1))
      if (!identical(dims, c('lon', 'lat', 'pft'))) {
         stop('Expected daily [lon, lat, pft] variable: ', name)
      }
   }
   for (name in hourly_vars) {
      dims = vapply(first$var[[name]]$dim, function(x) x$name, character(1))
      if (!identical(dims, c('lon', 'lat', 'pft', 'hour')) ||
          first$dim$hour$len != 24L) {
         stop('Expected 24-hour [lon, lat, pft, hour] variable: ', name)
      }
   }
   nlon = length(lon); nlat = length(lat); npft = length(pft_index)
   x = ncdf4::ncdim_def('lon', 'degrees_east', lon)
   y = ncdf4::ncdim_def('lat', 'degrees_north', lat)
   p = ncdf4::ncdim_def('pft', 'TEMIR PFT number', pft_numbers)
   tday = ncdf4::ncdim_def('day', 'days since 1970-01-01',
                           as.numeric(days - as.Date('1970-01-01')))
   if (hourly_mode %in% c('full', 'both')) {
      thour = ncdf4::ncdim_def('hour',
                               paste0('hours since ', format(start_day, '%Y-%m-%d'), ' 00:00:00 UTC'),
                               seq.int(0L, length(days) * 24L - 1L))
   }
   definitions = list()
   for (name in daily_vars) {
      definitions[[name]] = ncdf4::ncvar_def(name, first$var[[name]]$units,
                                             list(x, y, p, tday), missval = -9999,
                                             prec = 'float', compression = 4)
   }
   for (name in hourly_vars) {
      if (hourly_mode %in% c('daily_mean', 'both')) {
         output_name = if (startsWith(name, 'CUO_'))
            paste0('daily_end_', name) else paste0('daily_mean_', name)
         definitions[[output_name]] = ncdf4::ncvar_def(output_name,
            first$var[[name]]$units, list(x, y, p, tday),
            missval = -9999, prec = 'float', compression = 4)
      }
      if (hourly_mode %in% c('full', 'both')) {
         definitions[[name]] = ncdf4::ncvar_def(name,
            first$var[[name]]$units, list(x, y, p, thour),
            missval = -9999, prec = 'float', compression = 4)
      }
   }
   ncdf4::nc_close(first)
   first_open = FALSE

   out = ncdf4::nc_create(partial_path, definitions)
   out_open = TRUE
   on.exit(if (out_open) ncdf4::nc_close(out), add = TRUE)
   ncdf4::ncatt_put(out, 0, 'source', 'TEMIR daily hist_grid NetCDF files')
   ncdf4::ncatt_put(out, 0, 'hourly_mode', hourly_mode)
   for (day_index in seq_along(days)) {
      input = ncdf4::nc_open(input_files[day_index])
      if (!isTRUE(all.equal(ncdf4::ncvar_get(input, 'lon'), lon)) ||
          !isTRUE(all.equal(ncdf4::ncvar_get(input, 'lat'), lat)) ||
          !isTRUE(all.equal(ncdf4::ncvar_get(input, 'pft'), source_pft)) ||
          (!is.null(source_hour) &&
           !isTRUE(all.equal(ncdf4::ncvar_get(input, 'hour'), source_hour))) ||
          !all(c(daily_vars, hourly_vars) %in% names(input$var))) {
         ncdf4::nc_close(input)
         stop('Grid, PFT, or variable mismatch in ', input_files[day_index])
      }
      for (name in daily_vars) {
         values = array(NA_real_, c(nlon, nlat, npft, 1L))
         for (k in seq_along(pft_index)) {
            cell = ncdf4::ncvar_get(input, name,
                   start = c(1L, 1L, pft_index[k]),
                   count = c(nlon, nlat, 1L), collapse_degen = FALSE)
            values[,,k,1L] = cell[,,1L]
         }
         ncdf4::ncvar_put(out, name, values,
                          start = c(1L, 1L, 1L, day_index),
                          count = c(nlon, nlat, npft, 1L))
      }
      for (name in hourly_vars) {
         if (hourly_mode %in% c('full', 'both')) {
            hourly_values = array(NA_real_, c(nlon, nlat, npft, 24L))
         }
         if (hourly_mode %in% c('daily_mean', 'both')) {
            daily_values = array(NA_real_, c(nlon, nlat, npft, 1L))
         }
         for (k in seq_along(pft_index)) {
            cell = ncdf4::ncvar_get(input, name,
                   start = c(1L, 1L, pft_index[k], 1L),
                   count = c(nlon, nlat, 1L, 24L), collapse_degen = FALSE)
            if (hourly_mode %in% c('full', 'both')) {
               hourly_values[,,k,] = cell[,,1L,]
            }
            if (hourly_mode %in% c('daily_mean', 'both')) {
               if (startsWith(name, 'CUO_')) {
                  daily_values[,,k,1L] = cell[,,1L,24L]
               } else {
                  means = rowMeans(matrix(cell, nrow = nlon * nlat), na.rm = TRUE)
                  means[!is.finite(means)] = NA_real_
                  daily_values[,,k,1L] = matrix(means, nrow = nlon, ncol = nlat)
               }
            }
         }
         if (hourly_mode %in% c('daily_mean', 'both')) {
            output_name = if (startsWith(name, 'CUO_'))
               paste0('daily_end_', name) else paste0('daily_mean_', name)
            ncdf4::ncvar_put(out, output_name, daily_values,
               start = c(1L, 1L, 1L, day_index),
               count = c(nlon, nlat, npft, 1L))
         }
         if (hourly_mode %in% c('full', 'both')) {
            ncdf4::ncvar_put(out, name, hourly_values,
               start = c(1L, 1L, 1L, (day_index - 1L) * 24L + 1L),
               count = c(nlon, nlat, npft, 24L))
         }
      }
      ncdf4::nc_close(input)
      message('Processed ', day_codes[day_index], ' (', day_index, '/', length(days), ')')
   }
   ncdf4::nc_close(out)
   out_open = FALSE
   if (!file.rename(partial_path, output_path)) stop('Could not finalize ', output_path)
   success = TRUE
   invisible(output_path)
}

f_process_hist_grid = function(run_dir, start_date, end_date,
                                 pft_numbers = c(17L, 18L, 19L, 20L, 23L, 24L),
                                 output_format = 'nc', hourly_mode = 'daily_mean',
                                 output_dir = NULL,
                                 daily_variables = settings$daily_variables,
                                 hourly_variables = settings$hourly_variables) {
   if (!output_format %in% c('nc', 'rds', 'csv', 'xlsx')) {
      stop('output_format must be nc, rds, csv, or xlsx')
   }
   if (!hourly_mode %in% c('none', 'daily_mean', 'full', 'both')) {
      stop('hourly_mode must be none, daily_mean, full, or both')
   }
   run_dir = normalizePath(run_dir, winslash = '/', mustWork = TRUE)
   start_day = as.Date(as.character(start_date), format = '%Y%m%d')
   end_day = as.Date(as.character(end_date), format = '%Y%m%d')
   if (is.na(start_day) || is.na(end_day) || start_day > end_day) {
      stop('Supply a valid start and end date in YYYYMMDD format')
   }
   days = seq(start_day, end_day, by = 'day')
   day_codes = format(days, '%Y%m%d')
   files = file.path(run_dir, 'hist_data', paste0('hist_grid_', day_codes, '.nc'))
   missing_files = files[!file.exists(files)]
   if (length(missing_files)) {
      stop('Missing ', length(missing_files), ' history file(s); first: ', missing_files[1L])
   }
   if (is.null(output_dir)) output_dir = file.path(run_dir, 'postprocessed')
   dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
   prefix = paste0('processed_', day_codes[1L], '_', tail(day_codes, 1L),
                   '_pft', paste(pft_numbers, collapse = '-'))
   if (output_format == 'nc') {
      return(f_process_regional_hist(run_dir, start_date, end_date,
                pft_numbers, hourly_mode,
                file.path(output_dir, paste0(prefix, '.nc')),
                daily_variables, hourly_variables))
   }
   if (!requireNamespace('ncdf4', quietly = TRUE)) stop('Package ncdf4 is required')
   first = ncdf4::nc_open(files[1L])
   on.exit(ncdf4::nc_close(first), add = TRUE)
   lon = as.numeric(ncdf4::ncvar_get(first, 'lon'))
   lat = as.numeric(ncdf4::ncvar_get(first, 'lat'))
   source_pft = as.numeric(ncdf4::ncvar_get(first, 'pft'))
   source_hour = as.numeric(ncdf4::ncvar_get(first, 'hour'))
   pft_numbers = as.integer(pft_numbers)
   if (!length(pft_numbers) || anyNA(pft_numbers) || anyDuplicated(pft_numbers)) {
      stop('pft_numbers must be nonempty, unique integers')
   }
   pft_index = match(pft_numbers, source_pft)
   if (anyNA(pft_index)) stop('Requested PFT number is absent from history files')
   nlon = length(lon); nlat = length(lat); npft = length(pft_index)
   if (output_format %in% c('csv', 'xlsx') && nlon * nlat != 1L) {
      stop('CSV/XLSX export requires one grid cell; use nc or rds for a region')
   }
   if (output_format == 'xlsx' && !requireNamespace('openxlsx', quietly = TRUE)) {
      stop('Package openxlsx is required for XLSX export')
   }
   if (length(source_hour) != 24L) stop('Expected 24 hourly steps per history file')
   daily_candidates = unique(daily_variables)
   hourly_candidates = unique(hourly_variables)
   daily_vars = intersect(daily_candidates, names(first$var))
   hourly_vars = if (hourly_mode == 'none') character() else
      intersect(hourly_candidates, names(first$var))
   if (!length(c(daily_vars, hourly_vars))) stop('No selected variables in history files')
   for (name in daily_vars) {
      dims = vapply(first$var[[name]]$dim, function(x) x$name, character(1))
      if (!identical(dims, c('lon', 'lat', 'pft'))) {
         stop('Expected daily [lon, lat, pft] variable: ', name)
      }
   }
   for (name in hourly_vars) {
      dims = vapply(first$var[[name]]$dim, function(x) x$name, character(1))
      if (!identical(dims, c('lon', 'lat', 'pft', 'hour'))) {
         stop('Expected hourly [lon, lat, pft, hour] variable: ', name)
      }
   }
   want_summary = hourly_mode %in% c('daily_mean', 'both')
   want_hourly = hourly_mode %in% c('full', 'both')
   summary_name = function(name) if (startsWith(name, 'CUO_'))
      paste0('daily_end_', name) else paste0('daily_mean_', name)
   site = nlon == 1L && nlat == 1L
   output_path = file.path(output_dir, paste0(prefix, '.', output_format))
   if (file.exists(output_path)) stop('Processed output already exists: ', output_path)
   regional_rds = output_format == 'rds' && !site
   if (regional_rds) {
      daily_names = daily_vars
      summary_names = if (want_summary) vapply(hourly_vars, summary_name,
                                               character(1)) else character()
      hourly_names = if (want_hourly) hourly_vars else character()
      estimated_gib = (as.double(nlon) * nlat * npft * length(days) * 8 *
         (length(daily_names) + length(summary_names) +
          24 * length(hourly_names))) / 1024^3
      if (estimated_gib > 2) {
         warning(sprintf('Regional RDS arrays need at least %.1f GiB of RAM; processing may need more. Select fewer variables/PFTs or use NetCDF if memory is limited.',
                         estimated_gib))
      }
      regional_data = list(
         lon = lon, lat = lat, pft_number = pft_numbers,
         date = days,
         UTC_time = if (want_hourly) as.POSIXct(days[1L], tz = 'UTC') +
                    seq.int(0L, length(days) * 24L - 1L) * 3600 else NULL,
         dimension_order = c('lon', 'lat', 'pft', 'time'),
         daily = setNames(lapply(daily_names, function(x)
            array(NA_real_, c(nlon, nlat, npft, length(days)))), daily_names),
         daily_from_hourly = setNames(lapply(summary_names, function(x)
            array(NA_real_, c(nlon, nlat, npft, length(days)))), summary_names),
         hourly = setNames(lapply(hourly_names, function(x)
            array(NA_real_, c(nlon, nlat, npft, length(days) * 24L))), hourly_names)
      )
   }
   daily_rows = list(); hourly_rows = list()
   for (day_index in seq_along(days)) {
      input = ncdf4::nc_open(files[day_index])
      if (!isTRUE(all.equal(as.numeric(ncdf4::ncvar_get(input, 'lon')), lon)) ||
          !isTRUE(all.equal(as.numeric(ncdf4::ncvar_get(input, 'lat')), lat)) ||
          !isTRUE(all.equal(as.numeric(ncdf4::ncvar_get(input, 'pft')), source_pft)) ||
          !isTRUE(all.equal(as.numeric(ncdf4::ncvar_get(input, 'hour')), source_hour)) ||
          !all(c(daily_vars, hourly_vars) %in% names(input$var))) {
         ncdf4::nc_close(input)
         stop('Grid, PFT, hour, or variable mismatch in ', files[day_index])
      }
      native = list(); summaries = list(); hourly = list()
      for (name in daily_vars) {
         values = array(NA_real_, c(nlon, nlat, npft))
         for (k in seq_along(pft_index)) {
            cell = ncdf4::ncvar_get(input, name,
               start = c(1L, 1L, pft_index[k]),
               count = c(nlon, nlat, 1L), collapse_degen = FALSE)
            values[,,k] = cell[,,1L]
         }
         native[[name]] = values
      }
      for (name in hourly_vars) {
         if (want_hourly) full_values = array(NA_real_, c(nlon, nlat, npft, 24L))
         if (want_summary) daily_values = array(NA_real_, c(nlon, nlat, npft))
         for (k in seq_along(pft_index)) {
            cell = ncdf4::ncvar_get(input, name,
               start = c(1L, 1L, pft_index[k], 1L),
               count = c(nlon, nlat, 1L, 24L), collapse_degen = FALSE)
            if (want_hourly) full_values[,,k,] = cell[,,1L,]
            if (want_summary) {
               if (startsWith(name, 'CUO_')) {
                  daily_values[,,k] = cell[,,1L,24L]
               } else {
                  means = rowMeans(matrix(cell, nrow = nlon * nlat), na.rm = TRUE)
                  means[!is.finite(means)] = NA_real_
                  daily_values[,,k] = matrix(means, nrow = nlon, ncol = nlat)
               }
            }
         }
         if (want_hourly) hourly[[name]] = full_values
         if (want_summary) summaries[[summary_name(name)]] = daily_values
      }
      ncdf4::nc_close(input)
      if (regional_rds) {
         for (name in names(native)) {
            regional_data$daily[[name]][,,,day_index] = native[[name]]
         }
         for (name in names(summaries)) {
            regional_data$daily_from_hourly[[name]][,,,day_index] = summaries[[name]]
         }
         for (name in names(hourly)) {
            hour_indices = (day_index - 1L) * 24L + seq_len(24L)
            regional_data$hourly[[name]][,,,hour_indices] = hourly[[name]]
         }
      } else {
         daily_row = data.frame(UTC_date = rep(day_codes[day_index], npft),
                                 pft_number = pft_numbers)
         for (name in names(native)) daily_row[[name]] = as.vector(native[[name]])
         for (name in names(summaries)) daily_row[[name]] = as.vector(summaries[[name]])
         daily_rows[[day_index]] = daily_row
         if (want_hourly) {
            # TEMIR stores hourly slots as 1:24; the first slot is 00:00 UTC.
            times = as.POSIXct(days[day_index], tz = 'UTC') +
                    (seq_along(source_hour) - 1L) * 3600
            hourly_row = data.frame(
               UTC_time = rep(format(times, '%Y-%m-%d %H:%M:%S', tz = 'UTC'),
                              each = npft),
               pft_number = rep(pft_numbers, times = 24L))
            for (name in names(hourly)) hourly_row[[name]] = as.vector(hourly[[name]])
            hourly_rows[[day_index]] = hourly_row
         }
      }
      message('Processed ', day_codes[day_index], ' (', day_index,
              '/', length(days), ')')
   }
   if (regional_rds) {
      partial_path = paste0(output_path, '.partial')
      if (file.exists(partial_path)) stop('Partial output already exists: ', partial_path)
      success = FALSE
      on.exit(if (!success && file.exists(partial_path)) unlink(partial_path),
              add = TRUE)
      saveRDS(regional_data, partial_path)
      if (!file.rename(partial_path, output_path)) {
         stop('Could not finalize ', output_path)
      }
      success = TRUE
      return(invisible(output_path))
   }
   daily_df = do.call(rbind, daily_rows)
   hourly_df = if (want_hourly) do.call(rbind, hourly_rows) else NULL
   if (output_format == 'rds') {
      saveRDS(list(lon = lon, lat = lat, pft_number = pft_numbers,
                   daily = daily_df, hourly = hourly_df), output_path)
   } else if (output_format == 'csv') {
      daily_path = file.path(output_dir, paste0(prefix, '_daily.csv'))
      hourly_path = file.path(output_dir, paste0(prefix, '_hourly.csv'))
      if (file.exists(daily_path) || (want_hourly && file.exists(hourly_path))) {
         stop('CSV output already exists')
      }
      write.csv(daily_df, daily_path, row.names = FALSE, na = '')
      if (want_hourly) write.csv(hourly_df, hourly_path, row.names = FALSE, na = '')
      return(invisible(c(daily_path, if (want_hourly) hourly_path)))
   } else {
      workbook = openxlsx::createWorkbook()
      openxlsx::addWorksheet(workbook, 'daily')
      openxlsx::writeData(workbook, 'daily', daily_df)
      if (want_hourly) {
         openxlsx::addWorksheet(workbook, 'hourly')
         openxlsx::writeData(workbook, 'hourly', hourly_df)
      }
      openxlsx::saveWorkbook(workbook, output_path, overwrite = FALSE)
   }
   invisible(output_path)
}

if (sys.nframe() == 0L) {
   args = commandArgs(trailingOnly = TRUE)
   if (length(args) > 6L || (length(args) > 0L && length(args) < 3L)) {
      stop('Usage: Rscript hist_grid_processor.R RUN_DIR START END [PFT_NUMBERS_CSV] [nc|rds|csv|xlsx] [none|daily_mean|full|both]')
   }
   if (length(args)) {
      settings$run_dir = args[1L]
      settings$start_date = args[2L]
      settings$end_date = args[3L]
      if (length(args) >= 4L) settings$pft_numbers =
         as.integer(strsplit(args[4L], ',', fixed = TRUE)[[1L]])
      if (length(args) >= 5L) settings$output_format = args[5L]
      if (length(args) >= 6L) settings$hourly_mode = args[6L]
   }
   result = do.call(f_process_hist_grid, settings)
   message('Wrote ', paste(result, collapse = ', '))
}
