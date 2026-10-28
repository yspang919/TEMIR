library(ncdf4)
source('code_v2.0/read_o3_map.R')
source('output_postprocessing/hist_grid_processor.R')

run_checks = function() {
scratch = tempfile('temir_ozone_postprocessing_')
dir.create(scratch)
dir.create(file.path(scratch, 'hist_data'))
on.exit(unlink(scratch, recursive = TRUE), add = TRUE)

lon = c(110, 110.5)
lat = c(30, 30.5)
x = ncdim_def('lon', 'degrees_east', lon)
y = ncdim_def('lat', 'degrees_north', lat)
p = ncdim_def('pft', 'unitless', c(0, 17, 23))
h = ncdim_def('hour', 'hours', seq_len(24))
for (day in 1:2) {
   path = file.path(scratch, 'hist_data',
                    sprintf('hist_grid_201501%02d.nc', day))
   nc = nc_create(path, list(
      ncvar_def('GPP', 'gC m-2 d-1', list(x, y, p), missval = -9999),
      ncvar_def('A_can', 'umol m-2 s-1', list(x, y, p, h), missval = -9999),
      ncvar_def('CUO_can', 'mmol m-2', list(x, y, p, h), missval = -9999)
   ))
   daily = array(day + seq_len(12), c(2, 2, 3))
   hourly = array(rep(seq_len(24) + day, each = 12), c(2, 2, 3, 24))
   ncvar_put(nc, 'GPP', daily)
   ncvar_put(nc, 'A_can', hourly)
   ncvar_put(nc, 'CUO_can', hourly * 2)
   nc_close(nc)
}

daily_path = file.path(scratch, 'daily_mean.nc')
f_process_regional_hist(scratch, 20150101, 20150102, c(17, 23),
                         'daily_mean', daily_path)
nc = nc_open(daily_path)
stopifnot(identical(dim(ncvar_get(nc, 'GPP', collapse_degen = FALSE)),
                    c(2L, 2L, 2L, 2L)))
stopifnot(identical(as.numeric(ncvar_get(nc, 'pft')), c(17, 23)))
stopifnot(ncvar_get(nc, 'GPP', start = c(1, 1, 1, 1),
                    count = c(1, 1, 1, 1)) == 6)
stopifnot(isTRUE(all.equal(as.numeric(ncvar_get(nc, 'daily_mean_A_can',
                    start = c(1, 1, 1, 1), count = c(1, 1, 1, 1))), 13.5)))
stopifnot('daily_end_CUO_can' %in% names(nc$var))
stopifnot(ncvar_get(nc, 'daily_end_CUO_can', start = c(1, 1, 1, 1),
                    count = c(1, 1, 1, 1)) == 50)
nc_close(nc)

full_path = file.path(scratch, 'full_hourly.nc')
f_process_regional_hist(scratch, 20150101, 20150102, 17,
                         'full', full_path)
nc = nc_open(full_path)
stopifnot(identical(dim(ncvar_get(nc, 'A_can', collapse_degen = FALSE)),
                    c(2L, 2L, 1L, 48L)))
stopifnot(identical(as.numeric(ncvar_get(nc, 'hour')), as.numeric(0:47)))
stopifnot(ncvar_get(nc, 'A_can', start = c(1, 1, 1, 25),
                    count = c(1, 1, 1, 1)) == 3)
nc_close(nc)

both_dir = file.path(scratch, 'both_output')
both_path = f_process_hist_grid(scratch, 20150101, 20150102, 17,
                                'nc', 'both', both_dir)
nc = nc_open(both_path)
stopifnot(all(c('GPP', 'daily_mean_A_can', 'daily_end_CUO_can',
                'A_can', 'CUO_can') %in% names(nc$var)))
stopifnot(identical(dim(ncvar_get(nc, 'A_can', collapse_degen = FALSE)),
                    c(2L, 2L, 1L, 48L)))
nc_close(nc)

rds_path = f_process_hist_grid(scratch, 20150101, 20150102,
                               c(17, 23), 'rds', 'both',
                               file.path(scratch, 'rds_output'))
stopifnot(file.exists(rds_path),
          length(list.files(dirname(rds_path), pattern = '\\.rds$')) == 1L)
regional_case = readRDS(rds_path)
stopifnot(identical(regional_case$dimension_order,
                    c('lon', 'lat', 'pft', 'time')))
stopifnot(identical(dim(regional_case$daily$GPP), c(2L, 2L, 2L, 2L)))
stopifnot(identical(dim(regional_case$daily_from_hourly$daily_mean_A_can),
                    c(2L, 2L, 2L, 2L)))
stopifnot(identical(dim(regional_case$hourly$A_can), c(2L, 2L, 2L, 48L)))
stopifnot(length(regional_case$date) == 2L,
          length(regional_case$UTC_time) == 48L)
stopifnot(regional_case$daily$GPP[1,1,1,1] == 6,
          regional_case$daily$GPP[1,1,1,2] == 7,
          regional_case$daily_from_hourly$daily_end_CUO_can[1,1,1,1] == 50,
          regional_case$hourly$A_can[1,1,1,25] == 3)

site_dir = file.path(scratch, 'site')
dir.create(site_dir)
dir.create(file.path(site_dir, 'hist_data'))
site_x = ncdim_def('lon', 'degrees_east', 110)
site_y = ncdim_def('lat', 'degrees_north', 30)
for (day in 1:2) {
   nc = nc_create(file.path(site_dir, 'hist_data',
                            sprintf('hist_grid_201501%02d.nc', day)), list(
      ncvar_def('GPP', 'gC m-2 d-1', list(site_x, site_y, p), missval = -9999),
      ncvar_def('A_can', 'umol m-2 s-1', list(site_x, site_y, p, h), missval = -9999)
   ))
   ncvar_put(nc, 'GPP', array(day + 1:3, c(1, 1, 3)))
   ncvar_put(nc, 'A_can', array(rep(seq_len(24) + day, each = 3),
                               c(1, 1, 3, 24)))
   nc_close(nc)
}
csv_paths = f_process_hist_grid(site_dir, 20150101, 20150102, 17,
                                'csv', 'both', file.path(scratch, 'csv_output'))
stopifnot(length(csv_paths) == 2L)
daily_csv = read.csv(csv_paths[1L]); hourly_csv = read.csv(csv_paths[2L])
stopifnot(nrow(daily_csv) == 2L, nrow(hourly_csv) == 48L)
stopifnot(isTRUE(all.equal(daily_csv$daily_mean_A_can[1L], 13.5)))
stopifnot(hourly_csv$UTC_time[1L] == '2015-01-01 00:00:00',
          hourly_csv$UTC_time[24L] == '2015-01-01 23:00:00',
          hourly_csv$UTC_time[25L] == '2015-01-02 00:00:00')
site_rds = readRDS(f_process_hist_grid(site_dir, 20150101, 20150102,
                 17, 'rds', 'both', file.path(scratch, 'site_rds_output')))
stopifnot(nrow(site_rds$daily) == 2L, nrow(site_rds$hourly) == 48L)
if (requireNamespace('openxlsx', quietly = TRUE)) {
   xlsx_path = f_process_hist_grid(site_dir, 20150101, 20150102, 17,
                     'xlsx', 'both', file.path(scratch, 'site_xlsx_output'))
   stopifnot(identical(openxlsx::getSheetNames(xlsx_path),
                       c('daily', 'hourly')))
}
site_nc = f_process_hist_grid(site_dir, 20150101, 20150102, 17,
                             'nc', 'both', file.path(scratch, 'site_nc_output'))
nc = nc_open(site_nc)
stopifnot(identical(dim(ncvar_get(nc, 'A_can', collapse_degen = FALSE)),
                    c(1L, 1L, 1L, 48L)))
nc_close(nc)

ozone_path = file.path(scratch, 'ozone_2015.nc')
ot = ncdim_def('time', 'hours since 2015-01-01 00:00:00 UTC', 0:8759)
nc = nc_create(ozone_path, ncvar_def('houro3', 'ppbv', list(x, y, ot),
                                     missval = -9999))
ncvar_put(nc, 'houro3', array(40, c(2, 2, 8760)))
nc_close(nc)
ozone = f_read_o3_year(ozone_path, 'houro3',
                       c(longitude = 'lon', latitude = 'lat', time = 'time'),
                       2015, lon, lat)
stopifnot(identical(dim(ozone), c(2L, 2L, 8760L)), ozone[1, 1, 8760] == 40)
feb2_first_hour = (as.integer(as.Date('2015-02-02') -
                             as.Date('2015-01-01'))) * 24L + 1L
stopifnot(feb2_first_hour == 769L,
          ozone[1, 1, feb2_first_hour] == 40)
bad_year = tryCatch({
   f_read_o3_year(ozone_path, 'houro3',
                  c(longitude = 'lon', latitude = 'lat', time = 'time'),
                  2016, lon, lat)
   FALSE
}, error = function(e) grepl('8784', conditionMessage(e), fixed = TRUE))
stopifnot(bad_year)

cat('Ozone map and regional postprocessing checks passed\n')
}

run_checks()
