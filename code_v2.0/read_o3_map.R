# Read one calendar year of hourly ozone concentrations in ppbv.
# The model uses hour (day-of-year - 1) * 24 + hour-of-day. The reader checks
# the number of hours, but the user must supply a complete hourly UTC series
# beginning on January 1 at 00:00, in chronological order.
f_read_o3_year = function(path, array_name, dim_vec, year, lon, lat) {
   if (!file.exists(path)) stop('Ozone file does not exist: ', path)
   nc = ncdf4::nc_open(path)
   on.exit(ncdf4::nc_close(nc))

   expected_dims = unname(dim_vec[c('longitude', 'latitude', 'time')])
   if (length(expected_dims) != 3L || anyNA(expected_dims) ||
       !array_name %in% names(nc$var)) {
      stop('Ozone variable or dimension configuration is invalid for ', path)
   }
   actual_dims = vapply(nc$var[[array_name]]$dim, function(x) x$name, character(1))
   if (!identical(actual_dims, expected_dims)) {
      stop('Ozone variable ', array_name, ' must have dimensions [',
           paste(expected_dims, collapse = ', '), '] in ', path)
   }
   expected_hours = as.integer(as.Date(sprintf('%04d-01-01', year + 1L)) -
                               as.Date(sprintf('%04d-01-01', year))) * 24L
   if (nc$dim[[expected_dims[3L]]]$len != expected_hours) {
      stop('Ozone file for ', year, ' must contain ', expected_hours,
           ' hourly values, but ', path, ' contains ',
           nc$dim[[expected_dims[3L]]]$len)
   }
   lon_o3 = ncdf4::ncvar_get(nc, expected_dims[1L])
   lat_o3 = ncdf4::ncvar_get(nc, expected_dims[2L])
   grid_edges = function(x) {
      half_cell = abs(x[2L] - x[1L]) / 2
      c(min(x) - half_cell, max(x) + half_cell)
   }
   lon_o3_edges = grid_edges(lon_o3)
   lat_o3_edges = grid_edges(lat_o3)
   lon_model_edges = grid_edges(lon)
   lat_model_edges = grid_edges(lat)
   if (lon_o3_edges[1L] > lon_model_edges[1L] + 1e-8 ||
       lon_o3_edges[2L] < lon_model_edges[2L] - 1e-8 ||
       lat_o3_edges[1L] > lat_model_edges[1L] + 1e-8 ||
       lat_o3_edges[2L] < lat_model_edges[2L] - 1e-8) {
      warning('Ozone map does not cover the full model grid: ', path,
              '. Model cells without ozone overlap will remain NA after regridding.',
              call. = FALSE)
   }
   o3 = ncdf4::ncvar_get(nc, array_name, collapse_degen = FALSE)
   units = nc$var[[array_name]]$units
   if (!is.null(units) && nzchar(units) &&
       !tolower(trimws(units)) %in% c('ppb', 'ppbv')) {
      warning('Ozone concentrations are assumed to be ppbv, but ', path,
              ' declares units ', units)
   }
   if (!isTRUE(all.equal(as.numeric(lon), as.numeric(lon_o3),
                         tolerance = 1e-8, check.attributes = FALSE)) ||
       !isTRUE(all.equal(as.numeric(lat), as.numeric(lat_o3),
                         tolerance = 1e-8, check.attributes = FALSE))) {
      message('Regridding hourly ozone concentrations from ', path)
      o3 = sp.regrid(spdata = o3, lon.in = lon_o3, lat.in = lat_o3,
                     lon.out = lon, lat.out = lat)
   }
   if (!identical(dim(o3), c(length(lon), length(lat), expected_hours))) {
      stop('Ozone array does not match the model grid and simulation year: ', path)
   }
   if (!any(is.finite(o3))) stop('Ozone map has no finite values: ', path)
   o3
}
