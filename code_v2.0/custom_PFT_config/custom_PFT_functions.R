# Custom PFT parameter import. Existing PFT values are never recalibrated here.
# All functions return results before the caller changes its model environment.
f_custom_PFT_schema = function(biogeochem_flag = FALSE) {
    common = c('c3psn', 'crop', 'displar', 'dleaf', 'dsladlai', 'evergreen',
        'flnr', 'irrigated', 'leaf_long', 'leafcn', 'rholnir', 'rholvis',
        'rhosnir', 'rhosvis', 'roota_par', 'rootb_par', 'rootprof_beta',
        'season_decid', 'slatop', 'smpsc', 'smpso', 'stress_decid',
        'taulnir', 'taulvis', 'tausnir', 'tausvis', 'woody', 'xl', 'z0mr')
    bgc = c('aleaff', 'allconsl', 'bfact', 'fleafi', 'astemf', 'allconss',
        'declfact', 'arooti', 'arootf', 'baset', 'croot_stem', 'deadwdcn',
        'fcur', 'ffrootcn', 'fleafcn', 'flivewd', 'frootcn', 'froot_leaf',
        'fstemcn', 'graincn', 'grnfill', 'hybgdd', 'laimx', 'lfemerg',
        'lflitcn', 'livewdcn', 'min_planting_temp', 'mxmat', 'mxtmp',
        'planting_temp', 'stem_leaf', 'ztopmx')
    calendar = c('earliest_planting_jday_possible_NH',
        'latest_planting_jday_possible_NH', 'earliest_planting_jday_possible_SH',
        'latest_planting_jday_possible_SH')
    list(netcdf = c(common, if (biogeochem_flag) bgc),
         numeric = c(common, 'htop', 'hbot', 'g1_med_table',
                     if (biogeochem_flag) c(bgc, calendar)),
         character = 'plantgroup', calendar = calendar,
         all = c(common, 'htop', 'hbot', 'g1_med_table', 'plantgroup', bgc, calendar))
}

f_custom_PFT_mmdd = function(value) {
    if (length(value) != 1L || !is.finite(value) || value != floor(value))
        stop('CLM5 planting date must be one MMDD integer')
    date = as.Date(sprintf('2001-%02d-%02d', value %/% 100, value %% 100))
    if (is.na(date)) stop('Invalid CLM5 MMDD planting date: ', value)
    as.integer(format(date, '%j'))
}

f_read_CLM5_PFT = function(source, schema) {
    if (!requireNamespace('ncdf4', quietly = TRUE))
        stop('CLM5 PFT import requires the ncdf4 R package')
    if (!is.character(source$file) || length(source$file) != 1L ||
        !file.exists(source$file)) stop('CLM5 parameter file not found: ', source$file)
    nc = ncdf4::nc_open(source$file)
    on.exit(ncdf4::nc_close(nc))
    ids = ncdf4::ncvar_get(nc, 'pftnum')
    row = which(ids == source$pft_number)
    if (length(row) != 1L) stop('CLM5 source PFT number must identify exactly one row')
    names = trimws(ncdf4::ncvar_get(nc, 'pftname'))
    if (!is.null(source$pft_name) && !identical(names[row], source$pft_name))
        stop('CLM5 source PFT name does not match its number: ', source$pft_name)
    read_one = function(name) {
        variable = nc$var[[name]]
        if (is.null(variable)) return(NA_real_)
        axes = vapply(variable$dim, function(d) d$name, character(1))
        pft_axis = which(axes == 'pft')
        if (length(pft_axis) != 1L) stop('Expected a pft dimension for ', name)
        start = rep(1L, length(axes)); count = rep(1L, length(axes))
        start[pft_axis] = row
        other = setdiff(seq_along(axes), pft_axis)
        for (axis in other) {
            if (variable$dim[[axis]]$len > 1L) {
                variant = source$rootprof_beta_variant
                if (name != 'rootprof_beta' || is.null(variant) ||
                    length(variant) != 1L || !is.finite(variant) ||
                    variant != floor(variant) || variant < 1L ||
                    variant > variable$dim[[axis]]$len)
                    stop('Select rootprof_beta_variant explicitly for ', name)
                start[axis] = variant
            }
        }
        as.numeric(ncdf4::ncvar_get(nc, name, start = start, count = count))
    }
    values = setNames(lapply(schema$netcdf, read_one), schema$netcdf)
    values$g1_med_table = read_one('medlynslope')
    if (all(schema$calendar %in% schema$numeric)) {
        fields = c('min_NH_planting_date', 'max_NH_planting_date',
                   'min_SH_planting_date', 'max_SH_planting_date')
        for (i in seq_along(fields)) {
            value = read_one(fields[i])
            values[[schema$calendar[i]]] = if (is.finite(value))
                f_custom_PFT_mmdd(value) else NA_real_
        }
    }
    values
}

f_import_custom_PFTs = function(parameters, definitions, biogeochem_flag = FALSE) {
    schema = f_custom_PFT_schema(biogeochem_flag)
    required = c(schema$numeric, schema$character)
    n = length(parameters$pftnum)
    if (!n || !identical(as.integer(parameters$pftnum), seq_len(n) - 1L) ||
        length(parameters$pftname) != n || nrow(parameters$PFT_df) != n ||
        !identical(as.integer(parameters$PFT_df$PFT_number), seq_len(n) - 1L))
        stop('Existing PFT catalog must use contiguous numbers starting at zero')
    for (name in required) {
        if (length(parameters[[name]]) != n)
            stop('Existing PFT parameter has an inconsistent length: ', name)
    }
    if (!is.list(definitions) || !length(definitions))
        stop('custom_PFTs must be a nonempty list of PFT definitions')
    if (is.null(parameters$PFT_df$crop_type)) {
        # Legacy cases have only number/description columns. Preserve their
        # existing crop classifications while adding explicit type metadata.
        types = c(corn = 'maize', irrigated_corn = 'maize',
            spring_temperate_cereal = 'springwheat', irrigated_spring_temperate_cereal = 'springwheat',
            winter_temperate_cereal = 'winterwheat', irrigated_winter_temperate_cereal = 'winterwheat',
            soybean = 'soybean', irrigated_soybean = 'soybean')
        parameters$PFT_df$crop_type = unname(types[as.character(parameters$PFT_df$PFT_description)])
        parameters$PFT_df$crop_type[is.na(parameters$PFT_df$crop_type)] = 'none'
    }
    for (definition in definitions) {
        if (!is.list(definition)) stop('Each custom PFT definition must be a list')
        allowed = c('pft_number', 'pft_name', 'crop_type', 'initial_state', 'source', 'overrides', 'monthly_LAI', 'monthly_SAI')
        if (length(setdiff(names(definition), allowed)))
            stop('Unknown custom PFT setting: ', paste(setdiff(names(definition), allowed), collapse = ', '))
        id = definition$pft_number; name = definition$pft_name
        crop_type = if (is.null(definition$crop_type)) 'none' else definition$crop_type
        if (!is.character(crop_type) || length(crop_type) != 1L || is.na(crop_type) ||
            !crop_type %in% c('none', 'maize', 'springwheat', 'winterwheat', 'soybean', 'rice'))
            stop('Unknown crop_type in custom PFT definition')
        if (!is.null(definition$initial_state) && !identical(definition$initial_state, 'preplant'))
            stop('Supported custom crop initial_state is preplant (zero canopy and carbon before planting)')
        if (!is.numeric(id) || length(id) != 1L || !is.finite(id) || id != n)
            stop('Next custom TEMIR pft_number must be ', n, ' (R index ', n + 1L, ')')
        if (!is.character(name) || length(name) != 1L || is.na(name) || !nzchar(trimws(name)) ||
            name %in% c(parameters$pftname, as.character(parameters$PFT_df$PFT_description)))
            stop('Custom PFT name must be nonempty and unique')
        values = setNames(vector('list', length(required)), required)
        source = definition$source
        if (!is.null(source)) {
            if (!identical(source$type, 'CLM5')) stop('Supported source type is CLM5; omit source for manual values')
            if (!is.numeric(source$pft_number) || length(source$pft_number) != 1L ||
                !is.finite(source$pft_number) || source$pft_number != floor(source$pft_number))
                stop('CLM5 source pft_number must be one integer')
            imported = f_read_CLM5_PFT(source, schema)
            values[names(imported)] = imported
        }
        overrides = definition$overrides
        if (!is.null(overrides)) {
            if (!is.list(overrides) || is.null(names(overrides)) ||
                anyDuplicated(names(overrides)) || any(!nzchar(names(overrides))) ||
                length(setdiff(names(overrides), schema$all)))
                stop('Overrides must be a named list of registered PFT parameters')
            active = intersect(names(overrides), required)
            values[active] = overrides[active]
        }
        invalid = required[!vapply(required, function(key) {
            value = values[[key]]
            if (key %in% schema$numeric)
                is.numeric(value) && length(value) == 1L && is.finite(value)
            else is.character(value) && length(value) == 1L && !is.na(value) && nzchar(value)
        }, logical(1))]
        if (length(invalid)) stop('PFT ', name, ' needs explicit finite values for: ',
                                   paste(invalid, collapse = ', '))
        for (key in c('c3psn', 'crop', 'evergreen', 'irrigated', 'season_decid', 'stress_decid', 'woody'))
            if (!values[[key]] %in% c(0, 1)) stop('Expected a 0/1 flag for ', key)
        if (crop_type != 'none' && values$crop != 1) stop('A managed crop_type requires crop = 1')
        for (key in c('leafcn', 'slatop', 'leaf_long'))
            if (values[[key]] <= 0) stop(key, ' must be positive')
        if (values$hbot < 0 || values$htop < values$hbot || values$g1_med_table < 0)
            stop('Invalid canopy heights or Medlyn slope for ', name)
        for (key in c('monthly_LAI', 'monthly_SAI')) {
            value = definition[[key]]
            if (!is.null(value) && (!is.numeric(value) || length(value) != 12L ||
                any(!is.finite(value) | value < 0)))
                stop(key, ' must contain twelve nonnegative monthly values')
        }
        for (key in required) parameters[[key]] = c(parameters[[key]], values[[key]])
        parameters$pftnum = c(parameters$pftnum, id)
        parameters$pftname = c(parameters$pftname, name)
        parameters$PFT_df = rbind(parameters$PFT_df,
            data.frame(PFT_number = id, PFT_description = name, crop_type = crop_type, stringsAsFactors = FALSE))
        n = n + 1L
    }
    parameters
}

# Extend only the PFT axis; preserve every original surface/initial-state layer.
# New cover is zero. Canopy profiles are explicitly supplied by the user.
f_extend_custom_PFT_array = function(x, definitions, base_count) {
    shape = dim(x)
    if (length(shape) < 3L || shape[3] != base_count)
        stop('Expected the original surface PFT count on array dimension three')
    shape[3] = base_count + length(definitions)
    out = array(0, shape)
    subs = lapply(dim(x), seq_len)
    out = do.call('[<-', c(list(out), subs, list(value = x)))
    out
}

# Keep all caches (including later annual LAI loads) under a configuration key.
f_custom_PFT_cache_dir = function(directory, parameters, definitions, biogeochem_flag) {
    temporary = tempfile(fileext = '.rds')
    on.exit(unlink(temporary))
    saveRDS(list(version = 1L, parameters = parameters, definitions = definitions,
                 biogeochem_flag = biogeochem_flag), temporary, version = 2)
    key = unname(tools::md5sum(temporary))
    result = file.path(directory, paste0('custom_PFT_', key))
    dir.create(result, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(result)) stop('Cannot create custom PFT cache directory: ', result)
    paste0(result, '/')
}
