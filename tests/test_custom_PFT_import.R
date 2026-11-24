# Run from the repository root with ncdf4 available in .libPaths().
source('code_v2.0/custom_PFT_config/custom_PFT_functions.R')
stopifnot(requireNamespace('ncdf4', quietly = TRUE))

expect_error = function(expr, pattern) {
    message = tryCatch({ force(expr); NA_character_ }, error = conditionMessage)
    stopifnot(!is.na(message), grepl(pattern, message, fixed = TRUE))
}

base_parameters = function(bgc) {
    schema = f_custom_PFT_schema(bgc)
    x = setNames(lapply(schema$numeric, function(k) rep(1, 25)), schema$numeric)
    x$plantgroup = rep('C3_grass', 25)
    x$pftnum = 0:24
    x$pftname = paste0('original_', 0:24)
    x$PFT_df = data.frame(PFT_number = as.character(0:24),
                         PFT_description = x$pftname)
    x
}

# Deliberately unordered source IDs verify lookup by pftnum, not by row number.
# Store rootprof_beta on a separate variants axis, with different values.
fixture = tempfile(fileext = '.nc')
pft = ncdf4::ncdim_def('pft', '', 1:3, create_dimvar = FALSE)
variant = ncdf4::ncdim_def('variants', '', 1:2, create_dimvar = FALSE)
string = ncdf4::ncdim_def('string_length', '', 1:30, create_dimvar = FALSE)
schema = f_custom_PFT_schema(TRUE)
source_names = setdiff(schema$netcdf, 'rootprof_beta')
source_names = c(source_names, 'pftnum', 'medlynslope',
                 'min_NH_planting_date', 'max_NH_planting_date',
                 'min_SH_planting_date', 'max_SH_planting_date')
variables = lapply(source_names, function(k)
    ncdf4::ncvar_def(k, '', list(pft), missval = -999, prec = 'double'))
variables = c(variables, list(
    ncdf4::ncvar_def('rootprof_beta', '', list(variant, pft), missval = -999, prec = 'double'),
    ncdf4::ncvar_def('pftname', '', list(string, pft), prec = 'char')))
nc = ncdf4::nc_create(fixture, variables)
for (key in source_names) ncdf4::ncvar_put(nc, key, rep(1, 3))
ncdf4::ncvar_put(nc, 'pftnum', c(62, 61, 77))
ncdf4::ncvar_put(nc, 'pftname', c('irrigated_rice', 'rice', 'other'))
ncdf4::ncvar_put(nc, 'rootprof_beta', matrix(c(.91, .92, .93, .94, .95, .96), 2))
ncdf4::ncvar_put(nc, 'leafcn', c(30, 20, 10))
ncdf4::ncvar_put(nc, 'rholnir', c(.2, .35, .4))
ncdf4::ncvar_put(nc, 'arootf', c(.2, -999, .4))
ncdf4::ncvar_put(nc, 'mxmat', c(140, 150, 160))
ncdf4::ncvar_put(nc, 'medlynslope', c(4, 5.79, 6))
for (key in c('min_NH_planting_date', 'max_NH_planting_date',
              'min_SH_planting_date', 'max_SH_planting_date'))
    ncdf4::ncvar_put(nc, key, rep(switch(key, min_NH_planting_date = 101,
        max_NH_planting_date = 228, min_SH_planting_date = 1015,
        max_SH_planting_date = 1231), 3))
ncdf4::nc_close(nc)

definition = list(pft_number = 25, pft_name = 'rice', crop_type = 'rice', initial_state = 'preplant',
    source = list(type = 'CLM5', file = fixture, pft_number = 61,
                  pft_name = 'rice', rootprof_beta_variant = 2),
    overrides = list(htop = .5, hbot = .01, plantgroup = 'C3_grass', arootf = 0))
for (bgc in c(FALSE, TRUE)) {
    base = base_parameters(bgc)
    before = base
    result = f_import_custom_PFTs(base, list(definition), bgc)
    stopifnot(identical(base, before), length(result$pftnum) == 26,
        result$pftnum[26] == 25, result$pftname[26] == 'rice',
        result$PFT_df$PFT_description[26] == 'rice',
        result$PFT_df$crop_type[26] == 'rice',
        result$rholnir[26] == .35, result$rootprof_beta[26] == .94,
        result$leafcn[26] == 20, result$g1_med_table[26] == 5.79)
    for (key in c(f_custom_PFT_schema(bgc)$numeric, 'plantgroup', 'pftname', 'pftnum'))
        stopifnot(isTRUE(all.equal(result[[key]][1:25], as.vector(base[[key]]))))
    if (bgc) stopifnot(result$arootf[26] == 0, result$mxmat[26] == 150,
        result$earliest_planting_jday_possible_NH[26] == 1,
        result$latest_planting_jday_possible_NH[26] == 59,
        result$earliest_planting_jday_possible_SH[26] == 288,
        result$latest_planting_jday_possible_SH[26] == 365)
    else stopifnot(is.null(result$arootf))
}

bad = definition; bad$overrides$arootf = NULL
expect_error(f_import_custom_PFTs(base_parameters(TRUE), list(bad), TRUE), 'arootf')
# Missing BGC values must not prevent the non-BGC import.
stopifnot(length(f_import_custom_PFTs(base_parameters(FALSE), list(bad))$pftnum) == 26)
bad = definition; bad$pft_number = 61
expect_error(f_import_custom_PFTs(base_parameters(FALSE), list(bad)), 'must be 25')
bad = definition; bad$source$pft_name = 'wrong'
expect_error(f_import_custom_PFTs(base_parameters(FALSE), list(bad)), 'does not match')
bad = definition; bad$source$rootprof_beta_variant = NULL
expect_error(f_import_custom_PFTs(base_parameters(FALSE), list(bad)), 'variant explicitly')
bad = definition; bad$overrides$rholnir_typo = .3
expect_error(f_import_custom_PFTs(base_parameters(FALSE), list(bad)), 'registered')
bad = definition; bad$crop_type = 'rice_typo'
expect_error(f_import_custom_PFTs(base_parameters(FALSE), list(bad)), 'crop_type')
bad = definition; bad$monthly_LAI = rep(NA_real_, 12)
expect_error(f_import_custom_PFTs(base_parameters(FALSE), list(bad)), 'monthly_LAI')

# Manual definitions pass through the same validation without reading NetCDF.
manual = definition; manual$source = NULL
manual$overrides = lapply(base_parameters(FALSE)[f_custom_PFT_schema(FALSE)$numeric], `[`, 1)
manual$overrides$plantgroup = 'C3_grass'
stopifnot(length(f_import_custom_PFTs(base_parameters(FALSE), list(manual))$pftnum) == 26)

second = definition; second$pft_number = 26; second$pft_name = 'irrigated_rice'
second$source$pft_number = 62; second$source$pft_name = 'irrigated_rice'
stopifnot(identical(f_import_custom_PFTs(base_parameters(FALSE),
    list(definition, second))$pftname[26:27], c('rice', 'irrigated_rice')))
stopifnot(identical(f_import_custom_PFTs(base_parameters(FALSE),
    list(definition, second))$PFT_df$crop_type[26:27], c('rice', 'rice')))
bad = second; bad$pft_name = 'rice'
expect_error(f_import_custom_PFTs(base_parameters(FALSE), list(definition, bad)), 'unique')

for (shape in list(c(2, 1, 25), c(2, 1, 25, 12))) {
    x = array(seq_len(prod(shape)), shape)
    extended = f_extend_custom_PFT_array(x, list(definition), 25)
    stopifnot(dim(extended)[3] == 26)
    if (length(shape) == 3) stopifnot(all(extended[,,26] == 0), all(as.vector(extended[,,1:25]) == as.vector(x)))
    else stopifnot(all(extended[,,26,] == 0), all(as.vector(extended[,,1:25,]) == as.vector(x)))
}

cache = tempfile('custom-pft-cache-')
remove_test_cache = function() {
    target = normalizePath(cache, winslash = '/', mustWork = TRUE)
    root = paste0(normalizePath(tempdir(), winslash = '/', mustWork = TRUE), '/')
    stopifnot(startsWith(target, root))
    unlink(target, recursive = TRUE)
}
p = base_parameters(FALSE)
cache1 = f_custom_PFT_cache_dir(cache, p, list(definition), FALSE)
stopifnot(identical(cache1, f_custom_PFT_cache_dir(cache, p, list(definition), FALSE)),
    !identical(cache1, f_custom_PFT_cache_dir(cache, p, list(definition), TRUE)))
changed = definition; changed$monthly_LAI = rep(2, 12)
stopifnot(!identical(cache1, f_custom_PFT_cache_dir(cache, p, list(changed), FALSE)))
remove_test_cache()

# Exercise the actual loader hook, including the disabled path and derived Vcmax.
expressions = parse('code_v2.0/PFT_surf_data.R')
surface = which(vapply(expressions, function(e)
    identical(e, quote(print('Loading surface data...', quote = FALSE))), logical(1)))
config = tempfile(fileext = '.R')
dput(list(definition), config)
writeLines(c('custom_PFTs =', readLines(config)), config)
loader = function(enabled, selected = 18, bgc = FALSE) {
    env = new.env(parent = globalenv())
    base = base_parameters(bgc)
    env$nc_open = function(...) NULL
    env$nc_close = function(...) NULL
    env$ncvar_get = function(nc, name) base[[name]]
    env$surf_data_dir = ''; env$code_dir = 'code_v2.0/'
    env$biogeochem_flag = bgc; env$import_new_PFT = enabled
    env$PFT_df = base$PFT_df; env$sim_PFT = selected
    env$custom_PFT_config_file = config; env$processed_surf_data_dir = cache
    for (e in expressions[seq_len(surface - 1L)]) eval(e, env)
    env
}
disabled = loader(FALSE)
enabled = loader(TRUE)
stopifnot(length(disabled$rholnir) == 25, length(enabled$rholnir) == 26,
    isTRUE(all.equal(enabled$vcmax25top[26], enabled$flnr[26] * 7.16 * 60 /
        (enabled$leafcn[26] * enabled$slatop[26]))),
    all(enabled$vcmax25top[1:25] == disabled$vcmax25top))
expect_error(loader(TRUE, selected = 25), 'monthly_LAI and monthly_SAI')
bgc_rice = loader(TRUE, selected = 25, bgc = TRUE)
stopifnot(length(bgc_rice$arootf) == 26)
# Exercise the actual cold-start branch without any legacy initial-state file.
bgc_rice$lon = c(100, 101); bgc_rice$lat = 30
initial_hook = tail(which(vapply(expressions, function(e)
    is.call(e) && identical(e[[1]], as.name('if')), logical(1))), 1)
eval(expressions[[initial_hook]], bgc_rice)
for (key in c('LAI_initial_map', 'SAI_initial_map', 'leafC_initial_map', 'frootC_initial_map',
    'livestemC_initial_map', 'deadstemC_initial_map', 'livecrootC_initial_map', 'deadcrootC_initial_map', 'grainC_initial_map'))
    stopifnot(identical(dim(get(key, bgc_rice)), c(2L, 1L, 26L)), all(get(key, bgc_rice) == 0))
missing_state = definition; missing_state$initial_state = NULL
dput(list(missing_state), config)
writeLines(c('custom_PFTs =', readLines(config)), config)
expect_error(loader(TRUE, selected = 25, bgc = TRUE), 'initial_state = preplant')
dput(list(definition), config)
writeLines(c('custom_PFTs =', readLines(config)), config)
stopifnot(length(loader(TRUE, selected = 18, bgc = TRUE)$arootf) == 26)

# Exercise the loader's surface extension block with one longitude and two
# latitudes. Verify profiles are repeated spatially, not recycled across months.
surface_hook = which(vapply(expressions, function(e)
    is.call(e) && identical(e[[1]], as.name('if')) &&
    identical(e[[2]], as.name('custom_PFT_enabled')), logical(1)))[2]
enabled$custom_PFTs[[1]]$monthly_LAI = 1:12
enabled$custom_PFTs[[1]]$monthly_SAI = (1:12) / 10
enabled$PCT_PFT = array(4, c(1, 2, 25))
for (key in c('MONTHLY_LAI', 'MONTHLY_SAI', 'MONTHLY_HEIGHT_TOP', 'MONTHLY_HEIGHT_BOT'))
    assign(key, array(.2, c(1, 2, 25, 12)), envir = enabled)
eval(expressions[[surface_hook]], enabled)
stopifnot(identical(dim(enabled$PCT_PFT), c(1L, 2L, 26L)),
    all(enabled$PCT_PFT[,,1:25] == 4), all(enabled$PCT_PFT[,,26] == 0),
    all(enabled$MONTHLY_LAI[,,1:25,] == .2),
    identical(as.numeric(enabled$MONTHLY_LAI[1,1,26,]), as.numeric(1:12)),
    identical(as.numeric(enabled$MONTHLY_LAI[1,2,26,]), as.numeric(1:12)),
    identical(as.numeric(enabled$MONTHLY_SAI[1,2,26,]), (1:12) / 10),
    all(enabled$MONTHLY_HEIGHT_TOP[,,26,] == .5))
unlink(c(config, fixture))
remove_test_cache()

# Also verify the user's downloaded source when available; it is not a fixture
# committed to the repository and is not required to run the synthetic tests.
TEMIR_dir = paste0(getwd(), '/')
source('code_v2.0/custom_PFT_config/user_custom_PFT.R')
if (file.exists(custom_PFTs[[1]]$source$file)) {
    for (bgc in c(FALSE, TRUE)) {
        result = f_import_custom_PFTs(base_parameters(bgc), custom_PFTs, bgc)
        stopifnot(abs(result$rholnir[26] - .35) < 1e-6,
            result$leafcn[26] == 20, abs(result$rootprof_beta[26] - .943) < 1e-6,
            result$PFT_df$crop_type[26] == 'rice')
        if (bgc) stopifnot(result$arootf[26] == 0, result$mxmat[26] == 150,
            result$hybgdd[26] == 2100, abs(result$grnfill[26] - .4) < 1e-6,
            abs(result$lfemerg[26] - .01) < 1e-6)
    }
    cat('Downloaded CLM5 rice parameters verified in both BGC modes\n')
}
cat('Custom PFT import, validation, loader and surface dimensions OK\n')
