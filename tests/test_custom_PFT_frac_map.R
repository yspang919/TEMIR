source('code_v2.0/read_custom_PFT_frac.R')

# A synthetic map assigns one layer to two crop PFTs and another to a tree PFT.
# It also checks that model coordinates can select a reversed subset of the map.
fixture <- list(
    lon = c(100, 100.5), lat = c(30, 30.5),
    custom_PFT_frac = array(c(0.2, 0, 0.3, 0.4,
                              0.6, 0.7, 0.8, 0.9), c(2, 2, 2))
)
path <- tempfile(fileext = '.rds')
saveRDS(fixture, path)
baseline <- array(0.05, c(1, 2, 25))
result <- f_apply_custom_PFT_frac(
    baseline, lon = 100.5, lat = c(30.5, 30), path = path,
    layer_to_ipft = data.frame(layer = c(1L, 1L, 2L),
                               ipft = c(18L, 19L, 2L)))
actual <- result$PFT_frac
stopifnot(identical(dim(actual), dim(baseline)),
          identical(result$ipft, c(18L, 19L, 2L)),
          identical(as.numeric(actual[,,18]), c(0.4, 0)),
          identical(as.numeric(actual[,,19]), c(0.4, 0)),
          identical(as.numeric(actual[,,2]), c(0.9, 0.7)),
          identical(as.numeric(actual[,,17]), c(0.05, 0.05)))
unlink(path)

# The existing 2015 crop RDS uses 'fraction' for the payload. Its PFT mapping
# is supplied by input_TEMIR.R; any mapping embedded in the RDS is metadata.
crop_map <- file.path('TEMIR_inputs', 'custom_crop_distribution',
                      'china2015_staplecrops_distribution.rds')
phenology <- readRDS(file.path('TEMIR_inputs', 'crop_phenology',
                              'china2015_staplecrops_phenology.rds'))
map <- readRDS(crop_map)
stopifnot(identical(map$lon, phenology$lon),
          identical(map$lat, phenology$lat),
          identical(dim(map$fraction), c(124L, 72L, 3L)),
          identical(as.integer(vapply(1:3, function(k)
              sum(map$fraction[,,k] > 0), integer(1))),
              c(1803L, 657L, 1125L)))
for (k in 1:3) {
    source_layer <- c(1L, 2L, 4L)[[k]]
    stopifnot(identical(map$fraction[,,k] > 0,
                        is.finite(phenology$GDDmat[,,source_layer])))
}
baseline <- array(0.05, c(length(map$lon), length(map$lat), 25L))
applied <- f_apply_custom_PFT_frac(
    baseline, lon = map$lon, lat = map$lat, path = crop_map,
    layer_to_ipft = data.frame(
        layer = c(1L, 1L, 2L, 2L, 3L, 3L),
        ipft = c(18L, 19L, 20L, 21L, 24L, 25L)))$PFT_frac
stopifnot(identical(applied[,,18], map$fraction[,,1]),
          identical(applied[,,19], map$fraction[,,1]),
          identical(applied[,,20], map$fraction[,,2]),
          identical(applied[,,21], map$fraction[,,2]),
          identical(applied[,,24], map$fraction[,,3]),
          identical(applied[,,25], map$fraction[,,3]),
          all(applied[,,17] == baseline[,,17]))
cat('Generic PFT fraction map and 2015 crop input OK\n')
