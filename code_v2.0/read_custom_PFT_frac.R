# Read a coordinate-matched [lon, lat, layer] fraction map and assign selected
# layers to model PFT indices. The layer-to-ipft mapping lives in input_TEMIR.R.
f_apply_custom_PFT_frac <- function(PFT_frac, lon, lat, path, layer_to_ipft) {
    if (!file.exists(path)) stop('Custom PFT fraction RDS does not exist: ', path)
    map <- readRDS(path)
    if (!is.list(map) || !all(c('lon', 'lat') %in% names(map))) {
        stop('Custom PFT fraction RDS needs lon and lat vectors')
    }
    values <- if (!is.null(map$custom_PFT_frac)) map$custom_PFT_frac else map$fraction
    if (is.null(values)) {
        stop('Custom PFT fraction RDS needs custom_PFT_frac [lon, lat, layer]')
    }
    if (!is.numeric(map$lon) || !is.numeric(map$lat) ||
        any(!is.finite(map$lon)) || any(!is.finite(map$lat)) ||
        anyDuplicated(map$lon) || anyDuplicated(map$lat)) {
        stop('Custom PFT fraction coordinates must be unique finite numbers')
    }
    if (!is.numeric(values) || length(dim(values)) != 3L ||
        !identical(dim(values)[1:2], c(length(map$lon), length(map$lat))) ||
        any(!is.finite(values)) || any(values < 0 | values > 1)) {
        stop('Custom PFT fractions must be a finite [lon, lat, layer] array in [0,1]')
    }
    if (length(dim(PFT_frac)) != 3L ||
        !identical(dim(PFT_frac)[1:2], c(length(lon), length(lat)))) {
        stop('Model PFT_frac dimensions do not match lon and lat')
    }
    if (!is.data.frame(layer_to_ipft) ||
        !all(c('layer', 'ipft') %in% names(layer_to_ipft)) ||
        !is.numeric(layer_to_ipft$layer) ||
        !is.numeric(layer_to_ipft$ipft) ||
        nrow(layer_to_ipft) == 0L ||
        anyNA(layer_to_ipft[c('layer', 'ipft')]) ||
        any(!is.finite(layer_to_ipft$layer)) ||
        any(!is.finite(layer_to_ipft$ipft)) ||
        any(layer_to_ipft$layer != as.integer(layer_to_ipft$layer)) ||
        any(layer_to_ipft$ipft != as.integer(layer_to_ipft$ipft)) ||
        anyDuplicated(layer_to_ipft$ipft) ||
        any(layer_to_ipft$layer < 1L | layer_to_ipft$layer > dim(values)[3]) ||
        any(layer_to_ipft$ipft < 1L | layer_to_ipft$ipft > dim(PFT_frac)[3])) {
        stop('layer_to_ipft must map valid layers to unique existing PFT indices')
    }
    i <- match(round(lon, 6), round(map$lon, 6))
    j <- match(round(lat, 6), round(map$lat, 6))
    if (anyNA(i) || anyNA(j)) {
        stop('Model lon/lat must match coordinates in the PFT fraction RDS')
    }
    for (row in seq_len(nrow(layer_to_ipft))) {
        ipft <- layer_to_ipft$ipft[[row]]
        layer <- layer_to_ipft$layer[[row]]
        PFT_frac[,,ipft] <- values[i, j, layer, drop = FALSE][,,1]
    }
    list(PFT_frac = PFT_frac, ipft = as.integer(layer_to_ipft$ipft))
}
