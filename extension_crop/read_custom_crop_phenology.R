# Read one custom phenology RDS and align its coordinates to the simulation grid.
# Arrays are indexed [longitude, latitude, layer]. Missing crop cells remain NA
# until a selected PFT is simulated at that cell.
f_read_custom_crop_phenology <- function(path, lon, lat, pft_map = NULL,
                                         required_fields = character()) {
    if (!file.exists(path)) stop('Custom phenology RDS does not exist: ', path)
    map <- readRDS(path)
    if (!is.list(map) ||
        !all(c('lon', 'lat', 'layer_info', 'planting_date', 'GDDmat') %in% names(map))) {
        stop('Custom phenology RDS needs lon, lat, layer_info, planting_date, and GDDmat')
    }
    if (!is.numeric(map$lon) || !is.numeric(map$lat) ||
        any(!is.finite(map$lon)) || any(!is.finite(map$lat)) ||
        anyDuplicated(map$lon) || anyDuplicated(map$lat)) {
        stop('Custom phenology coordinates must be unique finite numbers')
    }
    layers <- map$layer_info$layer
    if (!is.data.frame(map$layer_info) || !is.numeric(layers) ||
        !identical(as.integer(layers), seq_along(layers))) {
        stop('Custom phenology layer_info$layer must be 1, 2, ... in array order')
    }
    expected_dim <- c(length(map$lon), length(map$lat), length(layers))
    for (field in c('planting_date', 'GDDmat', 'GDDrepr', 'GDDemer')) {
        value <- map[[field]]
        if (is.null(value)) {
            if (field %in% c('planting_date', 'GDDmat', required_fields)) {
                stop('Custom phenology RDS lacks ', field)
            }
            next
        }
        if (!is.numeric(value) || !identical(dim(value), expected_dim) ||
            any(!is.na(value) & !is.finite(value))) {
            stop('Custom phenology ', field, ' must be a finite numeric [lon, lat, layer] array or NA')
        }
        if (field == 'planting_date' &&
            any(!is.na(value) & (value < 1 | value > 366 | value != round(value)))) {
            stop('Custom phenology planting_date must contain integer days 1..366 or NA')
        }
        if (field != 'planting_date' && any(!is.na(value) & value <= 0)) {
            stop('Custom phenology ', field, ' must be positive or NA')
        }
    }
    selected_map <- if (is.null(pft_map)) map$pft_map else pft_map
    if (!is.data.frame(selected_map) ||
        !all(c('pft_number', 'layer') %in% names(selected_map)) ||
        anyNA(selected_map[c('pft_number', 'layer')]) ||
        anyDuplicated(selected_map$pft_number) ||
        !is.numeric(selected_map$pft_number) ||
        !is.numeric(selected_map$layer) ||
        any(!is.finite(selected_map$pft_number)) ||
        any(!is.finite(selected_map$layer)) ||
        any(selected_map$pft_number != round(selected_map$pft_number)) ||
        any(selected_map$layer != round(selected_map$layer)) ||
        any(!selected_map$layer %in% layers)) {
        stop('Custom phenology pft_map needs unique pft_number and valid layer columns')
    }
    i <- match(round(lon, 6), round(map$lon, 6))
    j <- match(round(lat, 6), round(map$lat, 6))
    if (anyNA(i) || anyNA(j)) {
        stop('Simulation lon/lat must match coordinates in the custom phenology RDS')
    }
    for (field in c('planting_date', 'GDDmat', 'GDDrepr', 'GDDemer')) {
        if (!is.null(map[[field]])) {
            map[[field]] <- map[[field]][i, j, , drop = FALSE]
        }
    }
    map$lon <- lon
    map$lat <- lat
    map$pft_map <- selected_map
    map
}

# Resolve one selected PFT/cell. Optional thresholds may be NA; the crop
# phenology function applies the configured CLM fallback at planting.
f_custom_crop_phenology_cell <- function(map, i, j, pft_number,
                                         required_fields = character()) {
    layer <- map$pft_map$layer[match(pft_number, map$pft_map$pft_number)]
    if (length(layer) != 1L || is.na(layer)) {
        stop('No custom phenology layer for PFT ', pft_number)
    }
    values <- lapply(c('planting_date', 'GDDmat', 'GDDrepr', 'GDDemer'),
                     function(field) {
                         if (is.null(map[[field]])) return(NA_real_)
                         map[[field]][i, j, layer]
                     })
    names(values) <- c('planting_date', 'GDDmat', 'GDDrepr', 'GDDemer')
    for (field in required_fields) {
        if (!is.finite(values[[field]])) {
            stop('Custom phenology ', field, ' is missing at lon=', map$lon[i],
                 ', lat=', map$lat[j], ', PFT=', pft_number, ', layer=', layer)
        }
    }
    values
}
