#!/usr/bin/env Rscript
# Build the first, deliberately incomplete China phenology map for TEMIR.
# Usage: Rscript build_custom_phenology_map_2015.R [crop_phenology_dir] [output_rds]

args <- commandArgs(trailingOnly = TRUE)
root <- if (length(args) >= 1L) args[[1L]] else
  "G:/My Drive/Angus_FYP/data/crop_phenology"
output <- if (length(args) >= 2L) args[[2L]] else
  file.path(root, "TEMIR_ready_input", "custom_phenology_map_2015_v2_draft.rds")
if (file.exists(output)) stop("Output already exists: ", output)
if (!requireNamespace("ncdf4", quietly = TRUE)) stop("The ncdf4 package is required")

sample_day <- file.path(dirname(root), "ERA5_data", "merged_file", "2015",
                        "01", "ERA5_hourly_20150101.nc")
if (!file.exists(sample_day)) stop("ERA5 grid sample is missing: ", sample_day)
nc <- ncdf4::nc_open(sample_day)
lon <- as.numeric(ncdf4::ncvar_get(nc, "longitude"))
lat <- as.numeric(ncdf4::ncvar_get(nc, "latitude"))
ncdf4::nc_close(nc)
if (anyDuplicated(lon) || anyDuplicated(lat) ||
    any(diff(lon) <= 0) || any(diff(lat) <= 0)) {
  stop("Longitude and latitude coordinates must be unique and ascending")
}

layer_info <- data.frame(
  layer = 1:6,
  layer_name = c("maize", "spring_wheat", "winter_wheat", "soybean",
                 "rice_1", "rice_2"),
  season = c("single", "spring", "winter", "single",
             "single_or_early_combined", "late"),
  available = c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE),
  stringsAsFactors = FALSE
)
# PFT numbers, not the one-based ipft indices inside simulate_ij.R.
# Rice PFTs are intentionally absent until their model parameters exist.
pft_map <- data.frame(
  pft_number = 17:24,
  layer = rep(1:4, each = 2),
  stringsAsFactors = FALSE
)
shape <- c(length(lon), length(lat), nrow(layer_info))
GDDemer <- array(NA_real_, dim = shape)
GDDrepr <- array(NA_real_, dim = shape)
GDDmat <- array(NA_real_, dim = shape)
planting_date <- array(NA_real_, dim = shape)
source_status <- array("not_prepared", dim = shape)

sources <- data.frame(
  layer = c(1L, 2L, 5L, 6L),
  file = c(
    file.path(root, "gdd_requirements_2015_maize_tolerance10_draft",
              "maize_gdd_requirements_2015.csv"),
    file.path(root, "gdd_requirements_2015_wheat_regional_draft",
              "spring_wheat_gdd_requirements_2015.csv"),
    file.path(root, "gdd_requirements_2015",
              "rice_sr_er_gdd_requirements_2015.csv"),
    file.path(root, "gdd_requirements_2015",
              "rice_lr_gdd_requirements_2015.csv")
  ),
  stringsAsFactors = FALSE
)
source_info <- data.frame(layer = integer(), layer_name = character(),
                          source_file = character(), source_rows = integer(),
                          valid_cells = integer(), stringsAsFactors = FALSE)

for (row in seq_len(nrow(sources))) {
  layer <- sources$layer[[row]]
  path <- sources$file[[row]]
  if (!file.exists(path)) stop("Source CSV is missing: ", path)
  records <- read.csv(path, stringsAsFactors = FALSE)
  required <- c("lon", "lat", "GDDemer", "GDDrepr", "GDDmat", "status")
  if (!all(required %in% names(records))) {
    stop("Missing required columns in ", path)
  }
  if (anyDuplicated(records[c("lon", "lat")])) {
    stop("Duplicate grid cells in ", path)
  }
  i <- match(round(records$lon, 6), round(lon, 6))
  j <- match(round(records$lat, 6), round(lat, 6))
  if (anyNA(i) || anyNA(j)) stop("Off-grid cells in ", path)
  all_index <- cbind(i, j, rep.int(layer, nrow(records)))
  source_status[, , layer] <- "no_record"
  source_status[all_index] <- records$status
  valid <- which(records$status == "ok")
  if (!length(valid)) stop("No usable GDD cells in ", path)
  g <- records[valid, c("GDDemer", "GDDrepr", "GDDmat")]
  if (any(!is.finite(as.matrix(g))) ||
      any(g$GDDemer <= 0 | g$GDDemer >= g$GDDrepr |
          g$GDDrepr >= g$GDDmat)) {
    stop("Invalid GDD thresholds among status=ok rows in ", path)
  }
  valid_index <- all_index[valid, , drop = FALSE]
  GDDemer[valid_index] <- g$GDDemer
  GDDrepr[valid_index] <- g$GDDrepr
  GDDmat[valid_index] <- g$GDDmat
  source_info <- rbind(source_info, data.frame(
    layer = layer, layer_name = layer_info$layer_name[[layer]],
    source_file = normalizePath(path, winslash = "/"),
    source_rows = nrow(records), valid_cells = length(valid)
  ))
}

planting_source <- file.path(root, "TEMIR_ready_input",
                             "planting_date_backtracked_2015_draft.csv")
if (!file.exists(planting_source)) stop("Planting-date CSV is missing: ", planting_source)
planting_records <- read.csv(planting_source, stringsAsFactors = FALSE)
if (!all(c("layer", "lon", "lat", "planting_date", "status") %in%
         names(planting_records))) stop("Planting-date CSV lacks required columns")
if (anyDuplicated(planting_records[c("layer", "lon", "lat")])) {
  stop("Duplicate layer/cell in planting-date CSV")
}
pi <- match(round(planting_records$lon, 6), round(lon, 6))
pj <- match(round(planting_records$lat, 6), round(lat, 6))
pl <- planting_records$layer
if (anyNA(pi) || anyNA(pj) || any(!pl %in% layer_info$layer)) {
  stop("Planting-date CSV contains off-grid cells or unknown layers")
}
plant_index <- cbind(pi, pj, pl)
if (anyNA(GDDmat[plant_index])) stop("Planting-date CSV contains cells without GDDmat")
valid_planting <- is.finite(planting_records$planting_date)
if (any(valid_planting &
        (planting_records$planting_date < 1 |
         planting_records$planting_date >= planting_records$DOE |
         planting_records$planting_date != round(planting_records$planting_date)))) {
  stop("Invalid planting date in CSV")
}
planting_date[plant_index[valid_planting, , drop = FALSE]] <-
  planting_records$planting_date[valid_planting]

custom_phenology_map <- list(
  format_version = 2L,
  year = 2015L,
  lon = lon,
  lat = lat,
  planting_date = planting_date,
  GDDemer = GDDemer,
  GDDrepr = GDDrepr,
  GDDmat = GDDmat,
  layer_info = layer_info,
  pft_map = pft_map,
  source_status = source_status,
  source_info = source_info,
  planting_source_info = list(
    source_file = normalizePath(planting_source, winslash = "/"),
    status_counts = as.list(table(planting_records$status))
  ),
  metadata = list(
    dimension_order = "lon, lat, layer",
    units = "degree_C_day",
    stage_meaning = c(GDDemer = "planting to emergence",
                      GDDrepr = "planting to reproductive-stage proxy",
                      GDDmat = "planting to maturity/harvest"),
    planting_method = paste(
      "Backtracked from emergence date using ERA5 daily mean TSOIL1 and",
      "TEMIR JULES soil GDD response; first threshold crossing within 3",
      "days of the recorded emergence date. See planting_source_info."
    ),
    note = paste(
      "Draft GDD requirements inferred from 2015 stage dates and ERA5 T2M.",
      "Winter wheat and soybean thresholds are unavailable.",
      "Rice_1 is combined single/early rice; rice_2 is late rice.",
      "Only source rows with status=ok have GDD values."
    )
  )
)

dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
saveRDS(custom_phenology_map, output, compress = "gzip")
loaded <- readRDS(output)
if (!identical(loaded, custom_phenology_map)) {
  stop("RDS round-trip verification failed: ", output)
}
write.csv(source_info, file.path(dirname(output), "source_summary.csv"),
          row.names = FALSE)
print(source_info)
cat("Wrote ", output, " with dimensions ", paste(shape, collapse = " x "), "\n", sep = "")
