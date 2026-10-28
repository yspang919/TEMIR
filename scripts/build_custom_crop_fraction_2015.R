#!/usr/bin/env Rscript
# Build an opt-in 2015 crop-presence map on the phenology/ERA5 grid.
# A crop's total fraction is copied to both its rainfed and irrigated PFT for
# separate per-unit-area simulations. These duplicated values must not be
# summed as simultaneous land-cover fractions.

args <- commandArgs(trailingOnly = TRUE)
phenology_dir <- if (length(args) >= 1L) args[[1L]] else
  "G:/My Drive/Angus_FYP/data/crop_phenology/TEMIR_ready_input"
output <- if (length(args) >= 2L) args[[2L]] else
  "G:/My Drive/Angus_FYP/data/crop_distribution/TEMIR_ready_input/china2015_staplecrops_distribution.rds"
if (file.exists(output)) stop("Output already exists: ", output)

phenology <- readRDS(file.path(
  phenology_dir, "custom_phenology_map_2015_mean_days_gapfill_draft.rds"))
lon <- phenology$lon
lat <- phenology$lat
stopifnot(is.numeric(lon), is.numeric(lat),
          !anyDuplicated(lon), !anyDuplicated(lat),
          all(diff(lon) > 0), all(diff(lat) > 0))

layer_info <- data.frame(
  layer = 1:3,
  layer_name = c("maize", "spring_wheat", "soybean"),
  stringsAsFactors = FALSE
)
pft_map <- data.frame(
  pft_number = c(17L, 18L, 19L, 20L, 23L, 24L),
  layer = c(1L, 1L, 2L, 2L, 3L, 3L)
)
fraction <- array(0, dim = c(length(lon), length(lat), nrow(layer_info)))
source_info <- data.frame(layer_name = character(), source_rows = integer(),
                          active_cells = integer(), fraction_sum = numeric())
input_dir <- file.path(phenology_dir, "mean_days_gapfill_2015_draft")

for (layer in seq_len(nrow(layer_info))) {
  name <- layer_info$layer_name[[layer]]
  file <- file.path(input_dir, paste0(name, "_phenology_2015_mean_days.csv"))
  records <- read.csv(file, stringsAsFactors = FALSE)
  required <- c("lon", "lat", "crop_fraction", "status")
  if (!all(required %in% names(records))) {
    stop("Missing fields in ", file, ": ",
         paste(setdiff(required, names(records)), collapse = ", "))
  }
  if (anyDuplicated(records[c("lon", "lat")])) stop("Duplicate cell in ", file)
  if (anyNA(records$crop_fraction) ||
      any(!is.finite(records$crop_fraction)) ||
      any(records$crop_fraction < 0 | records$crop_fraction > 1)) {
    stop("Crop fractions must be finite values in [0,1]: ", file)
  }
  i <- match(round(records$lon, 6), round(lon, 6))
  j <- match(round(records$lat, 6), round(lat, 6))
  if (anyNA(i) || anyNA(j)) stop("Off-grid cell in ", file)
  active <- records$status == "ok" & records$crop_fraction > 0
  fraction[cbind(i[active], j[active], rep.int(layer, sum(active)))] <-
    records$crop_fraction[active]
  # A positive simulation fraction requires a valid phenology value.
  phenology_layer <- c(1L, 2L, 4L)[[layer]]
  if (any(active & !is.finite(phenology$GDDmat[
      cbind(i, j, rep.int(phenology_layer, nrow(records)))]))) {
    stop("An active crop cell lacks phenology: ", name)
  }
  source_info <- rbind(source_info, data.frame(
    layer_name = name, source_rows = nrow(records),
    active_cells = sum(active), fraction_sum = sum(records$crop_fraction[active])))
}

custom_crop_fraction_map <- list(
  format_version = 1L,
  year = 2015L,
  lon = lon,
  lat = lat,
  fraction = fraction,
  layer_info = layer_info,
  pft_map = pft_map,
  source_info = source_info,
  metadata = list(
    dimension_order = "lon, lat, layer",
    fraction_basis = "fraction of entire 0.5-degree grid cell",
    selection = "positive crop fraction and status=ok in gap-filled 2015 phenology",
    paired_PFT_policy = "same total crop fraction copied to rainfed and irrigated PFT for separate per-area simulations",
    aggregation_warning = "paired PFT values and overlapping crop seasons are not additive land-cover fractions"
  )
)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
saveRDS(custom_crop_fraction_map, output, compress = "gzip")
if (!identical(readRDS(output), custom_crop_fraction_map)) {
  stop("RDS round-trip failed: ", output)
}
print(source_info)
cat("Wrote ", output, "\n", sep = "")
