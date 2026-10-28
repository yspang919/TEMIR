#!/usr/bin/env Rscript
# Package the latest gap-filled 2015 phenology CSVs into TEMIR's RDS schema.
#
# Usage:
#   Rscript scripts/build_custom_phenology_map_2015_mean_days.R
#   Rscript scripts/build_custom_phenology_map_2015_mean_days.R ROOT OUTPUT_RDS INPUT_DIR

args <- commandArgs(trailingOnly = TRUE)
root <- if (length(args) >= 1L) args[[1L]] else
  "G:/My Drive/Angus_FYP/data/crop_phenology"
input_dir <- if (length(args) >= 3L) args[[3L]] else
  file.path(root, "TEMIR_ready_input", "mean_days_gapfill_2015_draft")
output <- if (length(args) >= 2L) args[[2L]] else
  file.path(root, "TEMIR_ready_input",
            "custom_phenology_map_2015_mean_days_gapfill_draft.rds")
if (!requireNamespace("ncdf4", quietly = TRUE)) stop("ncdf4 is required")

sample_day <- file.path(dirname(root), "ERA5_data", "merged_file", "2015",
                        "01", "ERA5_hourly_20150101.nc")
if (!file.exists(sample_day)) stop("ERA5 grid sample is missing: ", sample_day)
nc <- ncdf4::nc_open(sample_day)
lon <- as.numeric(ncdf4::ncvar_get(nc, "longitude"))
lat <- as.numeric(ncdf4::ncvar_get(nc, "latitude"))
ncdf4::nc_close(nc)
stopifnot(length(lon) == 124L, length(lat) == 72L,
          !anyDuplicated(lon), !anyDuplicated(lat),
          all(diff(lon) > 0), all(diff(lat) > 0))

layer_info <- data.frame(
  layer = 1:6,
  layer_name = c("maize", "spring_wheat", "winter_wheat", "soybean",
                 "rice_1", "rice_2"),
  season = c("mixed_maize_seasons", "spring", "winter",
             "regional_climatology", "early_or_middle_single", "late"),
  available = c(TRUE, TRUE, FALSE, TRUE, TRUE, TRUE),
  stringsAsFactors = FALSE
)
pft_map <- data.frame(pft_number = 17:24, layer = rep(1:4, each = 2))
shape <- c(length(lon), length(lat), nrow(layer_info))
new_numeric <- function() array(NA_real_, dim = shape)
planting_date <- new_numeric()
GDDemer <- new_numeric()
GDDrepr <- new_numeric()
GDDmat <- new_numeric()
DOE <- new_numeric()
DOF <- new_numeric()
DOH <- new_numeric()
TR <- new_numeric()
source_status <- array("not_prepared", dim = shape)
source_method <- array(NA_character_, dim = shape)
crop_group <- array(NA_character_, dim = shape)

sources <- data.frame(
  layer = c(1L, 2L, 4L, 5L, 6L),
  name = c("maize", "spring_wheat", "soybean", "rice_1", "rice_2"),
  stringsAsFactors = FALSE
)
source_info <- data.frame(layer = integer(), layer_name = character(),
                          source_file = character(), source_rows = integer(),
                          valid_cells = integer(), stringsAsFactors = FALSE)

# Mark this non-ASCII metadata as UTF-8 explicitly. An unmarked string saved
# under the C locale can make the RDS unreadable in a Windows UTF-8 locale.
reference_book <- "G:/My Drive/Angus_FYP/data/publications/中国农业物候.pdf"
Encoding(reference_book) <- "UTF-8"

for (r in seq_len(nrow(sources))) {
  layer <- sources$layer[[r]]
  name <- sources$name[[r]]
  path <- file.path(input_dir, paste0(name, "_phenology_2015_mean_days.csv"))
  if (!file.exists(path)) stop("Source CSV missing: ", path)
  records <- read.csv(path, stringsAsFactors = FALSE)
  required <- c("lon", "lat", "planting_date", "DOE", "DOF", "DOH",
                "GDDemer", "GDDrepr", "GDDmat", "crop_group", "status")
  if (!all(required %in% names(records))) {
    stop("Missing columns in ", path, ": ",
         paste(setdiff(required, names(records)), collapse = ", "))
  }
  if (anyDuplicated(records[c("lon", "lat")])) {
    stop("Duplicate crop cells in ", path)
  }
  i <- match(round(records$lon, 6), round(lon, 6))
  j <- match(round(records$lat, 6), round(lat, 6))
  if (anyNA(i) || anyNA(j)) stop("Off-grid crop cell in ", path)
  all_index <- cbind(i, j, rep.int(layer, nrow(records)))
  source_status[, , layer] <- "no_record"
  source_status[all_index] <- records$status
  if ("source_status" %in% names(records)) {
    source_method[all_index] <- records$source_status
  }
  crop_group[all_index] <- records$crop_group
  valid <- which(records$status == "ok")
  if (!length(valid)) stop("No valid phenology cells in ", path)
  stage <- records[valid, c("planting_date", "DOE", "DOF", "DOH")]
  thermal <- records[valid, c("GDDemer", "GDDrepr", "GDDmat")]
  if (any(!is.finite(as.matrix(stage))) ||
      any(stage$planting_date < 1 | stage$planting_date >= stage$DOE |
          stage$DOE >= stage$DOF | stage$DOF >= stage$DOH |
          stage$DOH > 365 | stage$planting_date != round(stage$planting_date))) {
    stop("Invalid dates among status=ok rows in ", path)
  }
  if (any(!is.finite(as.matrix(thermal))) ||
      any(thermal$GDDemer <= 0 | thermal$GDDemer >= thermal$GDDrepr |
          thermal$GDDrepr >= thermal$GDDmat)) {
    stop("Invalid GDD thresholds among status=ok rows in ", path)
  }
  index <- all_index[valid, , drop = FALSE]
  planting_date[index] <- stage$planting_date
  DOE[index] <- stage$DOE
  DOF[index] <- stage$DOF
  DOH[index] <- stage$DOH
  GDDemer[index] <- thermal$GDDemer
  GDDrepr[index] <- thermal$GDDrepr
  GDDmat[index] <- thermal$GDDmat
  if ("TR" %in% names(records)) TR[index] <- records$TR[valid]
  source_info <- rbind(source_info, data.frame(
    layer = layer, layer_name = name,
    source_file = normalizePath(path, winslash = "/"),
    source_rows = nrow(records), valid_cells = length(valid)))
}

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
  source_method = source_method,
  source_info = source_info,
  crop_group = crop_group,
  observed_or_inferred_stages = list(DOE = DOE, DOF = DOF, DOH = DOH, TR = TR),
  metadata = list(
    dimension_order = "lon, lat, layer",
    GDD_units = "degree_C_day",
    date_units = "2015 day of year",
    method = if (grepl("gapfill", input_dir, fixed = TRUE))
      "fixed-day draft with explicit maize spatial stage fills, soybean calendar alternatives, and rice field-establishment proxy" else
      "fixed planting-to-emergence days or book regional soybean calendar",
    temperature = "ERA5 2015 daily mean TSOIL1 for GDDemer and T2M for GDDrepr/GDDmat; TEMIR JULES response",
    GDD_intervals = "(planting_date, stage_date]",
    reference_book = reference_book,
    notes = c(
      "Winter wheat remains empty pending cross-year weather.",
      "Rice_1 includes inferred early and middle/single groups; see crop_group.",
      if (grepl("gapfill", input_dir, fixed = TRUE))
        "Cold rice_1 cells use TR as a provisional field-establishment DOE and model planting at TR-7; other rice nursery dates are inferred before transplanting." else
        "Rice nursery sowing and emergence are inferred before observed transplanting.",
      if (grepl("gapfill", input_dir, fixed = TRUE))
        "Maize and soybean gap-fill methods are recorded in source_method; see the source CSVs for detailed reasons." else
        "Source status records the original stage and weather selection.",
      "Rice PFT parameters and nursery/transplant process are not yet implemented.",
      "Soybean dates are 1992-2011 regional means applied to the 2015 fraction grid.",
      "Only status=ok source rows are loaded into numeric arrays."
    )
  )
)

dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
staged_output <- tempfile(pattern = ".phenology_build_", tmpdir = dirname(output),
                          fileext = ".rds")
saveRDS(custom_phenology_map, staged_output, compress = "gzip")
if (!identical(readRDS(staged_output), custom_phenology_map)) {
  stop("RDS round-trip verification failed: ", output)
}
backup_output <- NULL
if (file.exists(output)) {
  backup_output <- tempfile(pattern = ".phenology_previous_",
                            tmpdir = dirname(output), fileext = ".rds")
  if (!file.copy(output, backup_output)) stop("Could not back up existing RDS: ", output)
}
replacement_ok <- file.copy(staged_output, output, overwrite = TRUE)
if (replacement_ok) {
  replacement_ok <- isTRUE(tryCatch(
    identical(readRDS(output), custom_phenology_map),
    error = function(e) FALSE))
}
if (!replacement_ok) {
  if (!is.null(backup_output) && !file.copy(backup_output, output, overwrite = TRUE)) {
    stop("RDS replacement failed and backup restore failed: ", backup_output)
  }
  stop("RDS replacement failed; original retained at ", backup_output)
}
unlink(staged_output)
if (!is.null(backup_output)) unlink(backup_output)
# source_info is stored in the RDS; no duplicate CSV is needed beside it.
print(source_info)
cat("Wrote ", output, " with dimensions ", paste(shape, collapse = " x "),
    "\n", sep = "")
