################################################################################
### Custom crop phenology map settings (draft)
################################################################################

# Used when planting date or any GDD method is 'prescribed-map' in
# input_TEMIR_crop_extension.R.

# One .rds file containing a list with lon, lat, planting_date, GDDmat,
# and layer_info. GDDrepr and GDDemer are optional. Arrays use
# [lon, lat, layer] order. Required map values must be finite for each
# selected crop cell; unavailable crop layers can remain NA.
# Replace this path for another simulation or place a customized copy of this
# settings script in the simulation directory.
custom_phenology_map_file =
    file.path(TEMIR_dir, 'TEMIR_inputs', 'crop_phenology',
              'china2015_staplecrops_phenology.rds')

# Map each file layer to TEMIR PFT numbers here. These are the zero-based
# PFT numbers in input_TEMIR.R, not the R loop's one-based ipft indices.
# Several PFTs may share one layer. Edit this table for another RDS/PFT set.
# Current 2015 file: 1 = maize, 2 = spring wheat, 4 = soybean.
# Layer 3 (winter wheat) has no usable 2015 values; rice PFTs are pending.
custom_phenology_pft_map = data.frame(
    # winter wheat and rice is not considered yet
    pft_number = c(17L, 18L, 19L, 20L, 23L, 24L),
    layer = c(1L, 1L, 2L, 2L, 4L, 4L)
)

# Alternatively, set custom_phenology_pft_map = NULL to use the pft_map
# stored in an RDS that includes one.

# Fallback for a missing optional array or an NA value at a selected cell.
# 'CLM4.5' means derive that threshold from the selected cell's GDDmat using
# TEMIR's existing PFT-specific CLM rule (including the maize GDDrepr rule).
# 'error' requires a mapped value. A finite but invalid mapped value should
# always raise an error rather than silently falling back. This policy is
# applied whenever a mapped optional threshold is absent at a selected cell.
custom_phenology_optional_fallback = c(
    GDDrepr = 'error',
    GDDemer = 'error'
)

if (!is.character(custom_phenology_map_file) ||
    length(custom_phenology_map_file) != 1L ||
    is.na(custom_phenology_map_file) ||
    !nzchar(custom_phenology_map_file)) {
    stop('custom_phenology_map_file must be a non-empty file path')
}
if (!is.null(custom_phenology_pft_map)) {
    if (!is.data.frame(custom_phenology_pft_map) ||
        !all(c('pft_number', 'layer') %in% names(custom_phenology_pft_map)) ||
        anyNA(custom_phenology_pft_map[c('pft_number', 'layer')]) ||
        anyDuplicated(custom_phenology_pft_map$pft_number)) {
        stop('custom_phenology_pft_map needs unique PFT numbers and layer columns')
    }
}
if (!is.character(custom_phenology_optional_fallback) ||
    length(custom_phenology_optional_fallback) != 2L ||
    !setequal(names(custom_phenology_optional_fallback),
              c('GDDrepr', 'GDDemer')) ||
    anyNA(custom_phenology_optional_fallback) ||
    !all(custom_phenology_optional_fallback %in% c('CLM4.5', 'error'))) {
    stop('custom_phenology_optional_fallback must name GDDrepr and GDDemer as CLM4.5 or error')
}
