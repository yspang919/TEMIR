# Enable import_new_PFT in input_TEMIR.R to read these definitions.
# Preserve the original PFT_df: new entries are appended automatically.
# Import works in both BGC modes. New BGC crops start before planting with zero
# canopy/carbon; seed carbon is controlled by emergence_carbon_to_leaf in the
# crop settings. Start the simulation on/before the planting date, not midseason.
# Rice JULES parameter provenance:
# Primary: Xu et al. (2025), https://doi.org/10.5194/gmd-18-7257-2025
# Fallback: rice defaults in Osborne et al. (2015),
# https://doi.org/10.5194/gmd-8-1139-2015
# Record the source and equation/table for each mapped JULES parameter. Values
# absent from both studies need an explicit documented assumption. These JULES
# values are separate from the CLM5 traits imported below. In particular,
# arootf = 0 below is a TEMIR allocation assumption, not a result of either paper.

clm5_parameter_file = file.path(TEMIR_dir, 'file_for_developement',
    'clm5_parameters', 'clm5_params.c200624.nc')

custom_PFTs = list(
    rice = list(
        pft_number = 25,                 # TEMIR number; R array index = 26
        pft_name = 'rice',
        crop_type = 'rice',              # Shared by every rice variety; independent of ID
        initial_state = 'preplant',      # Explicit zero canopy/biomass for a new BGC run
        source = list(
            type = 'CLM5',
            file = clm5_parameter_file,
            pft_number = 61,             # CLM5 source number, not R index
            pft_name = 'rice',           # Checked against the source row
            rootprof_beta_variant = 1    # CLM5 has two variants; unused by TEMIR
        ),
        overrides = list(
            htop = 0.5, hbot = 0.01,     # TEMIR prescribed crop canopy defaults (m)
            plantgroup = 'C3_grass',     # TEMIR ozone scheme classification
            arootf = 0                  # Explicit wheat-family allocation assumption;
                                       # missing in this CLM5 file. Review for rice BGC.
        ),
        # For a prescribed-LAI rice simulation, replace NULL with twelve monthly
        # values each (m2/m2). These profiles apply across the simulation grid.
        # Assign rice coverage through the existing custom PFT fraction map;
        # map its layer to ipft = 26. Import alone adds ZERO rice coverage.
        monthly_LAI = NULL,
        monthly_SAI = NULL
    )
)

# Manual definitions are also supported: omit source and supply all required
# parameters in overrides. Inspect f_custom_PFT_schema(FALSE/TRUE) for the names.
# g1_med_table is imported from CLM5 medlynslope. BGC planting windows are read
# from CLM5 min/max_NH/SH_planting_date and converted from MMDD to day of year.
# Imported values do not receive TEMIR's original fixed-index corrections or
# its additional 30 days of mxmat. Use overrides for intended adjustments.
