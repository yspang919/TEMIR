# TEMIR Input Data

This `TEMIR_inputs/` directory in the GitHub repository does **not** contain the large TEMIR input datasets. In practice, the GitHub repository contains the model code and description files only.

The required input folders and files are provided through Zenodo, not GitHub:

- Zenodo input package: [https://doi.org/10.5281/zenodo.21408341](https://doi.org/10.5281/zenodo.21408341)

After downloading the Zenodo archive, unzip its contents so that the folders are placed directly beneath:

`.../TEMIR/TEMIR_inputs/`

## Input folders included in the Zenodo package

**Inputs folders TEMIR v2.0**
- `initial_data/`: CLM4.5 initial data for initializing a simulation
- `met_data/`: MERRA-2 global meteorological forcing for the example test case (2010-04-01 to 2010-09-30)
- `soilT_map/`: MERRA-2 five-layer soil temperature data for the example test case
- `surf_data/`: raw CLM land surface data and PFT-specific biochemical and biophysical parameters
- `processed_surf_data/`: processed land surface data used by TEMIR simulations
- `crop_calender/`: global crop calendar based on Sacks et al. (2010)
- `GDDmat_maps/`: maps of the growing degree day requirement for crop maturity
- `CLM4.5_GDDx_map/`: CLM4.5-based GDDx maps used for climate-based crop planting
- `site_measurements_inputs/`: example site-level meteorology, LAI, and ozone inputs for a test-case crop simulation
- `FLUXNET/`: FLUXNET2015 site information distributed with the TEMIR input package; full FLUXNET data should still be obtained from the FLUXNET data portal if needed
- `Wesely_const/`: dry deposition constants
- `LAI_data/`: MODIS LAI data

## Additional notes

- `o3_input/` is **not** bundled in the GitHub repository. Users may create `TEMIR_inputs/o3_input/` and place ozone map inputs there if ozone simulations are required.
- The input archive is intended to support the example test-case simulation distributed with TEMIR v2.0.
- Folder names should remain unchanged after unzipping so the default TEMIR scripts can locate the input files correctly.
