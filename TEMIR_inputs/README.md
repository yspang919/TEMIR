# TEMIR Input Data

This repository keeps the TEMIR input-folder structure, but large input data
files are not included in the GitHub version of TEMIR v2.0.

The folder layout is preserved so a new user can place the required inputs in
the expected locations without changing the default scripts.

Large inputs that are expected but not tracked here include:

- meteorological reanalysis input under `met_data/`
- soil temperature maps under `soilT_map/`
- land surface and processed surface data under `surf_data/` and `processed_surf_data/`
- crop calendar and GDD maps under `crop_calender/`, `CLM4.5_GDDx_map/`, and `GDDmat_maps/`
- ozone input under `o3_input/`
- initial data under `initial_data/`
- dry deposition constants under `Wesely_const/`

The small example single-site input case is kept under:

- `site_measurements_inputs/example_case001/`

For downloading the required TEMIR inputs, see:

- `Download_data_here.txt`

