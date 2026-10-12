# Terrestrial Ecosystem Model in R (TEMIR)

## Introduction
The **Terrestrial Ecosystem Model in R (TEMIR)** is an ecosystem model developed by the [Tai Group for Atmosphere-Biosphere Interactions](http://www.cuhk.edu.hk/sci/essc/tgabi/index.html). It computes ecophysiological processes and responses of local and global terrestrial ecosystems, including canopy radiative transfer, photosynthesis, conductance, and dry deposition of air pollutants, driven by prescribed meteorological and land surface input data. The primary purpose is to evaluate changes in ecosystem functions in response to changes in the terrestrial and atmospheric environment, such as CO<sub>2</sub>, ozone, temperature, humidity, soil moisture, and plant functional type distribution, and to evaluate how these ecosystem responses influence atmospheric chemistry and climate.

The default meteorological inputs for TEMIR are from NASA/GMAO [MERRA-2](http://wiki.seas.harvard.edu/geos-chem/index.php/List_of_MERRA-2_met_fields) and [GEOS-FP](http://wiki.seas.harvard.edu/geos-chem/index.php/List_of_GEOS-FP_met_fields) reanalysis products, so that ecosystem simulations are entirely consistent and can be asynchronously coupled with the [GEOS-Chem](http://wiki.seas.harvard.edu/geos-chem/index.php/Main_Page) chemical transport model. Land surface inputs, including plant functional type and soil data, are mainly derived from the [Community Land Model (CLM)](http://www.cesm.ucar.edu/models/clm/) version 4.5, with 24 plant functional types including several crop types. The model is highly customizable, and users may replace the default inputs with their own data. TEMIR can also be run in parallel on multiple cores.

## Version and history (Last update: July 2026)
- 18 Jun 2019, v1.1; base plant-ecophysiological model with ozone dry deposition module
- 16 Jul 2026, v2.0; crop carbon-vegetation model with support for site-level meteorological, LAI, and ozone inputs for single-site simulations

## Getting started
The following components are required to run the model. Please refer to the TEMIR v2.0 manual after cloning the repository.

#### R
The latest version of R can be downloaded [here](https://www.r-project.org/). The current v2.0 branch has been tested with R 4.5.1.

#### Model code
To download the model code:

```bash
git clone https://github.com/yspang919/TEMIR.git
cd TEMIR
git switch v2.0
```

#### Required input data
This GitHub repository does **not** contain the large input datasets required to run TEMIR. In practice, the GitHub repository mainly contains the model code and description files.

Essential input data for the example TEMIR v2.0 test case are provided through Zenodo, not GitHub:

- Zenodo input package: [https://doi.org/10.5281/zenodo.21408341](https://doi.org/10.5281/zenodo.21408341)

After downloading the Zenodo archive, unzip its contents so that the data folders are placed directly under:

`TEMIR_inputs/`

For a detailed description of the required input folders and download guidance, see:

- [TEMIR_inputs/README.md](TEMIR_inputs/README.md)

In general, a TEMIR simulation may require the following input folders beneath `TEMIR_inputs/`:

**Mandatory for all simulations**
- `met_data/`: MERRA-2, GEOS-FP, or prepared custom meteorological forcing
- `surf_data/`: CLM land surface map and plant functional type properties
- `processed_surf_data/`: processed land surface files generated from the original surface data

**Additional inputs for crop carbon-vegetation simulations**
- `soilT_map/`: MERRA-2 soil temperature maps when the crop module uses MERRA-2 soil temperature; custom forcing can provide these layers in its prepared files
- `initial_data/`: initialization files for starting a simulation
- `crop_calender/`: crop planting and harvesting calendars
- `GDDmat_maps/`: crop growing degree day requirement for maturity
- `CLM4.5_GDDx_map/`: CLM4.5-based GDDx maps used for climate-based crop planting

**Optional inputs**
- `o3_input/`: ozone inputs maps
- `site_measurements_inputs/`: example site meteorology, LAI, and ozone inputs for single-site runs
- `FLUXNET/`: FLUXNET site information and related inputs for FLUXNET-driven simulations
- `Wesely_const/`: dry deposition constants
- `LAI_data/`: MODIS LAI inputs if prescribed LAI maps are used

#### MERRA-2 and GEOS-FP meteorological fields
Meteorological forcing can be downloaded by following the tutorial on the [GEOS-Chem website](http://wiki.seas.harvard.edu/geos-chem/index.php/Downloading_GEOS-Chem_data_directories). TEMIR uses the A1 meteorological fields as the default forcing input.

#### Prepared custom meteorological maps
Set `met_name = 'CUSTOM'` in `input_TEMIR.R` and edit the copied `settings_custom_met_inputs.R` in the simulation directory. The latter defines the daily NetCDF filename, variable names, static map filename, and whether wind is supplied as speed or components and pressure as surface or sea-level pressure. Custom values must be converted to the TEMIR units listed in `execution_v2.0.R` **before** the simulation. The model does not convert custom units or automatically fill missing map variables from MERRA-2 or GEOS-FP.

Daily arrays must be ordered `[longitude, latitude, hour]`, with 24 UTC hourly averages on the configured `lon` and `lat` grid. The default naming example is `met_data/GEOS_2x2.5/CUSTOM/CUSTOM.YYYYMMDD.nc`; change `custom_met_dir` and `custom_met_file` to suit prepared data. Custom runs continue to use the MERRA-2 `MERRA2.20150101.CN.2x25.nc4` invariant file for `lon`, `lat`, `FRLAKE`, `FRLAND`, `FRLANDIC`, `FROCEAN`, and `PHIS`. Set `custom_static_file` if that file is stored elsewhere. The default daily mapping requires cloud fraction, top and root-zone soil wetness, 2 m and 10 m temperature, sensible and latent heat flux, direct and diffuse PAR, downward shortwave radiation, total precipitation and snowfall, snow depth, evaporation, 2 m specific humidity, friction velocity, and momentum roughness length. It additionally reads either wind speed or both wind components, and either surface or sea-level pressure. See `settings_custom_met_inputs.R` for names and `execution_v2.0.R` for units.

For crop/biogeochemistry runs, also supply soil-temperature layers in the prepared daily file or set `custom_soil_temperature_file` to another daily NetCDF file. Configure their names and layer-bottom depths in `settings_custom_met_inputs.R`. Prescribed PAR must include direct and diffuse components; split other radiation products during input preparation.

MERRA-2 invariant fields, land surface maps, and prescribed LAI maps are regridded to the model grid when necessary. Surface and LAI results are cached under grid-specific filenames; a matching legacy 2° × 2.5° cache is reused automatically. Custom meteorological and soil-temperature maps themselves are not regridded by TEMIR. Runs at other resolutions, especially with the crop module's initial-condition and calendar maps, still need verification against actual input files.

#### MERRA-2 soil temperature fields
Soil temperature is required for carbon-vegetation simulations, including crop simulations. When using MERRA-2 soil temperature, data can be downloaded from the [NASA MERRA-2 repository](https://disc.gsfc.nasa.gov/datasets?project=MERRA-2). The required variables are `TSOIL1` to `TSOIL5` from the hourly-average land surface diagnostic files (`lnd_Nx`). Custom forcing can instead supply prepared soil-temperature layers as described above.

#### GEOS-Chem-simulated hourly ozone concentrations
This is optional and should be stored under `TEMIR_inputs/o3_input/` if used. Users may prepare their own ozone map files or obtain suitable ozone fields separately.

#### Monthly leaf area index (LAI)
MODIS-derived LAI data can be used when prescribed LAI maps are required. These should be stored under `TEMIR_inputs/LAI_data/`.

## Publications
- Sun et al. (2022), *Influence of plant ecophysiology on ozone dry deposition: comparing between multiplicative and photosynthesis-based dry deposition schemes and their responses to rising CO2 level*, Biogeosciences, https://doi.org/10.5194/bg-19-1753-2022
- Tai, A. P. K. et al. (2024), *Terrestrial Ecosystem Model in R (TEMIR) version 1.0: simulating ecophysiological responses of vegetation to atmospheric chemical and meteorological changes*, Geoscientific Model Development, https://doi.org/10.5194/gmd-17-3733-2024
- Pang, J. Y. S. et al. (2026), *Terrestrial Ecosystem Model in R (TEMIR) version 2.0: developing a crop model to simulate the response of maize and soybean to elevated CO2 and O3 concentrations*, Under review at Geoscientific Model Development.

## Other references (Accessed 20 July 2026)
- [*Ecological Climatology: Concepts and Applications (3<sup>rd</sup> Ed)*](https://www.cambridge.org/core/books/ecological-climatology/D146443B007985BC366B2512345692C0) by Gordon Bonan
- [Technical note of CLM4.5](https://files.cesm.ucar.edu/models/clm/4.5/CLM45_Tech_Note.pdf), latest access 20 July 2026.
- [Dry deposition module description](https://geos-chem.readthedocs.io/en/latest/geos-chem-shared-docs/supplemental-guides/drydep-guide.html#drydep-guide)
