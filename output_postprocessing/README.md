# TEMIR history postprocessing

TEMIR writes one `hist_data/hist_grid_YYYYMMDD.nc` file per simulation day when
`archive_format = 'nc'`. The source contains hourly variables with dimensions
`[lon, lat, pft, hour]` and native daily variables with dimensions
`[lon, lat, pft]`. PFT numbers in NetCDF start at zero.

Use `hist_grid_processor.R` for either a one-cell run or a regional run. Edit
its `settings` list and run it directly from the TEMIR repository:

```powershell
Rscript output_postprocessing/hist_grid_processor.R
```

In an interactive R session, use:

```r
source('output_postprocessing/hist_grid_processor.R')
settings$run_dir = 'C:/path/to/TEMIR_run/my_case'
settings$output_format = 'rds'
do.call(f_process_hist_grid, settings)
```

To override the main settings from a local terminal or cluster job:

```powershell
Rscript output_postprocessing/hist_grid_processor.R "C:/path/to/TEMIR_run/my_case" 20150101 20150131 17,18,19,20,23,24 nc daily_mean
```

Arguments are `RUN_DIR START_YYYYMMDD END_YYYYMMDD [PFT_NUMBERS_CSV]
[nc|rds|csv|xlsx] [none|daily_mean|full|both]`. On a Unix cluster, use the
same arguments with a Unix path and the cluster's `Rscript`. Set
`settings$daily_variables` and `settings$hourly_variables` in the script or
pass these vectors to `f_process_hist_grid()` from R to select variables.
Unavailable variables are skipped; ozone uptake variables are included when
they are present in the source files.

`hourly_mode` controls the temporal detail:

| Mode | Native daily variables | Daily summaries of hourly variables | Full hourly variables |
| --- | --- | --- | --- |
| `none` | Yes | No | No |
| `daily_mean` | Yes | Yes | No |
| `full` | Yes | No | Yes |
| `both` | Yes | Yes | Yes |

Daily summaries use a mean across the 24 hours for ordinary hourly variables.
The cumulative ozone uptake variables `CUO_*` use their last hourly value,
named `daily_end_CUO_*`. Full hourly timestamps run from 00:00 to 23:00 UTC;
TEMIR's source hour slots are numbered 1 through 24.

Output choices:

| Format | One-cell run | Regional run |
| --- | --- | --- |
| `nc` | One NetCDF file | One NetCDF file with `[lon,lat,pft,day]` and, when requested, `[lon,lat,pft,hour]` variables |
| `rds` | One RDS containing `daily` and optional `hourly` data frames | One RDS for the selected date range containing `lon`, `lat`, `pft_number`, `date`, optional `UTC_time`, and `daily`, `daily_from_hourly`, `hourly` array lists |
| `csv` | Separate daily and optional hourly CSV files | Not supported |
| `xlsx` | Daily and optional hourly workbook sheets | Not supported |

Processed results go to `RUN_DIR/postprocessed` by default. The function
accepts `output_dir` if a different destination is needed. Existing output
files are protected from overwrite. The processor checks for every requested
daily source file and consistent grid, PFT, and hourly coordinates. Regional
NetCDF output is streamed a day at a time so it does not require keeping an
entire year of hourly maps in R memory. Regional RDS output stores each
variable as a `[lon, lat, pft, time]` array: `daily` and
`daily_from_hourly` use `date` as their time axis, while `hourly` uses
`UTC_time`. All requested values are held in R memory before the one RDS is
saved. Long regional runs with full hourly output can require substantial RAM;
the processor warns when the arrays alone exceed 2 GiB. Select fewer PFTs or
variables, use `daily_mean`, or use NetCDF if memory is limited.

The processor currently reads NetCDF history files. A run saved with
`archive_format = 'RData'` needs the original history export path or a
conversion to daily NetCDF first. `model_config.RData` and `temp_data` are
simulation configuration and restart state, not history input to this script.
