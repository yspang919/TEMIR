################################################################################
### Terrestrial Ecosystem Model in R (TEMIR)
### Post-processing script for gridded history outputs and spreadsheet export
################################################################################

# This script reads gridded TEMIR history files from a simulation case,
# extracts selected hourly and daily variables, and saves processed outputs for
# each simulated PFT in a tabular format for further inspection and analysis.

library(ncdf4)
library(openxlsx)

############## To be modify by user
TEMIR_dir = 'C:/Users/jacky/Documents/TEMIR/TEMIR_publish_version/'
# location where simulation result are stored
run_dir= paste0(TEMIR_dir, 'TEMIR_run/')

# Required tool for post processing
source(paste0(TEMIR_dir, 'code_v2.0/tools.R'))

# name of the simulation case
casename = 'crop_sim_example'

# set working directory to be the output directory of the simulation case
setwd(paste0(run_dir,casename,'/hist_data/'))

# simulation start and end date
start_date = 20100401
end_date = 20100930

# variables to be output (that have daily resolution)
daily_variable = c('LAI', 'GPP', 'NPP', 
                   'GDDT2m',
                   'leafC', 'finerootC', 'livestemC', 'grainC', 'htop')

# variables to be output (that have hourly resolution)
hourly_variable = c('A_can', 'g_can', 'g_s', 'g_ssun', 'g_ssha', 'phi_sun', 'phi_sha', 'LAI_sun', 'LAI_sha', 'beta_t')


# For ozone simulation, also output the ozone-related hourly variable 
O3_simulation_flag = FALSE
if (O3_simulation_flag) {
    hourly_variable = c(hourly_variable, 'CUO_can', 'CUO_sun', 'CUO_sha')
}

# whether calculate daily mean and output them for hourly variable
output_hourly_var_as_daily_mean = FALSE


# PFT number(s) of the PFT of interest in the simulation
# R index starts from 1, and PFT index starts from 0. Therefore so we need to plus 1 for the PFT index
# target_PFT = 0:24 + 1    # full set of PFT
target_PFT = 17 + 1   # rainfed maize

# output file format for the post-processed data
# 'xlsx': data frame output as xlsx file, each sheet contains result for one PFT, it only supports for single-site simulations.
# 'Rdata': data frame will be stored in a .RData, 
output_format = 'xlsx'


######################## PFT number and PFT name
PFT_num_name_df = data.frame(
    PFT_r_index = 1:25,
    PFT_number = 0:24,
    PFT_description = c('not_vegetated',
                        'needleleaf_evergreen_temperate_tree',
                        'needleleaf_evergreen_boreal_tree',
                        'needleleaf_deciduous_boreal_tree',
                        'broadleaf_evergreen_tropical_tree',
                        'broadleaf_evergreen_temperate_tree',
                        'broadleaf_deciduous_tropical_tree',
                        'broadleaf_deciduous_temperate_tree',
                        'broadleaf_deciduous_boreal_tree',
                        'broadleaf_evergreen_shrub',
                        'broadleaf_deciduous_temperate_shrub',
                        'broadleaf_deciduous_boreal_shrub',
                        'c3_arctic_grass',
                        'c3_non-arctic_grass',
                        'c4_grass',
                        'c3_crop',
                        'c3_irrigated',
                        'corn',
                        'irrigated_corn',
                        'spring_temperate_cereal',
                        'irrigated_spring_temperate_cereal',
                        'winter_temperate_cereal',
                        'irrigated_winter_temperate_cereal',
                        'soybean',
                        'irrigated_soybean'),
    stringsAsFactors = FALSE
)

f_clean_sheet_name = function(sheet_name, existing_sheet_names = character()) {
    clean_name = gsub('[\\\\/\\?\\*\\[\\]:]', '_', sheet_name)
    clean_name = gsub("'", '', clean_name)
    clean_name = substr(clean_name, 1, 31)
    if (!nzchar(clean_name)) clean_name = 'PFT'

    candidate_name = clean_name
    suffix_counter = 1
    while (candidate_name %in% existing_sheet_names) {
        suffix = paste0('_', suffix_counter)
        candidate_name = paste0(substr(clean_name, 1, 31 - nchar(suffix)), suffix)
        suffix_counter = suffix_counter + 1
    }

    candidate_name
}

f_get_sheet_name = function(target_pft_r_index, existing_sheet_names = character()) {
    pft_row = PFT_num_name_df[match(target_pft_r_index, PFT_num_name_df$PFT_r_index), ]
    if (nrow(pft_row) != 1) {
        stop(paste0('Cannot find PFT description for target_PFT index ', target_pft_r_index))
    }

    raw_sheet_name = paste0('PFT', pft_row$PFT_number, '_', pft_row$PFT_description)
    f_clean_sheet_name(sheet_name = raw_sheet_name, existing_sheet_names = existing_sheet_names)
}

if (output_format == 'xlsx' && !requireNamespace('openxlsx', quietly = TRUE)) {
    stop("The 'openxlsx' package is required for xlsx output. Please install it or switch output_format to 'RData'.")
}

######################## code execution
day_vec = make.date.vec(start.date = start_date, end.date = end_date)
# starting_POSIXct = as.POSIXct(start_date, format = '%Y%m%d', tz = 'UTC')
# ending_POSIXct = as.POSIXct(end_date, format = '%Y%m%d', tz = 'UTC')


for (day in seq(day_vec)) {
    
    nc = nc_open(paste0('hist_grid_',day_vec[day],'.nc'))
    lon = ncvar_get(nc, 'lon')
    lat = ncvar_get(nc, 'lat')
    
    if ((length(lon) > 1 || length(lat) > 1) && output_format == 'xlsx') {stop('xlsx output format not support for multi-site simulations')}
    
    
    # create an empty vector for storing value of daily-resolution variables for each simulation day
    if (!is.null(daily_variable)) {
        tmp_daily_out = array(NA, dim = c(length(lon),length(lat),length(target_PFT),length(daily_variable)))
    }
    
    # create an empty vector for storing hourly-resolution variables for each simulation day
    tmp_hourly_out = array(NA, dim = c(length(lon),length(lat),length(target_PFT),24,length(hourly_variable)))
    
    if (day == 1) {
        
        # create an empty vector for storing daily-resolution variables for all simulation days 
        if (!is.null(daily_variable)) {
            # final_daily_output = array(data = NA, dim = c(length(lon),length(lat),length(day_vec),length(daily_variable)))
            final_daily_output = array(data = NA, dim = c(length(lon),length(lat),length(target_PFT),length(day_vec),length(daily_variable)))
        }
        
        if (output_hourly_var_as_daily_mean){
            # create an empty vector for storing daily averages of hourly-resolution variables for all simulation days, if needed
            final_dailyAvg_output = array(data = NA, dim = c(length(lon),length(lat),length(target_PFT),length(day_vec),length(hourly_variable)))
            
        } else {
            # create an empty vector for storing hourly-resolution variables for all simulation days, if needed
            final_hourly_output = array(data = NA, dim = c(length(lon),length(lat),length(target_PFT),length(day_vec)*24,length(hourly_variable)))
            
        }
    }
    
    # obtain output from the nc file
    # get values from daily variable
    if (!is.null(daily_variable)) {
        #get daily_variablrs
        for (i in seq(daily_variable)) {
            if (length(lon) == 1 && length(lat) == 1){
                tmp_daily = ncvar_get(nc, varid = daily_variable[i])[target_PFT]  # dim(tmp_hourly) = length(PFT)
            } else {
                tmp_daily = ncvar_get(nc, varid = daily_variable[i])[,,target_PFT] # dim(tmp_daily) = c(lon,lat,PFT)
            }
            # tmp_daily_out[,,i] = tmp_daily
            tmp_daily_out[,,,i] = tmp_daily
        }
    }
    
    # get values from hourly variables
    for (j in seq(hourly_variable)) {
        if (length(lon) == 1 && length(lat) == 1){
            tmp_hourly = ncvar_get(nc, varid = hourly_variable[j])[target_PFT,]   # dim() = PFT, hour
            tmp_hourly_out[1,1,,,j] = tmp_hourly                     
        } else {
            
            tmp_hourly = ncvar_get(nc, varid = hourly_variable[j])[,,target_PFT,]  # lon lat pft hour
            tmp_hourly_out[,,,,j] = tmp_hourly
        }
    }
    
    nc_close(nc)
    
    
    # calculate the daily mean of hourly variable 
    if (output_hourly_var_as_daily_mean) {
        if (length(lon) == 1 && length(lat) == 1){
            tmp_hourly_out = apply(tmp_hourly_out, c(1,2,3,5), FUN = mean, na.rm = T)
        } else {
            tmp_hourly_out = apply(tmp_hourly_out, c(1,2,3,5), FUN = mean, na.rm = T)
        }
    }
    
    # Insert the value of a daily variable to the daily output matrix
    if (!is.null(daily_variable)) {
        final_daily_output[,,,day,] = tmp_daily_out[,,,]
    }
    
    # Insert the value of a daily-averaged hourly variable to the output matrix
    if (output_hourly_var_as_daily_mean){
        if (length(lon) == 1 && length(lat) == 1) {
            final_dailyAvg_output[1,1,,day,] = tmp_hourly_out
        } else {
            final_dailyAvg_output[,,,day,] = tmp_hourly_out[,,,]
        }
    } else {
        # Insert the value of a hourly variable to the output matrix
        if (length(lon) == 1 && length(lat) == 1) {
            final_hourly_output[1,1,,((day-1)*24+1):((day-1)*24+24),] = tmp_hourly_out
        } else {
            final_hourly_output[,,,((day-1)*24+1):((day-1)*24+24),] = tmp_hourly_out[,,,,]
        }
    }
    
    # Creating object names during the processing of the final simulation day
    if (day == length(day_vec)) {
        for (j in seq(hourly_variable)) {
            if (output_hourly_var_as_daily_mean){
                assign(x = paste0('dailyAvg_hourly_',hourly_variable[j]), value = final_dailyAvg_output[,,,,j])
            } else {
                assign(x = paste0('hourly_',hourly_variable[j]), value = final_hourly_output[,,,,j])
            }
        }
        
        if (!is.null(daily_variable)) {
            for (i in seq(daily_variable)) {
                assign(x = paste0('daily_',daily_variable[i]), value = final_daily_output[,,,,i])
            }
        }
        
        # saving the output
        # the files are output at the hist_file in a simulation case
        setwd(paste0(run_dir,casename,'/'))
        
        if (output_format == 'xlsx') {
            daily_time_vec = as.POSIXct(as.character(day_vec), format = '%Y%m%d', tz = 'UTC')
            hourly_time_vec = seq(from = daily_time_vec[1], by = 'hour', length.out = length(day_vec)*24)

            if (output_hourly_var_as_daily_mean) {
                daily_wb = openxlsx::createWorkbook()
                existing_sheet_names = character()

                for (pft_pos in seq_along(target_PFT)) {
                    sheet_name = f_get_sheet_name(target_pft_r_index = target_PFT[pft_pos],
                                                  existing_sheet_names = existing_sheet_names)
                    existing_sheet_names = c(existing_sheet_names, sheet_name)

                    output_df = data.frame(
                        UTC_time = format(daily_time_vec, '%Y-%m-%d %H:%M:%S', tz = 'UTC'),
                        stringsAsFactors = FALSE
                    )

                    if (!is.null(daily_variable)) {
                        for (i in seq_along(daily_variable)) {
                            output_df[[daily_variable[i]]] = as.vector(final_daily_output[1,1,pft_pos,,i])
                        }
                    }

                    for (j in seq_along(hourly_variable)) {
                        output_df[[hourly_variable[j]]] = as.vector(final_dailyAvg_output[1,1,pft_pos,,j])
                    }

                    openxlsx::addWorksheet(daily_wb, sheetName = sheet_name)
                    openxlsx::writeData(daily_wb, sheet = sheet_name, x = output_df)
                }

                openxlsx::saveWorkbook(daily_wb,
                                       file = paste0(casename, '_daily_processed.xlsx'),
                                       overwrite = TRUE)
            } else {
                daily_wb = openxlsx::createWorkbook()
                hourly_wb = openxlsx::createWorkbook()
                daily_sheet_names = character()
                hourly_sheet_names = character()

                for (pft_pos in seq_along(target_PFT)) {
                    daily_sheet_name = f_get_sheet_name(target_pft_r_index = target_PFT[pft_pos],
                                                        existing_sheet_names = daily_sheet_names)
                    hourly_sheet_name = f_get_sheet_name(target_pft_r_index = target_PFT[pft_pos],
                                                         existing_sheet_names = hourly_sheet_names)
                    daily_sheet_names = c(daily_sheet_names, daily_sheet_name)
                    hourly_sheet_names = c(hourly_sheet_names, hourly_sheet_name)

                    daily_df = data.frame(
                        UTC_time = format(daily_time_vec, '%Y-%m-%d %H:%M:%S', tz = 'UTC'),
                        stringsAsFactors = FALSE
                    )
                    if (!is.null(daily_variable)) {
                        for (i in seq_along(daily_variable)) {
                            daily_df[[daily_variable[i]]] = as.vector(final_daily_output[1,1,pft_pos,,i])
                        }
                    }

                    hourly_df = data.frame(
                        UTC_time = format(hourly_time_vec, '%Y-%m-%d %H:%M:%S', tz = 'UTC'),
                        stringsAsFactors = FALSE
                    )
                    for (j in seq_along(hourly_variable)) {
                        hourly_df[[hourly_variable[j]]] = as.vector(final_hourly_output[1,1,pft_pos,,j])
                    }

                    openxlsx::addWorksheet(daily_wb, sheetName = daily_sheet_name)
                    openxlsx::writeData(daily_wb, sheet = daily_sheet_name, x = daily_df)

                    openxlsx::addWorksheet(hourly_wb, sheetName = hourly_sheet_name)
                    openxlsx::writeData(hourly_wb, sheet = hourly_sheet_name, x = hourly_df)
                }

                openxlsx::saveWorkbook(daily_wb,
                                       file = paste0(casename, '_daily_processed.xlsx'),
                                       overwrite = TRUE)
                openxlsx::saveWorkbook(hourly_wb,
                                       file = paste0(casename, '_hourly_processed.xlsx'),
                                       overwrite = TRUE)
            }
        } else if (output_format == 'RData') {
            # if output_hourly_var_as_daily_mean: output daily variables and daily averaged variables (1 file)
            # else: output daily variables and hourly variables (two separated files)
            if (output_hourly_var_as_daily_mean) {
                save(list = c('start_date', 'end_date', 'target_PFT', 'lon','lat', paste0('daily_',daily_variable), paste0('dailyAvg_hourly_',hourly_variable)), file = paste0(casename,'_daily_processed.RData'))   
            } else {
                save(list = c('start_date', 'end_date', 'target_PFT', 'lon','lat', paste0('daily_',daily_variable)), file = paste0(casename,'_daily_processed.RData'))   
                save(list = c('start_date', 'end_date', 'target_PFT', 'lon','lat', paste0('hourly_',hourly_variable)), file = paste0(casename,'_hourly_processed.RData'))   
            }
        }
        

    }
    
    print(paste0('Finished day = ',day, ' simulation date = ', day_vec[day]))
}

print(paste0('Finished without error, output files are located at ', run_dir,casename))
