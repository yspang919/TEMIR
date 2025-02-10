##################
# code to process hist_grid files into one single .RData that contains a timeseries of TEMIR variables
# the processed file names "processed_${casename}.RData" will appear at the case directory (${run_dir}/${casename}/)

library(ncdf4)
rm(list = ls())

# require tools.R in TEMIR
source('C:/Users/jacky/OneDrive/Documents/GitHub/TEMIR/code_v1.0/tools.R')

# base directory that contain all the simulation cases
run_dir='C:/Users/jacky/Documents/TEMIR/'

# case name of the simulation
casename='Syam_full_sim_with_fluxfile'

setwd(paste0(run_dir,casename,'/hist_data/'))

# year = 2002
# start_date = as.numeric(paste0(year, '0101'))
# end_date = as.numeric(paste0(year, '1231'))

start_date = 20190501
end_date = 20190930

### TEMIR variables that have a daily resolution (not appliable for v1.0)
### the processed variables will have a dimension of (lon x lat x #plant functional group x #day of the simulation)
# daily_variable = c('grainC', 'leafC', 'livestemC', 'LAI', 'GPP', 'NPP')
daily_variable = NULL

### TEMIR variables that have a hourly resolution
### the processed variables will have a dimension of (lon x lat x #plant functional group x #hour of the simulation)

hourly_variable = c('A_can', 'g_can', 'g_s', 'LAI_sun', 'LAI_sha')
# hourly_variable = c('A_can', 'g_can', 'g_s', 'CUO_can', 'CUO_sha', 'CUO_sun')

###
# 'daily': aggregating hourly variables for calculating daily mean
# 'hourly': keep the hourly variable as it (dim: lon x lat x #plant functional group x #hour of the simulation)
output_method = 'hourly'

# process which PFT in theoutputs?
# refer to 'PFT_df' in input_TEMIR.R
# target_PFT = c(1:24)
target_PFT = c(18)


day_vec = make.date.vec(start.date = start_date, end.date = end_date)
time_start = as.Date(start_date, format = '%Y%m%d')
time_end = as.Date(end_date, format = '%Y%m%d')
if (output_method == 'daily') {
    time_start = as.Date(as.character(start_date), format = '%Y%m%d')
    time_end = as.Date(as.character(end_date), format = '%Y%m%d')
    time_vec = seq.POSIXt(from = time_start, to = time_end, by = 'day')
}  else if (output_method == 'hourly') {
    time_start = as.Date(as.character(start_date), format = '%Y%m%d')
    time_end = as.Date(as.character(end_date), format = '%Y%m%d')
    time_vec = seq.POSIXt(from = as.POSIXct(time_start, tz = 'UTC'), to = as.POSIXct(time_end, tz = 'UTC') + 23*3600, by = 'hour')  # data until 23:00 of the last day
}

for (day in seq(day_vec)) {
    
    nc = nc_open(paste0('hist_grid_',day_vec[day],'.nc'))
    lon = ncvar_get(nc, 'lon')
    lat = ncvar_get(nc, 'lat')
   
    # tmp_daily_out = array(NA, dim = c(length(lon),length(lat),length(daily_variable)))
    if (!is.null(daily_variable)) {
       tmp_daily_out = array(NA, dim = c(length(lon),length(lat),length(target_PFT),length(daily_variable)))
    }

    # tmp_hourly_out = array(NA, dim = c(length(lon),length(lat),24,length(hourly_variable)))
    tmp_hourly_out = array(NA, dim = c(length(lon),length(lat),length(target_PFT),24,length(hourly_variable)))

    if (day == 1) {
        
        if (!is.null(daily_variable)) {
           # final_daily_output = array(data = NA, dim = c(length(lon),length(lat),length(day_vec),length(daily_variable)))
           final_daily_output = array(data = NA, dim = c(length(lon),length(lat),length(target_PFT),length(day_vec),length(daily_variable)))
        }

        if (output_method == 'daily'){
            # final_dailyAvg_output = array(data = NA, dim = c(length(lon),length(lat),length(day_vec), length(hourly_variable)))
            final_dailyAvg_output = array(data = NA, dim = c(length(lon),length(lat),length(target_PFT),length(day_vec),length(hourly_variable)))
            
        } else {
            # final_hourly_output = array(data = NA, dim = c(length(lon),length(lat),length(day_vec)*24, length(hourly_variable)))
            final_hourly_output = array(data = NA, dim = c(length(lon),length(lat),length(target_PFT),length(day_vec)*24,length(hourly_variable)))
            
        }
    }
    
    if (!is.null(daily_variable)) {
        #get daily_variablrs
        for (i in seq(daily_variable)) {
            if (length(lon) == 1 && length(lat) == 1){
                tmp_daily = ncvar_get(nc, varid = daily_variable[i])[target_PFT+1]  # dim(tmp_hourly) = length(PFT), PFT_index 1 is bare land in the output
            } else {
                tmp_daily = ncvar_get(nc, varid = daily_variable[i])[,,target_PFT+1] # dim(tmp_daily) = c(lon,lat,PFT)
            }
            # tmp_daily_out[,,i] = tmp_daily
            tmp_daily_out[,,,i] = tmp_daily
        }
    }
    
    #get hourly_variablrs
    for (j in seq(hourly_variable)) {
        if (length(lon) == 1 && length(lat) == 1){
            tmp_hourly = ncvar_get(nc, varid = hourly_variable[j])[target_PFT+1,]   # dim() = PFT, hour
            tmp_hourly_out[1,1,,,j] = tmp_hourly                      # dim(tmp_hourly) = 24
        } else {

            tmp_hourly = ncvar_get(nc, varid = hourly_variable[j])[,,target_PFT+1,]  # lon lat pft hour
            tmp_hourly_out[,,,,j] = tmp_hourly
        }
    }
    
    # print('Dim tmp_hourly_out = ')
    # print(dim(tmp_hourly_out))
    
    nc_close(nc)
    # calculate the daily mean of tmp_hourly_out
    if (output_method == 'daily') {
        if (length(lon) == 1 && length(lat) == 1){
            tmp_hourly_out = apply(tmp_hourly_out, c(1,2,3,5), FUN = mean, na.rm = T)
        } else {
            tmp_hourly_out = apply(tmp_hourly_out, c(1,2,3,5), FUN = mean, na.rm = T)
        }
    }
    
    if (!is.null(daily_variable)) {
       # final_daily_output[lon,lat,day,var] = tmp_daily_out[lon,lat,var]
       final_daily_output[,,,day,] = tmp_daily_out[,,,]
    }

    if (output_method == 'daily'){
        # final_dailyAvg_output[lon,lat,day,var] = tmp_hourly_out[lon,lat,var]
        if (length(lon) == 1 && length(lat) == 1) {
            final_dailyAvg_output[1,1,,day,] = tmp_hourly_out
        } else {
            final_dailyAvg_output[,,,day,] = tmp_hourly_out[,,,]
        }
    } else {
        # final_dailyAvg_output[lon,lat,day*24,var] = tmp_hourly_out[lon,lat,hr,var]
        if (length(lon) == 1 && length(lat) == 1) {
            final_hourly_output[1,1,,((day-1)*24+1):((day-1)*24+24),] = tmp_hourly_out
        } else {
            final_hourly_output[,,,((day-1)*24+1):((day-1)*24+24),] = tmp_hourly_out[,,,,]
        }
    }

    if (day == length(day_vec)) {
        for (j in seq(hourly_variable)) {
            if (output_method == 'daily'){
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
        
        if (output_method == 'daily'){
            setwd(paste0(run_dir,casename,'/'))
            print(paste0('Setting working directory at: ', getwd()))

            if (!is.null(daily_variable)) {
                save(list = c('target_PFT', 'lon','lat', paste0('daily_',daily_variable), paste0('dailyAvg_hourly_',hourly_variable), 'time_start', 'time_end', 'time_vec'), file = paste0(casename,'_processed.RData'))   
            } else {
                save(list = c('target_PFT', 'lon','lat', paste0('dailyAvg_hourly_',hourly_variable), 'time_start', 'time_end', 'time_vec'), file = paste0(casename,'_processed.RData'))   
            }
        } else {
            setwd(paste0(run_dir,casename,'/'))
            print(paste0('Setting working directory at: ', getwd()))

            if (!is.null(daily_variable)) {
                save(list = c('target_PFT', 'lon','lat', paste0('daily_',daily_variable), paste0('hourly_',hourly_variable), 'time_start', 'time_end', 'time_vec'), file = paste0(casename,'_processed.RData'))   
            } else {
                save(list = c('target_PFT', 'lon','lat', paste0('hourly_',hourly_variable), 'time_start', 'time_end', 'time_vec'), file = paste0(casename,'_processed.RData'))   
            }
        }

    }
    
    print(paste0('Finished day = ',day, ' date = ', day_vec[day]))
}

print('Finished without error')

