################################################################################
### Terrestrial Ecosystem Model in R (TEMIR)
### Input-reading script for site-level meteorological, LAI, ozone, and FLUXNET data
################################################################################

# This script reads optional single-site input data used to replace or augment
# the default MERRA2 / GEOS-FP forcing, including custom meteorological,
# prescribed LAI, prescribed ozone, and FLUXNET inputs for site simulations.
# Custom site input and FLUXNET are mutually exclusive input sources for the
# meteorological overlay.


#######################  Hourly meteorological and land surface inputs (except ozone)
if (using_site_met_flag) {
    
    #### Loading the full custom meteorological input
    print(paste0('Loading custom meteorological input from: ', field_met_base_dir, field_met_case, field_met_filename))
    if (!file.exists(paste0(field_met_base_dir, field_met_case, field_met_filename))) {stop(paste0('Files ', field_met_base_dir, field_met_case, field_met_filename,' does not exist'))}
    load(paste0(field_met_base_dir, field_met_case, field_met_filename))
    
    field_measurement_met_df = get(get('field_met_df_name'))
    
    # Retrieve the time column, as well as variable columns whose values will replace the map values
    desired_field_met_var = site_meas_option_df %>% 
        filter(replace_met_map_flag == TRUE) %>%
        pull(source_var_name)
    
    
    # Subset the data df by the required columns
    field_measurement_met_SUB_df = field_measurement_met_df %>%
        select(UTC_time, all_of(desired_field_met_var[!is.na(desired_field_met_var)])) %>%
        # create a column of UTC hour for filtering NA input later
        mutate(UTC_hr = hour(UTC_time)) %>%
        # create a column of date in YYYYMMDD for reading the data later
        mutate(YYYYMMDD = as.numeric(format(UTC_time, '%Y%m%d')))
}





####################### User-provided ozone input
if (O3_damage_flag & !O3_fixed_flag  & using_site_o3_flag)  {
    
    print(paste0('Loading O3 input for single-site simulations from: ', O3_data_dir, field_o3_case, field_o3_filename))
    if (!file.exists(paste0(O3_data_dir, field_o3_case, field_o3_filename))) {stop(paste0('Files ', O3_data_dir, field_o3_case, field_o3_filename,' does not exist'))}
    
    load(paste0(O3_data_dir, field_o3_case, field_o3_filename))
    
    field_measurement_o3_df = get(get('o3_met_df_name')) %>%
        # create a column of date in YYYYMMDD for subsetting the data later
        mutate(YYYYMMDD = as.numeric(format(UTC_time, '%Y%m%d')))
    
}


####################### User-provided leaf area index
if (using_site_LAI_flag) {
    if (!file.exists(paste0(site_prescribed_LAI_dir, prescribed_LAI_case, prescribed_LAI_filename))) {stop(paste0('Files ', site_prescribed_LAI_dir, prescribed_LAI_case, prescribed_LAI_filename,' does not exist'))}
    print(paste0('Loading custom LAI input from: ', site_prescribed_LAI_dir, prescribed_LAI_case, prescribed_LAI_filename))
    load(paste0(site_prescribed_LAI_dir, prescribed_LAI_case, prescribed_LAI_filename))
    
    prescriebd_LAI_df = get(get('prescribed_LAI_df_name')) %>% 
        mutate(YYYYMMDD = as.numeric(format(UTC_time, '%Y%m%d')))
    
}

####################### FLUXNET
# FLUXNET is currently kept as a dedicated reader, but it now feeds the same
# post-map site-level override stage as other single-site meteorological inputs.
if (FLUXNET_site_flag) {
    if (FLUXNET_flag) {
        
        # Check for FLUXNET data availability over the simulation days:
        FLUXNET_time_info = f_FLUXNET_UTC_2_local(FLUXNET.dir = FLUXNET_dir, date = start_date, site.id = FLUXNET_site_id, utc.offset = NA, out.utc.offset = TRUE)
        time_shift = FLUXNET_time_info$time_diff
        shifted_start = FLUXNET_time_info$time ; rm(FLUXNET_time_info)
        shifted_end = f_FLUXNET_UTC_2_local(FLUXNET.dir = FLUXNET_dir, date = to.yyyymmdd(from.yyyymmdd(end_date) + 24), site.id = FLUXNET_site_id, utc.offset = time_shift, out.utc.offset = FALSE)
        day_mod_flag = FALSE
        
        # Shift start date by one day to see if data is in range
        if (!f_FLUXNET_date_range(FLUXNET.dir = FLUXNET_dir, start.or.end.year = 'start', whole.date.for.year = shifted_start, site.id = FLUXNET_site_id)) {
            temp_date = start_date
            start_date = to.yyyymmdd(from.yyyymmdd(start_date) + 24)
            print(paste0('Time shifted FLUXNET data is not available for start_date!'), quote = FALSE) 
            print(paste0('So delaying start_date ', temp_date,' to the next day ', start_date), quote = FALSE); remove(temp_date)
            day_mod_flag = TRUE
        }
        
        # Shift end date by one day to see if data is in range
        if (!f_FLUXNET_date_range(FLUXNET.dir = FLUXNET_dir, start.or.end.year = 'end', whole.date.for.year = shifted_end, site.id = FLUXNET_site_id)) {
            temp_date = end_date
            end_date = to.yyyymmdd(from.yyyymmdd(end_date) - 24)
            print(paste0('Time shifted FLUXNET data is not available for end_date!'), quote = FALSE)
            print(paste0('So advancing end_date ', temp_date,' to the previous day ', end_date), quote = FALSE); remove(temp_date)
            day_mod_flag = TRUE
        }
        
        # Check for FLUXNET data availability over the shifted simulation days:
        if (day_mod_flag) {
            FLUXNET_time_info = f_FLUXNET_UTC_2_local(FLUXNET.dir = FLUXNET_dir, date = start_date, site.id = FLUXNET_site_id, utc.offset = NA, out.utc.offset = TRUE)
            time_shift = FLUXNET_time_info$time_diff
            shifted_start = FLUXNET_time_info$time ; rm(FLUXNET_time_info)
            shifted_end = f_FLUXNET_UTC_2_local(FLUXNET.dir = FLUXNET_dir, date = to.yyyymmdd(from.yyyymmdd(end_date) + 24), site.id = FLUXNET_site_id, utc.offset = time_shift, out.utc.offset = FALSE)
            
            # Check if FLUXNET data is available for shifted dates
            if (!f_FLUXNET_date_range(FLUXNET.dir = FLUXNET_dir, start.or.end.year = 'start', whole.date.for.year = shifted_start, site.id = FLUXNET_site_id)) stop(paste0('Time shifted FLUXNET data is not available for start_date even after delaying by a day!!! Please check FLUXNET data of site ', FLUXNET_site_id, '....'))
            if (!f_FLUXNET_date_range(FLUXNET.dir = FLUXNET_dir, start.or.end.year = 'end', whole.date.for.year = shifted_end, site.id = FLUXNET_site_id)) stop(paste0('Time shifted FLUXNET data is not available for end_date even after advancing by a day!!! Please check FLUXNET data of site ', FLUXNET_site_id, '....'))
        }
        
        # FLUXNET data taken as better data, replacing Monin-Obukhov outputs:
        infer_canopy_cond_flag = FALSE
        
        # Here is copied from a separate section, need to test if it still works.
        # Get FLUXNET data information:
        FLUXNET_file_settings = f_FLUXNET_file(FLUXNET.dir = FLUXNET_dir, site.id = FLUXNET_site_id, hr.part = dt_hr, hourly.data = TRUE)
        FLUXNET_file = FLUXNET_file_settings$filedir
        FLUXNET_nrows = as.numeric(FLUXNET_file_settings$nrows) ; remove(FLUXNET_file_settings)
        FLUXNET_header = names(read.csv(file = FLUXNET_file, nrows = 1, header = TRUE, stringsAsFactors = FALSE))
        FLUXNET_check = f_FLUXNET_variable_check(FLUXNET.dir = FLUXNET_dir, FLUXNET.header = FLUXNET_header, site.id = FLUXNET_site_id, var.match.df = FLUXNET_input_data_df)
        FLUXNET_input_checked_df = FLUXNET_check$variable_df
        FLUXNET_global_err = FLUXNET_check$global_err ; remove(FLUXNET_check)
        
        # Get starting row of FLUXNET data for start date:
        data_start = f_FLUXNET_row_skip(FLUXNET.dir = FLUXNET_dir, site.id = FLUXNET_site_id, current.date = shifted_start, direction = 'forward', rangeloc = 'front', FLUXNET.nrows = FLUXNET_nrows, hourly.data = TRUE)
        
        # Get subset of FLUXNET data for relevant dates:
        FLUXNET_selected_data = read.csv(file = FLUXNET_file, skip = data_start, nrows = f_FLUXNET_row_skip(FLUXNET.dir = FLUXNET_dir, site.id = FLUXNET_site_id, current.date = shifted_end, direction = 'forward', rangeloc = 'back', FLUXNET.nrows = FLUXNET_nrows, hourly.data = TRUE) - data_start, header=TRUE)
        colnames(FLUXNET_selected_data) = FLUXNET_header
        FLUXNET_selected_data = FLUXNET_selected_data[,c('TIMESTAMP_START', 'TIMESTAMP_END', unique(na.omit(FLUXNET_input_checked_df$FLUXNET_var_name)))]
        
        # Subset half-hourly data into hourly data if required
        if (dt_hr == 1 && substr(basename(FLUXNET_file), 32, 33) == 'HH') {
            print('NOTE : Half-hourly data is subsetted into hourly data BUT precipitation is converted and not subsetted!')
            HH_to_HR_flag = TRUE
        } else HH_to_HR_flag = FALSE
        
    }
    
    # Get location (lon, lat) from FLUXNET_site_id:
    FLUXNET_lon_lat = f_lon_lat_sim_from_FLUXNET(FLUXNET.dir = FLUXNET_dir, site.id = FLUXNET_site_id)
    lon_sim = FLUXNET_lon_lat$lon_sim
    lat_sim = FLUXNET_lon_lat$lat_sim
    rm(FLUXNET_lon_lat)
} else {
    FLUXNET_flag = FALSE
}

####################
# Functions for the replacing MERRA2 meteorological data maps with site-level observations

f_reset_met_replace_flags = function() {
    replaced_PARTOT_flag <<- FALSE
    replaced_map_PARDR_flag <<- FALSE
    replaced_map_PARDF_flag <<- FALSE
    replaced_map_WS_flag <<- FALSE
    replaced_map_precip_flag <<- FALSE
    replaced_map_ATMP_flag <<- FALSE
    replaced_map_VPD_flag <<- FALSE
    replaced_map_GWETBULK_flag <<- FALSE
    replaced_map_GWETTOP_flag <<- FALSE
    replaced_map_GWETROOT_flag <<- FALSE
    replaced_map_TS_flag <<- FALSE
}

f_set_met_replacement_flag = function(TEMIR_var_name) {
    if (TEMIR_var_name == 'PARTOT') replaced_PARTOT_flag <<- TRUE
    if (TEMIR_var_name == 'PARDR') replaced_map_PARDR_flag <<- TRUE
    if (TEMIR_var_name == 'PARDF') replaced_map_PARDF_flag <<- TRUE
    if (TEMIR_var_name == 'WS') replaced_map_WS_flag <<- TRUE
    if (TEMIR_var_name == 'PRECTOT') replaced_map_precip_flag <<- TRUE
    if (TEMIR_var_name == 'ATMP') replaced_map_ATMP_flag <<- TRUE
    if (TEMIR_var_name == 'VPD') replaced_map_VPD_flag <<- TRUE
    if (TEMIR_var_name == 'GWETBULK') replaced_map_GWETBULK_flag <<- TRUE
    if (TEMIR_var_name == 'GWETTOP') replaced_map_GWETTOP_flag <<- TRUE
    if (TEMIR_var_name == 'GWETROOT') replaced_map_GWETROOT_flag <<- TRUE
    if (TEMIR_var_name == 'TSOIL1') replaced_map_TS_flag <<- TRUE
}

# Subsetting the full site met. data based on the current simulation day
f_get_daily_site_met_df = function(current_date) {
    if (!exists('field_measurement_met_SUB_df')) {
        stop('Custom site meteorological input has not been loaded')
    }
    
    field_measurement_met_daily_df = subset.data.frame(field_measurement_met_SUB_df, YYYYMMDD == current_date)
    
    if (nrow(field_measurement_met_daily_df) > 0) {
        field_measurement_met_daily_df = field_measurement_met_daily_df %>%
            full_join(tibble(UTC_hr = 0:23), by = 'UTC_hr') %>%
            arrange(UTC_hr)
    }
    
    return(field_measurement_met_daily_df)
}

#######  Replacing MERRA2/GEOS-FP met. data map with user-provide values for a single site
f_replace_metmaps_with_custom_data = function(site_i, site_j) {
    
    if (!exists('site_meas_option_df')) {
        stop('Custom site meteorological replacement settings are not available')
    }
    
    f_reset_met_replace_flags()
    
    print('Reading non-FLUXNET site measure inputs')
    # Site-measured meteorology of the simulated day
    field_measurement_met_daily_df = f_get_daily_site_met_df(current_date = current_date)
    
    if (nrow(field_measurement_met_daily_df) == 0) {
        print(paste0('Site meterological data is not available on ', current_date,'. Using meteorological input map instead.'))
        return(invisible(NULL))
    }
    
    for (index in seq_len(nrow(site_meas_option_df))) {
        
        # Not to replace the target meteorological variable, if the user doesn't want to
        if (site_meas_option_df$replace_met_map_flag[index] != TRUE) next
        
        # Excluding the processing of TSOIL layers, it is processed with f_replace_soilT_with_custom_data()
        if (grepl('^TSOIL', site_meas_option_df$TEMIR_var_name[index])) next
        
        field_meas_name = site_meas_option_df$source_var_name[index]
        TEMIR_var_name = site_meas_option_df$TEMIR_var_name[index]
        if (is.na(field_meas_name)) next
        
        # Replacing the target meteorological variable
        # Skip replacing if at least one value is NA, could be relex in the future
        if (all(!is.na(field_measurement_met_daily_df[[field_meas_name]]))) {
            
            # If the variable also appeared in met. field,  obtain the whole met. field and then replace the reanalysis value at the grid cell of interest with the site-measured value, then return the whole met. field
            # Otherwise, just store the site-measured value in an array without assigning it to the met. field
            
            if (site_meas_option_df$var_also_in_metfield[index] == TRUE) {
                # value of the target variable from the site measurements
                tgt_var_daily_vec = field_measurement_met_daily_df[[field_meas_name]]
                # value of the target variable from the met. field
                tgt_var_map = get(x = TEMIR_var_name, envir = .GlobalEnv)
                # replace the value of the particular grid cell of the met. field
                tgt_var_map[site_i, site_j, ] = tgt_var_daily_vec
                # assign the new met. field to the orginal met. field
                assign(x = TEMIR_var_name, value = tgt_var_map, envir = .GlobalEnv)
            } else {
                assign(x = TEMIR_var_name, value = field_measurement_met_daily_df[[field_meas_name]], envir = .GlobalEnv)
            }

            # Indicate the met. field reanalysis value is replaced with site measurements.
            f_set_met_replacement_flag(TEMIR_var_name = TEMIR_var_name)
        }
    }
    
    invisible(NULL)
}


####### Replacing MERRA2 soil T map with user-provide values for a single site
f_replace_soilT_with_custom_data = function(site_i, site_j) {
    
    # obtain the measurement for current day
    field_measurement_met_daily_df = f_get_daily_site_met_df(current_date = current_date)
    if (nrow(field_measurement_met_daily_df) == 0) {
        return(invisible(NULL))
    }

    # resetting the data replacement flag 
    f_reset_met_replace_flags()
    
    # 5 layers of soil T
    for (k in 5:1) {
        
        TSOIL_name = paste0("TSOIL", k)
        
        if (replace_TS_flag_array[k]) {
            # Site soil temperature is available for this layer:
            # replace the target grid-cell time series in the MERRA2 soil temperature map.
            site_col = site_meas_option_df$source_var_name[site_meas_option_df$TEMIR_var_name == TSOIL_name]
            # swap the Tsoil value of that grid cell of interest with site measurements
            TSOIL_map = get(TSOIL_name, envir = .GlobalEnv)
            TSOIL_map[site_i, site_j, ] = field_measurement_met_daily_df[[site_col]]
            assign(TSOIL_name, TSOIL_map, envir = .GlobalEnv)
            
            # Indicate at least 1 layer of TS is replaced with site measurements
            # TS data is replace at least for layer 1
            if (k == 1) {f_set_met_replacement_flag(TEMIR_var_name = 'TSOIL1')}
        } else {
            # No site soil temperature is available for this layer:
            # set the target grid-cell time series to NA so unreplaced MERRA2 values
            # do not contribute to site-level soil-temperature calculations.
            TSOIL_map = get(TSOIL_name, envir = .GlobalEnv)
            TSOIL_map[site_i, site_j, ] = NA
            assign(TSOIL_name, TSOIL_map, envir = .GlobalEnv)
        }
    }
}

####### Replacing MERRA2/GEOS-FP met. data map with FLUXNET2015 data set for a single site
f_replace_metmaps_with_FLUXNET = function() {
    if (!exists('FLUXNET_selected_data')) {
        stop('FLUXNET meteorological input has not been loaded')
    }
    
    f_reset_met_replace_flags()
    
    start_ind = match(FLUXNET_current, FLUXNET_selected_data$TIMESTAMP_START)
    end_ind = match(FLUXNET_end, FLUXNET_selected_data$TIMESTAMP_START) - 1
    
    if (is.na(start_ind) || start_ind < 0 || is.na(end_ind)) {
        stop(paste('Error in FLUXNET data input range!!!', start_ind, end_ind))
    }
    
    FLUXNET_day_data = FLUXNET_selected_data[start_ind:end_ind,]
    FLUXNET_var_err <<- NULL
    print('Extracting corresponding FLUXNET data for each variable....', quote = FALSE)
    
    FLUXNET_var_df = if (exists('FLUXNET_input_checked_df')) FLUXNET_input_checked_df else updated_input_df
    
    for (imet in seq_len(nrow(FLUXNET_var_df))) {
        TEMIR_variable_name = as.character(FLUXNET_var_df$TEMIR_var_name[imet])
        current_FLUXNET_met = as.character(FLUXNET_var_df$FLUXNET_var_name[imet])
        
        if (is.na(current_FLUXNET_met)) next
        
        if (!exists(TEMIR_variable_name)) assign(x = TEMIR_variable_name, value = array(data = NA, dim = met_dim), envir = .GlobalEnv)
        
        temporary = f_FLUXNET_met_grid(FLUXNET.data = FLUXNET_day_data,
                                       TEMIR.variable = TEMIR_variable_name,
                                       FLUXNET.variable = current_FLUXNET_met,
                                       FLUXNET.nrows = FLUXNET_nrows,
                                       dt.hr = dt_hr,
                                       st.ind = start_ind,
                                       end.ind = end_ind,
                                       input.var.df = FLUXNET_var_df,
                                       ind.lon = ind_lon,
                                       ind.lat = ind_lat,
                                       hh.to.hr.flag = HH_to_HR_flag)
        
        assign(x = TEMIR_variable_name, value = temporary$data, envir = .GlobalEnv)
        
        if (!is.null(temporary$err_out)) {
            FLUXNET_var_err <<- if (is.null(FLUXNET_var_err)) rbind(temporary$err_out) else rbind(FLUXNET_var_err, temporary$err_out)
        } else {
            f_set_met_replacement_flag(TEMIR_var_name = TEMIR_variable_name)
        }
    }
    
    if (exists('temporary')) rm(temporary)
    
    invisible(NULL)
}
