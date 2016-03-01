#######DESCRIPTION###############################
#reads in raw VIC forcing data
#outputs monthly values as rdata object for use in impact assessments
#################################################
##load libraries
source('function_lib.r')
library(tools)
library(dplyr)
library(tidyr)
library(readr)
library(data.table)
library(ggplot2)

##user inputs
dir_vic = ''
#output location 
dir_output = ''
dir_output_hde = ''
vic_set = ''
run_name = ''
nset_sel = 10
#raw vic output headers
headers = c('date_ind', 'prcp', 'tmax', 'tmin', 'wind')
#historic and future periods to consider
hist_dt = data.table(period = 'hist', year = 1950:1999)
future_dt = data.table(period = rep(c('2020s', '2050s'), each = 30), year = c(2010:2039, 2040:2069))

##process data
scenarios = list.dirs(dir_vic)
for(j in 3:length(scenarios)){
	scenarios_temp = scenarios[j]
	file_path_temp = paste0(dir_vic, scenarios_temp, '/', vic_set, '/')
	file_list = list.files(file_path_temp)
	fluxes = NULL
	for(i in 1:length(file_list)){
		file_temp = file_list[i]
		name_temp = unlist(strsplit(file_temp, '_'))
		lon_temp = as.numeric(name_temp[3])
		lat_temp = as.numeric(name_temp[2])
		fluxes_temp = fread(paste0(file_path_temp, file_temp))
		fluxes_temp = setNames(fluxes_temp, headers)
		fluxes_temp$lon = lon_temp
		fluxes_temp$lat = lat_temp
		fluxes_temp$set = scenarios_temp
		fluxes_temp = fluxes_temp %>% dplyr::mutate(date = as.Date(paste(substr(date_ind, 1, 4), substr(date_ind, 5, 6), substr(date_ind, 7, 8), sep = '-'))) %>% dplyr::mutate(year = year(date), month = month(date))
		#outputs selected data - edit if different vars are desired
		fluxes_month_temp = fluxes_temp %>% group_by(year, month, lon, lat, set) %>% dplyr::summarise(prcp = sum(prcp), tmax = mean(tmax), tmin = mean(tmin), wind = mean(wind))
		fluxes = bind_rows(fluxes, fluxes_month_temp)
	}
	saveRDS(fluxes, paste0(dir_output, scenarios_temp, '.rda'))
}

##read in processed data
file_list = list.files(dir_output, pattern = '*.rda')
forcing_dt = NULL
for(i in 1:length(file_list)){
	name_temp = file_list[i]
	forcing_temp =  readRDS(paste0(dir_output, name_temp))

	forcing_hist_temp = forcing_temp %>% left_join(hist_dt) %>% dplyr::filter(!is.na(period)) %>% group_by(year, lon, lat, set) %>% dplyr::summarise(prcp = sum(prcp), tmax = mean(tmax), tmin = mean(tmin)) %>% group_by(lon, lat, set) %>% dplyr::summarise(prcp = mean(prcp), tmax = mean(tmax), tmin = mean(tmin)) %>% group_by(set) %>% dplyr::summarise(prcp_hist = mean(prcp), tmax_hist = mean(tmax), tmin_hist = mean(tmin))

	forcing_future_temp = forcing_temp %>% left_join(future_dt) %>% dplyr::filter(!is.na(period)) %>% group_by(year, period, lon, lat, set) %>% dplyr::summarise(prcp = sum(prcp), tmax = mean(tmax), tmin = mean(tmin)) %>% group_by(period, lon, lat, set) %>% dplyr::summarise(prcp = mean(prcp), tmax = mean(tmax), tmin = mean(tmin)) %>% group_by(period, set) %>% dplyr::summarise(prcp = mean(prcp), tmax = mean(tmax), tmin = mean(tmin))
	forcing_all_temp = forcing_hist_temp %>% left_join(forcing_future_temp) %>% dplyr::mutate(prcp_diff = pctdiff(prcp, prcp_hist), tmin_diff = magdiff(tmin, tmin_hist), tmax_diff = magdiff(tmax, tmax_hist))
	forcing_dt = bind_rows(forcing_dt, forcing_all_temp)
}
forcing_dt = forcing_dt %>% tidyr::separate(set, into = c('model', 'rcp', 'run'), sep = '_') 

##generate hde sets...currently hard coded hde scenarios
hde_table = data.table(prcp = c('prcp10', 'prcp90', 'prcp10', 'prcp90', 'prcp50'), temp = c('tmax10', 'tmax10', 'tmax90', 'tmax90', 'tmax50'), scenario = c('cool-dry', 'wet-dry', 'hot-dry', 'hot-wet', 'middle'))
forcing_stat = forcing_dt %>% group_by(period) %>% dplyr::summarise(prcp50 = as.numeric(quantile(prcp_diff, 0.5)), prcp10 = as.numeric(quantile(prcp_diff, 0.10)), prcp90 = as.numeric(quantile(prcp_diff, 0.90)), tmin50 = as.numeric(quantile(tmin_diff, 0.5)), tmin10 = as.numeric(quantile(tmin_diff, 0.10)), tmin90 = as.numeric(quantile(tmin_diff, 0.90)), tmax50 = as.numeric(quantile(tmax_diff, 0.5)), tmax10 = as.numeric(quantile(tmax_diff, 0.10)), tmax90 = as.numeric(quantile(tmax_diff, 0.90)))
forcing_stat_td = forcing_stat %>% gather(stat, value, -period)
forcing_stat_prcp_mg = forcing_stat_td %>% dplyr::rename(prcp = stat, prcp_value = value) %>% data.table()
forcing_stat_temp_mg = forcing_stat_td %>% dplyr::rename(temp = stat, temp_value = value) %>% data.table()
hde_table = hde_table %>% left_join(forcing_stat_prcp_mg) %>% left_join(forcing_stat_temp_mg)
period_list = unique(future_dt$period)
dist_dt = NULL
for(j in 1:length(period_list)){
	period_sel = period_list[j]
	forcing_dt_fl = forcing_dt %>% dplyr::filter(period == period_sel) 
	forcing_dt_meta = forcing_dt_fl %>% dplyr::select(model, rcp, run, period, prcp_diff, tmax_diff)
	forcing_dt_mat = forcing_dt_fl %>% dplyr::select(prcp_diff, tmax_diff)  %>% as.matrix() 
	forcing_dt_cov = cov(forcing_dt_mat)
	hde_table_fl = hde_table %>% dplyr::filter(period == period_sel)
	for(i in 1:nrow(hde_table_fl)){
		hde_table_temp = hde_table_fl[i, ]
		quant_vals_temp = c(hde_table_temp$prcp_value, hde_table_temp$temp_value)
		scenario_temp = hde_table_temp$scenario
		dist_temp = mahalanobis(forcing_dt_mat, quant_vals_temp, forcing_dt_cov)
		dist_temp_dt = forcing_dt_meta
		dist_temp_dt = data.table(forcing_dt_meta, scenario = scenario_temp, dist = dist_temp) %>% arrange(dist)
		dist_temp_dt_sel = dist_temp_dt[1:nset_sel, ]
		dist_dt = bind_rows(dist_dt, dist_temp_dt_sel)
	}
}
saveRDS(dist_dt, paste0(dir_output_hde, run_name, '_hdeset.rda'))
