#######DESCRIPTION###############################
#reads in processed gridded VIC data and plots 
#################################################
##load libraries
source(function_lib.r)
library(tools)
library(dplyr)
library(tidyr)
library(readr)
library(data.table)
library(ggplot2)
library(RColorBrewer)

##user inputs
dir_output = 'C:/Users/dbroman/Projects/A671F_Missouri Basin Impacts Assessment/Data/processed/'
dir_plots = 'C:/Users/dbroman/Projects/A671F_Missouri Basin Impacts Assessment/Plots/'
#list of HUC8 basins of interest
huc8_list = as.factor(c(10030202, 10030201, 10030203, 10030205, 10030104, 10030102, 10030101, 10020006, 10020005, 10020004, 10020003, 10020002, 10020001, 10020007, 10020008, 10030103, 10030105, 10040102, 10040101, 10040403, 10040201, 10040104, 10040205, 10040203, 10040202, 10040103, 10040204, 10040105, 10040106))

## read in data
# processed projection data
fluxes_proj_month = NULL
list_files_proj = list.files(dir_output, pattern = 'projected*')
for(i in 1:length(list_files_proj)){
	file_name_temp = list_files_proj[i]
	ids_temp = unlist(strsplit(file_path_sans_ext(file_name_temp), '_'))
	run_temp = ids_temp[2]
	period_temp = ids_temp[3]
	years_temp = ids_temp[4]
	fluxes_proj_temp = readRDS(paste0(dir_output, file_name_temp))
	fluxes_proj_temp$run = run_temp
	fluxes_proj_temp$period = period_temp
	fluxes_proj_temp$years = years_temp
	fluxes_proj_month = bind_rows(fluxes_proj_month, fluxes_proj_temp)
}
fluxes_proj_month = fluxes_proj_month %>% dplyr::mutate(year = wyear_yearmon(year, month))
#processed historic data
fluxes_month = readRDS(paste0(dir_output, 'historical_flux-month.rda'))
fluxes_month = fluxes_month %>% ungroup() %>% dplyr::mutate(year = wyear_yearmon(year, month))

#unit conversion...to F, cfs, in
fluxes_proj_month = fluxes_proj_month %>% dplyr::mutate(runoff = runoff * 0.0393701, prcp = prcp * 0.0393701, temp = (temp * 1.8) + 32, swe_max = swe_max * 0.0393700787, swe = swe * 0.0393700787, snow_depth = snow_depth * 0.0393700787)
fluxes_month = fluxes_month %>% dplyr::mutate(runoff = runoff * 0.0393701, prcp = prcp * 0.0393701, temp = (temp * 1.8) + 32, swe_max = swe_max * 0.0393700787, swe = swe * 0.0393700787, snow_depth = snow_depth * 0.0393700787)

#calculate climate measures
fluxes_clim = fluxes_month %>% group_by(year, lon, lat) %>% dplyr::summarise(temp = mean(temp), prcp = sum(prcp), runoff = sum(runoff)) %>% group_by(lon, lat) %>% dplyr::summarise(temp_hist = mean(temp), prcp_hist = mean(prcp), runoff_hist = mean(runoff)) %>% data.table()

fluxes_proj_clim = fluxes_proj_month %>% group_by(year, lon, lat, run, period, years) %>% dplyr::summarise(temp = mean(temp), prcp = sum(prcp), runoff = sum(runoff)) %>% group_by(lon, lat, run, period, years) %>% dplyr::summarise(temp = mean(temp), prcp = mean(prcp), runoff = mean(runoff)) %>% data.table()

fluxes_proj_clim = fluxes_proj_clim %>% left_join(fluxes_clim) %>% dplyr::mutate(temp_diff = magdiff(temp, temp_hist), prcp_diff = pctdiff(prcp, prcp_hist), runoff_diff = pctdiff(runoff, runoff_hist))

#calculate snow measures
fluxes_snow = fluxes_month %>% dplyr::filter(month == 4) %>% group_by(lon, lat) %>% dplyr::summarise(swe_hist = mean(swe)) %>% data.table()

fluxes_proj_snow = fluxes_proj_month %>% dplyr::filter(month == 4) %>% group_by(lon, lat, run, period, years) %>% dplyr::summarise(swe = mean(swe)) %>% data.table()

fluxes_proj_snow = fluxes_proj_snow %>% left_join(fluxes_snow) %>% dplyr::mutate(swe_diff = pctdiff(swe, swe_hist)) %>% dplyr::mutate(swe_diff = ifelse(is.infinite(swe_diff) == T, NA, swe_diff)) %>% dplyr::mutate(swe_diff = ifelse(is.nan(swe_diff) == T, NA, swe_diff))

fluxes_snowmax = fluxes_month %>% ungroup() %>% group_by(year, lon, lat) %>% dplyr::slice(which.max(swe_max)) %>% group_by(lon, lat) %>% dplyr::summarise(swe_max_hist = mean(swe), swe_max_jd_hist = round(mean(swe_max_jd))) %>% data.table()

fluxes_proj_snowmax = fluxes_proj_month %>% ungroup() %>% group_by(year, lon, lat, run, period) %>% dplyr::slice(which.max(swe_max)) %>% group_by(lon, lat, run, period) %>% dplyr::summarise(swe_max = mean(swe), swe_max_jd = round(mean(swe_max_jd))) %>% data.table()

fluxes_proj_snowmax = fluxes_proj_snowmax %>% left_join(fluxes_snowmax) %>% dplyr::mutate(swe_max_diff = pctdiff(swe_max, swe_max_hist), swe_max_jd_diff = magdiff(swe_max_jd, swe_max_jd_hist))

##plotting
#runoff
ggplot() + geom_raster(data = fluxes_proj_clim, aes(x = lon, y = lat, fill = runoff_diff)) + scale_fill_gradientn(colours = brewer.pal(8, 'RdBu'), name = 'Runoff Change (%)', limits = c(-150, 150)) + theme_bw() + xlab('Longitude') + ylab('Latitude') + theme(legend.position = 'bottom') + ggtitle('')  + facet_grid(run~period)
ggsave(paste0(dir_plots, 'projection_annrodiff_map_', '.png'), height = 10, width = 10, dpi = 300) 

#precipitation
ggplot() + geom_raster(data = fluxes_proj_clim, aes(x = lon, y = lat, fill = prcp_diff)) + scale_fill_gradientn(colours = brewer.pal(8, 'RdBu'), name = 'Precipitation Change (%)', limits = c(-100, 100)) + theme_bw() + xlab('Longitude') + ylab('Latitude') + theme(legend.position = 'bottom') + ggtitle('') + facet_grid(run~period)
ggsave(paste0(dir_plots, 'projection_annprcpdiff_map_', '.png'), height = 10, width = 10, dpi = 300) 
#temperature
ggplot() + geom_raster(data = fluxes_proj_clim, aes(x = lon, y = lat, fill = temp_diff)) + scale_fill_gradientn(colours = brewer.pal(8, 'YlOrRd'), name = 'Temperature Change (°F)') + theme_bw() + xlab('Longitude') + ylab('Latitude') + theme(legend.position = 'bottom') + ggtitle('') + facet_grid(run~period)
ggsave(paste0(dir_plots, 'projection_anntempdiff_map_', '.png'), height = 10, width = 10, dpi = 300) 
#april 1 swe
ggplot() + geom_raster(data = fluxes_proj_snow, aes(x = lon, y = lat, fill = swe_diff)) + scale_fill_gradientn(colours = brewer.pal(8, 'RdBu'), name = 'April 1st SWE Change (%)') + theme_bw() + xlab('Longitude') + ylab('Latitude') + theme(legend.position = 'bottom') + ggtitle('') + facet_grid(run~period)
ggsave(paste0(dir_plots, 'projection_annswediff_map_', '.png'), height = 10, width = 10, dpi = 300) 

#max swe
ggplot() + geom_raster(data = fluxes_proj_snowmax, aes(x = lon, y = lat, fill = swe_max_diff)) + scale_fill_gradientn(colours = brewer.pal(8, 'RdBu'), name = 'Max SWE Change (%)') + theme_bw() + xlab('Longitude') + ylab('Latitude') + theme(legend.position = 'bottom') + ggtitle('') + facet_grid(run~period)
ggsave(paste0(dir_plots, 'projection_maxswediff_map_', '.png'), height = 10, width = 10, dpi = 300) 

#max swe day-of-year
ggplot() + geom_raster(data = fluxes_proj_snowmax, aes(x = lon, y = lat, fill = swe_max_jd_diff)) + scale_fill_gradientn(colours = brewer.pal(8, 'RdBu'), name = 'Day of Year of Max SWE Change (days)') + theme_bw() + xlab('Longitude') + ylab('Latitude') + theme(legend.position = 'bottom') + ggtitle('') + facet_grid(run~period)
ggsave(paste0(dir_plots, 'projection_maxswedoydiff_map_', '.png'), height = 10, width = 10, dpi = 300) 
