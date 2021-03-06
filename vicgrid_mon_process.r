#######DESCRIPTION###############################
#reads in raw VIC gridded output
#outputs monthly values as rdata object for use in impact assessments
#data can be output in other formats if desired - csv etc, though file size and write time may be issues
#this verision is the same as vicgrid_process.r except it performs the monthly calculations in the loop
#and only saves the monthly values for time / resource limitations
#################################################
##load libraries
source(function_lib.r)
library(tools)
library(dplyr)
library(tidyr)
library(readr)
library(data.table)

##user inputs
#raw vic file location
#assumed structure is directories containing scenario and period information
#each scenario directory contains flow and flux directories
#flux directory contains files with coordinate information in name
#MAIN DIR -> scenario1_period1, scenario2_period2...
#SCENARIO_PERIOD -> flux, flow
#FLUX -> fluxes_lon1_lat1.dat...
#these can be modified to work with different file structures if needed
dir_vic = ' '
#output location 
dir_output = ' '
run_name = ' '
#raw vic output headers
headers = c('year', 'month', 'day', 'prcp', 'temp', 'longwave', 'shortwave', 'rh', 'vpd', 'et', 'runoff', 'baseflow', 'soil_moist1', 'soil_moist2', 'soil_moist3', 'swe', 'snow_depth', 'pet_veg', 'pet_h2o', 'pet_vegnocr', 'pet_tall', 'pet_short')
#additional user inputs can be found below - desired vars output and desired monthly quantities
#current monthly outputs - total precipitation, mean temperature, mean relative humidity, total runoff, total baseflow, max daily swe in month, day of year of max daily swe, first of month swe
##preprocess
scenarios = list.dirs(dir_vic)

##process
fluxes_mon = NULL
for(i in 1:length(file_list)){
	file_temp = file_list[i]
	name_temp = unlist(strsplit(file_temp, '_'))
	lon_temp = as.numeric(name_temp[3])
	lat_temp = as.numeric(name_temp[2])
	fluxes_temp = fread(file.path(dir_vic, file_temp))
	fluxes_temp = setNames(fluxes_temp, headers)
	fluxes_temp$lon = lon_temp
	fluxes_temp$lat = lat_temp
	#outputs selected data - edit if different vars are desired
	fluxes_temp = fluxes_temp %>% dplyr::mutate(date = as.Date(paste(year, month, day, sep = '-'))) %>% dplyr::mutate(jd = as.numeric(format(date, '%j'))) %>% dplyr:dplyr::select(lon, lat, year, month, day, jd, prcp, temp, longwave, shortwave, rh, vpd, et, runoff, baseflow, soil_moist1, soil_moist2, soil_moist3, swe, snow_depth)
  fluxes_temp_stat = fluxes_temp %>% group_by(year, month, lon, lat) %>% dplyr::summarise(prcp = sum(prcp), temp = mean(temp), rh = mean(rh), runoff = sum(runoff), baseflow = sum(baseflow), swe_max = max(swe), swe_max_jd = jd[which.max(swe)])
  fluxes_temp_snow = fluxes_temp %>% dplyr::filter(day == 1) %>% dplyr::select(lon, lat, year, month, swe, snow_depth)
  fluxes_temp_mon = left_join(fluxes_temp_stat, fluxes_temp_snow)
	fluxes_mon = bind_rows(fluxes_mon, fluxes_temp_mon)
}
saveRDS(fluxes_mon, paste0(dir_output, run_name, '-month.rda'))
