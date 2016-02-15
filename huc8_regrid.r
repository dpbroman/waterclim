#######DESCRIPTION###############################
#processes raw HUC8 shapefiles from USGS 
#classifies VIC grid (or any grid) by HUC8
#outputs grid and trimmed polygon data as data table r objects for use in plotting
#################################################
##load libraries
library(tools)
library(dplyr)
library(tidyr)
library(readr)
library(data.table)
library(lubridate)
library(rgdal)
library(maptools)
library(raster)
library(plyr)
library(ggplot2)

##user inputs
#output location
dir_output = 'C:/Users/dbroman/Projects/A617F_Missouri Basin Impacts Assessment/Data/processed/'
#huc8 file location
dir_huc8 = 'C:/Users/dbroman/Projects/A671F_Missouri Basin Impacts Assessment/Data/GIS/huc8_mt/'
#huc8 file name
filename_huc8 = 'wbdhu8_a_mt'
#lon and lat of study region. these can be specified, or read in. need to be vectors
vic_lon = c(-113.9375,-113.8125,-113.6875,-113.5625,-113.4375,-113.3125,-113.1875,-113.0625,-112.9375,-112.8125,-112.6875,-112.5625,-112.4375,-112.3125,-112.1875,-112.0625,-111.9375,-111.8125,-111.6875,-111.5625,-111.4375,-111.3125,-111.1875,-111.0625,-110.9375,-110.8125,-110.6875,-110.5625,-110.4375,-110.3125,-110.1875,-110.0625,-109.9375,-109.8125,-109.6875,-109.5625,-109.4375,-109.3125,-109.1875,-109.0625,-108.9375,-108.8125,-108.6875,-108.5625,-108.4375,-108.3125,-108.1875,-108.0625,-107.9375,-107.8125,-107.6875,-107.5625,-107.4375,-107.3125,-107.1875,-107.0625,-106.9375,-106.8125,-106.6875,-106.5625,-106.4375,-106.3125,-106.1875,-106.0625,-105.9375,-105.8125)
vic_lat = c(45.5625,45.6875,45.8125,45.4375,45.1875,45.3125,45.0625,45.9375,48.5625,44.8125,44.9375,46.0625,48.3125,48.4375,48.6875,44.6875,47.5625,47.6875,48.1875,44.4375,44.5625,47.4375,47.8125,47.9375,48.0625,48.8125,47.3125,44.3125,48.9375,47.1875,46.1875,46.3125,46.6875,46.8125,46.9375,46.4375,47.0625,46.5625)

##process data
setwd(dir_huc8)
huc8 = readOGR(dsn = '.', layer = filename_huc8)
huc8@data$id = rownames(huc8@data)
huc8_dt = huc8 %>% fortify() %>% dplyr::mutate(id = as.numeric(id)) %>% data.table()
huc8_name = huc8@data$NAME
huc8_id = as.numeric(huc8@data$id)
huc8_name_dt = data.table(name = huc8_name, id = huc8_id)
huc8_dt = huc8_dt %>% left_join(huc8_name_dt) %>% dplyr::rename(lon = long)

huc8_rg_dt = huc8_dt %>% group_by(long) %>% dplyr::mutate(lon_vic = vic_lon[which.min(abs(long - vic_lon))]) %>% group_by(lat) %>% dplyr::mutate(lat_vic = vic_lat[which.min(abs(lat - vic_lat))]) %>% ungroup() %>% dplyr::select(id, name, lon_vic, lat_vic)
huc8_rg_dt = huc8_rg_dt %>% dplyr::distinct(lon_vic, lat_vic) %>% dplyr::rename(lon = lon_vic, lat = lat_vic)

huc8_coord = list(x = unique(huc8_df$long), y = unique(huc8_df$lat))
vic_coord = list(x = vic_lon, y = vic_lat)
ext = extent(vic_coord)
r = raster(ncol = length(vic_lon), nrow = length(vic_lat), ext)
huc8_rast = rasterize(huc8, r)
huc8_spdf = as(huc8_rast, 'SpatialPointsDataFrame')
huc8_spdf_dt = as.data.frame(huc8_spdf)

huc8_spdf_dt = huc8_spdf_dt %>% data.table() %>% dplyr::rename(id = layer, lon = x, lat = y) %>% left_join(huc8_name_dt)

##save data
saveRDS(huc8_rg_dt, paste0(dir_output, 'huc8_regrid.rda'))
saveRDS(huc8_dt, paste0(dir_output, 'huc8.rda'))

#plotting commands to see data...make sure it looks correct
# ggplot() + geom_polygon(data = huc8_dt, aes(x = lon, y = lat, group = group, colour = name))
# ggplot() + geom_raster(data = huc8_spdf_dt, aes(x = lon, y = lat, fill = name))
