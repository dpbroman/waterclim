#######DESCRIPTION###############################
#uses hde sets and monthly streamflow to calculate
#monthly change factors 
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
dir_dat = ''
run_name = ''
#historic and future periods to consider
hist_dt = data.table(period = 'hist', year = 1950:1999)
future_dt = data.table(period = rep(c('2020s', '2050s'), each = 30), year = c(2010:2039, 2040:2069))
#stations
stano_set = c(5,7,13,14,16,20,22,23,26,33)

##read in data
streamflow_dat = readRDS(paste0(dir_dat, run_name, '_month.rda')) %>% data.table()
hde_tbl = readRDS(paste0(dir_dat, run_name, '_hdeset.rda')) %>% data.table()

streamflow_hist = streamflow_dat %>% left_join(hist_dt) %>% dplyr::filter(!is.na(period)) %>% dplyr::select(-period)
period_list = unique(future_dt$period)
streamflow_hist_hde = NULL
for(i in 1:length(period_list)){
	period_temp = period_list[i]
	hde_tbl_temp = hde_tbl %>% dplyr::filter(period == period_temp)
	streamflow_hist_temp = streamflow_hist %>% left_join(hde_tbl_temp) %>% dplyr::filter(!is.na(scenario))
	streamflow_hist_temp$period = period_temp
	streamflow_hist_hde = bind_rows(streamflow_hist_hde, streamflow_hist_temp)
}

streamflow_hist_hde_mean = streamflow_hist_hde %>% group_by(month, period, stano, scenario) %>% dplyr::summarise(flow_hist = mean(flow, na.rm = T)) %>% data.table()

streamflow_hde = streamflow_dat %>% left_join(future_dt) %>% left_join(data.table(dist_dt)) %>% dplyr::filter(!is.na(scenario))

streamflow_hde_mean = streamflow_hde %>% group_by(month, period, stano, scenario) %>% dplyr::summarise(flow = mean(flow, na.rm = T)) %>% data.table()

streamflow_hde_cf = streamflow_hde_mean %>% left_join(streamflow_hist_hde_mean) %>% dplyr::mutate(flow_diff = pctdiff(flow, flow_hist)) %>% dplyr::mutate(cf = flow_diff / 100) %>% dplyr::filter(stano %in% stano_set)
saveRDS(streamflow_hde_cf, paste0(dir_dat, run_name, '_cf.rda'))
write.csv(streamflow_hde_cf, paste0(dir_dat, run_name, '_cf.csv'), row.names = F)
