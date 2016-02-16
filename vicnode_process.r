#######DESCRIPTION###############################
#reads in raw VIC node output
#outputs daily and monthly values as rdata object for use in impact assessments
#data can be output in other formats if desired - csv etc, though file size and write time may be issues
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
#flow directory contains files with day / month / year in the name
#MAIN DIR -> scenario1_period1, scenario2_period2...
#SCENARIO_PERIOD -> flux, flow
#FLOW -> node1.day, node1.month, node1.year...
#these can be modified to work with different file structures if needed
dir_vic = ' '
#output location 
dir_output = ' '
run_name = ' '

##preprocess
scenarios = list.dirs(dir_vic)

##process
#daily
for(s in 1:length(scenarios)){
		flows_day = NULL
		file_path = file.path(dir_vic, scenarios[s], 'flow/')
		file_list_day = list.files(file_path, pattern = '*.day')
		headers_day = c('year', 'month', 'day', 'flow')
		flows_day = NULL
		for(i in 1:length(file_list_day)){
			file_temp = file_list_day[i]
			name_temp = file_path_sans_ext(file_temp)
			flows_temp = fread(file.path(file_path, file_temp))
			flows_temp$sta_name = name_temp
			flows_day = rbind_list(flows_day, flows_temp)
		}
		flows_day = setNames(flows_day, c(headers_day, 'sta_name'))
		saveRDS(flows_day, paste0(dir_output, scenarios[s], '_', run_name, '_day.rda'))
	}

#monthly
for(s in 1:length(scenarios)){
		flows_month = NULL
		file_path = file.path(dir_vic, scenarios[s], 'flow/')
		file_list_month = list.files(file_path, pattern = '*.month')
		headers_month = c('year', 'month', 'flow')
		flows_month = NULL
		for(i in 1:length(file_list_month)){
			file_temp = file_list_month[i]
			name_temp = file_path_sans_ext(file_temp)
			flows_temp = fread(file.path(file_path, file_temp))
			flows_temp$sta_name = name_temp
			flows_month = rbind_list(flows_month, flows_temp)
		}
		flows_month = setNames(flows_month, c(headers_month, 'sta_name'))
		saveRDS(flows_month, paste0(dir_output, scenarios[s], '_', run_name, '_month.rda'))
	}
	
#monthly mean
	for(s in 1:length(scenarios)){
		flows_month_mean = NULL
		file_path = file.path(dir_vic, scenarios[s], 'flow/')
		file_list_month_mean = list.files(file_path, pattern = '*.month_mean')
		headers_month_mean = c('month_mean', 'flow')
		flows_month_mean = NULL
		for(i in 1:length(file_list_month_mean)){
			file_temp = file_list_month_mean[i]
			name_temp = file_path_sans_ext(file_temp)
			flows_temp = fread(file.path(file_path, file_temp))
			flows_temp$sta_name = name_temp
			flows_month_mean = rbind_list(flows_month_mean, flows_temp)
		}
		flows_month_mean = setNames(flows_month_mean, c(headers_month_mean, 'sta_name'))
		saveRDS(flows_month_mean, paste0(dir_output, scenarios[s], '_', run_name, '_month_mean.rda'))
	}
}


