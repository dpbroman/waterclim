#######DESCRIPTION###############################
#contains commonly used functions for water resource
#climate impacts assessments - source this to load functions
#################################################

#wyear - calculates water year from date object, x
#default start month is October (10)
wyear = function(x, start_month = 10) {
  require(lubridate)
  x_wyr = ifelse(lubridate::month(x) >= start_month, lubridate::year(x) + 1, lubridate::year(x))
  return(x_wyr)
}

#wyear_yearmon - calculates water year from year and month (as numeric)
#dedault start month is October (10)
wyear_yearmon = function(year, month, start_month = 10) {
  x.wyr = ifelse(month >= start_month, year + 1, year)
  return(x.wyr)
}

#list.dirs - comparable to list.files with directories

list.dirs = function(path=".", pattern = NULL, all.dirs = FALSE, full.names=FALSE, ignore.case=FALSE) {
  # use full.names=TRUE to pass to file.info
  all = list.files(path, pattern, all.dirs, full.names=TRUE, recursive=FALSE, ignore.case)
  dirs = all[file.info(all)$isdir]
  # determine whether to return full names or just dir names
  if(isTRUE(full.names))
    return(dirs)
  else
    return(basename(dirs))
}

#pctdiff - calculates percent difference between future and historic values
pctdiff = function(fut, hist){
	diff = (fut - hist) / hist * 100
	return(diff)
}

#magdiff - calculates magintude difference between future and historic values
magdiff = function(fut, hist){
	diff = (fut - hist)
	return(diff)
}
