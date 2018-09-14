#################################################
#' @title Get CNRFC HEFS Forecasts
#' @author Dan Broman
#' @description Downloads daily ensemble
#' forecasts from CNRFC for the upper Klamath River Basin
#' Last Modified August 2 2018
###################################################
library(stringr)
library(lubridate)
setwd('/work/dbroman/projects/klamath_sro/get_forecasts/')

###################################################
urlhead = 'http://www.cnrfc.noaa.gov/csv/'
urltail = '_klamath_hefs_csv_daily.zip'
date_now_raw = Sys.Date()
date_now_str = paste0(
	year(date_now_raw),
	str_pad(month(date_now_raw), 2, pad = '0', 'left'),
	str_pad(day(date_now_raw), 2, pad = '0', 'left'),
	12)
url = paste0(urlhead, date_now_str, urltail)
outfile = paste0('data/raw/cnrfc/', date_now_str, urltail)

if(file.exists(outfile) == F){
	download.file(url, outfile)
}

# TO ADD - Check file size, catch error when downloading
