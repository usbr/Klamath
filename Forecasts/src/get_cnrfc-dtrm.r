#################################################
#' @title Get CNRFC Deterministic Forecasts
#' @author Dan Broman
#' @description Downloads daily deterministic
#' forecasts from CNRFC for the upper Klamath River Basin
#' Last Modified August 2 2018
#################################################
library(stringr)
library(lubridate)
setwd('/work/dbroman/projects/klamath_sro/get_forecasts/')

#################################################
urlhead = 'https://www.cnrfc.noaa.gov/graphicalRVF_tabularprinter.php?id='
siteList = c('WMSO3', 'BTYO3')
ctSite = length(siteList)
for(iterSite in 1:ctSite){
	siteSel = siteList[iterSite]
	fileTail = paste0('_', siteSel, '.txt')
	date_now_raw = Sys.Date()
	date_now_str = paste0(
		year(date_now_raw),
		str_pad(month(date_now_raw), 2, pad = '0', 'left'),
		str_pad(day(date_now_raw), 2, pad = '0', 'left'),
		12)
	url = paste0(urlhead, siteSel)
	outfile = paste0('data/raw/cnrfc/', date_now_str, fileTail)

	if(file.exists(outfile) == F){
		download.file(url, outfile)
	}
}
# TO ADD - Check file size, catch error when downloading
