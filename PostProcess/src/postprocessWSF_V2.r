#===========================================================
# Name: Klamath SRO RiverWare Post-Processing
# Author: D. Broman, USBR Technical Service Center
# Last Modified: 2018-09-24 by MMcGuire
# Description: This script simply reformats NRCS forecast data to be read by
#     postprocessWSF_forEVS.r
#===========================================================
# User Inputs:

#- Working Directory
wd = 'C:/Projects/KlamathRiverBasin/PostProcess/'

#===========================================================
# Williamson R below Sprague R near Chiloquin
# Seasonal Forecasts compared to NRCS by lead time (month)
nrcsFcst = fread('data/nrcs/nrcsWillFcstAll.csv')
nrcsFcstPlot = nrcsFcst %>%
	mutate(InitDate = as.Date(paste(Issue_year, Issue_month, '01', sep = '-'))) %>%
	dplyr::rename(Year = Issue_year) %>%
	dplyr::select(InitDate, Year, Fcst_min_vol, Fcst_low_vol, Fcst_mid_vol, Fcst_upp_vol, Fcst_max_vol) %>%
	setNames(c('InitDate', 'Year', paste0('Value', c(10, 30, 50, 70, 90)))) %>%
  mutate(Set = 'NRCS')
willSeasFcst = nrcsFcstPlot
willSeasFcst = willSeasFcst %>% mutate(Month = month(InitDate)) %>% dplyr::filter(Month <= 4)
willnrcsFcstTbl = willSeasFcst %>% filter(Set == 'NRCS') %>% arrange(Month) %>% mutate(Location = 'Williamson R blw Sprague near Chiloquin')
write.csv(file='data/output/Willimason_Fcast_apr-sep_Volumes.csv',willnrcsFcstTbl,row.names=F)


#===========================================================
# Sprague R near Chiloquin
# Seasonal Forecasts compared to NRCS by lead time (month)
nrcsFcst = fread('data/nrcs/nrcsSpragFcstAll.csv')
nrcsFcstPlot = nrcsFcst %>%
	mutate(InitDate = as.Date(paste(Issue_year, Issue_month, '01', sep = '-'))) %>%
	dplyr::rename(Year = Issue_year) %>%
	dplyr::select(InitDate, Year, Fcst_min_vol, Fcst_low_vol, Fcst_mid_vol, Fcst_upp_vol, Fcst_max_vol) %>%
	setNames(c('InitDate', 'Year', paste0('Value', c(10, 30, 50, 70, 90)))) %>%
  mutate(Set = 'NRCS')

spragSeasFcst = nrcsFcstPlot
spragSeasFcst = spragSeasFcst %>% mutate(Month = month(InitDate)) %>% dplyr::filter(Month <= 4)
spragnrcsFcstTbl = spragSeasFcst %>% filter(Set == 'NRCS') %>% arrange(Month) %>% mutate(Location = 'Sprague R near Chiloquin')
write.csv(file='data/output/Sprague_Fcast_apr-sep_Volumes.csv',spragnrcsFcstTbl,row.names=F)
