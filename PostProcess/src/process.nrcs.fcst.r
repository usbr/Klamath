#===========================================================
# Name: Upper Missouri RiverWare Post-Processing
# Author: D. Broman, USBR Technical Service Center
# Last Modified: 2017-05-09
# Description: Reads in RiverWare data files from the Upper
# Missouri River Basin model and calculates performance 
# measures. 
#===========================================================
# User Inputs:

#- Working Directory
wd = 'C:/Projects/KlamathRiverBasin/PostProcess/'

#-Scenario Directory [Location of RiverWare data]
dirDat = 'data/nrcs/'
#- Output Directory
dirOup = 'data/nrcs/'

dirNRCS = 'C:/Projects/KlamathRiverBasin/Forecasts/data/raw/nrcs/'

#===========================================================
#-postprocess_lib.r contains custom functions and required R packages
setwd(wd)
source('src/postprocess_lib.r')

trgtTbl = data.table(Target_Name = c('APR-SEP VOLUME', 'MAY-SEP VOLUME', 'JUN-SEP VOLUME'), mnthStrt = c(4:6))

# Williamson R nr Chiloquin location
locSel = c('WILLIAMSON near Chiloquin', 'WILLIAMSON R nr Chiloquin', 'WILLIAMSON R near Chiloquin', 'Williamson R bl Sprague R nr Chiloquin', 'WILLIAMSON R bl Sprague R nr Chiloquin')


#fileList = list.files(dirDat, pattern = 'fcstout_')
fileList = list.files(dirNRCS, pattern = 'fcstout_')
ctFile = length(fileList)
datOut = data.table()
for(iterFile in 1:ctFile){
	fileSel = fileList[iterFile]
	# metaFile = str_split(file_path_sans_ext(fileSel), '_')
	# yearSel = metaFile[2]
	# monthSel = metaFile[3]
#	datTmp = fread(paste0(dirDat, fileSel))
	datTmp = fread(paste0(dirNRCS, fileSel))
	datTmp = datTmp %>% filter(Location_name %in% locSel)
	datTmp = datTmp %>% left_join(trgtTbl) %>%
		filter(!is.na(mnthStrt)) %>%
		filter(mnthStrt >= Issue_month)
	datTmp = datTmp %>% dplyr::select(Issue_year, Issue_month, Target_Name, Prob_exceedence_levels, Fcst_min_vol, Fcst_low_vol, Fcst_mid_vol, Fcst_upp_vol, Fcst_max_vol, Fcst_min_pct, Fcst_low_pct, Fcst_mid_pct, Fcst_upp_pct, Fcst_max_pct)
	datTmp$Fcst_upp_vol = as.numeric(datTmp$Fcst_upp_vol)
	datTmp$Fcst_max_vol = as.numeric(datTmp$Fcst_max_vol)	
	datOut = bind_rows(datOut, datTmp)
}
datOut = datOut %>% dplyr::filter(Issue_year == 2006 | Issue_year == 2010)
datOut$Location = 'Williamson R near Chiloquin'
write.csv(datOut, paste0(dirOup, 'nrcsWillFcst.csv'), row.names = F, quote = F)




# Sprague R nr Chiloquin location
locSel = c('SPRAGUE near Chiloquin', 'SPRAGUE R nr Chiloquin', 'Sprague R nr Chiloquin', 'Sprague River near Chiloquin')

#fileList = list.files(dirDat, pattern = 'fcstout_')
fileList = list.files(dirNRCS, pattern = 'fcstout_')
ctFile = length(fileList)
datOut = data.table()
for(iterFile in 1:ctFile){
	fileSel = fileList[iterFile]
	# metaFile = str_split(file_path_sans_ext(fileSel), '_')
	# yearSel = metaFile[2]
	# monthSel = metaFile[3]
#	datTmp = fread(paste0(dirDat, fileSel))
	datTmp = fread(paste0(dirNRCS, fileSel))
	datTmp = datTmp %>% filter(Location_name %in% locSel)
	datTmp = datTmp %>% left_join(trgtTbl) %>%
		filter(!is.na(mnthStrt)) %>%
		filter(mnthStrt >= Issue_month)
	datTmp = datTmp %>% dplyr::select(Issue_year, Issue_month, Target_Name, Prob_exceedence_levels, Fcst_min_vol, Fcst_low_vol, Fcst_mid_vol, Fcst_upp_vol, Fcst_max_vol, Fcst_min_pct, Fcst_low_pct, Fcst_mid_pct, Fcst_upp_pct, Fcst_max_pct)
	datTmp$Fcst_upp_vol = as.numeric(datTmp$Fcst_upp_vol)
	datTmp$Fcst_max_vol = as.numeric(datTmp$Fcst_max_vol)	
	datOut = bind_rows(datOut, datTmp)
}
datOut = datOut %>% dplyr::filter(Issue_year == 2006 | Issue_year == 2010)
datOut$Location = 'Sprague R near Chiloquin'
write.csv(datOut, paste0(dirOup, 'nrcsSpragFcst.csv'), row.names = F, quote = F)




# Gerber Reservoir Inflow
locSel = c('Gerber Res Inflow (2)',	'Gerber Reservoir Inflow (2)',	'GERBER RESERVOIR Net Inflow (2)',	'GERBER RESERVOIR net Inflow (2)')

#fileList = list.files(dirDat, pattern = 'fcstout_')
fileList = list.files(dirNRCS, pattern = 'fcstout_')
ctFile = length(fileList)
datOut = data.table()
for(iterFile in 1:ctFile){
	fileSel = fileList[iterFile]
	# metaFile = str_split(file_path_sans_ext(fileSel), '_')
	# yearSel = metaFile[2]
	# monthSel = metaFile[3]
#	datTmp = fread(paste0(dirDat, fileSel))
	datTmp = fread(paste0(dirNRCS, fileSel))
#	datTmp = datTmp %>% filter(Location_name %in% locSel)
	datTmp = datTmp %>% filter(Location_name %like% 'GERBER' | Location_name %like% 'Gerber')	
	datTmp = datTmp %>% left_join(trgtTbl) %>%
		filter(!is.na(mnthStrt)) %>%
		filter(mnthStrt >= Issue_month)
	datTmp = datTmp %>% dplyr::select(Issue_year, Issue_month, Target_Name, Prob_exceedence_levels, Fcst_min_vol, Fcst_low_vol, Fcst_mid_vol, Fcst_upp_vol, Fcst_max_vol, Fcst_min_pct, Fcst_low_pct, Fcst_mid_pct, Fcst_upp_pct, Fcst_max_pct)
	datTmp$Fcst_upp_vol = as.numeric(datTmp$Fcst_upp_vol)
	datTmp$Fcst_max_vol = as.numeric(datTmp$Fcst_max_vol)	
	datOut = bind_rows(datOut, datTmp)
}
datOut = datOut %>% dplyr::filter(Issue_year == 2006 | Issue_year == 2010)
datOut$Location = 'Gerber Reservoir Inflow'
write.csv(datOut, paste0(dirOup, 'nrcsGerberFcst.csv'), row.names = F, quote = F)






# Clear Lake Inflow
locSel = c('CLEAR LAKE NET INFLOW (2)',	'CLEAR LAKE net Inflow  (2)', 'Clear Lake Inflow (2)', 'Clear Lk Inflow (2)', )

#fileList = list.files(dirDat, pattern = 'fcstout_')
fileList = list.files(dirNRCS, pattern = 'fcstout_')
ctFile = length(fileList)
datOut = data.table()
for(iterFile in 1:ctFile){
	fileSel = fileList[iterFile]
	# metaFile = str_split(file_path_sans_ext(fileSel), '_')
	# yearSel = metaFile[2]
	# monthSel = metaFile[3]
#	datTmp = fread(paste0(dirDat, fileSel))
	datTmp = fread(paste0(dirNRCS, fileSel))
	datTmp = datTmp %>% filter(Location_name %in% locSel)
	datTmp = datTmp %>% left_join(trgtTbl) %>%
		filter(!is.na(mnthStrt)) %>%
		filter(mnthStrt >= Issue_month)
	datTmp = datTmp %>% dplyr::select(Issue_year, Issue_month, Target_Name, Prob_exceedence_levels, Fcst_min_vol, Fcst_low_vol, Fcst_mid_vol, Fcst_upp_vol, Fcst_max_vol, Fcst_min_pct, Fcst_low_pct, Fcst_mid_pct, Fcst_upp_pct, Fcst_max_pct)
	datTmp$Fcst_upp_vol = as.numeric(datTmp$Fcst_upp_vol)
	datTmp$Fcst_max_vol = as.numeric(datTmp$Fcst_max_vol)	
	datOut = bind_rows(datOut, datTmp)
}
datOut = datOut %>% dplyr::filter(Issue_year == 2006 | Issue_year == 2010)
datOut$Location = 'Clear Lake Inflow'
write.csv(datOut, paste0(dirOup, 'nrcsClearFcst.csv'), row.names = F, quote = F)
