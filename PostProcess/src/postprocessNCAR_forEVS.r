#===========================================================
# Name: Klamath SRO RiverWare Post-Processing
# Author: D. Broman, USBR Technical Service Center
# Last Modified: 2018-08-02 by MMcGuire
# Description: Reads in RiverWare data files from the Klamath
# River Basin model and calculates performance measures.
#===========================================================
# User Inputs:

#- Working Directory
wd = 'C:/Projects/KlamathRiverBasin/PostProcess/'
#wd = 'C:/Users/MMcguire/Documents/GitHub/Klamath/PostProcess/'

#- Output Directory
dirOup = 'data/output/'
dirEVS = 'C:/Projects/KlamathRiverBasin/EVS_5.6/Inputs/'

#===========================================================
#-postprocess_lib.r contains custom functions and required R packages
setwd(wd)
source('src/fncnLib.r')

# Read in Data

#read NCAR forecast files generated by Andy Wood
#reformat data for EVS
dirFCST = 'C:/Projects/KlamathRiverBasin/PreProcess/data/InflowOut/'
#fcstFiles = c('Willimason_FcastHist_apr-sep_Volumes.csv','Sprague_FcastHist_apr-sep_Volumes.csv','Gerber_FcastHist_apr-sep_Volumes.csv','Clear_FcastHist_apr-sep_Volumes.csv')
FCSTdirs = 'C:/Projects/KlamathRiverBasin/PreProcess/data/InflowOut/fcst_dirs.txt'
nrcsNames = c('WILLILOC','SPRAG')
startDates = c('19990101','19990101')
endDates = c('20160401','20160401')

listDirs = fread(FCSTdirs)
ctDirs = length(listDirs$V1)
ctLocs = length(nrcsNames)
for(iterLoc in 1:ctLocs){
	outFile=data.table()
	for(iterDir in 1:ctDirs){
		datFile = fread(paste0(dirFCST,nrcsNames[iterLoc],"/",listDirs[iterDir,1],"/flow_cfs_ens.001.pp.",nrcsNames[iterLoc],".txt"))
		datFile = datFile %>% dplyr::mutate(date1 = as.Date(paste0(year,"/",mo,"/",dy))) %>% dplyr::mutate(date2 = paste0(format(date1, '%m/%d/%Y'),' 12'))
		datFlowSeas = datFile %>%  dplyr::filter(month(date1)==4 | month(date1)==5 | month(date1)==6 | month(date1)==7 | month(date1)==8 | month(date1)==9) %>% select(-year,-mo,-dy,-hr,-date1,-date2)
		datFlowSeas2 = datFlowSeas %>%	dplyr::mutate_all(funs(. * 1.9835)) %>% summarise_all(funs(sum)) %>% mutate_all(funs(. /1000)) %>% mutate(InitDate = as.numeric(listDirs[iterDir,1]))
		outFile=bind_rows(outFile, datFlowSeas2)
	}

	outFile2 = outFile %>% dplyr::mutate(InitDate=as.Date(as.character(InitDate), "%Y%m%d")) %>% dplyr::mutate(date2 = paste0(format(InitDate, '%m/%d/%Y'),' 12')) %>% dplyr::mutate(lead = ifelse(month(InitDate)==1,2160,ifelse(month(InitDate)==2,1440,ifelse(month(InitDate)==3,720,ifelse(month(InitDate)==4,0,0)))))
	outFile3 = outFile2 %>% select(date2,lead,c00_e1:mean_e3)
	write.table(outFile3,file=paste0(dirEVS,'NCAR_flow_ensemble_forecasts_',nrcsNames[iterLoc],'.fcst'),row.names=FALSE,col.names=FALSE,quote=F,sep='\t')
}

#sum SPRAG and WILLOLOC forecasts to get Williamson River near Chiloquin Forecasts
outFile=data.table()
for(iterDir in 1:ctDirs){
	datWILLI = data.table()
	for(iterLoc in 1:ctLocs){
		datFile = fread(paste0(dirFCST,nrcsNames[iterLoc],"/",listDirs[iterDir,1],"/flow_cfs_ens.001.pp.",nrcsNames[iterLoc],".txt"))
		datFile = datFile %>% dplyr::mutate(date1 = as.Date(paste0(year,"/",mo,"/",dy))) # %>% dplyr::mutate(date2 = paste0(format(date1, '%m/%d/%Y'),' 12'))
		datWILLI = bind_rows(datWILLI,datFile) %>% select(-year,-mo,-dy,-hr)
	}
	datWILLI2 = datWILLI %>% group_by(date1) %>% summarise_all(sum, na.rm = TRUE)
	datFlowSeas = datWILLI2 %>%  dplyr::filter(month(date1)==4 | month(date1)==5 | month(date1)==6 | month(date1)==7 | month(date1)==8 | month(date1)==9) %>% select(-date1)
	datFlowSeas2 = datFlowSeas %>%	dplyr::mutate_all(funs(. * 1.9835)) %>% summarise_all(funs(sum)) %>% mutate_all(funs(. /1000)) %>% mutate(InitDate = as.numeric(listDirs[iterDir,1]))
	outFile=bind_rows(outFile, datFlowSeas2)
}
outFile2 = outFile %>% dplyr::mutate(InitDate=as.Date(as.character(InitDate), "%Y%m%d")) %>% dplyr::mutate(date2 = paste0(format(InitDate, '%m/%d/%Y'),' 12')) %>% dplyr::mutate(lead = ifelse(month(InitDate)==1,2160,ifelse(month(InitDate)==2,1440,ifelse(month(InitDate)==3,720,ifelse(month(InitDate)==4,0,0)))))
outFile3 = outFile2 %>% select(date2,lead,c00_e1:mean_e3)
write.table(outFile3,file=paste0(dirEVS,'NCAR_flow_ensemble_forecasts_WILLI.fcst'),row.names=FALSE,col.names=FALSE,quote=F,sep='\t')



#below need to format naturalized flow data used in NCAR model calibrations...

wyear = function(x, start_month=10) {
  x.wyr = ifelse(lubridate::month(x)>=start_month, lubridate::year(x)+1, lubridate::year(x))
  return(x.wyr)
}

setwd('C:/Users/MMcguire/Documents/GitHub/Klamath/')
dirInp = 'C:/Users/MMcguire/Documents/GitHub/Klamath/'
dirOup = 'C:/Users/MMcguire/Documents/GitHub/Klamath/'

lookup = fread('lib/cal_loc_lookup.csv')
ctFiles = nrow(lookup)

#clear, igerb, sprag, willi,williloc
startDates = c('2005-01-01','2005-01-01','2001-01-01','2001-01-01','2001-01-01')
endDates = c('2015-04-01','2015-04-01','2016-04-01','2016-04-01','2016-04-01')
startDates = as.Date(startDates)
endDates = as.Date(endDates)

# Observed Streamflow
for(iterFile in 3:ctFiles){
  ObsfilePath = paste0('data/NCAR/',lookup$Observed[iterFile])
  datTmp = fread(ObsfilePath)
  datTmp$date = as.Date(paste(datTmp$year,datTmp$mo,datTmp$dy,sep='-'), format = "%Y-%m-%d")
  datTmp = datTmp %>% mutate(flow_af = flow_cfs*1.9835)      # add flow_af column
  datTmp = datTmp %>% dplyr::mutate(Location = lookup$ID[iterFile])
  datTmp = datTmp %>% dplyr::mutate(Type = 'Observed')

	NatFlowSeas = datTmp %>% dplyr::filter(mo %in% c(4,5,6,7,8,9)) %>%	group_by(year) %>% dplyr::summarise(Value_obs=sum(flow_af)/1000)
	natflow = NatFlowSeas %>% left_join(datTmp) %>% filter(mo<=4) %>% filter(dy==1)
	natflow2 = natflow %>% dplyr::mutate(InitDate = as.Date(paste0(year,'-',mo,'-',01))) %>% dplyr::filter(InitDate >= startDates[iterFile]) %>% filter(InitDate <= endDates[iterFile])
	outFile = natflow2 %>% dplyr::mutate(date2 = paste0(format(as.Date(paste0(year,'-',mo,'-',01)), '%m/%d/%Y'),' 12')) %>% dplyr::select(date2, Value_obs)
	write.table(outFile,file=paste0(dirEVS,'NCAR_flow_obs_Apr-Sep_',lookup$ID[iterFile],'.obs'),row.names=FALSE,col.names=FALSE,quote=F,sep='\t')

}
