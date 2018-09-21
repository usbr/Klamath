library(verification)

##########################################################################################################
#
# This script reads various file formats and performs other utility operations, including
# some verification tasks.
#
##########################################################################################################


#- Working Directory
wd = 'C:/Users/MMcguire/Documents/GitHub/Klamath/PostProcess/'



dirObsNRCS = 'C:/Users/MMcguire/Documents/GitHub/Klamath/PostProcess/data/nrcs/'
obsFiles = c('WMSO3_nrcs_natflow.csv','SPRAG_nrcs_natflow.csv')
obsNames = c('Will','Sprag')
startDates = c('1924-01-01','1924-01-01')
endDates = c('2016-04-01','2016-04-01')

dirFcstNRCS = 'C:/Projects/KlamathRiverBasin/EVS_5.6/Inputs/'
fcstFiles = c('EVS_flow_ensemble_forecasts_Will.fcst','EVS_flow_ensemble_forecasts_Sprag.fcst')

### note that the following were run interactively - 2012 forecasts were missing for several lead times
ctFiles = length(obsFiles)
for(iterFile in 1:ctFiles){
	nrcsNatFlow = fread(paste0(dirObsNRCS,obsFiles[iterFile]))
	nrcsNatFlowSeas = nrcsNatFlow %>% dplyr::filter(Date %in% c('Apr','May','Jun','Jul','Aug','Sep')) %>%	group_by(Year) %>% summarise(Value_obs=sum(Value)/1000) %>% mutate(Location = obsNames[iterFile])
	natflow = nrcsNatFlowSeas %>% left_join(nrcsNatFlow) %>% mutate(mon=match(Date,month.abb)) %>% filter(mon<=4)
	natflow = natflow %>% dplyr::mutate(InitDate = as.Date(paste0(Year,'-',mon,'-',01))) %>% dplyr::filter(InitDate >= startDates[iterFile] & InitDate <= endDates[iterFile])
  tst = natflow %>% filter(mon==1)
	nrcsquant = as.numeric(quantile(tst$Value_obs,probs=c(0,0.40,0.60,1.0)))

  fcst = read.table(paste0(dirFcstNRCS,fcstFiles[iterFile]))
	fcst2 =	fcst %>% filter(year(as.Date(as.character(fcst$V1),format="%m/%d/%Y"))>=2001)
	fcst3 = fcst2	%>% filter(year(as.Date(as.character(fcst2$V1),format="%m/%d/%Y"))<=2015) %>% dplyr::select(-V2,-V3)

  janfcst = fcst3 %>% filter(month(as.Date(as.character(fcst3$V1),format="%m/%d/%Y"))==1)
	cut50 = cut(janfcst$V6,breaks=nrcsquant,labels=c('low','med','high'))
	fcstbins = data.frame(Jan50 = cut50)

	febfcst = fcst3 %>% filter(month(as.Date(as.character(fcst3$V1),format="%m/%d/%Y"))==2)
	cut50 = cut(febfcst$V6,breaks=nrcsquant,labels=c('low','med','high'))
	fcstbins$Feb50 = cut50

	marfcst = fcst3 %>% filter(month(as.Date(as.character(fcst3$V1),format="%m/%d/%Y"))==3)
	cut50 = cut(marfcst$V6,breaks=nrcsquant,labels=c('low','med','high'))
	fcstbins$Mar50 = cut50

	aprfcst = fcst3 %>% filter(month(as.Date(as.character(fcst3$V1),format="%m/%d/%Y"))==4)
	cut50 = cut(aprfcst$V6,breaks=nrcsquant,labels=c('low','med','high'))
	fcstbins$Apr50 = cut50

	ObsNRCS = tst %>% filter(Year>=2001 & Year<=2015)
	cutObs = cut(ObsNRCS$Value_obs,breaks=nrcsquant,labels=c('low','med','high'))
}




dirObsNCAR = 'C:/Users/MMcguire/Documents/GitHub/Klamath/data/NCAR/'
obsFiles = c('WILLI_usgs_daily_cfs_11502500_txt_adj.csv','SPRAG_usgs_daily_cfs_11501000_txt_adj.csv')
obsNames = c('Will','Sprag')
startDates = c('1950-01-01','1950-01-01')
endDates = c('2016-04-01','2016-04-01')

dirFcstNCAR = 'C:/Projects/KlamathRiverBasin/EVS_5.6/Inputs/'
fcstFiles = c('NCAR_flow_ensemble_forecasts_WILLI.fcst','NCAR_flow_ensemble_forecasts_SPRAG.fcst')

### note that the following were run interactively - 2012 forecasts were missing for several lead times
ctFiles = length(obsFiles)
for(iterFile in 1:ctFiles){
	ncarNatFlow = fread(paste0(dirObsNCAR,obsFiles[iterFile]))
	ncarNatFlowSeas = ncarNatFlow %>% dplyr::filter(mo %in% c(4,5,6,7,8,9)) %>%	group_by(year) %>% summarise(Value_obs=sum(flow_cfs)/1000) %>% mutate(Location = obsNames[iterFile])
	ncarquant = as.numeric(quantile(ncarNatFlowSeas$Value_obs,probs=c(0,0.40,0.60,1.0)))

  fcst = read.table(paste0(dirFcstNCAR,fcstFiles[iterFile]))
	fcst2 =	fcst %>% filter(year(as.Date(as.character(fcst$V1),format="%m/%d/%Y"))>=2001)
	fcst3 = fcst2	%>% filter(year(as.Date(as.character(fcst2$V1),format="%m/%d/%Y"))<=2015) %>% dplyr::select(-V2)

  janfcst = fcst3 %>% filter(fcst3$V3==2160)
	janfcst$Median <- apply(janfcst[,3:38], 1, median)
	cut50 = cut(janfcst$Median,breaks=ncarquant,labels=c('low','med','high'))

	febfcst = fcst3 %>% filter(fcst3$V3==1440)
	febfcst$Median <- apply(febfcst[,3:38], 1, median)
	cut50 = cut(febfcst$Median,breaks=ncarquant,labels=c('low','med','high'))

	marfcst = fcst3 %>% filter(fcst3$V3==720)
	marfcst$Median <- apply(marfcst[,3:38], 1, median)
	cut50 = cut(marfcst$Median,breaks=ncarquant,labels=c('low','med','high'))

	aprfcst = fcst3 %>% filter(fcst3$V3==0)
	aprfcst$Median <- apply(aprfcst[,3:38], 1, median)
	cut50 = cut(aprfcst$Median,breaks=ncarquant,labels=c('low','med','high'))

	ObsNCAR = ncarNatFlowSeas %>% filter(year>=2001 & year<=2015)
	cutObs = cut(ObsNCAR$Value_obs,breaks=ncarquant,labels=c('low','med','high'))
}
