
##########################################################################################################
#
# This script reads various file formats and performs other utility operations, including
# some verification tasks.
#
##########################################################################################################
library(verification)



#- Working Directory
wd = 'C:/Users/MMcguire/Documents/GitHub/Klamath/PostProcess/'

#source("C:/Users/MMcguire/Documents/GitHub/Klamath/src/calcdf.R")
source("C:/Users/MMcguire/Documents/GitHub/Klamath/src/rpscore02.R")


dirObsNCAR = 'C:/Users/MMcguire/Documents/GitHub/Klamath/data/NCAR/'
obsFiles = c('WILLI_usgs_daily_cfs_11502500_txt_adj.csv','SPRAG_usgs_daily_cfs_11501000_txt_adj.csv')
obsNames = c('Will','Sprag')
#startDates = c('1950-01-01','1950-01-01')
#endDates = c('2016-04-01','2016-04-01')

leadtime = c(2160,1440,720,0)

dirFcstNCAR = 'C:/Projects/KlamathRiverBasin/EVS_5.6/Inputs/'
fcstFiles = c('NCAR_flow_ensemble_forecasts_WILLI.fcst','NCAR_flow_ensemble_forecasts_SPRAG.fcst')

ctFiles = length(obsFiles)
#obscdf = data.frame()

#loop on location
fin.rps=data.table()
for(iterFile in 1:ctFiles){
	ncarNatFlow = fread(paste0(dirObsNCAR,obsFiles[iterFile]))
	ncarNatFlowSeas = ncarNatFlow %>% dplyr::filter(mo %in% c(4,5,6,7,8,9)) %>%	group_by(year) %>% summarise(Value_obs=sum(flow_cfs)/1000) %>% mutate(Location = obsNames[iterFile])
#	tst = calcdf(ncarNatFlowSeas$Value_obs)
#	cdflookup = data.frame(X = tst$x, Y=tst$y)
#	ncarquant = as.numeric(quantile(ncarNatFlowSeas$Value_obs,probs=c(0,0.40,0.60,1.0)))

	#loop on lead time (Jan-Apr)
	rpscores = data.table()
	for(imon in 1:4){
  	fcst = read.table(paste0(dirFcstNCAR,fcstFiles[iterFile]))
		fcst2 =	fcst %>% filter(year(as.Date(as.character(fcst$V1),format="%m/%d/%Y"))>=2001)
		fcst3 = fcst2	%>% filter(year(as.Date(as.character(fcst2$V1),format="%m/%d/%Y"))<=2015) %>% dplyr::select(-V2)
		monfcst = fcst3 %>% filter(fcst3$V3==leadtime[imon]) %>% dplyr::select(-V1,-V3)

		ObsNCAR = ncarNatFlowSeas %>% filter(year>=2001 & year<=2015)

		#calc rps for each year of hindcast - 2001-1015
		yrrps = data.table()
		for(nyr in 1:length(ObsNCAR$year)){
			fcst = as.numeric(monfcst[nyr,])
			obsv = ObsNCAR$Value_obs[nyr]
			baseline = ncarNatFlowSeas$Value_obs

			tst = rpscore(fcst,obsv,baseline,3)
			yrrps=bind_rows(yrrps,tst)
		}
		yrrps = yrrps %>% dplyr::mutate(mon=rep(imon,length(ObsNCAR$year)))
		rpscores=bind_rows(rpscores,yrrps)
	}
	finrps = rpscores %>% group_by(mon) %>% dplyr::summarise(avgrps=mean(rps),avgrpsc=mean(rpsc))
	finrps = finrps %>%  dplyr::mutate(Location=obsNames[iterFile])
	fin.rps=bind_rows(fin.rps,finrps)

}



#	fcstyrs = length(janfcst[,1])
#  ensprob = data.table()
#	obsprob = data.table()
#	for(i in 1:fcstyrs){
#		ens = t(janfcst[i,])
#		z=approx(cdflookup$X,cdflookup$Y,ens,yleft=0,yright=1)
#		tmp = data.table(t(data.frame(z$y)))
#		ensprob = bind_rows(ensprob,tmp)

#		o=approx(cdflookup$X,cdflookup$Y,ObsNCAR$Value_obs[i],yleft=0,yright=1)
#		tmp = data.table(t(data.frame(o$y)))
#		obsprob = bind_rows(obsprob,tmp)

#	}
#  ensprob2 = t(ensprob)


#	enscateg <- apply(ensprob2, 1, function(X) ifelse(X<=0.4,0,ifelse(X>0.4 & X<=0.6,1,2)))
#  obscateg <- apply(obsprob, 1, function(X) ifelse(X<=0.4,0,ifelse(X>0.4 & X<=0.6,1,2)))

#	tst = data.table()
#	for (i in 1:36){
#		tst1 = obscateg %in% enscateg[,i]
#		tst2 = data.table(as.numeric(tst1))
#		tst = append(tst,tst2)
#	}

#	cutObs = cut(ObsNCAR$Value_obs,breaks=ncarquant,labels=c('low','med','high'))
