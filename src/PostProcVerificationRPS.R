
##########################################################################################################
#
# This script reads various file formats and performs other utility operations, including
# some verification tasks.
#
##########################################################################################################
library(verification)
library(data.table)
library(tidyverse)
library(MASS)

#- Working Directory
wd = 'C:/Users/MMcguire/Documents/GitHub/Klamath/PostProcess/'

#source("C:/Users/MMcguire/Documents/GitHub/Klamath/src/calcdf.R")
source("C:/Users/MMcguire/Documents/GitHub/Klamath/src/rpscore02.R")


dirObsNCAR = 'C:/Users/MMcguire/Documents/GitHub/Klamath/data/NCAR/'
obsFiles = c('WILLI_usgs_daily_cfs_11502500_txt_adj.csv','SPRAG_usgs_daily_cfs_11501000_txt_adj.csv')
obsNames = c('Will','Sprag')

leadtime = c(2160,1440,720,0)

dirFcstNCAR = 'C:/Projects/KlamathRiverBasin/EVS_5.6/Inputs/'
fcstFiles = c('NCAR_flow_ensemble_forecasts_WILLI.fcst','NCAR_flow_ensemble_forecasts_SPRAG.fcst')

ctFiles = length(obsFiles)
#loop on location
fin.rps=data.table()
for(iterFile in 1:ctFiles){
	ncarNatFlow = fread(paste0(dirObsNCAR,obsFiles[iterFile]))
	ncarNatFlow_af = ncarNatFlow %>% dplyr::mutate(flow_af = flow_cfs*1.9835)
	ncarNatFlowSeas = ncarNatFlow_af %>% dplyr::filter(mo %in% c(4,5,6,7,8,9)) %>%	group_by(year) %>% summarise(Value_obs=sum(flow_af)/1000) %>% mutate(Location = obsNames[iterFile])
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

RPSSncar = fin.rps %>% dplyr::mutate(rpss=1-(avgrps/avgrpsc))
write.csv(RPSSncar,file='C:/Users/MMcguire/Documents/GitHub/Klamath/PostProcess/RPSSncar.csv')


# calc RPS and RPSS using NRCS forecasts and following ensembles as is: 10%, 30%, 50%, 70%, 90%
dirFcst = 'C:/Projects/KlamathRiverBasin/EVS_5.6/Inputs/'
fcstFiles = c('EVS_flow_ensemble_forecasts_Will.fcst','EVS_flow_ensemble_forecasts_Sprag.fcst')
dirObs = 'C:/Projects/KlamathRiverBasin/EVS_5.6/Inputs/'
obsFiles = c('EVS_flow_obs_Apr-SepWill.obs','EVS_flow_obs_Apr-SepSprag.obs')

ctFiles = length(obsFiles)
#loop on location
fin.rps=data.table()
for(iterFile in 1:ctFiles){
	NatFlow = read.table(paste0(dirObs,obsFiles[iterFile]))
	NatFlow2 =	NatFlow %>% filter(month(as.Date(as.character(NatFlow$V1),format="%m/%d/%Y"))==1)
	NatFlow3 =	NatFlow2 %>% dplyr::mutate(year = year(as.Date(as.character(NatFlow2$V1),format="%m/%d/%Y"))) %>% select(year,V3)

	#loop on lead time (Jan-Apr)
	rpscores = data.table()
	for(imon in 1:4){
  	fcst = read.table(paste0(dirFcst,fcstFiles[iterFile]))
		fcst2 =	fcst %>% filter(year(as.Date(as.character(fcst$V1),format="%m/%d/%Y"))>=2001)
		fcst3 = fcst2	%>% filter(year(as.Date(as.character(fcst2$V1),format="%m/%d/%Y"))<=2015) %>% dplyr::select(-V2)
		monfcst = fcst3 %>% filter(fcst3$V3==leadtime[imon]) %>% dplyr::select(-V1,-V3)

		Obs = NatFlow3 %>% filter(year>=2001 & year<=2015)

		#calc rps for each year of hindcast - 2001-1015
		yrrps = data.table()
		for(nyr in 1:length(Obs$year)){
			fcst = as.numeric(monfcst[nyr,])
			obsv = Obs$V3[nyr]
			baseline = NatFlow3$V3

			tst = rpscore(fcst,obsv,baseline,3)
			yrrps=bind_rows(yrrps,tst)
		}
		yrrps = yrrps %>% dplyr::mutate(mon=rep(imon,length(Obs$year)))
		rpscores=bind_rows(rpscores,yrrps)
	}
	finrps = rpscores %>% group_by(mon) %>% dplyr::summarise(avgrps=mean(rps),avgrpsc=mean(rpsc))
	finrps = finrps %>%  dplyr::mutate(Location=obsNames[iterFile])
	fin.rps=bind_rows(fin.rps,finrps)

}

RPSSnrcs = fin.rps %>% dplyr::mutate(rpss=1-(avgrps/avgrpsc))
write.csv(RPSSnrcs,file='C:/Users/MMcguire/Documents/GitHub/Klamath/PostProcess/RPSSnrcs.csv')




# calc RPS and RPSS using NRCS forecasts and fitting a distribution and generating 36 ensemble members
dirFcst = 'C:/Projects/KlamathRiverBasin/EVS_5.6/Inputs/'
fcstFiles = c('EVS_flow_ensemble_forecasts_Will.fcst','EVS_flow_ensemble_forecasts_Sprag.fcst')
dirObs = 'C:/Projects/KlamathRiverBasin/EVS_5.6/Inputs/'
obsFiles = c('EVS_flow_obs_Apr-SepWill.obs','EVS_flow_obs_Apr-SepSprag.obs')

ctFiles = length(obsFiles)
#loop on location
fin.rps=data.table()
for(iterFile in 1:ctFiles){
	NatFlow = read.table(paste0(dirObs,obsFiles[iterFile]))
	NatFlow2 =	NatFlow %>% filter(month(as.Date(as.character(NatFlow$V1),format="%m/%d/%Y"))==1)
	NatFlow3 =	NatFlow2 %>% dplyr::mutate(year = year(as.Date(as.character(NatFlow2$V1),format="%m/%d/%Y"))) %>% select(year,V3)

	#loop on lead time (Jan-Apr)
	rpscores = data.table()
	for(imon in 1:4){
  	fcst = read.table(paste0(dirFcst,fcstFiles[iterFile]))
		fcst2 =	fcst %>% filter(year(as.Date(as.character(fcst$V1),format="%m/%d/%Y"))>=2001)
		fcst3 = fcst2	%>% filter(year(as.Date(as.character(fcst2$V1),format="%m/%d/%Y"))<=2015)
		fcst4 = fcst3 %>% filter(year(as.Date(as.character(fcst3$V1),format="%m/%d/%Y"))!=2012) %>% dplyr::select(-V2)
		monfcst = fcst4 %>% filter(fcst4$V3==leadtime[imon]) %>% dplyr::select(-V1,-V3)

		Obs = NatFlow3 %>% filter(year>=2001 & year<=2015) %>% filter(year!=2012)

		#generate nrcs ensembles for each year at lead time
		genfcst=matrix(0,nrow=14,ncol=36)
		pval = matrix(0,nrow=14,ncol=1)
		for(iyr in 1:length(Obs$year)){
			yfcst = as.numeric(monfcst[iyr,])
#			dist = fitdistr(yfcst,"gamma")
#			set.seed(123)
#			x=(rgamma(36, shape = dist$estimate[1], rate = dist$estimate[2]))
#			genfcst[iyr,]=x
#			pval[iyr]=ks.test(yfcst,x)[2]
#			dist = fitdistr(yfcst,"lognormal")
#			set.seed(123)
#			x=(rlnorm(36, meanlog = dist$estimate[1], sdlog = dist$estimate[2]))
#			genfcst[iyr,]=x
#			pval[iyr]=ks.test(yfcst,x)[2]
			dist = fitdistr(yfcst,"exponential")
			set.seed(123)
			x=(rexp(36, rate = dist$estimate[1]))
			genfcst[iyr,]=x
			pval[iyr]=ks.test(yfcst,x)[2]

		}

		#calc rps for each year of hindcast - 2001-1015
		yrrps = data.table()
		for(nyr in 1:length(Obs$year)){
			fcst = as.numeric(genfcst[nyr,])
			obsv = Obs$V3[nyr]
			baseline = NatFlow3$V3

			tst = rpscore(fcst,obsv,baseline,3)
			yrrps=bind_rows(yrrps,tst)
		}
		yrrps = yrrps %>% dplyr::mutate(mon=rep(imon,length(Obs$year)))
		rpscores=bind_rows(rpscores,yrrps)
	}
	finrps = rpscores %>% group_by(mon) %>% dplyr::summarise(avgrps=mean(rps),avgrpsc=mean(rpsc))
	finrps = finrps %>%  dplyr::mutate(Location=obsNames[iterFile])
	fin.rps=bind_rows(fin.rps,finrps)

}

RPSSnrcs = fin.rps %>% dplyr::mutate(rpss=1-(avgrps/avgrpsc))
write.csv(RPSSnrcs,file='C:/Users/MMcguire/Documents/GitHub/Klamath/PostProcess/RPSSnrcsENSgamma.csv')





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
