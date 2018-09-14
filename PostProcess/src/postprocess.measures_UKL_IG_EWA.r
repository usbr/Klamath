#===========================================================
# Name: Klamath SRO RiverWare Post-Processing
# Author: D. Broman, USBR Technical Service Center
# Last Modified: 2018-01-22
# Description: Reads in RiverWare data files from the Klamath
# River Basin model and calculates performance measures.
#===========================================================
# User Inputs:

#- Working Directory
#wd = 'C:/Projects/KlamathRiverBasin/PostProcessAW/'
wd = 'C:/Users/MMcguire/Documents/GitHub/Klamath/PostProcess/'
#-RiverWare Directory [Location of RiverWare data]
rwDir = 'C:/Projects/KlamathRiverBasin/RiverSmart/Scenario/'

#-Folder Header for Forecast Runs
fcstHdr = 'MRM,RW,Rls,FcstInp'

#-Folder Header for Historical Runs
histHdr = 'MRM,RW,Rls,HistInp'

# All models are run from Jan 1, with historical data appended from
# Jan 1 to start of forecast - forecast start times 1/1, 2/1, 3/1, 4/1, 5/1
dateList = c('2006-01-01', '2010-01-01')

#- Output Directory
dirOup = 'data/output/'

#===========================================================
#-postprocess_lib.r contains custom functions and required R packages
setwd(wd)
source('src/postprocess_lib.r')
dat_name_list = c('Run', 'Trace', 'RiverWareSlot', 'Time', 'Value', 'InputDMIName')

# List of Traces from 2006 Forecast Runs
trcTbl06 = fread('lib/trcTbl.csv')
trcTbl06 = trcTbl06 %>% mutate(Set = '2006-01-01')

# List of Traces from 2010 Forecast Runs
trcTbl10 = fread('lib/trcTbl2010.csv')
trcTbl10 = trcTbl10 %>% mutate(Set = '2010-01-01')

# Combine forecast trace tables
trcTbl = bind_rows(trcTbl06, trcTbl10)
trcTbl = trcTbl %>% mutate(InitDate = as.Date(InitDate))

# List of traces from 2006 historical runs
trcTbl06Hist = fread('lib/trcTblHist.csv')

# List of tracers from 2010 historical runs
trcTbl06Hist = trcTbl06Hist %>% mutate(Set = '2006-01-01')

trcTbl10Hist = fread('lib/trcTbl2010Hist.csv')
trcTbl10Hist = trcTbl10Hist %>% mutate(Set = '2010-01-01')

# Combine historical trace tables
trcTblHist = bind_rows(trcTbl06Hist, trcTbl10Hist)
trcTblHist = trcTblHist %>% mutate(InitDate = as.Date(InitDate))

#===========================================================
# Read in Data

ctDate = length(dateList)

datStrg = data.table()
datEWA = data.table()
datGage = data.table()
datIg = data.table()
datPjct = data.table()
datInfl = data.table()
datSply = data.table()
for(iterDate in 1:ctDate){
	dateSel  = dateList[iterDate]
	filePathTmp = paste0(rwDir, fcstHdr, ',', dateSel, '/')

	# Storage
	strgTmp = read.rdf(paste0(filePathTmp, 'Storage.rdf'))
	datStrgTmp = Rdf2dt(strgTmp)
	datStrgTmp = datStrgTmp %>% mutate(Set = dateSel)
	datStrg = datStrg %>% bind_rows(datStrgTmp)

	# EWA
	EWATmp = read.rdf(paste0(filePathTmp, 'EWA.rdf'))
	datEWATmp = Rdf2dt(EWATmp)
	datEWATmp = datEWATmp %>% mutate(Set = dateSel)
	datEWA = datEWA %>% bind_rows(datEWATmp)

	#GaugeFlow
	gageTmp = read.rdf(paste0(filePathTmp, 'GageFlow.rdf'))
	datGageTmp = Rdf2dt(gageTmp)
	datGageTmp = datGageTmp %>% mutate(Set = dateSel)
	datGage = datGage %>% bind_rows(datGageTmp)

	# Iron Gate
	igTmp = read.rdf(paste0(filePathTmp, 'IronGateReqs.rdf'))
	datIgTmp = Rdf2dt(igTmp)
	datIgTmp = datIgTmp %>% mutate(Set = dateSel)
	datIg = datIg %>% bind_rows(datIgTmp)

	# Project Supplies
	pjctTmp = read.rdf(paste0(filePathTmp, 'ProjectSupplies.rdf'))
	datPjctTmp = Rdf2dt(pjctTmp)
	datPjctTmp = datPjctTmp %>% mutate(Set = dateSel)
	datPjct = datPjct %>% bind_rows(datPjctTmp)

	# Inflow
	inflTmp = read.rdf(paste0(filePathTmp, 'Inflow.rdf'))
	datInflTmp = Rdf2dt(inflTmp)
	datInflTmp = datInflTmp %>% mutate(Set = dateSel)
	datInfl = datInfl %>% bind_rows(datInflTmp)

	# Supply (Forecasts)
	splyTmp = read.rdf(paste0(filePathTmp, 'Supply.rdf'))
	datSplyTmp =  Rdf2dt(splyTmp)
	datSplyTmp = datSplyTmp %>% mutate(Set = dateSel)
	datSply = datSply %>% bind_rows(datSplyTmp)

}

#===========================================================
# Read in Historical Data
ctDate = length(dateList)

datStrgHist = data.table()
datEWAHist = data.table()
datGageHist = data.table()
datIgHist = data.table()
datPjctHist = data.table()
datInflHist = data.table()
datSplyHist = data.table()
for(iterDate in 1:ctDate){
	dateSel = dateList[iterDate]
	filePathTmp = paste0(rwDir, histHdr, ',', dateSel, '/')

	# Storage
	strgTmp = read.rdf(paste0(filePathTmp, 'Storage.rdf'))
	datStrgTmp =  Rdf2dt(strgTmp)
	datStrgTmp = datStrgTmp %>% mutate(Set = dateSel)
	datStrgHist  = datStrgHist %>% bind_rows(datStrgTmp)

	# EWA
	EWATmp = read.rdf(paste0(filePathTmp, 'EWA.rdf'))
	datEWATmp = Rdf2dt(EWATmp)
	datEWATmp = datEWATmp %>% mutate(Set = dateSel)
	datEWAHist = datEWAHist  %>% bind_rows(datEWATmp)

	#GaugeFlow
	gageTmp = read.rdf(paste0(filePathTmp, 'GageFlow.rdf'))
	datGageTmp = Rdf2dt(gageTmp)
	datGageTmp = datGageTmp %>% mutate(Set = dateSel)
	datGageHist = datGageHist %>% bind_rows(datGageTmp)

	# Iron Gate
	igTmp = read.rdf(paste0(filePathTmp, 'IronGateReqs.rdf'))
	datIgTmp = Rdf2dt(igTmp)
	datIgTmp = datIgTmp %>% mutate(Set = dateSel)
	datIgHist = datIgHist %>% bind_rows(datIgTmp)

	# Project Supplies
	pjctTmp = read.rdf(paste0(filePathTmp, 'ProjectSupplies.rdf'))
	datPjctTmp = Rdf2dt(pjctTmp)
	datPjctTmp = datPjctTmp %>% mutate(Set = dateSel)
	datPjctHist = datPjctHist %>% bind_rows(datPjctTmp)

	# Inflow
	inflTmp = read.rdf(paste0(filePathTmp, 'Inflow.rdf'))
	datInflTmp = Rdf2dt(inflTmp)
	datInflTmp = datInflTmp %>% mutate(Set = dateSel)
	datInflHist = datInflHist %>% bind_rows(datInflTmp)

	# Supply (Forecasts)
	splyTmp = read.rdf(paste0(filePathTmp, 'Supply.rdf'))
	datSplyTmp = Rdf2dt(splyTmp)
	datSplyTmp = datSplyTmp %>% mutate(Set = dateSel)
	datSplyHist = datSplyHist %>% bind_rows(datSplyTmp)

}


#===========================================================
# Storage Plots
datStrgHist = datStrgHist %>% left_join(trcTblHist)
datStrgHist = datStrgHist %>% mutate(InitDate = as.Date(InitDate))

datStrgPlotHist = datStrgHist %>%
	filter(RiverWareSlot == 'Upper Klamath Lake.Storage') %>%
	mutate(Year = year(Date), Month = month(Date)) %>%
	mutate(Ensemble = paste0('Trace', Trace), InitPlt = format(InitDate, '%b %d')) %>%
	dplyr::rename(ValueHist = Value)

datStrgPlotHist$InitPlt = factor(datStrgPlotHist$InitPlt, levels = c('Jan 01', 'Feb 01', 'Mar 01', 'Apr 01', 'May 01'))


datStrg = datStrg %>% left_join(trcTbl)
datStrg = datStrg %>% mutate(InitDate = as.Date(InitDate))

datStrgPlot = datStrg %>%
	filter(RiverWareSlot == 'Upper Klamath Lake.Storage') %>%
	mutate(Year = year(Date), Month = month(Date)) %>%
	mutate(Ensemble = paste0('Trace', Trace), InitPlt = format(InitDate, '%b %d')) %>%
	mutate(FcstFlag = ifelse(Date >= InitDate, 'T', 'F'))

datStrgPlot$InitPlt = factor(datStrgPlot$InitPlt, levels = c('Jan 01', 'Feb 01', 'Mar 01', 'Apr 01', 'May 01'))

datStrgPlotFcst = datStrgPlot %>% filter(FcstFlag == 'T')
datStrgPlotObs = datStrgPlot %>% filter(FcstFlag == 'F', Trace %in% c(37,73,109,145))

ggplot() +
	geom_line(data = datStrgPlotHist, aes(x = Date, y = ValueHist / 1000), colour = 'black') +
	geom_line(data = datStrgPlotFcst, aes(x = Date, y = Value / 1000, group = Ensemble), size = 0.3, alpha = 0.8, colour = '#1874CD') +
	geom_line(data = datStrgPlotObs, aes(x = Date, y = Value / 1000, group = Ensemble), size = 0.5, colour = '#FF9B22') +

	facet_grid(InitPlt~Year, scales = 'free_x') +
	theme_bw(base_size = 16) +
	theme(strip.background = element_rect(fill = 'white'),
		plot.title = element_text(hjust = 0.5),
		legend.position = 'bottom',
		legend.title = element_blank()) +
		xlab('') +
		ylab('Storage (per 1000 AF)') +
	ggtitle('Upper Klamath Lake Storage') +
	scale_x_date(date_breaks = '2 month', date_labels = '%b')


ggsave('data/output/testUKLStorage.png', height = 10, width = 8)


# Iron Gate Target Releases Plot ==================================
datIgHist = datIgHist %>% left_join(trcTblHist)
datIgHist = datIgHist %>% mutate(InitDate = as.Date(InitDate))

datIg = datIg %>% left_join(trcTbl)
datIg = datIg %>% mutate(InitDate = as.Date(InitDate))

datIgPlotHist = datIgHist %>%
	filter(RiverWareSlot == 'ProjectData.ProjectReleaseForIronGateMin') %>%
	mutate(Year = year(Date), Month = month(Date)) %>%
	mutate(Ensemble = paste0('Trace', Trace), InitPlt = format(InitDate, '%b %d')) %>%
	dplyr::rename(ValueHist = Value)

datIgPlotHist$InitPlt = factor(datIgPlotHist$InitPlt, levels = c('Jan 01', 'Feb 01', 'Mar 01', 'Apr 01', 'May 01'))

datIgPlotHist = datIgPlotHist %>% filter(Month %in% 4:7)

datIgPlot = datIg %>%
	filter(RiverWareSlot == 'ProjectData.ProjectReleaseForIronGateMin') %>%
	mutate(Year = year(Date), Month = month(Date)) %>%
	mutate(Ensemble = paste0('Trace', Trace), InitPlt = format(InitDate, '%b %d')) %>%
	mutate(FcstFlag = ifelse(Date >= InitDate, 'T', 'F'))

datIgPlot$InitPlt = factor(datIgPlot$InitPlt, levels = c('Jan 01', 'Feb 01', 'Mar 01', 'Apr 01', 'May 01'))

datIgPlot = datIgPlot %>% filter(Month %in% 4:7)

datIgPlotFcst = datIgPlot %>% filter(FcstFlag == 'T')
datIgPlotObs = datIgPlot %>% filter(FcstFlag == 'F', Trace %in% c(37,73,109,145))


ggplot() +
	geom_line(data = datIgPlotFcst, aes(x = Date, y = Value, group = Ensemble), size = 0.3, alpha = 0.8, colour = '#1874CD') +
	geom_line(data = datIgPlotObs, aes(x = Date, y = Value, group = Ensemble), size = 0.5, colour = '#FF9B22') +
	geom_line(data = datIgPlotHist, aes(x = Date, y = ValueHist), colour = 'black') +

	facet_grid(InitPlt~Year, scales = 'free_x') +
	theme_bw() +
	theme(strip.background = element_rect(fill = 'white'),
		plot.title = element_text(hjust = 0.5),
		legend.position = 'bottom',
		legend.title = element_blank()) +
	xlab('') +
	ylab('Remaining Volume (AF)') +
	ggtitle('Iron Gate Dam Target Releases') +
	scale_x_date(date_breaks = '1 month', date_labels = '%b')

ggsave('data/output/IronGateTargetRelease.png', height = 10, width = 8)

# EWA Plot ==================================
datEWAHist = datEWAHist %>% left_join(trcTblHist)
datEWAHist = datEWAHist %>% mutate(InitDate = as.Date(InitDate))

datEWA = datEWA %>% left_join(trcTbl)
datEWA = datEWA %>% mutate(InitDate = as.Date(InitDate))

datEWAPlotHist = datEWAHist %>%
	filter(RiverWareSlot == 'ProjectData.EWARemain') %>%
	mutate(Year = year(Date), Month = month(Date)) %>%
	mutate(Ensemble = paste0('Trace', Trace), InitPlt = format(InitDate, '%b %d')) %>%
	dplyr::rename(ValueHist = Value)

datEWAPlotHist$InitPlt = factor(datEWAPlotHist$InitPlt, levels = c('Jan 01', 'Feb 01', 'Mar 01', 'Apr 01', 'May 01'))

datEWAPlotHist = datEWAPlotHist %>% filter(Month %in% 3:9)

datEWAPlot = datEWA %>%
	filter(RiverWareSlot == 'ProjectData.EWARemain') %>%
	mutate(Year = year(Date), Month = month(Date)) %>%
	mutate(Ensemble = paste0('Trace', Trace), InitPlt = format(InitDate, '%b %d')) %>%
	mutate(FcstFlag = ifelse(Date >= InitDate, 'T', 'F'))

datEWAPlot$InitPlt = factor(datEWAPlot$InitPlt, levels = c('Jan 01', 'Feb 01', 'Mar 01', 'Apr 01', 'May 01'))

datEWAPlot = datEWAPlot %>% filter(Month %in% 3:9)

datEWAPlotFcst = datEWAPlot %>% filter(FcstFlag == 'T')
datEWAPlotObs = datEWAPlot %>% filter(FcstFlag == 'F', Trace %in% c(37,73,109,145))


ggplot() +
	geom_line(data = datEWAPlotFcst, aes(x = Date, y = Value, group = Ensemble), size = 0.3, alpha = 0.8, colour = '#1874CD') +
	geom_line(data = datEWAPlotHist, aes(x = Date, y = ValueHist), colour = 'black') +
	geom_line(data = datEWAPlotObs, aes(x = Date, y = Value, group = Ensemble), size = 0.5, colour = '#FF9B22') +

	facet_grid(InitPlt~Year, scales = 'free_x') +
	theme_bw() +
	theme(strip.background = element_rect(fill = 'white'),
		plot.title = element_text(hjust = 0.5),
		legend.position = 'bottom',
		legend.title = element_blank()) +
	xlab('') +
	ylab('Remaining Volume (AF)') +
	ggtitle('Environmental Water Account') +
	scale_x_date(date_breaks = '1 month', date_labels = '%b')

ggsave('data/output/EnvironmentalWaterAccount.png', height = 10, width = 8)
