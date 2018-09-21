#===========================================================
# Name: Klamath SRO RiverWare Post-Processing
# Author: D. Broman, USBR Technical Service Center
# Last Modified: 2018-08-02 by MMcGuire
# Description: Reads in RiverWare data files from the Klamath
# River Basin model and calculates performance measures.
#===========================================================
# User Inputs:

#- Working Directory
wd = 'C:/Users/MMcguire/Documents/GitHub/Klamath/PostProcess/'

#-Scenario Directory [Location of RiverWare data]
scenDir = 'C:/Projects/KlamathRiverBasin/RiverSmart/Scenario/'

#-Folder Header for Forecast Scenario Runs
scenHdr = 'MRM,RW,Rls,FcstInp'

#-Folder Header for Historical Runs
histHdr = 'MRM,RW,Rls,HistInp'
histDir = 'C:/Projects/KlamathRiverBasin/RiverSmart/Scenario/Historical/trace1'
# All models are run from Jan 1, with historical data appended from
# Jan 1 to start of forecast - forecast start times 1/1, 2/1, 3/1, 4/1, 5/1
dateList = c('2006-01-01', '2010-01-01')

#- Output Directory
dirOup = 'data/output/'

#===========================================================
#-postprocess_lib.r contains custom functions and required R packages
setwd(wd)
source('src/fncnLib.r')
dat_name_list = c('Run', 'Trace', 'RiverWareSlot', 'Time', 'Value', 'InputDMIName')

# List of Traces from 2006 Forecast Runs
trcTbl06 = fread('C:/Projects/KlamathRiverBasin/RiverSmart/Control/trcTbl.csv')
trcTbl06 = trcTbl06 %>% mutate(Set = '2006-01-01')

# List of Traces from 2010 Forecast Runs
trcTbl10 = fread('C:/Projects/KlamathRiverBasin/RiverSmart/Control/trcTbl2010.csv')
trcTbl10 = trcTbl10 %>% mutate(Set = '2010-01-01')

# Combine forecast trace tables
trcTbl = bind_rows(trcTbl06, trcTbl10)
trcTbl = trcTbl %>% mutate(InitDate = as.Date(InitDate))

# List of traces from 2006 historical runs
trcTbl06Hist = fread('C:/Projects/KlamathRiverBasin/RiverSmart/Control/trcTblHist.csv')

# List of tracers from 2010 historical runs
trcTbl06Hist = trcTbl06Hist %>% mutate(Set = '2006-01-01')

trcTbl10Hist = fread('C:/Projects/KlamathRiverBasin/RiverSmart/Control/trcTbl2010Hist.csv')
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
	filePathTmp = paste0(scenDir, scenHdr, ',', dateSel, '/')

	# Storage
	strgTmp = read.rdf(paste0(filePathTmp, 'Storage.rdf'))
	datStrgTmp =  Rdf2dt(strgTmp)
	datStrgTmp = datStrgTmp %>% mutate(Set = dateSel)
	datStrg = datStrg %>% bind_rows(datStrgTmp)

	# EWA
	EWATmp = read.rdf(paste0(filePathTmp, 'EWA.rdf'))
	datEWATmp =  Rdf2dt(EWATmp)
	datEWATmp = datEWATmp %>% mutate(Set = dateSel)
	datEWA = datEWA %>% bind_rows(datEWATmp)

	#GaugeFlow
	gageTmp = read.rdf(paste0(filePathTmp, 'GageFlow.rdf'))
	datGageTmp =  Rdf2dt(gageTmp)
	datGageTmp = datGageTmp %>% mutate(Set = dateSel)
	datGage = datGage %>% bind_rows(datGageTmp)

	# Iron Gate
	igTmp = read.rdf(paste0(filePathTmp, 'IronGateReqs.rdf'))
	datIgTmp =  Rdf2dt(igTmp)
	datIgTmp = datIgTmp %>% mutate(Set = dateSel)
	datIg = datIg %>% bind_rows(datIgTmp)

	# Project Supplies
	pjctTmp = read.rdf(paste0(filePathTmp, 'ProjectSupplies.rdf'))
	datPjctTmp =  Rdf2dt(pjctTmp)
	datPjctTmp = datPjctTmp %>% mutate(Set = dateSel)
	datPjct = datPjct %>% bind_rows(datPjctTmp)

	# Inflow
	inflTmp = read.rdf(paste0(filePathTmp, 'Inflow.rdf'))
	datInflTmp =  Rdf2dt(inflTmp)
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
datEWAHist  = data.table()
datGageHist  = data.table()
datIgHist  = data.table()
datPjctHist  = data.table()
datInflHist  = data.table()
datSplyHist  = data.table()
for(iterDate in 1:ctDate){
	dateSel  = dateList[iterDate]
	filePathTmp = paste0(scenDir, histHdr, ',', dateSel, '/')

	# Storage
	strgTmp = read.rdf(paste0(filePathTmp, 'Storage.rdf'))
	datStrgTmp =  Rdf2dt(strgTmp)
	datStrgTmp = datStrgTmp %>% mutate(Set = dateSel)
	datStrgHist  = datStrgHist %>% bind_rows(datStrgTmp)

	# EWA
	EWATmp = read.rdf(paste0(filePathTmp, 'EWA.rdf'))
	datEWATmp =  Rdf2dt(EWATmp)
	datEWATmp = datEWATmp %>% mutate(Set = dateSel)
	datEWAHist  = datEWAHist  %>% bind_rows(datEWATmp)

	#GaugeFlow
	gageTmp = read.rdf(paste0(filePathTmp, 'GageFlow.rdf'))
	datGageTmp =  Rdf2dt(gageTmp)
	datGageTmp = datGageTmp %>% mutate(Set = dateSel)
	datGageHist = datGageHist %>% bind_rows(datGageTmp)

	# Iron Gate
	igTmp = read.rdf(paste0(filePathTmp, 'IronGateReqs.rdf'))
	datIgTmp =  Rdf2dt(igTmp)
	datIgTmp = datIgTmp %>% mutate(Set = dateSel)
	datIgHist = datIgHist %>% bind_rows(datIgTmp)

	# Project Supplies
	pjctTmp = read.rdf(paste0(filePathTmp, 'ProjectSupplies.rdf'))
	datPjctTmp =  Rdf2dt(pjctTmp)
	datPjctTmp = datPjctTmp %>% mutate(Set = dateSel)
	datPjctHist = datPjctHist %>% bind_rows(datPjctTmp)

	# Inflow
	inflTmp = read.rdf(paste0(filePathTmp, 'Inflow.rdf'))
	datInflTmp =  Rdf2dt(inflTmp)
	datInflTmp = datInflTmp %>% mutate(Set = dateSel)
	datInflHist = datInflHist %>% bind_rows(datInflTmp)

	# Supply (Forecasts)
	splyTmp = read.rdf(paste0(filePathTmp, 'Supply.rdf'))
	datSplyTmp =  Rdf2dt(splyTmp)
	datSplyTmp = datSplyTmp %>% mutate(Set = dateSel)
	datSplyHist = datSplyHist %>% bind_rows(datSplyTmp)

}


#===========================================================
# Inflows
datInflHist = datInflHist %>% left_join(trcTblHist)
datInflHist = datInflHist %>% mutate(InitDate = as.Date(InitDate))

datInfl = datInfl %>% left_join(trcTbl)
datInfl = datInfl %>% mutate(InitDate = as.Date(InitDate))

# Williamson River Inflow Plot
datInflPlotHist = datInflHist %>%
	filter(RiverWareSlot == 'Williamson River.Inflow') %>%
	mutate(Year = year(Date), Month = month(Date)) %>%
	mutate(Ensemble = paste0('Trace', Trace), InitPlt = format(InitDate, '%b %d')) %>%
	dplyr::rename(ValueHist = Value)

datInflPlotHist$InitPlt = factor(datInflPlotHist$InitPlt, levels = c('Jan 01', 'Feb 01', 'Mar 01', 'Apr 01', 'May 01'))


datInflPlot = datInfl %>%
	filter(RiverWareSlot == 'Williamson River.Inflow') %>%
	mutate(Year = year(Date), Month = month(Date)) %>%
	mutate(Ensemble = paste0('Trace', Trace), InitPlt = format(InitDate, '%b %d')) %>%
	mutate(FcstFlag = ifelse(Date >= InitDate, 'T', 'F'))

datInflPlot$InitPlt = factor(datInflPlot$InitPlt, levels = c('Jan 01', 'Feb 01', 'Mar 01', 'Apr 01', 'May 01'))

datInflPlotFcst = datInflPlot %>% filter(FcstFlag == 'T')
datInflPlotObs = datInflPlot %>% filter(FcstFlag == 'F')

ggplot() +
	geom_line(data = datInflPlotFcst, aes(x = Date, y = Value, group = Ensemble), size = 0.3, alpha = 0.8, colour = '#1874CD') +
	geom_line(data = datInflPlotObs, aes(x = Date, y = Value, group = Ensemble), size = 0.5, colour = 'black') +
	geom_line(data = datInflPlotHist, aes(x = Date, y = ValueHist), colour = 'black') +
	facet_grid(InitPlt~Year, scales = 'free_x') +
	theme_bw(base_size = 16) +
	theme(strip.background = element_rect(fill = 'white'),
		plot.title = element_text(hjust = 0.5),
		legend.position = 'bottom',
		legend.title = element_blank()) +
	xlab('') +
	ylab('Streamflow (cfs)') +
	ggtitle('Williamson River Inflow') +
	scale_x_date(date_breaks = '1 month', date_labels = '%b')

ggsave('data/output/WilliamsonInflowTS.png', height = 10, width = 10)


# Williamson Short-Term Inflow
datInflShrt = datInflPlotFcst %>% filter(Date <= (InitDate + days(7)))
datInflHistShrt = datInflPlotHist %>% filter(Date >= (InitDate - days(3)), Date <= (InitDate + days(7)))

datInflShrt06 = datInflShrt %>% mutate(datenum = as.numeric(Date - InitDate) + 1)
datInflHistShrt06 = datInflHistShrt %>% mutate(datenum = as.numeric(Date - InitDate) + 1)

ggplot() +
	geom_line(data = datInflShrt06, aes(x = datenum, y = Value, group = Ensemble), size = 0.3, alpha = 0.8, colour = '#1874CD') +
	geom_line(data = datInflHistShrt06, aes(x = datenum, y = ValueHist), colour = 'black') +
	facet_grid(InitPlt~Year, scales = 'free') +
	theme_bw(base_size = 16) +
	theme(strip.background = element_rect(fill = 'white')) + xlab('') + ylab('Streamflow (cfs)') +
	ggtitle('Williamson River Inflow') +
	scale_x_continuous(breaks = 1:7, labels = 1:7, limits = c(1,7))

	ggsave('data/output/WilliamsonInflowTSShortTerm.png', height = 10, width = 10)


	# Sprague River Inflow Plot
	datInflPlotHist = datInflHist %>%
		filter(RiverWareSlot == 'Sprague River.Inflow') %>%
		mutate(Year = year(Date), Month = month(Date)) %>%
		mutate(Ensemble = paste0('Trace', Trace), InitPlt = format(InitDate, '%b %d')) %>%
		dplyr::rename(ValueHist = Value)

	datInflPlotHist$InitPlt = factor(datInflPlotHist$InitPlt, levels = c('Jan 01', 'Feb 01', 'Mar 01', 'Apr 01', 'May 01'))


	datInflPlot = datInfl %>%
		filter(RiverWareSlot == 'Sprague River.Inflow') %>%
		mutate(Year = year(Date), Month = month(Date)) %>%
		mutate(Ensemble = paste0('Trace', Trace), InitPlt = format(InitDate, '%b %d')) %>%
		mutate(FcstFlag = ifelse(Date >= InitDate, 'T', 'F'))

	datInflPlot$InitPlt = factor(datInflPlot$InitPlt, levels = c('Jan 01', 'Feb 01', 'Mar 01', 'Apr 01', 'May 01'))

	datInflPlotFcst = datInflPlot %>% filter(FcstFlag == 'T')
	datInflPlotObs = datInflPlot %>% filter(FcstFlag == 'F')

	ggplot() +
		geom_line(data = datInflPlotFcst, aes(x = Date, y = Value, group = Ensemble), size = 0.3, alpha = 0.8, colour = '#1874CD') +
		geom_line(data = datInflPlotObs, aes(x = Date, y = Value, group = Ensemble), size = 0.5, colour = 'black') +
		geom_line(data = datInflPlotHist, aes(x = Date, y = ValueHist), colour = 'black') +
		facet_grid(InitPlt~Year, scales = 'free_x') +
		theme_bw(base_size = 16) +
		theme(strip.background = element_rect(fill = 'white'),
			plot.title = element_text(hjust = 0.5),
			legend.position = 'bottom',
			legend.title = element_blank()) +
		xlab('') +
		ylab('Streamflow (cfs)') +
		ggtitle('Sprague River Inflow') +
		scale_x_date(date_breaks = '1 month', date_labels = '%b')

	ggsave('data/output/SpragueInflowTS.png', height = 10, width = 10)

#===========================================================
# Seasonal Forecasts compared to NRCS
nrcsFcst = fread('data/nrcs/nrcsWillFcst.csv')
nrcsFcstPlot = nrcsFcst %>%
	mutate(InitDate = as.Date(paste(Issue_year, Issue_month, '01', sep = '-'))) %>%
	dplyr::rename(Year = Issue_year) %>%
	dplyr::select(InitDate, Year, Fcst_min_vol, Fcst_low_vol, Fcst_mid_vol, Fcst_upp_vol, Fcst_max_vol) %>%
	setNames(c('InitDate', 'Year', paste0('Fcst', c(10, 30, 50, 70, 90)))) %>%
	gather(FcstLvl, Value, -InitDate, -Year) %>% filter(Year == 2006 | Year == 2010)

lvlTbl = data.table(FcstLvl = paste0('Fcst', c(10, 30, 50, 70, 90)), FcstLvlName = paste0(c(10, 30, 50, 70, 90), 'th Percent'))
nrcsFcstPlot = nrcsFcstPlot %>% left_join(lvlTbl)

datInflWillSeas = datInfl %>%
	filter(RiverWareSlot %in% c('Williamson River.Inflow', 'Sprague River.Inflow')) %>%
	left_join(trcTbl) %>%
	group_by(Trace, Date, InitDate, Ensemble) %>%
	dplyr::summarise(Value = sum(Value)) %>%
	mutate(Month = month(Date), MonthInit = month(InitDate)) %>%
	filter(Month >= MonthInit, Month <= 9, Month >= 4) %>%
	mutate(Value = Value * 1.98347) %>%

	group_by(InitDate, Ensemble) %>%
	dplyr::summarise(Value = sum(Value) / 1000) %>%
	mutate(Year = year(InitDate))

datInflHistWillSeas = datInflHist %>%
	filter(RiverWareSlot %in% c('Williamson River.Inflow', 'Sprague River.Inflow')) %>%
	left_join(trcTblHist) %>%
	group_by(Trace, Date, InitDate, Ensemble) %>%
	dplyr::summarise(Value = sum(Value)) %>%
	mutate(Month = month(Date), MonthInit = month(InitDate)) %>%
	filter(Month >= MonthInit, Month <= 9, Month >= 4) %>%
	mutate(Value = Value * 1.98347) %>%

	group_by(InitDate, Ensemble) %>%
	dplyr::summarise(Value = sum(Value) / 1000) %>%
	mutate(Year = year(InitDate))

ncarFcstPlot = datInflWillSeas %>% group_by(InitDate, Year) %>% dplyr::summarise(ValueMax = max(Value), ValueMin = min(Value), Value50 = quantile(Value, 0.5), Value10 = quantile(Value, 0.1), Value90 = quantile(Value, 0.9)) %>% mutate(Set = 'NCAR')

nrcsFcstPlot = nrcsFcst %>%
	mutate(InitDate = as.Date(paste(Issue_year, Issue_month, '01', sep = '-'))) %>%
	dplyr::rename(Year = Issue_year) %>%
	dplyr::select(InitDate, Year, Fcst_min_vol, Fcst_mid_vol, Fcst_max_vol) %>%
	setNames(c('InitDate', 'Year', paste0('Value', c(10, 50, 90)))) %>%
	mutate(ValueMin = Value10, ValueMax = Value90) %>% mutate(Set = 'NRCS')

willSeasFcst = bind_rows(ncarFcstPlot, nrcsFcstPlot)
willSeasHist = bind_rows(mutate(datInflHistWillSeas, Set = 'NRCS'), mutate(datInflHistWillSeas, Set = 'NCAR'))


willSeasFcst = willSeasFcst %>% mutate(Month = month(InitDate)) %>% dplyr::filter(Month <= 4)
willSeasHist = willSeasHist %>% mutate(Month = month(InitDate)) %>% dplyr::filter(Month <= 4)
willSeas2006 = willSeasHist %>% mutate(Month = month(InitDate)) %>% dplyr::filter(Month <= 4) %>% dplyr::filter(Year <= 2006)

tst = bind_rows(data.table(Month=-Inf,Value=605),data.table(Month=Inf,Value=605))
tst$Set='NRCS'
tst$Year=2006
tst2 = tst %>% left_join(willSeas2006)
tst = bind_rows(willSeas2006,tst2)
willSeas2006 = tst

willSeas2010 = willSeasHist %>% mutate(Month = month(InitDate)) %>% dplyr::filter(Month <= 4) %>% dplyr::filter(Year <= 2010)
tst = bind_rows(data.table(Month=-Inf,Value=316),data.table(Month=Inf,Value=316))
tst$Set='NRCS'
tst$Year=2010
tst2 = tst %>% left_join(willSeas2010)
tst = bind_rows(willSeas2010,tst2)
willSeas2010 = tst


ggplot() +
	geom_linerange(data = willSeasFcst, aes(x = Month, ymin = ValueMin, ymax = ValueMax, colour = Set), position = position_dodge(width = 0.5), alpha = 0.8, size = 3) +
	geom_point(data = willSeasFcst, aes(x = Month, y = Value50, group = Set), position = position_dodge(width = 0.5), alpha = 0.8, size = 10, shape = '-') +
	geom_point(data = willSeasFcst, aes(x = Month, y = Value10, group = Set), position = position_dodge(width = 0.5), alpha = 0.8, size = 10, shape = '-') +
	geom_point(data = willSeasFcst, aes(x = Month, y = Value90, group = Set), position = position_dodge(width = 0.5), alpha = 0.8, size = 10, shape = '-') +

	geom_text(data = willSeasFcst, aes(x = Month, y = Value50, group = Set, label = '    50th'), position = position_dodge(width = 0.5), alpha = 0.8, size = 2, hjust = 0) +
	geom_text(data = willSeasFcst, aes(x = Month, y = Value90, group = Set, label = '    90th'), position = position_dodge(width = 0.5), alpha = 0.8, size = 2, hjust = 0) +
	geom_text(data = willSeasFcst, aes(x = Month, y = Value10, group = Set, label = '    10th'), position = position_dodge(width = 0.5), alpha = 0.8, size = 2, hjust = 0) +
#	geom_point(data = willSeasHist, aes(x = InitDate, y = Value, group = Set), fill = '#FF8C00', shape = 21, size = 2.5, position = position_dodge(width = 25), alpha = 0.7) +
	geom_line(data = willSeas2006, aes(x = Month, y = Value, group = Set), linetype = 2, size = 0.75, colour = '#FF8C00') +
	geom_line(data = willSeas2010, aes(x = Month, y = Value, group = Set), linetype = 2, size = 0.75, colour = '#FF8C00') +
	facet_wrap(~Year, nrow = 1, scales = 'free_x') +
#	scale_x_date(date_breaks = '1 month', date_labels = '%b') +
	scale_x_discrete(limits=c("Jan","Feb","Mar", "Apr")) +
	xlab('') +
	ylab('April - September Volume (per 1000 AF)') +
	scale_color_manual(values = c('#1874CD', 'gray40')) +
	theme_bw() +
	theme(strip.background = element_rect(fill = 'white'),
		plot.title = element_text(hjust = 0.5),
		legend.position = 'bottom', legend.title = element_blank()) +
	ggtitle('Williamson River near Chiloquin\n Water Supply Forecast')

ggsave('data/output/WilliamsonWSF-Bar_V2.png', height = 6, width = 8)



#===========================================================
# Seasonal Forecasts compared to NRCS
nrcsFcst = fread('data/nrcs/nrcsSpragFcst.csv')
nrcsFcstPlot = nrcsFcst %>%
	mutate(InitDate = as.Date(paste(Issue_year, Issue_month, '01', sep = '-'))) %>%
	dplyr::rename(Year = Issue_year) %>%
	dplyr::select(InitDate, Year, Fcst_min_vol, Fcst_low_vol, Fcst_mid_vol, Fcst_upp_vol, Fcst_max_vol) %>%
	setNames(c('InitDate', 'Year', paste0('Fcst', c(10, 30, 50, 70, 90)))) %>%
	gather(FcstLvl, Value, -InitDate, -Year) %>% filter(Year == 2006 | Year == 2010)

lvlTbl = data.table(FcstLvl = paste0('Fcst', c(10, 30, 50, 70, 90)), FcstLvlName = paste0(c(10, 30, 50, 70, 90), 'th Percent'))
nrcsFcstPlot = nrcsFcstPlot %>% left_join(lvlTbl)

datInflSpragSeas = datInfl %>%
	filter(RiverWareSlot %in% c('Sprague River.Inflow')) %>%
	left_join(trcTbl) %>%
	group_by(Trace, Date, InitDate, Ensemble) %>%
	dplyr::summarise(Value = sum(Value)) %>%
	mutate(Month = month(Date), MonthInit = month(InitDate)) %>%
	filter(Month >= MonthInit, Month <= 9, Month >= 4) %>%
	mutate(Value = Value * 1.98347) %>%

	group_by(InitDate, Ensemble) %>%
	dplyr::summarise(Value = sum(Value) / 1000) %>%
	mutate(Year = year(InitDate))

datInflHistSpragSeas = datInflHist %>%
	filter(RiverWareSlot %in% c('Sprague River.Inflow')) %>%
	left_join(trcTblHist) %>%
	group_by(Trace, Date, InitDate, Ensemble) %>%
	dplyr::summarise(Value = sum(Value)) %>%
	mutate(Month = month(Date), MonthInit = month(InitDate)) %>%
	filter(Month >= MonthInit, Month <= 9, Month >= 4) %>%
	mutate(Value = Value * 1.98347) %>%

	group_by(InitDate, Ensemble) %>%
	dplyr::summarise(Value = sum(Value) / 1000) %>%
	mutate(Year = year(InitDate))

ncarFcstPlot = datInflSpragSeas %>% group_by(InitDate, Year) %>% dplyr::summarise(ValueMax = max(Value), ValueMin = min(Value), Value50 = quantile(Value, 0.5), Value10 = quantile(Value, 0.1), Value90 = quantile(Value, 0.9)) %>% mutate(Set = 'NCAR')

nrcsFcstPlot = nrcsFcst %>%
	mutate(InitDate = as.Date(paste(Issue_year, Issue_month, '01', sep = '-'))) %>%
	dplyr::rename(Year = Issue_year) %>%
	dplyr::select(InitDate, Year, Fcst_min_vol, Fcst_mid_vol, Fcst_max_vol) %>%
	setNames(c('InitDate', 'Year', paste0('Value', c(10, 50, 90)))) %>%
	mutate(ValueMin = Value10, ValueMax = Value90) %>% mutate(Set = 'NRCS')

spragSeasFcst = bind_rows(ncarFcstPlot, nrcsFcstPlot)
spragSeasHist = bind_rows(mutate(datInflHistSpragSeas, Set = 'NRCS'), mutate(datInflHistSpragSeas, Set = 'NCAR'))


spragSeasFcst = spragSeasFcst %>% mutate(Month = month(InitDate)) %>% dplyr::filter(Month <= 4)
spragSeasHist = spragSeasHist %>% mutate(Month = month(InitDate)) %>% dplyr::filter(Month <= 4)
spragSeas2006 = spragSeasHist %>% mutate(Month = month(InitDate)) %>% dplyr::filter(Month <= 4) %>% dplyr::filter(Year <= 2006)

tst = bind_rows(data.table(Month=-Inf,Value=426),data.table(Month=Inf,Value=426))
tst$Set='NRCS'
tst$Year=2006
tst2 = tst %>% left_join(spragSeas2006)
tst = bind_rows(spragSeas2006,tst2)
spragSeas2006 = tst

spragSeas2010 = spragSeasHist %>% mutate(Month = month(InitDate)) %>% dplyr::filter(Month <= 4) %>% dplyr::filter(Year <= 2010)
tst = bind_rows(data.table(Month=-Inf,Value=179),data.table(Month=Inf,Value=179))
tst$Set='NRCS'
tst$Year=2010
tst2 = tst %>% left_join(spragSeas2010)
tst = bind_rows(spragSeas2010,tst2)
spragSeas2010 = tst


ggplot() +
	geom_linerange(data = spragSeasFcst, aes(x = Month, ymin = ValueMin, ymax = ValueMax, colour = Set), position = position_dodge(width = 0.5), alpha = 0.8, size = 3) +
	geom_point(data = spragSeasFcst, aes(x = Month, y = Value50, group = Set), position = position_dodge(width = 0.5), alpha = 0.8, size = 10, shape = '-') +
	geom_point(data = spragSeasFcst, aes(x = Month, y = Value10, group = Set), position = position_dodge(width = 0.5), alpha = 0.8, size = 10, shape = '-') +
	geom_point(data = spragSeasFcst, aes(x = Month, y = Value90, group = Set), position = position_dodge(width = 0.5), alpha = 0.8, size = 10, shape = '-') +

	geom_text(data = spragSeasFcst, aes(x = Month, y = Value50, group = Set, label = '    50th'), position = position_dodge(width = 0.5), alpha = 0.8, size = 2, hjust = 0) +
	geom_text(data = spragSeasFcst, aes(x = Month, y = Value90, group = Set, label = '    90th'), position = position_dodge(width = 0.5), alpha = 0.8, size = 2, hjust = 0) +
	geom_text(data = spragSeasFcst, aes(x = Month, y = Value10, group = Set, label = '    10th'), position = position_dodge(width = 0.5), alpha = 0.8, size = 2, hjust = 0) +
#	geom_point(data = spragSeasHist, aes(x = InitDate, y = Value, group = Set), fill = '#FF8C00', shape = 21, size = 2.5, position = position_dodge(width = 25), alpha = 0.7) +
	geom_line(data = spragSeas2006, aes(x = Month, y = Value, group = Set), linetype = 2, size = 0.75, colour = '#FF8C00') +
	geom_line(data = spragSeas2010, aes(x = Month, y = Value, group = Set), linetype = 2, size = 0.75, colour = '#FF8C00') +
	facet_wrap(~Year, nrow = 1, scales = 'free_x') +
#	scale_x_date(date_breaks = '1 month', date_labels = '%b') +
	scale_x_discrete(limits=c("Jan","Feb","Mar", "Apr")) +
	xlab('') +
	ylab('April - September Volume (per 1000 AF)') +
	scale_color_manual(values = c('#1874CD', 'gray40')) +
	theme_bw() +
	theme(strip.background = element_rect(fill = 'white'),
		plot.title = element_text(hjust = 0.5),
		legend.position = 'bottom', legend.title = element_blank()) +
	ggtitle('Sprague River near Chiloquin\n Water Supply Forecast')

ggsave('data/output/SpragueWSF-Bar_V2.png', height = 6, width = 8)



#===========================================================
# Seasonal Forecasts compared to NRCS
nrcsFcst = fread('data/nrcs/nrcsGerberFcst.csv')
nrcsFcstPlot = nrcsFcst %>%
	mutate(InitDate = as.Date(paste(Issue_year, Issue_month, '01', sep = '-'))) %>%
	dplyr::rename(Year = Issue_year) %>%
	dplyr::select(InitDate, Year, Fcst_min_vol, Fcst_low_vol, Fcst_mid_vol, Fcst_upp_vol, Fcst_max_vol) %>%
	setNames(c('InitDate', 'Year', paste0('Fcst', c(10, 30, 50, 70, 90)))) %>%
	gather(FcstLvl, Value, -InitDate, -Year) %>% filter(Year == 2006 | Year == 2010)

lvlTbl = data.table(FcstLvl = paste0('Fcst', c(10, 30, 50, 70, 90)), FcstLvlName = paste0(c(10, 30, 50, 70, 90), 'th Percent'))
nrcsFcstPlot = nrcsFcstPlot %>% left_join(lvlTbl)

datInflGerberSeas = datInfl %>%
	filter(RiverWareSlot %in% c('Gerber Reservoir.Inflow')) %>%
	left_join(trcTbl) %>%
	group_by(Trace, Date, InitDate, Ensemble) %>%
	dplyr::summarise(Value = sum(Value)) %>%
	mutate(Month = month(Date), MonthInit = month(InitDate)) %>%
	filter(Month >= MonthInit, Month <= 9, Month >= 4) %>%
	mutate(Value = Value * 1.98347) %>%

	group_by(InitDate, Ensemble) %>%
	dplyr::summarise(Value = sum(Value) / 1000) %>%
	mutate(Year = year(InitDate))

datInflHistGerberSeas = datInflHist %>%
	filter(RiverWareSlot %in% c('Gerber Reservoir.Inflow')) %>%
	left_join(trcTblHist) %>%
	group_by(Trace, Date, InitDate, Ensemble) %>%
	dplyr::summarise(Value = sum(Value)) %>%
	mutate(Month = month(Date), MonthInit = month(InitDate)) %>%
	filter(Month >= MonthInit, Month <= 9, Month >= 4) %>%
	mutate(Value = Value * 1.98347) %>%

	group_by(InitDate, Ensemble) %>%
	dplyr::summarise(Value = sum(Value) / 1000) %>%
	mutate(Year = year(InitDate))

ncarFcstPlot = datInflGerberSeas %>% group_by(InitDate, Year) %>% dplyr::summarise(ValueMax = max(Value), ValueMin = min(Value), Value50 = quantile(Value, 0.5), Value10 = quantile(Value, 0.1), Value90 = quantile(Value, 0.9)) %>% mutate(Set = 'NCAR')

nrcsFcstPlot = nrcsFcst %>%
	mutate(InitDate = as.Date(paste(Issue_year, Issue_month, '01', sep = '-'))) %>%
	dplyr::rename(Year = Issue_year) %>%
	dplyr::select(InitDate, Year, Fcst_min_vol, Fcst_mid_vol, Fcst_max_vol) %>%
	setNames(c('InitDate', 'Year', paste0('Value', c(10, 50, 90)))) %>%
	mutate(ValueMin = Value10, ValueMax = Value90) %>% mutate(Set = 'NRCS')

GerberSeasFcst = bind_rows(ncarFcstPlot, nrcsFcstPlot)
GerberSeasHist = bind_rows(mutate(datInflHistGerberSeas, Set = 'NRCS'), mutate(datInflHistGerberSeas, Set = 'NCAR'))


GerberSeasFcst = GerberSeasFcst %>% mutate(Month = month(InitDate)) %>% dplyr::filter(Month <= 4)
GerberSeasHist = GerberSeasHist %>% mutate(Month = month(InitDate)) %>% dplyr::filter(Month <= 4)
GerberSeas2006 = GerberSeasHist %>% mutate(Month = month(InitDate)) %>% dplyr::filter(Month <= 4) %>% dplyr::filter(Year <= 2006)

tst = bind_rows(data.table(Month=-Inf,Value=35.4),data.table(Month=Inf,Value=35.4))
tst$Set='NRCS'
tst$Year=2006
tst2 = tst %>% left_join(GerberSeas2006)
tst = bind_rows(GerberSeas2006,tst2)
GerberSeas2006 = tst

GerberSeas2010 = GerberSeasHist %>% mutate(Month = month(InitDate)) %>% dplyr::filter(Month <= 4) %>% dplyr::filter(Year <= 2010)
tst = bind_rows(data.table(Month=-Inf,Value=33.0),data.table(Month=Inf,Value=33.0))
tst$Set='NRCS'
tst$Year=2010
tst2 = tst %>% left_join(GerberSeas2010)
tst = bind_rows(GerberSeas2010,tst2)
GerberSeas2010 = tst


ggplot() +
	geom_linerange(data = GerberSeasFcst, aes(x = Month, ymin = ValueMin, ymax = ValueMax, colour = Set), position = position_dodge(width = 0.5), alpha = 0.8, size = 3) +
	geom_point(data = GerberSeasFcst, aes(x = Month, y = Value50, group = Set), position = position_dodge(width = 0.5), alpha = 0.8, size = 10, shape = '-') +
	geom_point(data = GerberSeasFcst, aes(x = Month, y = Value10, group = Set), position = position_dodge(width = 0.5), alpha = 0.8, size = 10, shape = '-') +
	geom_point(data = GerberSeasFcst, aes(x = Month, y = Value90, group = Set), position = position_dodge(width = 0.5), alpha = 0.8, size = 10, shape = '-') +

	geom_text(data = GerberSeasFcst, aes(x = Month, y = Value50, group = Set, label = '    50th'), position = position_dodge(width = 0.5), alpha = 0.8, size = 2, hjust = 0) +
	geom_text(data = GerberSeasFcst, aes(x = Month, y = Value90, group = Set, label = '    90th'), position = position_dodge(width = 0.5), alpha = 0.8, size = 2, hjust = 0) +
	geom_text(data = GerberSeasFcst, aes(x = Month, y = Value10, group = Set, label = '    10th'), position = position_dodge(width = 0.5), alpha = 0.8, size = 2, hjust = 0) +
#	geom_point(data = GerberSeasHist, aes(x = InitDate, y = Value, group = Set), fill = '#FF8C00', shape = 21, size = 2.5, position = position_dodge(width = 25), alpha = 0.7) +
	geom_line(data = GerberSeas2006, aes(x = Month, y = Value, group = Set), linetype = 2, size = 0.75, colour = '#FF8C00') +
	geom_line(data = GerberSeas2010, aes(x = Month, y = Value, group = Set), linetype = 2, size = 0.75, colour = '#FF8C00') +
	facet_wrap(~Year, nrow = 1, scales = 'free_x') +
#	scale_x_date(date_breaks = '1 month', date_labels = '%b') +
	scale_x_discrete(limits=c("Jan","Feb","Mar", "Apr")) +
	xlab('') +
	ylab('April - September Volume (per 1000 AF)') +
	scale_color_manual(values = c('#1874CD', 'gray40')) +
	theme_bw() +
	theme(strip.background = element_rect(fill = 'white'),
		plot.title = element_text(hjust = 0.5),
		legend.position = 'bottom', legend.title = element_blank()) +
	ggtitle('Gerber Reservoir Inflow\n Water Supply Forecast')

ggsave('data/output/GerberWSF-Bar_V2.png', height = 6, width = 8)





#===========================================================
# Seasonal Forecasts compared to NRCS
nrcsFcst = fread('data/nrcs/nrcsClearFcst.csv')
nrcsFcstPlot = nrcsFcst %>%
	mutate(InitDate = as.Date(paste(Issue_year, Issue_month, '01', sep = '-'))) %>%
	dplyr::rename(Year = Issue_year) %>%
	dplyr::select(InitDate, Year, Fcst_min_vol, Fcst_low_vol, Fcst_mid_vol, Fcst_upp_vol, Fcst_max_vol) %>%
	setNames(c('InitDate', 'Year', paste0('Fcst', c(10, 30, 50, 70, 90)))) %>%
	gather(FcstLvl, Value, -InitDate, -Year) %>% filter(Year == 2006 | Year == 2010)

lvlTbl = data.table(FcstLvl = paste0('Fcst', c(10, 30, 50, 70, 90)), FcstLvlName = paste0(c(10, 30, 50, 70, 90), 'th Percent'))
nrcsFcstPlot = nrcsFcstPlot %>% left_join(lvlTbl)

datInflClearSeas = datInfl %>%
	filter(RiverWareSlot %in% c('Clear Lake.Inflow')) %>%
	left_join(trcTbl) %>%
	group_by(Trace, Date, InitDate, Ensemble) %>%
	dplyr::summarise(Value = sum(Value)) %>%
	mutate(Month = month(Date), MonthInit = month(InitDate)) %>%
	filter(Month >= MonthInit, Month <= 9, Month >= 4) %>%
	mutate(Value = Value * 1.98347) %>%

	group_by(InitDate, Ensemble) %>%
	dplyr::summarise(Value = sum(Value) / 1000) %>%
	mutate(Year = year(InitDate))

datInflHistClearSeas = datInflHist %>%
	filter(RiverWareSlot %in% c('Clear Lake.Inflow')) %>%
	left_join(trcTblHist) %>%
	group_by(Trace, Date, InitDate, Ensemble) %>%
	dplyr::summarise(Value = sum(Value)) %>%
	mutate(Month = month(Date), MonthInit = month(InitDate)) %>%
	filter(Month >= MonthInit, Month <= 9, Month >= 4) %>%
	mutate(Value = Value * 1.98347) %>%

	group_by(InitDate, Ensemble) %>%
	dplyr::summarise(Value = sum(Value) / 1000) %>%
	mutate(Year = year(InitDate))

ncarFcstPlot = datInflClearSeas %>% group_by(InitDate, Year) %>% dplyr::summarise(ValueMax = max(Value), ValueMin = min(Value), Value50 = quantile(Value, 0.5), Value10 = quantile(Value, 0.1), Value90 = quantile(Value, 0.9)) %>% mutate(Set = 'NCAR')

nrcsFcstPlot = nrcsFcst %>%
	mutate(InitDate = as.Date(paste(Issue_year, Issue_month, '01', sep = '-'))) %>%
	dplyr::rename(Year = Issue_year) %>%
	dplyr::select(InitDate, Year, Fcst_min_vol, Fcst_mid_vol, Fcst_max_vol) %>%
	setNames(c('InitDate', 'Year', paste0('Value', c(10, 50, 90)))) %>%
	mutate(ValueMin = Value10, ValueMax = Value90) %>% mutate(Set = 'NRCS')

ClearSeasFcst = bind_rows(ncarFcstPlot, nrcsFcstPlot)
ClearSeasHist = bind_rows(mutate(datInflHistClearSeas, Set = 'NRCS'), mutate(datInflHistClearSeas, Set = 'NCAR'))


ClearSeasFcst = ClearSeasFcst %>% mutate(Month = month(InitDate)) %>% dplyr::filter(Month <= 4)
ClearSeasHist = ClearSeasHist %>% mutate(Month = month(InitDate)) %>% dplyr::filter(Month <= 4)
ClearSeas2006 = ClearSeasHist %>% mutate(Month = month(InitDate)) %>% dplyr::filter(Month <= 4) %>% dplyr::filter(Year <= 2006)

tst = bind_rows(data.table(Month=-Inf,Value=67.7),data.table(Month=Inf,Value=67.7))
tst$Set='NRCS'
tst$Year=2006
tst2 = tst %>% left_join(ClearSeas2006)
tst = bind_rows(ClearSeas2006,tst2)
ClearSeas2006 = tst

ClearSeas2010 = ClearSeasHist %>% mutate(Month = month(InitDate)) %>% dplyr::filter(Month <= 4) %>% dplyr::filter(Year <= 2010)
tst = bind_rows(data.table(Month=-Inf,Value=7.22),data.table(Month=Inf,Value=7.22))
tst$Set='NRCS'
tst$Year=2010
tst2 = tst %>% left_join(ClearSeas2010)
tst = bind_rows(ClearSeas2010,tst2)
ClearSeas2010 = tst


ggplot() +
	geom_linerange(data = ClearSeasFcst, aes(x = Month, ymin = ValueMin, ymax = ValueMax, colour = Set), position = position_dodge(width = 0.5), alpha = 0.8, size = 3) +
	geom_point(data = ClearSeasFcst, aes(x = Month, y = Value50, group = Set), position = position_dodge(width = 0.5), alpha = 0.8, size = 10, shape = '-') +
	geom_point(data = ClearSeasFcst, aes(x = Month, y = Value10, group = Set), position = position_dodge(width = 0.5), alpha = 0.8, size = 10, shape = '-') +
	geom_point(data = ClearSeasFcst, aes(x = Month, y = Value90, group = Set), position = position_dodge(width = 0.5), alpha = 0.8, size = 10, shape = '-') +

	geom_text(data = ClearSeasFcst, aes(x = Month, y = Value50, group = Set, label = '    50th'), position = position_dodge(width = 0.5), alpha = 0.8, size = 2, hjust = 0) +
	geom_text(data = ClearSeasFcst, aes(x = Month, y = Value90, group = Set, label = '    90th'), position = position_dodge(width = 0.5), alpha = 0.8, size = 2, hjust = 0) +
	geom_text(data = ClearSeasFcst, aes(x = Month, y = Value10, group = Set, label = '    10th'), position = position_dodge(width = 0.5), alpha = 0.8, size = 2, hjust = 0) +
#	geom_point(data = ClearSeasHist, aes(x = InitDate, y = Value, group = Set), fill = '#FF8C00', shape = 21, size = 2.5, position = position_dodge(width = 25), alpha = 0.7) +
	geom_line(data = ClearSeas2006, aes(x = Month, y = Value, group = Set), linetype = 2, size = 0.75, colour = '#FF8C00') +
	geom_line(data = ClearSeas2010, aes(x = Month, y = Value, group = Set), linetype = 2, size = 0.75, colour = '#FF8C00') +
	facet_wrap(~Year, nrow = 1, scales = 'free_x') +
#	scale_x_date(date_breaks = '1 month', date_labels = '%b') +
	scale_x_discrete(limits=c("Jan","Feb","Mar", "Apr")) +
	xlab('') +
	ylab('April - September Volume (per 1000 AF)') +
	scale_color_manual(values = c('#1874CD', 'gray40')) +
	theme_bw() +
	theme(strip.background = element_rect(fill = 'white'),
		plot.title = element_text(hjust = 0.5),
		legend.position = 'bottom', legend.title = element_blank()) +
	ggtitle('Clear Lake Inflow\n Water Supply Forecast')

ggsave('data/output/ClearWSF-Bar_V2.png', height = 6, width = 8)
