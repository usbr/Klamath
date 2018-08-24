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

#-Scenario Directory [Location of RiverWare data]
scenDir = 'C:/Projects/KlamathRiverBasin/RiverSmart/Scenario/'

#-Folder Header for Forecast Scenario Runs
scenHdr = 'MRM,RW,Rls,FcstInp'

#-Folder Header for Historical Runs
histHdr = 'MRM,RW,Rls,HistInp'
histDir = 'C:/Projects/KlamathRiverBasin/RiverSmart/Scenario/Historical/trace1'
# All models are run from Jan 1, with historical data appended from
# Jan 1 to start of forecast - forecast start times 1/1, 2/1, 3/1, 4/1, 5/1
#dateList = c('2006-01-01', '2010-01-01')

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

# datStrgHist = data.table()
# datEWAHist  = data.table()
# datGageHist  = data.table()
# datIgHist  = data.table()
# datPjctHist  = data.table()
# datInflHist  = data.table()
# datSplyHist  = data.table()
# for(iterDate in 1:ctDate){
	# dateSel  = dateList[iterDate]
	# filePathTmp = paste0(scenDir, histHdr, ',', dateSel, '/')
	
	# # Storage
	# strgTmp = read.rdf(paste0(filePathTmp, 'Storage.rdf'))
	# datStrgTmp =  Rdf2dt(strgTmp)
	# datStrgTmp = datStrgTmp %>% mutate(Set = dateSel)
	# datStrgHist  = datStrgHist %>% bind_rows(datStrgTmp)

	# # EWA
	# EWATmp = read.rdf(paste0(filePathTmp, 'EWA.rdf'))
	# datEWATmp =  Rdf2dt(EWATmp)
	# datEWATmp = datEWATmp %>% mutate(Set = dateSel)
	# datEWAHist  = datEWAHist  %>% bind_rows(datEWATmp)

	# #GaugeFlow
	# gageTmp = read.rdf(paste0(filePathTmp, 'GageFlow.rdf'))
	# datGageTmp =  Rdf2dt(gageTmp)
	# datGageTmp = datGageTmp %>% mutate(Set = dateSel)
	# datGageHist = datGageHist %>% bind_rows(datGageTmp)

	# # Iron Gate
	# igTmp = read.rdf(paste0(filePathTmp, 'IronGateReqs.rdf'))
	# datIgTmp =  Rdf2dt(igTmp)
	# datIgTmp = datIgTmp %>% mutate(Set = dateSel)
	# datIgHist = datIgHist %>% bind_rows(datIgTmp)
	
	# # Project Supplies
	# pjctTmp = read.rdf(paste0(filePathTmp, 'ProjectSupplies.rdf'))
	# datPjctTmp =  Rdf2dt(pjctTmp)
	# datPjctTmp = datPjctTmp %>% mutate(Set = dateSel)
	# datPjctHist = datPjctHist %>% bind_rows(datPjctTmp)

	# # Inflow
	# inflTmp = read.rdf(paste0(filePathTmp, 'Inflow.rdf'))
	# datInflTmp =  Rdf2dt(inflTmp)
	# datInflTmp = datInflTmp %>% mutate(Set = dateSel)
	# datInflHist = datInflHist %>% bind_rows(datInflTmp)

	# # Supply (Forecasts)
	# splyTmp = read.rdf(paste0(filePathTmp, 'Supply.rdf'))
	# datSplyTmp =  Rdf2dt(splyTmp)
	# datSplyTmp = datSplyTmp %>% mutate(Set = dateSel)
	# datSplyHist = datSplyHist %>% bind_rows(datSplyTmp)

# }


dirHist = 'C:/Projects/KlamathRiverBasin/PreProcess/data/HistoricalOut/'
histFiles = c('Gerber_Reservoir.Inflow.csv', 'Clear_Lake.Inflow.csv','Sprague_River.Inflow.csv')
Locations = c('Gerber Reservoir Inflow','Clear Lake Inflow','Sprague R near Chiloquin')
datHist = data.table()
ctHist = length(histFiles)
for(iterCT in 1:ctHist){
	hist_cfs = fread(paste0(dirHist,histFiles[iterCT]))
	hist_af = hist_cfs %>% dplyr::mutate(Location=Locations[iterCT]) %>%
		mutate(Month = month(Date)) %>% 
		filter(Month <= 9, Month >= 4) %>% 
		mutate(Value = Value * 1.98347) %>% 
		mutate(Year = year(Date)) %>% 
		group_by(Year,Location) %>%
		dplyr::summarise(Value = sum(Value) / 1000)
	datHist = bind_rows(datHist,hist_af)
}

Locations = c('Sprague R near Chiloquin', 'Williamson R blw Sprague near Chiloquin')
histWill_cfs = fread(paste0(dirHist,'Williamson_River.Inflow.csv'))
histWill_af = histWill_cfs %>% dplyr::mutate(Location=Locations[2]) %>%
	mutate(Month = month(Date)) %>% 
	filter(Month <= 9, Month >= 4) %>% 
	mutate(Value = Value * 1.98347) %>% 
	mutate(Year = year(Date)) %>% 
	group_by(Year,Location) %>%
	dplyr::summarise(Value = sum(Value) / 1000)

histSprag_cfs = fread(paste0(dirHist,'Sprague_River.Inflow.csv'))
histSprag_af = histSprag_cfs %>% dplyr::mutate(Location=Locations[1]) %>%
	mutate(Month = month(Date)) %>% 
	filter(Month <= 9, Month >= 4) %>% 
	mutate(Value = Value * 1.98347) %>% 
	mutate(Year = year(Date)) %>% 
	group_by(Year,Location) %>%
	dplyr::summarise(Value = sum(Value) / 1000)

tmp = histWill_af %>% full_join(histSprag_af)
histWill_af = tmp %>% group_by(Year) %>% mutate(Value=sum(Value)) %>% filter(Location == 'Williamson R blw Sprague near Chiloquin')

datHist = bind_rows(datHist,histWill_af)
write.csv(file='data/output/Hist_apr-sep_Volumes.csv',datHist,row.names=F)

#===========================================================
# Williamson R below Sprague R near Chiloquin 
# Seasonal Forecasts compared to NRCS by lead time (month)
nrcsFcst = fread('data/nrcs/nrcsWillFcstAll.csv')
nrcsFcstPlot = nrcsFcst %>% 
	mutate(InitDate = as.Date(paste(Issue_year, Issue_month, '01', sep = '-'))) %>%
	dplyr::rename(Year = Issue_year) %>% 
	dplyr::select(InitDate, Year, Fcst_min_vol, Fcst_low_vol, Fcst_mid_vol, Fcst_upp_vol, Fcst_max_vol) %>%
	setNames(c('InitDate', 'Year', paste0('Fcst', c(10, 30, 50, 70, 90)))) %>% 
	gather(FcstLvl, Value, -InitDate, -Year) %>% filter(Year == 2006 | Year == 2010)

lvlTbl = data.table(FcstLvl = paste0('Fcst', c(10, 30, 50, 70, 90)), FcstLvlName = paste0(c(10, 30, 50, 70, 90), 'th Percent'))
nrcsFcstPlot = nrcsFcstPlot %>% left_join(lvlTbl)

#Fcst data
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

ncarFcstPlot = datInflWillSeas %>% group_by(InitDate, Year) %>% dplyr::summarise(ValueMax = max(Value), ValueMin = min(Value), Value50 = quantile(Value, 0.5), Value10 = quantile(Value, 0.1), Value90 = quantile(Value, 0.9)) %>% mutate(Set = 'NCAR')

nrcsFcstPlot = nrcsFcst %>% 
	mutate(InitDate = as.Date(paste(Issue_year, Issue_month, '01', sep = '-'))) %>%
	dplyr::rename(Year = Issue_year) %>% 
	dplyr::select(InitDate, Year, Fcst_min_vol, Fcst_mid_vol, Fcst_max_vol) %>%
	setNames(c('InitDate', 'Year', paste0('Value', c(10, 50, 90)))) %>% 
	mutate(ValueMin = Value10, ValueMax = Value90) %>% mutate(Set = 'NRCS')

willSeasFcst = bind_rows(ncarFcstPlot, nrcsFcstPlot)
willSeasFcst = willSeasFcst %>% mutate(Month = month(InitDate)) %>% dplyr::filter(Month <= 4)

#Put NRCS and simulated historical volumes together
willnrcsFcstTbl = willSeasFcst %>% filter(Set == 'NRCS') %>% arrange(Month) %>% mutate(Location = 'Williamson R blw Sprague near Chiloquin')
willFcstandHist = willnrcsFcstTbl %>% left_join(datHist)
write.csv(file='data/output/Willimason_FcastHist_apr-sep_Volumes.csv',willFcstandHist,row.names=F)

#Add NRCS observed naturalized flow
nrcsNatFlow = fread('data/nrcs/WMSO3_nrcs_natflow.csv')
nrcsNatFlowSeas = nrcsNatFlow %>% 
	filter(Date %in% c('Apr','May','Jun','Jul','Aug','Sep')) %>% 
	group_by(Year) %>% summarise(Value_obs=sum(Value)/1000) %>% 
	mutate(Location = 'Williamson R blw Sprague near Chiloquin')

willFcstandHist = willFcstandHist %>% left_join(nrcsNatFlowSeas)
willFcstandHist$monab = month.abb[willFcstandHist$Month]
willFcstandHist$monab = factor(willFcstandHist$monab, levels = c('Jan', 'Feb', 'Mar', 'Apr'))

ggplot() + 
	geom_linerange(data = willFcstandHist, aes(x = Year, ymin = ValueMin, ymax = ValueMax, colour = Set), position = position_dodge(width = 0.2), alpha = 0.8, size = 3) +
	geom_point(data = willFcstandHist, aes(x = Year, y = Value50, group = Set), position = position_dodge(width = 0.2), alpha = 0.8, size = 10, shape = '-') + 
	geom_point(data = willFcstandHist, aes(x = Year, y = Value10, group = Set), position = position_dodge(width = 0.2), alpha = 0.8, size = 10, shape = '-') + 
	geom_point(data = willFcstandHist, aes(x = Year, y = Value90, group = Set), position = position_dodge(width = 0.2), alpha = 0.8, size = 10, shape = '-') + 
#	geom_text(data = willFcstandHist, aes(x = Year, y = Value50, group = Set, label = '    50th'), position = position_dodge(width = 0.2), alpha = 0.8, size = 2, hjust = 0) + 
#	geom_text(data = willFcstandHist, aes(x = Year, y = Value90, group = Set, label = '    90th'), position = position_dodge(width = 0.2), alpha = 0.8, size = 2, hjust = 0) + 
#	geom_text(data = willFcstandHist, aes(x = Year, y = Value10, group = Set, label = '    10th'), position = position_dodge(width = 0.2), alpha = 0.8, size = 2, hjust = 0) + 
	geom_point(data = willFcstandHist, aes(x = Year, y = Value, group = Set), fill = '#FF8C00', shape = 21, size = 2.5, position = position_dodge(width = 0.2), alpha = 0.7) + 
	geom_point(data = willFcstandHist, aes(x = Year, y = Value_obs, group = Set), fill = '#244A9F', shape = 21, size = 2.5, position = position_dodge(width = 0.2), alpha = 0.7) + 
	facet_wrap(~monab, nrow = 5, scales = 'free_x') + 
	scale_x_continuous(breaks=c(1995,2000,2005,2010,2015)) +
	xlab('') + 
	ylab('April - September Volume (per 1000 AF)') + 
#	scale_color_manual(values = c('#1874CD', 'gray40')) + 
	scale_color_manual(values = c('gray40')) + 
	theme_bw() +
	theme(strip.background = element_rect(fill = 'white'),
		plot.title = element_text(hjust = 0.5),
		legend.position = 'bottom', legend.title = element_blank()) +
	ggtitle('Williamson River near Chiloquin\n Water Supply Forecast')
ggsave('data/output/WilliamsonWSF-Bar_byLeadTime.png', height = 8, width = 6)


#===========================================================
# Sprague R near Chiloquin 
# Seasonal Forecasts compared to NRCS by lead time (month)
nrcsFcst = fread('data/nrcs/nrcsSpragFcstAll.csv')
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

ncarFcstPlot = datInflSpragSeas %>% group_by(InitDate, Year) %>% dplyr::summarise(ValueMax = max(Value), ValueMin = min(Value), Value50 = quantile(Value, 0.5), Value10 = quantile(Value, 0.1), Value90 = quantile(Value, 0.9)) %>% mutate(Set = 'NCAR')


nrcsFcstPlot = nrcsFcst %>% 
	mutate(InitDate = as.Date(paste(Issue_year, Issue_month, '01', sep = '-'))) %>%
	dplyr::rename(Year = Issue_year) %>% 
	dplyr::select(InitDate, Year, Fcst_min_vol, Fcst_mid_vol, Fcst_max_vol) %>%
	setNames(c('InitDate', 'Year', paste0('Value', c(10, 50, 90)))) %>% 
	mutate(ValueMin = Value10, ValueMax = Value90) %>% mutate(Set = 'NRCS')

spragSeasFcst = bind_rows(ncarFcstPlot, nrcsFcstPlot)
spragSeasFcst = spragSeasFcst %>% mutate(Month = month(InitDate)) %>% dplyr::filter(Month <= 4)


#Put NRCS and simulated historical volumes together
spragnrcsFcstTbl = spragSeasFcst %>% filter(Set == 'NRCS') %>% arrange(Month) %>% mutate(Location = 'Sprague R near Chiloquin')
spragFcstandHist = spragnrcsFcstTbl %>% left_join(datHist)
write.csv(file='data/output/Sprague_FcastHist_apr-sep_Volumes.csv',spragFcstandHist,row.names=F)

#Add NRCS observed naturalized flow
nrcsNatFlow = fread('data/nrcs/SPRAG_nrcs_natflow.csv')
nrcsNatFlowSeas = nrcsNatFlow %>% 
	filter(Date %in% c('Apr','May','Jun','Jul','Aug','Sep')) %>% 
	group_by(Year) %>% summarise(Value_obs=sum(Value)/1000) %>% 
	mutate(Location = 'Sprague R near Chiloquin')

spragFcstandHist = spragFcstandHist %>% left_join(nrcsNatFlowSeas)
spragFcstandHist$monab = month.abb[spragFcstandHist$Month]
spragFcstandHist$monab = factor(spragFcstandHist$monab, levels = c('Jan', 'Feb', 'Mar', 'Apr'))

ggplot() + 
	geom_linerange(data = spragFcstandHist, aes(x = Year, ymin = ValueMin, ymax = ValueMax, colour = Set), position = position_dodge(width = 0.2), alpha = 0.8, size = 3) +
	geom_point(data = spragFcstandHist, aes(x = Year, y = Value50, group = Set), position = position_dodge(width = 0.2), alpha = 0.8, size = 10, shape = '-') + 
	geom_point(data = spragFcstandHist, aes(x = Year, y = Value10, group = Set), position = position_dodge(width = 0.2), alpha = 0.8, size = 10, shape = '-') + 
	geom_point(data = spragFcstandHist, aes(x = Year, y = Value90, group = Set), position = position_dodge(width = 0.2), alpha = 0.8, size = 10, shape = '-') + 
#	geom_text(data = spragFcstandHist, aes(x = Year, y = Value50, group = Set, label = '    50th'), position = position_dodge(width = 0.2), alpha = 0.8, size = 2, hjust = 0) + 
#	geom_text(data = spragFcstandHist, aes(x = Year, y = Value90, group = Set, label = '    90th'), position = position_dodge(width = 0.2), alpha = 0.8, size = 2, hjust = 0) + 
#	geom_text(data = spragFcstandHist, aes(x = Year, y = Value10, group = Set, label = '    10th'), position = position_dodge(width = 0.2), alpha = 0.8, size = 2, hjust = 0) + 
	geom_point(data = spragFcstandHist, aes(x = Year, y = Value, group = Set), fill = '#FF8C00', shape = 21, size = 2.5, position = position_dodge(width = 0.2), alpha = 0.7) + 
	geom_point(data = spragFcstandHist, aes(x = Year, y = Value_obs, group = Set), fill = '#244A9F', shape = 21, size = 2.5, position = position_dodge(width = 0.2), alpha = 0.7) + 
	facet_wrap(~monab, nrow = 5, scales = 'free_x') + 
	scale_x_continuous(breaks=c(1995,2000,2005,2010,2015)) +
	xlab('') + 
	ylab('April - September Volume (per 1000 AF)') + 
#	scale_color_manual(values = c('#1874CD', 'gray40')) + 
	scale_color_manual(values = c('gray40')) + 
	theme_bw() +
	theme(strip.background = element_rect(fill = 'white'),
		plot.title = element_text(hjust = 0.5),
		legend.position = 'bottom', legend.title = element_blank()) +
	ggtitle('Sprague River near Chiloquin\n Water Supply Forecast')
ggsave('data/output/SpragueWSF-Bar_byLeadTime.png', height = 8, width = 6)






#===========================================================
# Gerber Reservoir Inflows
# Seasonal Forecasts compared to NRCS by lead time (month)
nrcsFcst = fread('data/nrcs/nrcsGerberFcstAll.csv')
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

ncarFcstPlot = datInflGerberSeas %>% group_by(InitDate, Year) %>% dplyr::summarise(ValueMax = max(Value), ValueMin = min(Value), Value50 = quantile(Value, 0.5), Value10 = quantile(Value, 0.1), Value90 = quantile(Value, 0.9)) %>% mutate(Set = 'NCAR')

nrcsFcstPlot = nrcsFcst %>% 
	mutate(InitDate = as.Date(paste(Issue_year, Issue_month, '01', sep = '-'))) %>%
	dplyr::rename(Year = Issue_year) %>% 
	dplyr::select(InitDate, Year, Fcst_min_vol, Fcst_mid_vol, Fcst_max_vol) %>%
	setNames(c('InitDate', 'Year', paste0('Value', c(10, 50, 90)))) %>% 
	mutate(ValueMin = Value10, ValueMax = Value90) %>% mutate(Set = 'NRCS')

GerberSeasFcst = bind_rows(ncarFcstPlot, nrcsFcstPlot)
GerberSeasFcst = GerberSeasFcst %>% mutate(Month = month(InitDate)) %>% dplyr::filter(Month <= 4)

#Put NRCS and simulated historical volumes together
GerbernrcsFcstTbl = GerberSeasFcst %>% filter(Set == 'NRCS') %>% arrange(Month) %>% mutate(Location = 'Gerber Reservoir Inflow')
GerberFcstandHist = GerbernrcsFcstTbl %>% left_join(datHist)
write.csv(file='data/output/Gerber_FcastHist_apr-sep_Volumes.csv',GerberFcstandHist,row.names=F)


#Add NRCS observed naturalized flow
nrcsNatFlow = fread('data/nrcs/GERB_nrcs_natflow.csv')
nrcsNatFlowSeas = nrcsNatFlow %>% 
	filter(Date %in% c('Apr','May','Jun','Jul','Aug','Sep')) 
tst = nrcsNatFlowSeas %>% group_by(Year) %>% summarise(Value_min=min(Value))
tst2 = nrcsNatFlowSeas %>% left_join(tst)
tst3 = tst2 %>% filter(Value_min >= 0) %>% select(-Value_min)

nrcsNatFlowSeas = tst3	%>% 
	group_by(Year) %>% summarise(Value_obs=sum(Value)/1000) %>% 
	mutate(Location = 'Gerber Reservoir Inflow')
	
GerberFcstandHist = GerberFcstandHist %>% left_join(nrcsNatFlowSeas)
GerberFcstandHist$monab = month.abb[GerberFcstandHist$Month]
GerberFcstandHist$monab = factor(GerberFcstandHist$monab, levels = c('Jan', 'Feb', 'Mar', 'Apr'))

write.csv(file='testGerber.csv',GerberFcstandHist)

ggplot() + 
	geom_linerange(data = GerberFcstandHist, aes(x = Year, ymin = ValueMin, ymax = ValueMax, colour = Set), position = position_dodge(width = 0.2), alpha = 0.8, size = 3) +
	geom_point(data = GerberFcstandHist, aes(x = Year, y = Value50, group = Set), position = position_dodge(width = 0.2), alpha = 0.8, size = 10, shape = '-') + 
	geom_point(data = GerberFcstandHist, aes(x = Year, y = Value10, group = Set), position = position_dodge(width = 0.2), alpha = 0.8, size = 10, shape = '-') + 
	geom_point(data = GerberFcstandHist, aes(x = Year, y = Value90, group = Set), position = position_dodge(width = 0.2), alpha = 0.8, size = 10, shape = '-') + 
#	geom_text(data = GerberFcstandHist, aes(x = Year, y = Value50, group = Set, label = '    50th'), position = position_dodge(width = 0.2), alpha = 0.8, size = 2, hjust = 0) + 
#	geom_text(data = GerberFcstandHist, aes(x = Year, y = Value90, group = Set, label = '    90th'), position = position_dodge(width = 0.2), alpha = 0.8, size = 2, hjust = 0) + 
#	geom_text(data = GerberFcstandHist, aes(x = Year, y = Value10, group = Set, label = '    10th'), position = position_dodge(width = 0.2), alpha = 0.8, size = 2, hjust = 0) + 
	geom_point(data = GerberFcstandHist, aes(x = Year, y = Value, group = Set), fill = '#FF8C00', shape = 21, size = 2.5, position = position_dodge(width = 0.2), alpha = 0.7) + 
	geom_point(data = GerberFcstandHist, aes(x = Year, y = Value_obs, group = Set), fill = '#244A9F', shape = 21, size = 2.5, position = position_dodge(width = 0.2), alpha = 0.7) + 
	facet_wrap(~monab, nrow = 5, scales = 'free_x') + 
	scale_x_continuous(breaks=c(2005,2010,2015)) +
	xlab('') + 
	ylab('April - September Volume (per 1000 AF)') + 
#	scale_color_manual(values = c('#1874CD', 'gray40')) + 
	scale_color_manual(values = c('gray40')) + 
	theme_bw() +
	theme(strip.background = element_rect(fill = 'white'),
		plot.title = element_text(hjust = 0.5),
		legend.position = 'bottom', legend.title = element_blank()) +
	ggtitle('Gerber Reservoir Inflow\n Water Supply Forecast')
ggsave('data/output/GerberWSF-Bar_byLeadTime.png', height = 8, width = 6)


#===========================================================
# Clear Lake Inflows
# Seasonal Forecasts compared to NRCS by lead time (month)
nrcsFcst = fread('data/nrcs/nrcsClearFcstAll.csv')
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

ncarFcstPlot = datInflClearSeas %>% group_by(InitDate, Year) %>% dplyr::summarise(ValueMax = max(Value), ValueMin = min(Value), Value50 = quantile(Value, 0.5), Value10 = quantile(Value, 0.1), Value90 = quantile(Value, 0.9)) %>% mutate(Set = 'NCAR')

nrcsFcstPlot = nrcsFcst %>% 
	mutate(InitDate = as.Date(paste(Issue_year, Issue_month, '01', sep = '-'))) %>%
	dplyr::rename(Year = Issue_year) %>% 
	dplyr::select(InitDate, Year, Fcst_min_vol, Fcst_mid_vol, Fcst_max_vol) %>%
	setNames(c('InitDate', 'Year', paste0('Value', c(10, 50, 90)))) %>% 
	mutate(ValueMin = Value10, ValueMax = Value90) %>% mutate(Set = 'NRCS')

ClearSeasFcst = bind_rows(ncarFcstPlot, nrcsFcstPlot)
ClearSeasFcst = ClearSeasFcst %>% mutate(Month = month(InitDate)) %>% dplyr::filter(Month <= 4)

#Put NRCS and simulated historical volumes together
ClearnrcsFcstTbl = ClearSeasFcst %>% filter(Set == 'NRCS') %>% arrange(Month) %>% mutate(Location = 'Clear Lake Inflow')
ClearFcstandHist = ClearnrcsFcstTbl %>% left_join(datHist)
write.csv(file='data/output/Clear_FcastHist_apr-sep_Volumes.csv',ClearFcstandHist,row.names=F)


#Add NRCS observed naturalized flow - Doesn't exist for Clear Lake

ClearFcstandHist$monab = month.abb[ClearFcstandHist$Month]
ClearFcstandHist$monab = factor(ClearFcstandHist$monab, levels = c('Jan', 'Feb', 'Mar', 'Apr'))

ggplot() + 
	geom_linerange(data = ClearFcstandHist, aes(x = Year, ymin = ValueMin, ymax = ValueMax, colour = Set), position = position_dodge(width = 0.2), alpha = 0.8, size = 3) +
	geom_point(data = ClearFcstandHist, aes(x = Year, y = Value50, group = Set), position = position_dodge(width = 0.2), alpha = 0.8, size = 10, shape = '-') + 
	geom_point(data = ClearFcstandHist, aes(x = Year, y = Value10, group = Set), position = position_dodge(width = 0.2), alpha = 0.8, size = 10, shape = '-') + 
	geom_point(data = ClearFcstandHist, aes(x = Year, y = Value90, group = Set), position = position_dodge(width = 0.2), alpha = 0.8, size = 10, shape = '-') + 
#	geom_text(data = ClearFcstandHist, aes(x = Year, y = Value50, group = Set, label = '    50th'), position = position_dodge(width = 0.2), alpha = 0.8, size = 2, hjust = 0) + 
#	geom_text(data = ClearFcstandHist, aes(x = Year, y = Value90, group = Set, label = '    90th'), position = position_dodge(width = 0.2), alpha = 0.8, size = 2, hjust = 0) + 
#	geom_text(data = ClearFcstandHist, aes(x = Year, y = Value10, group = Set, label = '    10th'), position = position_dodge(width = 0.2), alpha = 0.8, size = 2, hjust = 0) + 
	geom_point(data = ClearFcstandHist, aes(x = Year, y = Value, group = Set), fill = '#FF8C00', shape = 21, size = 2.5, position = position_dodge(width = 0.2), alpha = 0.7) + 
#	geom_point(data = ClearFcstandHist, aes(x = Year, y = Value_obs, group = Set), fill = '#244A9F', shape = 21, size = 2.5, position = position_dodge(width = 0.2), alpha = 0.7) + 
	facet_wrap(~monab, nrow = 5, scales = 'free_x') + 
	scale_x_continuous(breaks=c(2005,2010,2015)) +
	xlab('') + 
	ylab('April - September Volume (per 1000 AF)') + 
#	scale_color_manual(values = c('#1874CD', 'gray40')) + 
	scale_color_manual(values = c('gray40')) + 
	theme_bw() +
	theme(strip.background = element_rect(fill = 'white'),
		plot.title = element_text(hjust = 0.5),
		legend.position = 'bottom', legend.title = element_blank()) +
	ggtitle('Clear Lake Inflow\n Water Supply Forecast')
ggsave('data/output/ClearWSF-Bar_byLeadTime.png', height = 8, width = 6)

