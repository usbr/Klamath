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
scenDir = 'C:/Projects/KlamathRiverBasin/RiverSmart/Scenario/'
scenHdr = 'MRM,RW,Rls,FcstInp'
dateList = c('2006-01-01', '2006-02-01', '2006-03-01', '2006-04-01', '2006-05-01', '2010-01-01', '2010-02-01', '2010-03-01')
#- Output Directory
dirOup = 'data/output/'

#===========================================================
#-postprocess_lib.r contains custom functions and required R packages
source('src/postprocess_lib.r')
dat_name_list = c('Run', 'Trace', 'RiverWareSlot', 'Time', 'Value', 'InputDMIName')
setwd(wd)
#===========================================================
# Measures Tables
# Description: These tables contain the RiverWare slots of interest,
# and any relevent values (e.g. minimum flow thresholds)

#-storage (reservoir) key locatons and exceedance levels (full storage)
storage_tbl = fread(paste0('lib/storage_tbl.csv'))
#-flood pool elevations for reservoirs with flood control
flood_pool_tbl = fread(paste0('lib/flood_pool_tbl.csv'))
#-reservoirs mapped to river basins
resinflow_tbl = fread(paste0('lib/resinflow_tbl.csv'))

resoutflow_tbl = fread(paste0('lib/resoutflow_tbl.csv'))

#-future climate change scenarios and historic scenarios mapped
# to RiverSmart Traces
scenario_cc_tbl = fread(paste0('lib/scenario_cc_tbl.csv'))

#===========================================================
# Spatial Plotting Data [maps etc.]
states_dat_dt = readRDS('data/processed/states.rda') %>% dplyr::filter(name %in% c('Montana', 'Idaho', 'Wyoming'), admin == 'United States of America')
basin_dat = readRDS('data/processed/MBBasinOutline_V3lonlat.rda')
subbasins_dat = readRDS('data/processed/MBIA_MajorSub.rda')
ewbrks = seq(-121, -101 , 2)
nsbrks = seq(40, 50, 2)
ewlbls = unlist(lapply(ewbrks, function(x) ifelse(x < 0, paste0(abs(x), '°W'), ifelse(x > 0, paste0(x, '°E'),x))))
nslbls = unlist(lapply(nsbrks, function(x) ifelse(x < 0, paste0(x, '°S'), ifelse(x > 0, paste0(x, '°N'),x))))

ylims = c(44, 49.25)
xlims = c(-115, -105)

#===========================================================
scen_dir = 'C:/Projects/UpperMissouriRiverBasin/RiverSmart/Scenario/CC All,GeneralMRM,RiverWare Model/'
scen_dir_paleo1 = 'C:/Projects/UpperMissouriRiverBasin/RiverSmart/Scenario/Paleo,GeneralMRM,RiverWare Model/'
scen_dir_paleo2 = 'C:/Projects/UpperMissouriRiverBasin/RiverSmart/Scenario/PaleoPCA,GeneralMRM,RiverWare Model/'
scen_dir_paleo3 = 'C:/Projects/UpperMissouriRiverBasin/RiverSmart/Scenario/PaleoTransient,GeneralMRM,RiverWare Model/'

#===========================================================
storage_ls = read.rdf(paste0(scen_dir, 'Storage.rdf'))
dat_storage = Rdf2dt(storage_ls)

storage_paleo1_ls = read.rdf(paste0(scen_dir_paleo1, 'Storage.rdf'))
dat_storage_paleo1 = Rdf2dt(storage_paleo1_ls)

storage_paleo2_ls = read.rdf(paste0(scen_dir_paleo2, 'Storage.rdf'))
dat_storage_paleo2 = Rdf2dt(storage_paleo2_ls)

storage_paleo3_ls = read.rdf(paste0(scen_dir_paleo3, 'Storage.rdf'))
dat_storage_paleo3 = Rdf2dt(storage_paleo3_ls)


dat_storage_calc = dat_storage %>% mutate(Year = wyear(Date), Month = month(Date), Day = day(Date)) %>% left_join(storage_tbl) %>% left_join(scenario_cc_tbl)
dat_storage_calc = dat_storage_calc %>% group_by(Location, Scenario, Period, Month) %>% summarise(Value = mean(Value, na.rm = T)) %>% ungroup()

dat_storage_calc_hist = dat_storage_calc %>% filter(Scenario == 'Historical') %>% dplyr::select(-Period)
dat_storage_calc_fut = dat_storage_calc %>% filter(Scenario != 'Historical')

dat_storage_paleo1_calc = dat_storage_paleo1 %>% mutate(Year = wyear(Date), Month = month(Date), Day = day(Date)) %>% left_join(storage_tbl)
dat_storage_paleo1_calc = dat_storage_paleo1_calc %>% group_by(Location, Trace, Month) %>% summarise(Value = mean(Value, na.rm = T)) 

dat_storage_paleo2_calc = dat_storage_paleo2 %>% mutate(Year = wyear(Date), Month = month(Date), Day = day(Date)) %>% left_join(storage_tbl)
dat_storage_paleo2_calc = dat_storage_paleo2_calc %>% group_by(Location, Trace, Month) %>% summarise(Value = mean(Value, na.rm = T)) 

dat_storage_paleo3_calc = dat_storage_paleo3 %>% mutate(Year = wyear(Date), Month = month(Date), Day = day(Date)) %>% left_join(storage_tbl)
dat_storage_paleo3_calc = dat_storage_paleo3_calc %>% group_by(Location, Trace, Month) %>% summarise(Value = mean(Value, na.rm = T)) 

dat_storage_calc_fut$Scenario = factor(dat_storage_calc_fut$Scenario,levels = c('HD', 'HW', 'CT', 'WW', 'WD'))

location_list = unique(dat_storage_calc_fut$Location)
nloc = length(location_list)

for(i in 1:nloc){
	location_sel = location_list[i]
	dat_storage_calc_fut_fl = dat_storage_calc_fut %>% filter(Location == location_sel)
	dat_storage_calc_hist_fl = dat_storage_calc_hist %>% filter(Location == location_sel)
	dat_storage_paleo1_calc_fl = dat_storage_paleo1_calc %>% filter(Location == location_sel)
	dat_storage_paleo2_calc_fl = dat_storage_paleo2_calc %>% filter(Location == location_sel)
	dat_storage_paleo3_calc_fl = dat_storage_paleo3_calc %>% filter(Location == location_sel)

	ggplot() + geom_line(data = dat_storage_paleo1_calc_fl, aes(x = Month, y = Value, group = Trace), colour = 'gray50') + geom_line(data = dat_storage_paleo2_calc_fl, aes(x = Month, y = Value, group = Trace), colour = 'gray50') + geom_line(data = dat_storage_paleo3_calc_fl, aes(x = Month, y = Value, group = Trace), colour = 'gray50') + geom_line(data = dat_storage_calc_fut_fl, aes(x = Month, y = Value, color=Scenario, group = Scenario), size=1.5) + scale_color_manual(values=c('#DE8C6D', '#FCCB89', '#FFEF94', '#C7B7D9', '#A6DDCA'))  + facet_wrap(~Period,ncol=1) + geom_line(data = dat_storage_calc_hist_fl, aes(x = Month, y = Value), colour = 'black', size = 1.5) + facet_wrap(~Period,ncol=1) + ylab('Mean EOM Storage (AF)') + theme_bw() + theme(strip.background = element_rect(fill = FALSE)) + scale_x_continuous(breaks = 1:12, labels = month.abb) + ggtitle(paste(location_sel, 'End-of-Month Storage'))
	ggsave(paste(out_dir, location_sel, '_mean_monthly_EOM_storage.png'), height = 6, width = 8)

}

# Reservoir Inflow
resinflow_ls = read.rdf(paste0(scen_dir, 'ResInflow.rdf'))
dat_resinflow = Rdf2dt(resinflow_ls)

resinflow_paleo1_ls = read.rdf(paste0(scen_dir_paleo1, 'ResInflow.rdf'))
dat_resinflow_paleo1 = Rdf2dt(resinflow_paleo1_ls)

resinflow_paleo2_ls = read.rdf(paste0(scen_dir_paleo2, 'ResInflow.rdf'))
dat_resinflow_paleo2 = Rdf2dt(resinflow_paleo2_ls)

resinflow_paleo3_ls = read.rdf(paste0(scen_dir_paleo3, 'ResInflow.rdf'))
dat_resinflow_paleo3 = Rdf2dt(resinflow_paleo3_ls)


dat_resinflow_calc = dat_resinflow %>% mutate(Year = wyear(Date), Month = month(Date), Day = day(Date)) %>% left_join(resinflow_tbl) %>% left_join(scenario_cc_tbl)
dat_resinflow_calc = dat_resinflow_calc %>% group_by(Location, Scenario, Period, Month) %>% summarise(Value = mean(Value, na.rm = T)) %>% ungroup()
dat_resinflow_calc_hist = dat_resinflow_calc %>% filter(Scenario == 'Historical') %>% dplyr::select(-Period)
dat_resinflow_calc_fut = dat_resinflow_calc %>% filter(Scenario != 'Historical')

dat_resinflow_paleo1_calc = dat_resinflow_paleo1 %>% mutate(Year = wyear(Date), Month = month(Date), Day = day(Date)) %>% left_join(resinflow_tbl)
dat_resinflow_paleo1_calc = dat_resinflow_paleo1_calc %>% group_by(Location, Trace, Month) %>% summarise(Value = mean(Value, na.rm = T)) 

dat_resinflow_paleo2_calc = dat_resinflow_paleo2 %>% mutate(Year = wyear(Date), Month = month(Date), Day = day(Date)) %>% left_join(resinflow_tbl)
dat_resinflow_paleo2_calc = dat_resinflow_paleo2_calc %>% group_by(Location, Trace, Month) %>% summarise(Value = mean(Value, na.rm = T)) 

dat_resinflow_paleo3_calc = dat_resinflow_paleo3 %>% mutate(Year = wyear(Date), Month = month(Date), Day = day(Date)) %>% left_join(resinflow_tbl)
dat_resinflow_paleo3_calc = dat_resinflow_paleo3_calc %>% group_by(Location, Trace, Month) %>% summarise(Value = mean(Value, na.rm = T)) 

dat_resinflow_calc_fut$Scenario = factor(dat_resinflow_calc_fut$Scenario,levels = c('HD', 'HW', 'CT', 'WW', 'WD'))

location_list = unique(dat_resinflow_calc_fut$Location)
nloc = length(location_list)

for(i in 1:nloc){
	location_sel = location_list[i]
	dat_resinflow_calc_fut_fl = dat_resinflow_calc_fut %>% filter(Location == location_sel)
	dat_resinflow_calc_hist_fl = dat_resinflow_calc_hist %>% filter(Location == location_sel)
	dat_resinflow_paleo1_calc_fl = dat_resinflow_paleo1_calc %>% filter(Location == location_sel)
	dat_resinflow_paleo2_calc_fl = dat_resinflow_paleo2_calc %>% filter(Location == location_sel)
	dat_resinflow_paleo3_calc_fl = dat_resinflow_paleo3_calc %>% filter(Location == location_sel)

	ggplot() + geom_line(data = dat_resinflow_paleo1_calc_fl, aes(x = Month, y = Value, group = Trace), colour = 'gray50') + geom_line(data = dat_resinflow_paleo2_calc_fl, aes(x = Month, y = Value, group = Trace), colour = 'gray50') + geom_line(data = dat_resinflow_paleo3_calc_fl, aes(x = Month, y = Value, group = Trace), colour = 'gray50') + geom_line(data = dat_resinflow_calc_fut_fl, aes(x = Month, y = Value, color=Scenario, group = Scenario), size=1.5) + scale_color_manual(values=c('#DE8C6D', '#FCCB89', '#FFEF94', '#C7B7D9', '#A6DDCA'))  + facet_wrap(~Period,ncol=1) + geom_line(data = dat_resinflow_calc_hist_fl, aes(x = Month, y = Value), colour = 'black', size = 1.5) + facet_wrap(~Period,ncol=1) + ylab('Mean Inflow (cfs)') + theme_bw() + theme(strip.background = element_rect(fill = FALSE)) + scale_x_continuous(breaks = 1:12, labels = month.abb) + ggtitle(paste(location_sel))
	ggsave(paste(out_dir, location_sel, '_mean_monthly_resinflow.png'), height = 6, width = 8)

}

dat_flowcentroid = dat_resinflow %>% left_join(resinflow_tbl) %>% filter(!is.na(Location)) %>% mutate(Flow_af = Value * 1.9835, Year = wyear(Date), DOWY = dowy(Date)) %>% mutate(tq = DOWY * Flow_af) %>% group_by(Trace, Location, Basin, Year) %>% summarise(tq_sum = sum(tq), q_sum = sum(Flow_af)) %>% mutate(Flow_centroid = round(tq_sum / q_sum)) %>% ungroup()

# centroid timing
dat_flowcentroid_hist = dat_flowcentroid %>% left_join(scenario_cc_tbl) %>% filter(Scenario == 'Historical') %>% group_by(Scenario, Location) %>% dplyr::summarise(Flow_centroid_hist = round(mean(Flow_centroid))) %>% ungroup() %>% dplyr::select(Location, Flow_centroid_hist)

dat_flowcentroid_fut = dat_flowcentroid %>% left_join(scenario_cc_tbl) %>% filter(Scenario != 'Historical') %>% group_by(Scenario, Period, Location) %>% dplyr::summarise(Flow_centroid = round(mean(Flow_centroid))) %>% dplyr::select(Scenario, Period, Location, Flow_centroid)

dat_flowcentroid_plot = dat_flowcentroid_fut %>% left_join(dat_flowcentroid_hist) %>% mutate(Flow_centroid_diff = Flow_centroid - Flow_centroid_hist) %>% mutate(Flow_centroid_diff_bin = cut(Flow_centroid_diff, c(-Inf, -35, -25, -15, -5, 5, 15, 25, 35, Inf), labels = c('>35d earlier', '25-35d earlier', '15-25d earlier', '5-15d earlier', '<5d', '5-15d later', '15-25d later', '25-35d later', '>35d later')))
dat_flowcentroid_plot$Scenario = factor(dat_flowcentroid_plot$Scenario, , levels = c('HD', 'HW', 'CT', 'WW', 'WD'))

location_list = unique(dat_flowcentroid_plot$Location)[-1]
period_list = unique(dat_flowcentroid_plot$Period)
plot_table = data.table(location = location_list, period = rep(period_list, each = length(location_list)))
nloc = nrow(plot_table)

for(i in 1:nloc){
	location_sel = plot_table$location[i]
	# period_sel = plot_table$period[i]
	dat_flowcentroid_plot_fl = dat_flowcentroid_plot %>% filter(Location == location_sel)
		# , Period == period_sel)
	ggplot() + geom_col(data = dat_flowcentroid_plot_fl, aes(x = Scenario, fill = Scenario, y = Flow_centroid_diff)) + geom_text(data = dat_flowcentroid_plot_fl, aes(x = Scenario, y = Flow_centroid_diff / 2, label = round(Flow_centroid_diff, 2)), colour = 'black') + theme_bw() + xlab('') + ylab('Days') + scale_fill_manual(values=c("#DE8C6D", "#FCCB89", "#FFEF94", "#C7B7D9", "#A6DDCA")) + ggtitle(location_sel) + facet_wrap(~Period, ncol = 1)
	ggsave(paste(out_dir, location_sel, '_flow_doychange.png'), height = 6, width = 8)
}


 



# try doy as well

dat_resinflow_calc = dat_resinflow %>% mutate(Year = wyear(Date), Month = month(Date), Day = day(Date), DOY = as.numeric(format(Date, '%j'))) %>% left_join(resinflow_tbl) %>% left_join(scenario_cc_tbl)
dat_resinflow_calc = dat_resinflow_calc %>% group_by(Location, Scenario, Period, DOY) %>% summarise(Value = mean(Value, na.rm = T)) %>% ungroup()

dat_resinflow_calc_hist = dat_resinflow_calc %>% filter(Scenario == 'Historical') %>% dplyr::select(-Period)
dat_resinflow_calc_fut = dat_resinflow_calc %>% filter(Scenario != 'Historical')

dat_resinflow_paleo1_calc = dat_resinflow_paleo1 %>% mutate(Year = wyear(Date), Month = month(Date), Day = day(Date), DOY = as.numeric(format(Date, '%j'))) %>% left_join(resinflow_tbl)
dat_resinflow_paleo1_calc = dat_resinflow_paleo1_calc %>% group_by(Location, Trace, DOY) %>% summarise(Value = mean(Value, na.rm = T)) 

dat_resinflow_paleo2_calc = dat_resinflow_paleo2 %>% mutate(Year = wyear(Date), Month = month(Date), Day = day(Date), DOY = as.numeric(format(Date, '%j'))) %>% left_join(resinflow_tbl)
dat_resinflow_paleo2_calc = dat_resinflow_paleo2_calc %>% group_by(Location, Trace, DOY) %>% summarise(Value = mean(Value, na.rm = T)) 

dat_resinflow_paleo3_calc = dat_resinflow_paleo3 %>% mutate(Year = wyear(Date), Month = month(Date), Day = day(Date), DOY = as.numeric(format(Date, '%j'))) %>% left_join(resinflow_tbl)
dat_resinflow_paleo3_calc = dat_resinflow_paleo3_calc %>% group_by(Location, Trace, DOY) %>% summarise(Value = mean(Value, na.rm = T)) 

dat_resinflow_calc_fut$Scenario = factor(dat_resinflow_calc_fut$Scenario,levels = c('HD', 'HW', 'CT', 'WW', 'WD'))

location_list = unique(dat_resinflow_calc_fut$Location)
nloc = length(location_list)

for(i in 1:nloc){
	location_sel = location_list[i]
	dat_resinflow_calc_fut_fl = dat_resinflow_calc_fut %>% filter(Location == location_sel)
	dat_resinflow_calc_hist_fl = dat_resinflow_calc_hist %>% filter(Location == location_sel)
	dat_resinflow_paleo1_calc_fl = dat_resinflow_paleo1_calc %>% filter(Location == location_sel)
	dat_resinflow_paleo2_calc_fl = dat_resinflow_paleo2_calc %>% filter(Location == location_sel)
	dat_resinflow_paleo3_calc_fl = dat_resinflow_paleo3_calc %>% filter(Location == location_sel)

	ggplot() + geom_line(data = dat_resinflow_paleo1_calc_fl, aes(x = DOY, y = Value, group = Trace), colour = 'gray50') + geom_line(data = dat_resinflow_paleo2_calc_fl, aes(x = DOY, y = Value, group = Trace), colour = 'gray50') + geom_line(data = dat_resinflow_paleo3_calc_fl, aes(x = DOY, y = Value, group = Trace), colour = 'gray50') + geom_line(data = dat_resinflow_calc_fut_fl, aes(x = DOY, y = Value, color=Scenario, group = Scenario), size = 0.8) + scale_color_manual(values=c('#DE8C6D', '#FCCB89', '#FFEF94', '#C7B7D9', '#A6DDCA'))  + facet_wrap(~Period,ncol=1) + geom_line(data = dat_resinflow_calc_hist_fl, aes(x = DOY, y = Value), colour = 'black', size = 0.8) + facet_wrap(~Period,ncol=1) + ylab('Mean Inflow (cfs)') + theme_bw() + theme(strip.background = element_rect(fill = FALSE)) + scale_x_continuous(breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335), labels = month.abb) + ggtitle(paste(location_sel)) + xlab('') 
	ggsave(paste(out_dir, location_sel, '_mean_doy_resinflow.png'), height = 6, width = 8)

}

# Reservoir Outflow
resoutflow_ls = read.rdf(paste0(scen_dir, 'ResOutflow.rdf'))
dat_resoutflow = Rdf2dt(resoutflow_ls)

resoutflow_paleo1_ls = read.rdf(paste0(scen_dir_paleo1, 'ResOutflow.rdf'))
dat_resoutflow_paleo1 = Rdf2dt(resoutflow_paleo1_ls)

resoutflow_paleo2_ls = read.rdf(paste0(scen_dir_paleo2, 'ResOutflow.rdf'))
dat_resoutflow_paleo2 = Rdf2dt(resoutflow_paleo2_ls)

resoutflow_paleo3_ls = read.rdf(paste0(scen_dir_paleo3, 'ResOutflow.rdf'))
dat_resoutflow_paleo3 = Rdf2dt(resoutflow_paleo3_ls)


dat_resoutflow_calc = dat_resoutflow %>% mutate(Year = wyear(Date), Month = month(Date), Day = day(Date)) %>% left_join(resoutflow_tbl) %>% left_join(scenario_cc_tbl)
dat_resoutflow_calc = dat_resoutflow_calc %>% group_by(Location, Scenario, Period, Month) %>% summarise(Value = mean(Value, na.rm = T)) %>% ungroup()

dat_resoutflow_calc_hist = dat_resoutflow_calc %>% filter(Scenario == 'Historical') %>% dplyr::select(-Period)
dat_resoutflow_calc_fut = dat_resoutflow_calc %>% filter(Scenario != 'Historical')

dat_resoutflow_paleo1_calc = dat_resoutflow_paleo1 %>% mutate(Year = wyear(Date), Month = month(Date), Day = day(Date)) %>% left_join(resoutflow_tbl)
dat_resoutflow_paleo1_calc = dat_resoutflow_paleo1_calc %>% group_by(Location, Trace, Month) %>% summarise(Value = mean(Value, na.rm = T)) 

dat_resoutflow_paleo2_calc = dat_resoutflow_paleo2 %>% mutate(Year = wyear(Date), Month = month(Date), Day = day(Date)) %>% left_join(resoutflow_tbl)
dat_resoutflow_paleo2_calc = dat_resoutflow_paleo2_calc %>% group_by(Location, Trace, Month) %>% summarise(Value = mean(Value, na.rm = T)) 

dat_resoutflow_paleo3_calc = dat_resoutflow_paleo3 %>% mutate(Year = wyear(Date), Month = month(Date), Day = day(Date)) %>% left_join(resoutflow_tbl)
dat_resoutflow_paleo3_calc = dat_resoutflow_paleo3_calc %>% group_by(Location, Trace, Month) %>% summarise(Value = mean(Value, na.rm = T)) 

dat_resoutflow_calc_fut$Scenario = factor(dat_resoutflow_calc_fut$Scenario,levels = c('HD', 'HW', 'CT', 'WW', 'WD'))

location_list = unique(dat_resoutflow_calc_fut$Location)
nloc = length(location_list)

for(i in 1:nloc){
	location_sel = location_list[i]
	dat_resoutflow_calc_fut_fl = dat_resoutflow_calc_fut %>% filter(Location == location_sel)
	dat_resoutflow_calc_hist_fl = dat_resoutflow_calc_hist %>% filter(Location == location_sel)
	dat_resoutflow_paleo1_calc_fl = dat_resoutflow_paleo1_calc %>% filter(Location == location_sel)
	dat_resoutflow_paleo2_calc_fl = dat_resoutflow_paleo2_calc %>% filter(Location == location_sel)
	dat_resoutflow_paleo3_calc_fl = dat_resoutflow_paleo3_calc %>% filter(Location == location_sel)

	ggplot() + geom_line(data = dat_resoutflow_paleo1_calc_fl, aes(x = Month, y = Value, group = Trace), colour = 'gray50') + geom_line(data = dat_resoutflow_paleo2_calc_fl, aes(x = Month, y = Value, group = Trace), colour = 'gray50') + geom_line(data = dat_resoutflow_paleo3_calc_fl, aes(x = Month, y = Value, group = Trace), colour = 'gray50') + geom_line(data = dat_resoutflow_calc_fut_fl, aes(x = Month, y = Value, color=Scenario, group = Scenario), size=1.5) + scale_color_manual(values=c('#DE8C6D', '#FCCB89', '#FFEF94', '#C7B7D9', '#A6DDCA'))  + facet_wrap(~Period,ncol=1) + geom_line(data = dat_resoutflow_calc_hist_fl, aes(x = Month, y = Value), colour = 'black', size = 1.5) + facet_wrap(~Period,ncol=1) + ylab('Mean Inflow (cfs)') + theme_bw() + theme(strip.background = element_rect(fill = FALSE)) + scale_x_continuous(breaks = 1:12, labels = month.abb) + ggtitle(paste(location_sel))
	ggsave(paste(out_dir, location_sel, '_mean_monthly_resoutflow.png'), height = 6, width = 8)
}