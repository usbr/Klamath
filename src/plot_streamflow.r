#
#Main program to:
# create calibration plots of streamflow
#
# Bureau of Reclamation July 2018
#######################################################################

#default - clear namespace
rm(list=ls())
library(plyr)
library(dplyr)
library(readr)
library(data.table)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(tools)
library(stringr)
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
library(cowplot)


wyear = function(x, start_month=10) {
  x.wyr = ifelse(lubridate::month(x)>=start_month, lubridate::year(x)+1, lubridate::year(x))
  return(x.wyr)
}


setwd('C:/Users/MMcguire/Documents/GitHub/Klamath/')
dirInp = 'C:/Users/MMcguire/Documents/GitHub/Klamath/'
dirOup = 'C:/Users/MMcguire/Documents/GitHub/Klamath/'


lookup = fread('lib/cal_loc_lookup.csv')
ctFiles = nrow(lookup)

# Observed Streamflow
datFlow = data.table()
for(iterFile in 1:ctFiles){
  ObsfilePath = paste0('data/NCAR/',lookup$Observed[iterFile])
  datTmp = fread(ObsfilePath)
  datTmp$date = as.Date(paste(datTmp$year,datTmp$mo,datTmp$dy,sep='-'), format = "%Y-%m-%d")
  datTmp = datTmp %>% mutate(WYear = wyear(date))      # add water year column
  datTmp = datTmp %>% dplyr::mutate(Location = lookup$ID[iterFile])
  datTmp = datTmp %>% dplyr::mutate(Type = 'Observed')
	datFlow = bind_rows(datFlow, datTmp)
}

# Simulated Streamflow
for(iterFile in 1:ctFiles){
  SimfilePath = paste0('data/NCAR/',lookup$Simulated[iterFile])
  datTmp = fread(SimfilePath)
  datTmp$date = as.Date(paste(datTmp$year,datTmp$mo,datTmp$dy,sep='-'), format = "%Y-%m-%d")
  datTmp = datTmp %>% mutate(WYear = wyear(date))         # add water year column
  datTmp = datTmp %>% dplyr::mutate(Location = lookup$ID[iterFile])
  datTmp = datTmp %>% dplyr::mutate(Type = 'Simulated')
	datFlow = bind_rows(datFlow, datTmp)
}

##############################################################################

datPlot = datFlow %>% filter(Location == 'CLEAR') %>% dplyr::mutate(Lbl = 'Clear Lake Inflow')
datPlot1 = datPlot %>% filter(WYear>=1980 & WYear<=1989)

P1 = ggplot(data = datPlot1) +
  geom_line(aes(x = date, y = flow_cfs, colour = Type)) +
  scale_colour_manual(values = c('black', 'blue')) +
#  scale_x_continuous(labels = function(x) round(as.numeric(x), digits=0)) +
  scale_x_date(date_breaks = "2 years", date_labels="%Y") +
  xlab('') +
  ylab('Flow (cfs)') +
  ylim(0,2500) +
  theme(
    axis.line.x=element_line(size=0.5, colour = 'gray60'),
    axis.line.y=element_line(size=0.5, colour = 'gray60'),
    axis.line=element_blank(),
    axis.text.x=element_text(angle = 90, hjust = 0, vjust = 0.5, size = 10),
    axis.text.y=element_text(hjust = 0, vjust = 0.5, size = 10),
    axis.title.x=element_blank(),
#    axis.title.y=element_blank(),
    legend.position="NA",
    legend.title=element_blank(),
    panel.background=element_blank(),
    panel.border=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_blank(),
    strip.background = element_blank(),
    strip.text.x=element_text(size = 10),
    strip.text.y=element_text(size = 10)
  )

datPlot2 = datPlot %>% filter(WYear>=1990 & WYear<=1999)
P2 = ggplot(data = datPlot2) +
  geom_line(aes(x = date, y = flow_cfs, colour = Type)) +
  scale_colour_manual(values = c('black', 'blue')) +
#  scale_x_continuous(labels = function(x) round(as.numeric(x), digits=0)) +
  scale_x_date(date_breaks = "2 years", date_labels="%Y") +
  ylim(0,2500) +
  xlab('') +
  ylab('Flow (cfs)') +
  theme(
    axis.line.x=element_line(size=0.5, colour = 'gray60'),
    axis.line.y=element_line(size=0.5, colour = 'gray60'),
    axis.line=element_blank(),
    axis.text.x=element_text(angle = 90, hjust = 0, vjust = 0.5, size = 10),
    axis.text.y=element_text(hjust = 0, vjust = 0.5, size = 10),
    axis.title.x=element_blank(),
#    axis.title.y=element_blank(),
    legend.position="NA",
    legend.title=element_blank(),
    panel.background=element_blank(),
    panel.border=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_blank(),
    strip.background = element_blank(),
    strip.text.x=element_text(size = 10),
    strip.text.y=element_text(size = 10)
  )

datPlot3 = datPlot %>%
  filter(WYear>=1982 & WYear<=1999) %>%
  group_by(Lbl,Type,mo) %>% summarise(flow_avg = mean(flow_cfs)) %>% ungroup
datPlot3$mon = month.abb[datPlot3$mo]
datPlot3$mon = factor(datPlot3$mon,
        levels = c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'))
P3 = ggplot(data = datPlot3) +
  geom_line(aes(x = as.numeric(mon), y = flow_avg, colour = Type)) +
  scale_colour_manual(values = c('black', 'blue')) +
  scale_x_discrete(name = "months", limits = datPlot3$mon)+
#  scale_x_continuous(labels = function(x) round(as.numeric(x), digits=0)) +
  xlab('') +
  ylab('Flow (cfs)') +
  theme(
    axis.line.x=element_line(size=0.5, colour = 'gray60'),
    axis.line.y=element_line(size=0.5, colour = 'gray60'),
    axis.line=element_blank(),
    axis.text.x=element_text(angle = 90, hjust = 0, vjust = 0.5, size = 10),
    axis.text.y=element_text(hjust = 0, vjust = 0.5, size = 10),
    axis.title.x=element_blank(),
#    axis.title.y=element_blank(),
    legend.position="bottom",
    legend.title=element_blank(),
    panel.background=element_blank(),
    panel.border=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_blank(),
    strip.background = element_blank(),
    strip.text.x=element_text(size = 10),
    strip.text.y=element_text(size = 10)
  )

plot_grid(P1, P2, P3, align = "v", nrow = 3, rel_heights = c(1/3, 1/3, 1/3))
ggsave(paste0(dirOup, 'output/Clear.png'), height = 7, width = 6)

####################################################################

datPlot = datFlow %>% filter(Location == 'IGERB') %>% dplyr::mutate(Lbl = 'Gerber Reservoir Inflow')
datPlot1 = datPlot %>% filter(WYear>=1980 & WYear<=1989)

P1 = ggplot(data = datPlot1) +
  geom_line(aes(x = date, y = flow_cfs, colour = Type)) +
  scale_colour_manual(values = c('black', 'blue')) +
#  scale_x_continuous(labels = function(x) round(as.numeric(x), digits=0)) +
  scale_x_date(date_breaks = "2 years", date_labels="%Y") +
  ylim(0,2000) +
  xlab('') +
  ylab('Flow (cfs)') +
  theme(
    axis.line.x=element_line(size=0.5, colour = 'gray60'),
    axis.line.y=element_line(size=0.5, colour = 'gray60'),
    axis.line=element_blank(),
    axis.text.x=element_text(angle = 90, hjust = 0, vjust = 0.5, size = 10),
    axis.text.y=element_text(hjust = 0, vjust = 0.5, size = 10),
    axis.title.x=element_blank(),
#    axis.title.y=element_blank(),
    legend.position="NA",
    legend.title=element_blank(),
    panel.background=element_blank(),
    panel.border=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_blank(),
    strip.background = element_blank(),
    strip.text.x=element_text(size = 10),
    strip.text.y=element_text(size = 10)
  )

datPlot2 = datPlot %>% filter(WYear>=1990 & WYear<=1999)
P2 = ggplot(data = datPlot2) +
  geom_line(aes(x = date, y = flow_cfs, colour = Type)) +
  scale_colour_manual(values = c('black', 'blue')) +
#  scale_x_continuous(labels = function(x) round(as.numeric(x), digits=0)) +
  scale_x_date(date_breaks = "2 years", date_labels="%Y") +
  ylim(0,2000) +
  xlab('') +
  ylab('Flow (cfs)') +
  theme(
    axis.line.x=element_line(size=0.5, colour = 'gray60'),
    axis.line.y=element_line(size=0.5, colour = 'gray60'),
    axis.line=element_blank(),
    axis.text.x=element_text(angle = 90, hjust = 0, vjust = 0.5, size = 10),
    axis.text.y=element_text(hjust = 0, vjust = 0.5, size = 10),
    axis.title.x=element_blank(),
#    axis.title.y=element_blank(),
    legend.position="NA",
    legend.title=element_blank(),
    panel.background=element_blank(),
    panel.border=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_blank(),
    strip.background = element_blank(),
    strip.text.x=element_text(size = 10),
    strip.text.y=element_text(size = 10)
  )

datPlot3 = datPlot %>%
  filter(WYear>=1982 & WYear<=1998) %>%
  group_by(Lbl,Type,mo) %>% summarise(flow_avg = mean(flow_cfs)) %>% ungroup
datPlot3$mon = month.abb[datPlot3$mo]
datPlot3$mon = factor(datPlot3$mon,
        levels = c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'))
P3 = ggplot(data = datPlot3) +
  geom_line(aes(x = as.numeric(mon), y = flow_avg, colour = Type)) +
  scale_colour_manual(values = c('black', 'blue')) +
  scale_x_discrete(name = "months", limits = datPlot3$mon)+
#  scale_x_continuous(labels = function(x) round(as.numeric(x), digits=0)) +
  xlab('') +
  ylab('Flow (cfs)') +
  theme(
    axis.line.x=element_line(size=0.5, colour = 'gray60'),
    axis.line.y=element_line(size=0.5, colour = 'gray60'),
    axis.line=element_blank(),
    axis.text.x=element_text(angle = 90, hjust = 0, vjust = 0.5, size = 10),
    axis.text.y=element_text(hjust = 0, vjust = 0.5, size = 10),
    axis.title.x=element_blank(),
#    axis.title.y=element_blank(),
    legend.position="bottom",
    legend.title=element_blank(),
    panel.background=element_blank(),
    panel.border=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_blank(),
    strip.background = element_blank(),
    strip.text.x=element_text(size = 10),
    strip.text.y=element_text(size = 10)
  )

plot_grid(P1, P2, P3, align = "v", nrow = 3, rel_heights = c(1/3, 1/3, 1/3))
ggsave(paste0(dirOup, 'output/Gerber.png'), height = 7, width = 6)

############################################################################


datPlot = datFlow %>% filter(Location == 'WILLI') %>% dplyr::mutate(Lbl = 'Williamson River')
datPlot1 = datPlot %>% filter(WYear>=1980 & WYear<=1994)

P1 = ggplot(data = datPlot1) +
  geom_line(aes(x = date, y = flow_cfs, colour = Type)) +
  scale_colour_manual(values = c('black', 'blue')) +
#  scale_x_continuous(labels = function(x) round(as.numeric(x), digits=0)) +
  scale_x_date(date_breaks = "2 years", date_labels="%Y") +
  lim(0,10000) +
  xlab('') +
  ylab('Flow (cfs)') +
  theme(
    axis.line.x=element_line(size=0.5, colour = 'gray60'),
    axis.line.y=element_line(size=0.5, colour = 'gray60'),
    axis.line=element_blank(),
    axis.text.x=element_text(angle = 90, hjust = 0, vjust = 0.5, size = 10),
    axis.text.y=element_text(hjust = 0, vjust = 0.5, size = 10),
    axis.title.x=element_blank(),
#    axis.title.y=element_blank(),
    legend.position="NA",
    legend.title=element_blank(),
    panel.background=element_blank(),
    panel.border=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_blank(),
    strip.background = element_blank(),
    strip.text.x=element_text(size = 10),
    strip.text.y=element_text(size = 10)
  )

datPlot2 = datPlot %>% filter(WYear>=1995 & WYear<=2009)
P2 = ggplot(data = datPlot2) +
  geom_line(aes(x = date, y = flow_cfs, colour = Type)) +
  scale_colour_manual(values = c('black', 'blue')) +
#  scale_x_continuous(labels = function(x) round(as.numeric(x), digits=0)) +
  scale_x_date(date_breaks = "2 years", date_labels="%Y") +
  ylim(0,10000) +
  xlab('') +
  ylab('Flow (cfs)') +
  theme(
    axis.line.x=element_line(size=0.5, colour = 'gray60'),
    axis.line.y=element_line(size=0.5, colour = 'gray60'),
    axis.line=element_blank(),
    axis.text.x=element_text(angle = 90, hjust = 0, vjust = 0.5, size = 10),
    axis.text.y=element_text(hjust = 0, vjust = 0.5, size = 10),
    axis.title.x=element_blank(),
#    axis.title.y=element_blank(),
    legend.position="NA",
    legend.title=element_blank(),
    panel.background=element_blank(),
    panel.border=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_blank(),
    strip.background = element_blank(),
    strip.text.x=element_text(size = 10),
    strip.text.y=element_text(size = 10)
  )

datPlot3 = datPlot %>%
  filter(WYear>=1981 & WYear<=2008) %>%
  group_by(Lbl,Type,mo) %>% summarise(flow_avg = mean(flow_cfs)) %>% ungroup
datPlot3$mon = month.abb[datPlot3$mo]
datPlot3$mon = factor(datPlot3$mon,
        levels = c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'))
P3 = ggplot(data = datPlot3) +
  geom_line(aes(x = as.numeric(mon), y = flow_avg, colour = Type)) +
  scale_colour_manual(values = c('black', 'blue')) +
  scale_x_discrete(name = "months", limits = datPlot3$mon)+
#  scale_x_continuous(labels = function(x) round(as.numeric(x), digits=0)) +
  xlab('') +
  ylab('Flow (cfs)') +
  theme(
    axis.line.x=element_line(size=0.5, colour = 'gray60'),
    axis.line.y=element_line(size=0.5, colour = 'gray60'),
    axis.line=element_blank(),
    axis.text.x=element_text(angle = 90, hjust = 0, vjust = 0.5, size = 10),
    axis.text.y=element_text(hjust = 0, vjust = 0.5, size = 10),
    axis.title.x=element_blank(),
#    axis.title.y=element_blank(),
    legend.position="bottom",
    legend.title=element_blank(),
    panel.background=element_blank(),
    panel.border=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_blank(),
    strip.background = element_blank(),
    strip.text.x=element_text(size = 10),
    strip.text.y=element_text(size = 10)
  )

plot_grid(P1, P2, P3, align = "v", nrow = 3, rel_heights = c(1/3, 1/3, 1/3))
ggsave(paste0(dirOup, 'output/WILLI.png'), height = 7, width = 6)

###############################################################################



datPlot = datFlow %>% filter(Location == 'SPRAG') %>% dplyr::mutate(Lbl = 'Sprague River')
datPlot1 = datPlot %>% filter(WYear>=1980 & WYear<=1994)

P1 = ggplot(data = datPlot1) +
  geom_line(aes(x = date, y = flow_cfs, colour = Type)) +
  scale_colour_manual(values = c('black', 'blue')) +
#  scale_x_continuous(labels = function(x) round(as.numeric(x), digits=0)) +
  scale_x_date(date_breaks = "2 years", date_labels="%Y") +
  ylim(0,8000) +
  xlab('') +
  ylab('Flow (cfs)') +
  theme(
    axis.line.x=element_line(size=0.5, colour = 'gray60'),
    axis.line.y=element_line(size=0.5, colour = 'gray60'),
    axis.line=element_blank(),
    axis.text.x=element_text(angle = 90, hjust = 0, vjust = 0.5, size = 10),
    axis.text.y=element_text(hjust = 0, vjust = 0.5, size = 10),
    axis.title.x=element_blank(),
#    axis.title.y=element_blank(),
    legend.position="NA",
    legend.title=element_blank(),
    panel.background=element_blank(),
    panel.border=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_blank(),
    strip.background = element_blank(),
    strip.text.x=element_text(size = 10),
    strip.text.y=element_text(size = 10)
  )

datPlot2 = datPlot %>% filter(WYear>=1995 & WYear<=2009)
P2 = ggplot(data = datPlot2) +
  geom_line(aes(x = date, y = flow_cfs, colour = Type)) +
  scale_colour_manual(values = c('black', 'blue')) +
#  scale_x_continuous(labels = function(x) round(as.numeric(x), digits=0)) +
  scale_x_date(date_breaks = "2 years", date_labels="%Y") +
  ylim(0,8000) +
  xlab('') +
  ylab('Flow (cfs)') +
  theme(
    axis.line.x=element_line(size=0.5, colour = 'gray60'),
    axis.line.y=element_line(size=0.5, colour = 'gray60'),
    axis.line=element_blank(),
    axis.text.x=element_text(angle = 90, hjust = 0, vjust = 0.5, size = 10),
    axis.text.y=element_text(hjust = 0, vjust = 0.5, size = 10),
    axis.title.x=element_blank(),
#    axis.title.y=element_blank(),
    legend.position="NA",
    legend.title=element_blank(),
    panel.background=element_blank(),
    panel.border=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_blank(),
    strip.background = element_blank(),
    strip.text.x=element_text(size = 10),
    strip.text.y=element_text(size = 10)
  )

datPlot3 = datPlot %>%
  filter(WYear>=1981 & WYear<=2008) %>%
  group_by(Lbl,Type,mo) %>% summarise(flow_avg = mean(flow_cfs)) %>% ungroup
datPlot3$mon = month.abb[datPlot3$mo]
datPlot3$mon = factor(datPlot3$mon,
        levels = c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'))
P3 = ggplot(data = datPlot3) +
  geom_line(aes(x = as.numeric(mon), y = flow_avg, colour = Type)) +
  scale_colour_manual(values = c('black', 'blue')) +
  scale_x_discrete(name = "months", limits = datPlot3$mon)+
#  scale_x_continuous(labels = function(x) round(as.numeric(x), digits=0)) +
  xlab('') +
  ylab('Flow (cfs)') +
  theme(
    axis.line.x=element_line(size=0.5, colour = 'gray60'),
    axis.line.y=element_line(size=0.5, colour = 'gray60'),
    axis.line=element_blank(),
    axis.text.x=element_text(angle = 90, hjust = 0, vjust = 0.5, size = 10),
    axis.text.y=element_text(hjust = 0, vjust = 0.5, size = 10),
    axis.title.x=element_blank(),
#    axis.title.y=element_blank(),
    legend.position="bottom",
    legend.title=element_blank(),
    panel.background=element_blank(),
    panel.border=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_blank(),
    strip.background = element_blank(),
    strip.text.x=element_text(size = 10),
    strip.text.y=element_text(size = 10)
  )

plot_grid(P1, P2, P3, align = "v", nrow = 3, rel_heights = c(1/3, 1/3, 1/3))
ggsave(paste0(dirOup, 'output/SPRAG.png'), height = 7, width = 6)

#######################################################################################

datPlot = datFlow %>% filter(Location == 'WILLILOC') %>% dplyr::mutate(Lbl = 'Upper Williamson River')
datPlot1 = datPlot %>% filter(WYear>=1980 & WYear<=1994)

P1 = ggplot(data = datPlot1) +
  geom_line(aes(x = date, y = flow_cfs, colour = Type)) +
  scale_colour_manual(values = c('black', 'blue')) +
#  scale_x_continuous(labels = function(x) round(as.numeric(x), digits=0)) +
  scale_x_date(date_breaks = "2 years", date_labels="%Y") +
  ylim(0,2000) +
  xlab('') +
  ylab('Flow (cfs)') +
  theme(
    axis.line.x=element_line(size=0.5, colour = 'gray60'),
    axis.line.y=element_line(size=0.5, colour = 'gray60'),
    axis.line=element_blank(),
    axis.text.x=element_text(angle = 90, hjust = 0, vjust = 0.5, size = 10),
    axis.text.y=element_text(hjust = 0, vjust = 0.5, size = 10),
    axis.title.x=element_blank(),
#    axis.title.y=element_blank(),
    legend.position="NA",
    legend.title=element_blank(),
    panel.background=element_blank(),
    panel.border=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_blank(),
    strip.background = element_blank(),
    strip.text.x=element_text(size = 10),
    strip.text.y=element_text(size = 10)
  )

datPlot2 = datPlot %>% filter(WYear>=1995 & WYear<=2009)
P2 = ggplot(data = datPlot2) +
  geom_line(aes(x = date, y = flow_cfs, colour = Type)) +
  scale_colour_manual(values = c('black', 'blue')) +
#  scale_x_continuous(labels = function(x) round(as.numeric(x), digits=0)) +
  scale_x_date(date_breaks = "2 years", date_labels="%Y") +
  ylim(0,2000) +
  xlab('') +
  ylab('Flow (cfs)') +
  theme(
    axis.line.x=element_line(size=0.5, colour = 'gray60'),
    axis.line.y=element_line(size=0.5, colour = 'gray60'),
    axis.line=element_blank(),
    axis.text.x=element_text(angle = 90, hjust = 0, vjust = 0.5, size = 10),
    axis.text.y=element_text(hjust = 0, vjust = 0.5, size = 10),
    axis.title.x=element_blank(),
#    axis.title.y=element_blank(),
    legend.position="NA",
    legend.title=element_blank(),
    panel.background=element_blank(),
    panel.border=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_blank(),
    strip.background = element_blank(),
    strip.text.x=element_text(size = 10),
    strip.text.y=element_text(size = 10)
  )

datPlot3 = datPlot %>%
  filter(WYear>=1981 & WYear<=2008) %>%
  group_by(Lbl,Type,mo) %>% summarise(flow_avg = mean(flow_cfs)) %>% ungroup
datPlot3$mon = month.abb[datPlot3$mo]
datPlot3$mon = factor(datPlot3$mon,
        levels = c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'))
P3 = ggplot(data = datPlot3) +
  geom_line(aes(x = as.numeric(mon), y = flow_avg, colour = Type)) +
  scale_colour_manual(values = c('black', 'blue')) +
  scale_x_discrete(name = "months", limits = datPlot3$mon)+
#  scale_x_continuous(labels = function(x) round(as.numeric(x), digits=0)) +
  xlab('') +
  ylab('Flow (cfs)') +
  theme(
    axis.line.x=element_line(size=0.5, colour = 'gray60'),
    axis.line.y=element_line(size=0.5, colour = 'gray60'),
    axis.line=element_blank(),
    axis.text.x=element_text(angle = 90, hjust = 0, vjust = 0.5, size = 10),
    axis.text.y=element_text(hjust = 0, vjust = 0.5, size = 10),
    axis.title.x=element_blank(),
#    axis.title.y=element_blank(),
    legend.position="bottom",
    legend.title=element_blank(),
    panel.background=element_blank(),
    panel.border=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_blank(),
    strip.background = element_blank(),
    strip.text.x=element_text(size = 10),
    strip.text.y=element_text(size = 10)
  )

plot_grid(P1, P2, P3, align = "v", nrow = 3, rel_heights = c(1/3, 1/3, 1/3))
ggsave(paste0(dirOup, 'output/WILLILOC.png'), height = 7, width = 6)
