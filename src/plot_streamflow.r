#
#Main program to:
#  (1) file of means by month of flow
#      units are:  ??
#
#  (2) file of areas under the mean hydrographs
#
#  (3) to be used as input in runoff timing analysis

#U.S. Bureau of Reclamation: August 2013.
#
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

#base directory for dependencies and user input file for run setup
#dirpath="C:/Projects/A671F Upper Missouri Basin Impacts Assessment/Analysis/Hydrology/bias correction and disagg/Bias Correction and Disaggregation"
dirpath="T:/WaterResources/PlanningOperations/8210_Projects/PolicyOffice/Missouri Basin Impacts Assessment/Analysis/Hydrology/bias correction and disagg/Bias Correction and Disaggregation"
#NATURAL FLOW

# paleo
p.list = paste(dirpath,"/paleo.list",sep="")
p.list.read = fread(p.list, header=FALSE)
list.p <- list()
for(n in 1:length(p.list.read$V1)){
	p.fin = paste(dirpath,"/",p.list.read$V1[n],sep="")
	p.in <- read.csv(p.fin, stringsAsFactors=FALSE)
	sites.p <- colnames(p.in)
	sites.p <- sites.p[-1]
	nsites.p <- length(sites.p)
	list.p[[n]] <- p.in
}

# historical
h.fin = paste(dirpath,"/mbia_08172017_daily_historical-flows.csv",sep="")
h.in <- read.csv(h.fin, stringsAsFactors=FALSE)
sites.h <- colnames(h.in)
sites.h <- sites.h[-1]

# climate change scenarios
c.list = paste(dirpath,"/future.list",sep="")
c.list.read = fread(c.list, header=FALSE)
list.c <- list()
for(n in 1:length(c.list.read$V1)){
	c.fin = paste(dirpath,"/",c.list.read$V1[n],sep="")
	c.in <- read.csv(c.fin, stringsAsFactors=FALSE)
	sites.c <- colnames(c.in)
	sites.c <- sites.c[-1]
	nsites.c <- length(sites.c)
	list.c[[n]] <- c.in
}

#OUTDIR="C:/Projects/A671F Upper Missouri Basin Impacts Assessment/Analysis/Hydrology/bias correction and disagg/Bias Correction and Disaggregation/plots"
OUTDIR="T:/WaterResources/PlanningOperations/8210_Projects/PolicyOffice/Missouri Basin Impacts Assessment/Analysis/Hydrology/bias correction and disagg/Bias Correction and Disaggregation/plots_10272017"

slist <- read.table(paste(dirpath,"/scenariolist.txt",sep=""),sep="\t",stringsAsFactors=FALSE)
		
for(n in 1:nsites.c){
#for(n in 1:2){
	site <- sites.c[n]
	#create array for mean flows by month
				   #data, nrow, ncol
	meansim=matrix(0,34,12) #35 traces total including 1 historical, 15 CC, and 19 paleo
	
	# historical
	#-------------------------------------------------------------------
		inflow.h <- data.frame(h.in[ , grep(site, colnames(h.in))])
		inflow.h$date <- as.Date(h.in[,1], format = "%Y-%m-%d")
		inflow.h <- inflow.h[which(inflow.h$date >= as.Date("1949-10-01") & inflow.h$date <= as.Date("1999-09-30")),]
		colnames(inflow.h) <- c("hist","date")
		
		inflow.h$Mon <- as.numeric(format(inflow.h$date, format="%m"))
		inflow.h$YR <- as.numeric(format(inflow.h$date, format="%Y"))
		inflow.h$WY <- ifelse(inflow.h$Mon > 9,inflow.h$YR+1,inflow.h$YR)
		inflow.h$JD <- format(inflow.h$date, "%j")

		#need to compute averages by month...possibly use this
		mean.sim<-rep(0,12)
		mean.sim[4]<-with(inflow.h, mean(inflow.h[,1][inflow.h$Mon==1]))
		mean.sim[5]<-with(inflow.h, mean(inflow.h[,1][inflow.h$Mon==2]))
		mean.sim[6]<-with(inflow.h, mean(inflow.h[,1][inflow.h$Mon==3]))
		mean.sim[7]<-with(inflow.h, mean(inflow.h[,1][inflow.h$Mon==4]))
		mean.sim[8]<-with(inflow.h, mean(inflow.h[,1][inflow.h$Mon==5]))
		mean.sim[9]<-with(inflow.h, mean(inflow.h[,1][inflow.h$Mon==6]))
		mean.sim[10]<-with(inflow.h, mean(inflow.h[,1][inflow.h$Mon==7]))
		mean.sim[11]<-with(inflow.h, mean(inflow.h[,1][inflow.h$Mon==8]))
		mean.sim[12]<-with(inflow.h, mean(inflow.h[,1][inflow.h$Mon==9]))
		mean.sim[1]<-with(inflow.h, mean(inflow.h[,1][inflow.h$Mon==10]))
		mean.sim[2]<-with(inflow.h, mean(inflow.h[,1][inflow.h$Mon==11]))
		mean.sim[3]<-with(inflow.h, mean(inflow.h[,1][inflow.h$Mon==12]))
		meansim[1,]<-mean.sim
	#-------------------------------------------------------------------
		
	# climate change scenarios
	#-------------------------------------------------------------------
		for(scen in 2:16){  #number of cc scenarios
			scenario <- paste(slist[ which(slist[,3]==scen),1],slist[ which(slist[,3]==scen),2],sep="")
			inflow.c <- data.frame(list.c[[scen-1]][ , grep(site, colnames(list.c[[scen-1]]))])
			inflow.c$date <- as.Date(unlist(list.c[[scen-1]][1]), format = "%Y-%m-%d")
			inflow.c <- inflow.c[which(inflow.c$date >= as.Date("1949-10-01") & inflow.c$date <= as.Date("1999-09-30")),]
			colnames(inflow.c) <- c(scenario,"date")
			inflow.c$Mon <- as.numeric(format(inflow.c$date, format="%m"))
			inflow.c$YR <- as.numeric(format(inflow.c$date, format="%Y"))
			inflow.c$WY <- ifelse(inflow.c$Mon > 9,inflow.c$YR+1,inflow.c$YR)
			inflow.c$JD <- format(inflow.c$date, "%j")

			#need to compute averages by month...possibly use this
			mean.sim<-rep(0,12)
			mean.sim[4]<-with(inflow.c, mean(inflow.c[,1][inflow.c$Mon==1]))
			mean.sim[5]<-with(inflow.c, mean(inflow.c[,1][inflow.c$Mon==2]))
			mean.sim[6]<-with(inflow.c, mean(inflow.c[,1][inflow.c$Mon==3]))
			mean.sim[7]<-with(inflow.c, mean(inflow.c[,1][inflow.c$Mon==4]))
			mean.sim[8]<-with(inflow.c, mean(inflow.c[,1][inflow.c$Mon==5]))
			mean.sim[9]<-with(inflow.c, mean(inflow.c[,1][inflow.c$Mon==6]))
			mean.sim[10]<-with(inflow.c, mean(inflow.c[,1][inflow.c$Mon==7]))
			mean.sim[11]<-with(inflow.c, mean(inflow.c[,1][inflow.c$Mon==8]))
			mean.sim[12]<-with(inflow.c, mean(inflow.c[,1][inflow.c$Mon==9]))
			mean.sim[1]<-with(inflow.c, mean(inflow.c[,1][inflow.c$Mon==10]))
			mean.sim[2]<-with(inflow.c, mean(inflow.c[,1][inflow.c$Mon==11]))
			mean.sim[3]<-with(inflow.c, mean(inflow.c[,1][inflow.c$Mon==12]))

			meansim[scen,]<-mean.sim
			
		} #traces loop
	#-------------------------------------------------------------------

	# paleo
	#-------------------------------------------------------------------
		for(scen in 17:34){  #number of paleo traces
			scenario <- slist[scen,2]
			inflow.p <- data.frame(list.p[[scen-16]][ , grep(site, colnames(list.p[[scen-16]]))])
			inflow.p$date <- as.Date(unlist(list.p[[scen-16]][1]), format = "%Y-%m-%d")
			inflow.p <- inflow.p[which(inflow.p$date >= as.Date("1950-10-01") & inflow.p$date <= as.Date("1999-09-30")),]
			colnames(inflow.p) <- c(scenario,"date")
			inflow.p$Mon <- as.numeric(format(inflow.p$date, format="%m"))
			inflow.p$YR <- as.numeric(format(inflow.p$date, format="%Y"))
			inflow.p$WY <- ifelse(inflow.p$Mon > 9,inflow.p$YR+1,inflow.p$YR)
			inflow.p$JD <- format(inflow.p$date, "%j")

			#need to compute averages by month...possibly use this
			mean.sim<-rep(0,12)
			mean.sim[4]<-with(inflow.p, mean(inflow.p[,1][inflow.p$Mon==1]))
			mean.sim[5]<-with(inflow.p, mean(inflow.p[,1][inflow.p$Mon==2]))
			mean.sim[6]<-with(inflow.p, mean(inflow.p[,1][inflow.p$Mon==3]))
			mean.sim[7]<-with(inflow.p, mean(inflow.p[,1][inflow.p$Mon==4]))
			mean.sim[8]<-with(inflow.p, mean(inflow.p[,1][inflow.p$Mon==5]))
			mean.sim[9]<-with(inflow.p, mean(inflow.p[,1][inflow.p$Mon==6]))
			mean.sim[10]<-with(inflow.p, mean(inflow.p[,1][inflow.p$Mon==7]))
			mean.sim[11]<-with(inflow.p, mean(inflow.p[,1][inflow.p$Mon==8]))
			mean.sim[12]<-with(inflow.p, mean(inflow.p[,1][inflow.p$Mon==9]))
			mean.sim[1]<-with(inflow.p, mean(inflow.p[,1][inflow.p$Mon==10]))
			mean.sim[2]<-with(inflow.p, mean(inflow.p[,1][inflow.p$Mon==11]))
			mean.sim[3]<-with(inflow.p, mean(inflow.p[,1][inflow.p$Mon==12]))

			meansim[scen,]<-mean.sim
			
		} #traces loop
	#-------------------------------------------------------------------

	meansim = data.frame(meansim)
	colnames(meansim) = c("Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep")
	meansim$Scenario = as.character(slist[,1])
	meansim$Period = slist[,2]

	# meansim.h <- meansim[1,]
	# meansim.c <- meansim[2:16,]
	# meansim.p <- meansim[17:35,]
	#meansim.p$Scenario <- "P"
	
	meansim[35,]<-meansim[which(meansim$Scenario=="H"),]
	meansim[36,]<-meansim[which(meansim$Scenario=="H"),]
	meansim[1,14] = "2020"
	meansim[35,14] = "2050"
	meansim[36,14] = "2080"
	meansim[37,]<-meansim[which(meansim$Scenario=="P1"),]
	meansim[37,14]<-"2050"
	meansim[38,]<-meansim[which(meansim$Scenario=="P2"),]
	meansim[38,14]<-"2050"
	meansim[39,]<-meansim[which(meansim$Scenario=="P3"),]
	meansim[39,14]<-"2050"
	meansim[40,]<-meansim[which(meansim$Scenario=="P4"),]
	meansim[40,14]<-"2050"
	meansim[41,]<-meansim[which(meansim$Scenario=="P5"),]
	meansim[41,14]<-"2050"
	meansim[42,]<-meansim[which(meansim$Scenario=="P6"),]
	meansim[42,14]<-"2050"
	meansim[43,]<-meansim[which(meansim$Scenario=="P7"),]
	meansim[43,14]<-"2050"
	meansim[44,]<-meansim[which(meansim$Scenario=="P8"),]
	meansim[44,14]<-"2050"
	meansim[45,]<-meansim[which(meansim$Scenario=="P9"),]
	meansim[45,14]<-"2050"
	meansim[46,]<-meansim[which(meansim$Scenario=="P10"),]
	meansim[46,14]<-"2050"
	meansim[47,]<-meansim[which(meansim$Scenario=="P11"),]
	meansim[47,14]<-"2050"
	meansim[48,]<-meansim[which(meansim$Scenario=="P12"),]
	meansim[48,14]<-"2050"
	meansim[49,]<-meansim[which(meansim$Scenario=="P13"),]
	meansim[49,14]<-"2050"
	meansim[50,]<-meansim[which(meansim$Scenario=="P14"),]
	meansim[50,14]<-"2050"
	meansim[51,]<-meansim[which(meansim$Scenario=="P15"),]
	meansim[51,14]<-"2050"
	meansim[52,]<-meansim[which(meansim$Scenario=="P16"),]
	meansim[52,14]<-"2050"
	meansim[53,]<-meansim[which(meansim$Scenario=="P17"),]
	meansim[53,14]<-"2050"
	meansim[54,]<-meansim[which(meansim$Scenario=="P18"),]
	meansim[54,14]<-"2050"
	# meansim[56,]<-meansim[which(meansim$Scenario=="P19"),]
	# meansim[56,14]<-"2050"
	
	meansim[55,]<-meansim[which(meansim$Scenario=="P1"),]
	meansim[55,14]<-"2080"
	meansim[56,]<-meansim[which(meansim$Scenario=="P2"),]
	meansim[56,14]<-"2080"
	meansim[57,]<-meansim[which(meansim$Scenario=="P3"),]
	meansim[57,14]<-"2080"
	meansim[58,]<-meansim[which(meansim$Scenario=="P4"),]
	meansim[58,14]<-"2080"
	meansim[59,]<-meansim[which(meansim$Scenario=="P5"),]
	meansim[59,14]<-"2080"
	meansim[60,]<-meansim[which(meansim$Scenario=="P6"),]
	meansim[60,14]<-"2080"
	meansim[61,]<-meansim[which(meansim$Scenario=="P7"),]
	meansim[61,14]<-"2080"
	meansim[62,]<-meansim[which(meansim$Scenario=="P8"),]
	meansim[62,14]<-"2080"
	meansim[63,]<-meansim[which(meansim$Scenario=="P9"),]
	meansim[63,14]<-"2080"
	meansim[64,]<-meansim[which(meansim$Scenario=="P10"),]
	meansim[64,14]<-"2080"
	meansim[65,]<-meansim[which(meansim$Scenario=="P11"),]
	meansim[65,14]<-"2080"
	meansim[66,]<-meansim[which(meansim$Scenario=="P12"),]
	meansim[66,14]<-"2080"
	meansim[67,]<-meansim[which(meansim$Scenario=="P13"),]
	meansim[67,14]<-"2080"
	meansim[68,]<-meansim[which(meansim$Scenario=="P14"),]
	meansim[68,14]<-"2080"
	meansim[69,]<-meansim[which(meansim$Scenario=="P15"),]
	meansim[69,14]<-"2080"
	meansim[70,]<-meansim[which(meansim$Scenario=="P16"),]
	meansim[70,14]<-"2080"
	meansim[71,]<-meansim[which(meansim$Scenario=="P17"),]
	meansim[71,14]<-"2080"
	meansim[72,]<-meansim[which(meansim$Scenario=="P18"),]
	meansim[72,14]<-"2080"
	# meansim[75,]<-meansim[which(meansim$Scenario=="P19"),]
	# meansim[75,14]<-"2080"
	
	meansim[17:34,14] <- "2020"
	
	meansim2 = meansim %>% gather(Scenario,Period)
	colnames(meansim2)<-c("Scenario","Period","Month","Flow")

	set_tbl = data.table(Scenario = c("P1","P2","P3","P4","P5","P6","P7","P8","P9","P10","P11","P12","P13","P14","P15","P16","P17","P18", "H","HD", "HW", "CT", "WW", "WD"), Set = factor(c(rep('P', 18), "H","HD", "HW", "CT", "WW", "WD"), levels = c('P', "H","HD", "HW", "CT", "WW", "WD")))

	meansim2 = meansim2 %>% left_join(set_tbl)
	meansim2$Scenario <- factor(meansim2$Scenario,levels = c("P1","P2","P3","P4","P5","P6","P7","P8","P9","P10","P11","P12","P13","P14","P15","P16","P17","P18", "H","HD", "HW", "CT", "WW", "WD"))
	meansim2$Month <- factor(meansim2$Month,levels = c("Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep"))
	meansim2$Period <- factor(meansim2$Period,levels = c("2020","2050", "2080"))
	
	ggplot() + geom_line(data = meansim2, aes(x = Month, y = Flow, color=Set, group=Scenario, size=Set)) + scale_color_manual(values=c("#AFAFAF","#000000","#DE8C6D", "#FCCB89", "#FFEF94", "#C7B7D9","#A6DDCA"), name = 'Scenario') + scale_size_manual(values = c(1, rep(1.5, 6)), name = 'Scenario') + facet_wrap(~Period,nrow=3) + ylab("Mean Monthly Natural Flow (cfs)") + theme_bw() + theme(strip.background = element_rect(fill = FALSE))
	ggsave(paste(OUTDIR,"/", site, '_mean_monthly_projected_hydrographs.png',sep=""), height = 6, width = 8)

}


