library(XML)

##########################################################################################################
#
# This script reads various file formats and performs other utility operations, including
# some verification tasks.
#
##########################################################################################################

#Function that reads an XML output file from the EVS for a single-valued
#measure, which includes ensemble scores and deterministic error statistics,
#and returns a paired list comprising the lead times (lead.times), events
#(events), numeric event values (events.numeric), climatological probabilities
#of the events (events.prob), score values (scores) and the lower and upper
#bounds of a main sampling interval (lower and upper) if available. The scores
#are contained in a matrix with events in the rows and lead times in the
#columns.  If there are several components to a score, specify the index of
#the component to return.
#
#The threshold "All data" is given an NA indicator in the first index of the
#returned thresholds

readEVSScores<-function(file, index=1) {
	doc = xmlRoot(xmlInternalTreeParse(file))
	#Read the lead times
	lead.times=as.numeric(unique(unlist(xpathApply(doc, "//lead_hour", xmlValue))))
	#Read the events
	events<-(unique(unlist(xpathApply(doc, "//threshold_value", xmlValue))))
	events<-gsub("GTE",">=",events)
	events<-gsub("LTE","<=",events)
	events<-gsub("GT",">",events)
	events<-gsub("LT","<",events)

	events.data<-gsub("All data","-9999 -9999",events)
	events.data<-gsub("Pr=","",events.data)
	events.data<-gsub(">= ","",events.data)
	events.data<-gsub("<= ","",events.data)
	events.data<-gsub("> ","",events.data)
	events.data<-gsub("< ","",events.data)
	events.data<-gsub("\\(|\\)","",events.data)

	events.data<-unlist(strsplit(events.data," "))
	events.numeric.all<-unlist(lapply(events.data,as.numeric))

	#Remove any probability references from the numeric thresholds
	if(sum(grepl("Pr",events))>0) {
		events.numeric<-events.numeric.all[seq(1, length(events.numeric.all), 2)]
	} else {
		events.numeric<-events.numeric.all
	}

	#Has probability thresholds?
	probs<-FALSE

	events.probs<-c()
	if(length(events.numeric.all)>1) {
		if(sum(grepl("Pr",events))>0) {
			events.probs<-events.numeric.all[seq(2, length(events.numeric.all), 2)]
			probs=TRUE
		} else {
			events.probs<-rep(NA,length(events.numeric))
		}
	}

	events.numeric[events.numeric==-9999]<-NA
	events.probs[events.probs==-9999]<-NA
	#In case some probabilities are associated with different real values (e.g. due to aggregation over missing metrics)
	#only use unique probability thresholds
	if(probs) {  #Probability thresholds are available, so check them
		ind<-match(unique(events.probs),events.probs)  #Find positions of unique probabilities
		events.probs<-events.probs[ind]
		events.numeric<-events.numeric[ind]
		events<-events[ind]
	}
	#Read the scores and intervals (if available)
	scores<-matrix(nrow=length(events),ncol=length(lead.times))
	lower<-matrix(nrow=length(events),ncol=length(lead.times))
	upper<-matrix(nrow=length(events),ncol=length(lead.times))

	eventsFound<-vector("character")
	eventsFound.numeric<-vector("double")
	eventsFound.probs<-vector("double")
	inc<-0
	for(j in 1:length(lead.times)) {
		tmp=doc[[j+2]]
		for(i in 1:length(events.numeric)) {
			threshold_node = tmp[2]$threshold[i]$threshold
			raw_data<-threshold_node[2]$data[1]$values
			if(!is.null(raw_data)) {
				#Allow additions of duplicate thresholds on first lead time
				#since only trying to avoid duplication across lead times.
				if(j==1 || match(events.numeric[i],eventsFound.numeric,nomatch=-999) == -999) {
					inc=inc+1
					eventsFound[inc]=events[i]
					eventsFound.numeric[inc]=events.numeric[i]
					eventsFound.probs[inc]=events.probs[i]
				}
				raw_data<-unlist(strsplit(xmlValue(raw_data),", "))
				scores[i,j]=as.numeric(raw_data[index])
				interval=threshold_node[2]$data[2]$sampling_intervals[1]$main_interval
				low=interval[2]$lower_bound[1]$values
				high=interval[3]$upper_bound[1]$values
				if(!is.null(low)) {
					raw_low<-unlist(strsplit(xmlValue(low),", "))
					raw_high<-unlist(strsplit(xmlValue(high),", "))
					lower[i,j]=as.numeric(raw_low[index])
					upper[i,j]=as.numeric(raw_high[index])
				}
				if(!is.na(scores[i,j]) && scores[i,j]==-999.0) {
					scores[i,j]=NA
					lower[i,j]=NA
					upper[i,j]=NA
				}
				if(!is.finite(scores[i,j])) {
					scores[i,j]=NA
					lower[i,j]=NA
					upper[i,j]=NA
				}
				if(!is.na(lower[i,j]) && lower[i,j]==-999.0) {
					lower[i,j]=NA
					upper[i,j]=NA
				}
				if(!is.na(upper[i,j]) && upper[i,j]==-999.0) {
					lower[i,j]=NA
					upper[i,j]=NA
				}
			}
		}
	}
	pairlist(lead.times=lead.times,events=eventsFound,events.numeric=eventsFound.numeric,events.probs=eventsFound.probs,
	scores=scores,lower=lower,upper=upper)
}

metrics_final=data.table()
#proc MAE data from xml files - NRCS
mae = readEVSScores('C:/Projects/KlamathRiverBasin/EVS_5.6/xml_output/WILLI.default_variable.NRCS50.Mean_absolute_error.xml')
colnames(mae$scores)<-c('L0','L720','L1440','L2160')
mae_tmp = data.table(mae$scores)
mae_tmp = mae_tmp %>% dplyr::mutate(Location = "WILLI", metric = "MAE",source='NRCS')
metrics_final = mae_tmp[1,]
mae = readEVSScores('C:/Projects/KlamathRiverBasin/EVS_5.6/xml_output/SPRAG.default_variable.NRCS50.Mean_absolute_error.xml')
colnames(mae$scores)<-c('L0','L720','L1440','L2160')
mae_tmp = data.table(mae$scores)
mae_tmp = mae_tmp %>% dplyr::mutate(Location = "SPRAG", metric = "MAE",source='NRCS')
metrics_final = bind_rows(metrics_final,mae_tmp[1,])

#proc Relative Mean Error (relative bias) data from xml files - NRCS
rme = readEVSScores('C:/Projects/KlamathRiverBasin/EVS_5.6/xml_output/WILLI.default_variable.NRCS50.Relative_mean_error.xml')
colnames(rme$scores)<-c('L0','L720','L1440','L2160')
rme_tmp = data.table(rme$scores)
rme_tmp = rme_tmp %>% dplyr::mutate(Location = "WILLI", metric = "RME",source='NRCS')
metrics_final = bind_rows(metrics_final,rme_tmp[1,])
rme = readEVSScores('C:/Projects/KlamathRiverBasin/EVS_5.6/xml_output/SPRAG.default_variable.NRCS50.Relative_mean_error.xml')
colnames(rme$scores)<-c('L0','L720','L1440','L2160')
rme_tmp = data.table(rme$scores)
rme_tmp = rme_tmp %>% dplyr::mutate(Location = "SPRAG", metric = "RME",source='NRCS')
metrics_final = bind_rows(metrics_final,rme_tmp[1,])

#proc Correlation Coefficient data from xml files - NRCS
cc = readEVSScores('C:/Projects/KlamathRiverBasin/EVS_5.6/xml_output/WILLI.default_variable.NRCS50.Correlation_coefficient.xml')
colnames(cc$scores)<-c('L0','L720','L1440','L2160')
cc_tmp = data.table(cc$scores)
cc_tmp = cc_tmp %>% dplyr::mutate(Location = "WILLI", metric = "COR",source='NRCS')
metrics_final = bind_rows(metrics_final,cc_tmp[1,])
cc = readEVSScores('C:/Projects/KlamathRiverBasin/EVS_5.6/xml_output/SPRAG.default_variable.NRCS50.Correlation_coefficient.xml')
colnames(cc$scores)<-c('L0','L720','L1440','L2160')
cc_tmp = data.table(cc$scores)
cc_tmp = cc_tmp %>% dplyr::mutate(Location = "SPRAG", metric = "COR",source='NRCS')
metrics_final = bind_rows(metrics_final,cc_tmp[1,])



#proc MAE data from xml files - NCAR
mae = readEVSScores('C:/Projects/KlamathRiverBasin/EVS_5.6/xml_output/WILLI.default_variable.NCAR.Mean_absolute_error.xml')
colnames(mae$scores)<-c('L0','L720','L1440','L2160')
mae_tmp = data.table(mae$scores)
mae_tmp = mae_tmp %>% dplyr::mutate(Location = "WILLI", metric = "MAE",source='NCAR')
metrics_final = bind_rows(metrics_final,mae_tmp[1,])
mae = readEVSScores('C:/Projects/KlamathRiverBasin/EVS_5.6/xml_output/SPRAG.default_variable.NCAR.Mean_absolute_error.xml')
colnames(mae$scores)<-c('L0','L720','L1440','L2160')
mae_tmp = data.table(mae$scores)
mae_tmp = mae_tmp %>% dplyr::mutate(Location = "SPRAG", metric = "MAE",source='NCAR')
metrics_final = bind_rows(metrics_final,mae_tmp[1,])

#proc Relative Mean Error (relative bias) data from xml files - NCAR
rme = readEVSScores('C:/Projects/KlamathRiverBasin/EVS_5.6/xml_output/WILLI.default_variable.NCAR.Relative_mean_error.xml')
colnames(rme$scores)<-c('L0','L720','L1440','L2160')
rme_tmp = data.table(rme$scores)
rme_tmp = rme_tmp %>% dplyr::mutate(Location = "WILLI", metric = "RME",source='NCAR')
metrics_final = bind_rows(metrics_final,rme_tmp[1,])
rme = readEVSScores('C:/Projects/KlamathRiverBasin/EVS_5.6/xml_output/SPRAG.default_variable.NCAR.Relative_mean_error.xml')
colnames(rme$scores)<-c('L0','L720','L1440','L2160')
rme_tmp = data.table(rme$scores)
rme_tmp = rme_tmp %>% dplyr::mutate(Location = "SPRAG", metric = "RME",source='NCAR')
metrics_final = bind_rows(metrics_final,rme_tmp[1,])

#proc Correlation Coefficient data from xml files - NCAR
cc = readEVSScores('C:/Projects/KlamathRiverBasin/EVS_5.6/xml_output/WILLI.default_variable.NCAR.Correlation_coefficient.xml')
colnames(cc$scores)<-c('L0','L720','L1440','L2160')
cc_tmp = data.table(cc$scores)
cc_tmp = cc_tmp %>% dplyr::mutate(Location = "WILLI", metric = "COR",source='NCAR')
metrics_final = bind_rows(metrics_final,cc_tmp[1,])
cc = readEVSScores('C:/Projects/KlamathRiverBasin/EVS_5.6/xml_output/SPRAG.default_variable.NCAR.Correlation_coefficient.xml')
colnames(cc$scores)<-c('L0','L720','L1440','L2160')
cc_tmp = data.table(cc$scores)
cc_tmp = cc_tmp %>% dplyr::mutate(Location = "SPRAG", metric = "COR",source='NCAR')
metrics_final = bind_rows(metrics_final,cc_tmp[1,])

#proc Brier Score data from xml files - NCAR
bs = readEVSScores('C:/Projects/KlamathRiverBasin/EVS_5.6/xml_output/WILLI.default_variable.NCAR.Brier_score.xml')
colnames(bs$scores)<-c('L0','L720','L1440','L2160')
bs_tmp = data.table(bs$scores)
bs_tmp = bs_tmp %>% dplyr::mutate(Location = "WILLI", metric = "BrierScore",source='NCAR')
metrics_final = bind_rows(metrics_final,bs_tmp[1,])
bs = readEVSScores('C:/Projects/KlamathRiverBasin/EVS_5.6/xml_output/SPRAG.default_variable.NCAR.Brier_score.xml')
colnames(bs$scores)<-c('L0','L720','L1440','L2160')
bs_tmp = data.table(bs$scores)
bs_tmp = bs_tmp %>% dplyr::mutate(Location = "SPRAG", metric = "BrierScore",source='NCAR')
metrics_final = bind_rows(metrics_final,bs_tmp[1,])

#proc Brier Score data from xml files - NRCS
bs = readEVSScores('C:/Projects/KlamathRiverBasin/EVS_5.6/xml_output/Will.default_variable.NRCS.Brier_score.xml')
colnames(bs$scores)<-c('L0','L720','L1440','L2160')
bs_tmp = data.table(bs$scores)
bs_tmp = bs_tmp %>% dplyr::mutate(Location = "WILLI", metric = "BrierScore",source='NRCS')
metrics_final = bind_rows(metrics_final,bs_tmp[1,])
bs = readEVSScores('C:/Projects/KlamathRiverBasin/EVS_5.6/xml_output/Sprag.default_variable.NRCS.Brier_score.xml')
colnames(bs$scores)<-c('L0','L720','L1440','L2160')
bs_tmp = data.table(bs$scores)
bs_tmp = bs_tmp %>% dplyr::mutate(Location = "SPRAG", metric = "BrierScore",source='NNRCS')
metrics_final = bind_rows(metrics_final,bs_tmp[1,])

write.csv(metrics_final,file='C:/Projects/KlamathRiverBasin/EVS_5.6/deterministic_metrics.csv',row.names=FALSE,quote=F)
