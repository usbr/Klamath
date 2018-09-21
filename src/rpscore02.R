"rpscore" <- function(fcst,obsv,baseline,ncat){

  #Function to calculate rank probability score (RPS) for a
  #single forecast-event pair.
  #Refer to Wilks (1995), pp. 271-272. Equation 7.47 Wilks (2005), p. 300.

  #Input(s)
  #--------
  #fcst - vector of forecasts of length nens for a given month 
          #(e.g., Jan, Feb, etc.)
  #obsv - a monthly observation (note - not a vector)
  #baseline - vector of all observations for a given month; say,
              #for 30 years of data there will be 30 January values;
	      #reference values to define categories
  #ncat - number of categories

  #Output(s)
  #---------
  #rps - rank probability score for a single forecast-event pair
  #rpc - climatological rank probability score

  #Note - code is applicable for daily forecasts too.

  #initialize
  countf <- counto <- rep(0,ncat)    #count for forecasted and observed
  countc = cumsum(rep(1/ncat, ncat)) #cumulative climatological probability

  #bin forecasts
  breaks=c(min(baseline),quantile(baseline,countc))
  f=cut(fcst,breaks,labels=F)    #bin forecasts
  which(is.na(f)==TRUE) -> index #next, assign f[index]=1; 
                                 #becuase, lower bound is open, 
                                 #e.g. (-3.7834550 -0.8083644] from 
				 #a 1000 realization of standard normal
				 #deviates, and the minimum value is 
				 #assigned to category NA by the function cut,
				 #above

  f[index]=1                     #closes the lower bound for first category    

  for (icat in 1:ncat){
    countf[icat]=length(which(f==icat))
  } #icat

  #category in which observed falls
  amin=breaks[1]
  for (icat in 1:ncat){
    if (icat==1){
      catmin=amin
    }
    else{
      catmin=breaks[icat]
    }
    catmax=breaks[icat+1]
    
    if (obsv==amin){ #assigns minimum observed value to category 1,i.e.,
                     #closes the lower bound for category 1
      counto[1]=1
      break
    }
    if (obsv > catmin && obsv <= catmax) {
      counto[icat]=1
    }
  } #icat
  
  #calculate rps
  nens=length(fcst)
  countf=countf/nens
  countf=cumsum(countf)
  counto=cumsum(counto)

  rps = sum((countf - counto)^2)
  rpsc = sum((countc - counto)^2)


  retlist=list("rps"=rps,"rpsc"=rpsc)
  
  return(retlist)
  
} #end function






