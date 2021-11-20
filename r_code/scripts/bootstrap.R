# Bootstrapping function for metabolism model; JAZ 2014-11-3; modified from Winslow and GLEON fellows 

bootstrap.metab <- function(parGuess, dataTemp, n=1000, ar1.resids=FALSE){
  
  n.obs = length(dataTemp$DOObs)
  
  doHat  = metabPredix_v4(optimTemp$par,dataTemp)$DOHat
  resids = dataTemp$DOObs - doHat
  
  #If we are maintaining the ar1 component of the residuals, 
  # we must estimate ar1 coeff and the ar1 residual standard deviation
  if(ar1.resids){
    ar1.lm    = lm(resids[1:n.obs-1] ~ resids[2:n.obs]-1)
    ar1.coeff = ar1.lm$coefficients
    ar1.sd    = sd(ar1.lm$residuals)
  }
  
  
  #Pre-allocate the result data frame
  result <- data.frame(boot.iter = 1:n,
                       iota = rep(NA,n),
                       rho = rep(NA,n),
                       DOInit = rep(NA,n),
                       covergence = rep(NA,n),
                       nll = rep(NA,n),
                       GPP = rep(NA,n))
  
  for(i in 1:n){
    
    #Randomize the residuals using one of two methods
    if(ar1.resids){ #residual randomization keeping the ar1 data structure
      simRes = rep(NA, n.obs)
      simRes[1] = sample(resids[!is.na(resids)],1)
      for(j in 2:n.obs){
        simRes[j] = ar1.coeff*simRes[j-1] + rnorm(n=1, sd=ar1.sd)
      }
      
    }else{ #Raw residual randomization
      #Randomize residuals without replacement
      simRes = sample(resids[!is.na(resids)], length(resids), replace=FALSE) 
    }
    
    doSim = doHat + simRes
    
    #Run optim again with new simulated DO signal
    dataBoot<-dataTemp
    dataBoot$DOObs<-doSim
    optimTemp <- optim(parGuess,metabLoss_v4,dataIn=dataBoot)
    
    result[i,2:3] <- exp(optimTemp$par[1:2])*(1440/timeStep) #iota and rho to units of mg O2 L-1 day-1 
    result[i,4] <- exp(optimTemp$par[3]) #initial DO estimate 
    result[i,5] <- optimTemp$convergence  #did model converge or not (0=yes, 1=no)
    result[i,6] <- optimTemp$value #value of nll 
    result[i,7] <- (result[i,2]/(60*60*24))*sum(dataTemp$irr)*timeStep*60 #GPP in units of mg O2 L-1 d-1 
    
  }
  
  return(result)
}




