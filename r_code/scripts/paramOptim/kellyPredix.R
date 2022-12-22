#This function takes a set of parameters and some data about a set of lakes. It uses those
#inputs to run the Kelly model (huisman.r) to equilibrium for each lake. It returns the
#equilibrium predictions of GPP, DOC, and TP.

kellyPredix <- function(p,d) {
  
  #Arguments
  # p   Named vector of (log-transformed) parameters used in the Kelly ('huisman') model,
  #     plus scale parameters for the Gamma distributions of observations.
  #     (kDOC,kA,lA,pA,hA,mA,decay,cA,v,rec,scaleGPP,scaleP,scaleDOC).
  #     See huisman.r for definitions.
  # d   Data.frame containing data for each lake on additional input values and observations of state variables.
  #     Must contain the following columns (and possibly others):
  #       lakeID    Lake names
  #       TP        Total phosphorus concentration in the mixed layer, ug/L OR mg/m3
  #       DOC       Dissolved organic carbon concentration in the mixed layer, mg/L or g/m3
  #       medianGPP Median GPP mg O2 m-2 day-1
  #       I0        Average incident light at the surface umol photons m-2 s-1
  
  #Exponentiate parameter vector to force all positive
  p <- exp(p)

  #Set up vectors to hold predicted values of GPP, P, DOC, zMix
  GPPHat=numeric(dim(d)[1])
  PHat=numeric(dim(d)[1])
  CHat=numeric(dim(d)[1])
  zMixHat=numeric(dim(d)[1])
  
  #For each lake in the data set, run the Kelly model to generate predictions
  for (i in 1:dim(d)[1]) {
    
    #Set vector of initial conditions for state variables
    initConds <- c(A=500,P=d$TP[i],DOC=d$DOC[i])
    
    #Run the model and save predictions of algal biomass, P, DOC
    curRun=ode(y=initConds,times=c(1:2000),func=huisman,parms=p,SA=d$SA[i],zMax=d$zMax[i],zMean=d$zMean[i],Qin=d$Qin[i],I0=d$I0[i],Pin=d$Pin[i],DOCin=d$DOCin[i])  
    Astar=curRun[nrow(curRun),2]
    Pstar=curRun[nrow(curRun),3]
    Cstar=curRun[nrow(curRun),4]
    
    #GPP at equilibrum
    #First, kD at equilibrium
    kD <- p["kA"]*Astar+p["kDOC"]*Cstar-0.05	#m-1; based on Morris paper
    #zMix at equilibrium
    zMix <- 10^(-0.515*log10(Cstar)+0.115*log10(2*sqrt(d$SA[i]/pi))+0.991)
    if(zMix>d$zMax[i]){zMix=d$zMax[i]}
    #Light at bottom of mixed layer
    IZMix <- lightAtten(z=zMix,I0=d$I0[i],kD=kD)
    #Algal productivity d-1
    prod <- (p["pA"]/(kD*zMix))*log((p["hA"]+d$I0[i])/(p["hA"]+IZMix))*(Pstar/(Pstar+p["mA"]))	# d-1
    #Converted to areal GPP
    GPPHat[i] <- prod*Astar*zMix*1.25
    
    #TP at equilibrium
    PHat[i] <- Astar*p["cA"]+Pstar # predicted lake TP; algal biomass * Phosphorus to carbon quota of phytoplankton + dissolvedP
    
    #DOC at equilibrium
    CHat[i] <- Cstar # predicted lake DOC
    
    #zMix at equilibrium
    zMixHat[i] <- zMix
  }
  
  #Return predictions
  hats <- cbind(GPPHat,PHat,CHat,zMixHat)
  return(hats)
}
