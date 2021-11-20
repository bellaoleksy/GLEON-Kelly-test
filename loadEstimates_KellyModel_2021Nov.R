#Includs all the default Kelly model parameters from the paper
I0=1000  #umol photons m-2 s-1 
# define function for integrating light curves
lightAtten<-function(z,I0,kD){
  Iz=I0*exp(-kD*z)
  return(Iz)
}

# define function for simulationg lake primary productivity as a function of DOC and P supply, lake surface area, and residence time
huisman<-function(t,y,params){
  with(as.list(c(y,params)),{
    
    # using Morris paper relationship, but symbols from Huisman
    kD=kA*A+kDOC*DOC-0.05	#m-1; based on Morris paper
    
    # from a published paper -> I'll refind the paper eventually
    zmix=10^(-0.515*log10(DOC)+0.115*log10(2*sqrt(SA/pi))+0.991)
    # if(zmix<zmax){zmix=zmax} #old
    if(zmix>zmax){zmix=zmax}
    
    Izmix=lightAtten(z=zmix,I0=I0,kD=kD)
    
    # biomass specific growth integrated across mixed layer
    prod=(pA/(kD*zmix))*log((hA+I0)/(hA+Izmix))*(P/(P+mA))	# d-1
    
    dA.dt=A*prod-lA*A-v/zmix*A-Qin/(zmix*SA*1e6)*A	# mg C m-3
    dP.dt=Qin/(zmix*SA*1e6)*(Pin-P)+cA*lA*A*rec-cA*A*prod		# mg P m-3 (in epi)
    dDOC.dt=(Qin/(zmix*SA*1e6))*(DOCin-DOC)-decay*DOC				# g C m-3 (in epi)
    
    return(list(c(dA.dt,dP.dt,dDOC.dt)))
  })
}


loadPredix <- function(parms,d) {
  
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
  
  times=1:10000
  
  parms = parms
  
  d = d

  #Set up vectors to hold predicted values of GPP, P, DOC, zMix
  TPstar=numeric(dim(d)[1])
  DOCstar=numeric(dim(d)[1])
  
  #For each lake in the data set, run the Kelly model to generate predictions
  for (i in 1:dim(d)[1]) {
    
  # starting state variables
    n<-c(A=100,P=d$lakeTP[i],DOC=d$lakeDOC[i])
  
    
  # simulate with ode
    run=ode(y=n,times=times,func=huisman,parms=parms)

  # store equilibrium values
    TPstar<-run[nrow(run),3]+run[nrow(run),2]*0.015  # 0.015 is cA (P content of algae)
    DOCstar<-run[nrow(run),4]
    
  }
  
  
  # calculate squared error of lake TP and DOC
  return((TPstar-lakeTP)^2+(DOCstar-lakeDOC)^2) #change made on 2021-05-13
}


###Create dataframe d


d <- master_df %>%
  filter(lakeName %in% c("Nastjarn","Jordan")) %>%
  mutate(SA=SA_ha*10000) %>% #m2 
  rename(V=V_m3,
         HRT=HRT_days,
         lakeDOC=DOC_mgL,
         lakeTP=TP_ugL,
         zmax=zMax)


### Parameter set
#Kelly values
parms <- c(kDOC=0.42,kA=0.00022,lA=0.1,pA=1.2,hA=55,mA=2,decay=0.001,cA=0.015,v=0.01,rec=0.95)


start_time <- Sys.time()
guess=c(0.001, 1)
fit=optim(guess,loadPredix,SA=d$SA[i],V=d$V[i],HRT=d$HRT[i],lakeDOC=d$lakeDOC[i],lakeTP=d$lakeTP[i],I0=1000,
          method="BFGS")
end_time <- Sys.time()
end_time - start_time
optimOut

