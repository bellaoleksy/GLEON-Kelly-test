#Model from Huissman and Weissing 1995 Am. Nat. modified to use DOC inputs to change average light climate to estimate GPP -- this version is the original, loads are handled as only making contact with the epilimnion with all loads essentially going to the mixed layer
#Stuart Jones & Patrick Kelly 20  November 2017

library(deSolve)
library(googlesheets4)
# library(parallel)
# numCores <- detectCores()
# numCores


# LOAD ESTs FROM 2022-07-26 - SEJ set -----------------------------------------------
# In this version I used the optimized parameter set from Stuart Jones before SIL ("SEJ set"). Doesn't look great, so below I also 
# run the "Oleksy set" which is the version that I manually tweaked in 2021 sometime. Values from table in this google doc:
# https://docs.google.com/document/d/1nMDyDvns0Hbp52hij--gFf7qyAJq0D_JoY5jJO1Lj7s/edit

#calculates volume within the function instead of using the "known" volume from metadata sheet -- did this to be consistent with the KellyPredix GPP function
I0=350  #umol photons m-2 s-1 
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
    if(zmix>zmax | is.na(zmix)){zmix=zmax}
    
    V=SA*1e6*zmean #added this back in 2022-07-27
    
    Qin=V/HRT
    
    Izmix=lightAtten(z=zmix,I0=I0,kD=kD)
    
    # biomass specific growth integrated across mixed layer
    prod=(pA/(kD*zmix))*log((hA+I0)/(hA+Izmix))*(P/(P+mA))	# d-1
    
    dA.dt=A*prod-lA*A-v/zmix*A-Qin/(zmix*SA*1e6)*A	# mg C m-3
    dP.dt=Qin/(zmix*SA*1e6)*(Pin-P)+cA*lA*A*rec-cA*A*prod		# mg P m-3 (in epi)
    dDOC.dt=(Qin/(zmix*SA*1e6))*(DOCin-DOC)-decay*DOC				# g C m-3 (in epi)
    
    return(list(c(dA.dt,dP.dt,dDOC.dt)))
  })
}


# define function for estimating Cin and Pin
estInputs<-function(ins,SA,HRT,lakeDOC,lakeTP,I0=1000){
  
  times=1:10000
  
  parms=c(SA=SA/1e6,# convert SA to km2
          zmax=zmax,
          # V=V,
          kDOC=0.020748,
          kA=0.0009966, 
          lA=0.7213,
          pA=1.25,
          hA=2.4842,
          mA=1.8157,
          decay=0.0000686,
          cA=0.012,
          v = 0.31, 
          rec=1,
          Pin=ins[1],
          DOCin=ins[2],
          HRT=HRT,
          
          zmean=zmean)
  
  # starting state variables
  n<-c(A=100,P=lakeTP,DOC=lakeDOC)
  
  # simulate with ode
  run=ode(y=n,times=times,func=huisman,parms=parms)
  
  # store equilibrium values
  # Pstar<-run[nrow(run),3]
  TPstar<-run[nrow(run),3]+run[nrow(run),2]*0.012  # predicted lake TP; algal biomass * Phosphorus to carbon quota of phytoplankton + dissolvedP
  DOCstar<-run[nrow(run),4]
  
  
  # calculate squared error of lake TP and DOC
  return((TPstar-lakeTP)^2+(DOCstar-lakeDOC)^2) #change made on 2021-05-13
  # return((TPstar*250-lakeTP*250)^2+(DOCstar-lakeDOC)^2) #Change suggested by SEJ-
  # the combined square error is really small and doesn’t actually matter much to the solver. 
  # The *250 scales the phosphorus concentrations to be closer in magnitude to the DOC concentrations. 
}




# LOAD ESTs FROM 2022-08-01 - Oleksy set -----------------------------------------------
# Below I also ran the "Oleksy set" which is the version that I manually tweaked in 2021 sometime. Values from table in this google doc:
# https://docs.google.com/document/d/1nMDyDvns0Hbp52hij--gFf7qyAJq0D_JoY5jJO1Lj7s/edit

#calculates volume within the function instead of using the "known" volume from metadata sheet -- did this to be consistent with the KellyPredix GPP function
I0=350  #umol photons m-2 s-1 
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
    if(zmix>zmax | is.na(zmix)){zmix=zmax}
    
    V=SA*1e6*zmean #added this back in 2022-07-27
    
    Qin=V/HRT
    
    Izmix=lightAtten(z=zmix,I0=I0,kD=kD)
    
    # biomass specific growth integrated across mixed layer
    prod=(pA/(kD*zmix))*log((hA+I0)/(hA+Izmix))*(P/(P+mA))	# d-1
    
    dA.dt=A*prod-lA*A-v/zmix*A-Qin/(zmix*SA*1e6)*A	# mg C m-3
    dP.dt=Qin/(zmix*SA*1e6)*(Pin-P)+cA*lA*A*rec-cA*A*prod		# mg P m-3 (in epi)
    dDOC.dt=(Qin/(zmix*SA*1e6))*(DOCin-DOC)-decay*DOC				# g C m-3 (in epi)
    
    return(list(c(dA.dt,dP.dt,dDOC.dt)))
  })
}


# define function for estimating Cin and Pin
estInputs<-function(ins,SA,HRT,lakeDOC,lakeTP,zmax,V,I0=350){
  
  times=1:10000
  
  parms=c(SA=SA/1e6,# convert SA to km2
          zmax=zmax,
          # V=V,
          kDOC=0.020748,
          kA=0.00022, 
          lA=0.1,
          pA=1.2,
          hA=55,
          mA=2,
          decay=0.001,
          cA=0.015,
          v = 0.01, 
          rec=0.95,
          Pin=ins[1],
          DOCin=ins[2],
          HRT=HRT,
          zmean=zmean)
  
  # starting state variables
  n<-c(A=100,P=lakeTP,DOC=lakeDOC)
  
  # simulate with ode
  run=ode(y=n,times=times,func=huisman,parms=parms)
  
  # store equilibrium values
  # Pstar<-run[nrow(run),3]
  TPstar<-run[nrow(run),3]+run[nrow(run),2]*0.012  # predicted lake TP; algal biomass * Phosphorus to carbon quota of phytoplankton + dissolvedP
  DOCstar<-run[nrow(run),4]
  
  
  # calculate squared error of lake TP and DOC
  return((TPstar-lakeTP)^2+(DOCstar-lakeDOC)^2) #change made on 2021-05-13
  # return((TPstar*250-lakeTP*250)^2+(DOCstar-lakeDOC)^2) #Change suggested by SEJ-
  # the combined square error is really small and doesn’t actually matter much to the solver. 
  # The *250 scales the phosphorus concentrations to be closer in magnitude to the DOC concentrations. 
}


# LOAD ESTs FROM 2022-07-25 -----------------------------------------------
# Following optimization routine that Stuart Jones did (see Slack thread from July 21, 2022)


#Read in metadata google sheet
metadata<-read_sheet("https://docs.google.com/spreadsheets/d/1is87WT3n_TU76pTyiis3Os08JJiSmWc2G6K4bthJhU8/edit#gid=952562522") 
glimpse(metadata)

metadata_trim <- metadata %>%
  select(lakeName, `Surface area (ha)`, `Volume (m3)`,`Lake residence time (year)` )
# write.csv(master_df, "data/master_df.txt", row.names=FALSE)
# write.csv(metadata_trim, "data/metadata.txt", row.names=FALSE)


I0=350  #umol photons m-2 s-1 
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
    if(zmix>zmax | is.na(zmix)){zmix=zmax}
    
    V=SA*1e6*zmean #added this back in 2022-07-27
    
    Izmix=lightAtten(z=zmix,I0=I0,kD=kD)
    
    # biomass specific growth integrated across mixed layer
    prod=(pA/(kD*zmix))*log((hA+I0)/(hA+Izmix))*(P/(P+mA))	# d-1
    
    dA.dt=A*prod-lA*A-v/zmix*A-Qin/(zmix*SA*1e6)*A	# mg C m-3
    dP.dt=Qin/(zmix*SA*1e6)*(Pin-P)+cA*lA*A*rec-cA*A*prod		# mg P m-3 (in epi)
    dDOC.dt=(Qin/(zmix*SA*1e6))*(DOCin-DOC)-decay*DOC				# g C m-3 (in epi)
    
    return(list(c(dA.dt,dP.dt,dDOC.dt)))
  })
}


# define function for estimating Cin and Pin
estInputs<-function(ins,SA,V,HRT,lakeDOC,lakeTP,I0=1000){
  
  times=1:10000
  
  parms=c(SA=SA/1e6,# convert SA to km2
          zmax=zmax,
          V=V,
          kDOC=0.020748,
          kA=0.0009966, 
          lA=0.7213,
          pA=1.25,
          hA=2.4842,
          mA=1.8157,
          decay=0.0000686,
          cA=0.012,
          v = 0.31, 
          rec=1,
          Pin=ins[1],
          DOCin=ins[2],
          HRT=HRT,
          Qin=V/HRT,
          zmean=zmean)
  
  # starting state variables
  n<-c(A=100,P=lakeTP,DOC=lakeDOC)
  
  # simulate with ode
  run=ode(y=n,times=times,func=huisman,parms=parms)
  
  # store equilibrium values
  # Pstar<-run[nrow(run),3]
  TPstar<-run[nrow(run),3]+run[nrow(run),2]*0.012  # predicted lake TP; algal biomass * Phosphorus to carbon quota of phytoplankton + dissolvedP
  DOCstar<-run[nrow(run),4]
  
  
  # calculate squared error of lake TP and DOC
  return((TPstar-lakeTP)^2+(DOCstar-lakeDOC)^2) #change made on 2021-05-13
  # return((TPstar*250-lakeTP*250)^2+(DOCstar-lakeDOC)^2) #Change suggested by SEJ-
  # the combined square error is really small and doesn’t actually matter much to the solver. 
  # The *250 scales the phosphorus concentrations to be closer in magnitude to the DOC concentrations. 
}

# initial guess for Pin (mg P m-3) and DOCin (g C m-3)



# LOAD ESTs FROM 2021_05_13 -----------------------------------------------



#Read in metadata google sheet
metadata<-read_sheet("https://docs.google.com/spreadsheets/d/1is87WT3n_TU76pTyiis3Os08JJiSmWc2G6K4bthJhU8/edit#gid=952562522") 
glimpse(metadata)

metadata_trim <- metadata %>%
  select(lakeName, `Surface area (ha)`, `Volume (m3)`,`Lake residence time (year)` )
# write.csv(master_df, "data/master_df.txt", row.names=FALSE)
# write.csv(metadata_trim, "data/metadata.txt", row.names=FALSE)


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
    # V=SA*1e6*zmax
    # V=V
    # Qin=V/HRT
    
    Izmix=lightAtten(z=zmix,I0=I0,kD=kD)
    
    # biomass specific growth integrated across mixed layer
    prod=(pA/(kD*zmix))*log((hA+I0)/(hA+Izmix))*(P/(P+mA))	# d-1
    
    dA.dt=A*prod-lA*A-v/zmix*A-Qin/(zmix*SA*1e6)*A	# mg C m-3
    dP.dt=Qin/(zmix*SA*1e6)*(Pin-P)+cA*lA*A*rec-cA*A*prod		# mg P m-3 (in epi)
    dDOC.dt=(Qin/(zmix*SA*1e6))*(DOCin-DOC)-decay*DOC				# g C m-3 (in epi)
    
    return(list(c(dA.dt,dP.dt,dDOC.dt)))
  })
}


# define function for estimating Cin and Pin
estInputs<-function(ins,SA,V,HRT,lakeDOC,lakeTP,I0=1000){
  
  times=1:10000
  
  parms=c(SA=SA/1e6,# convert SA to km2
          # zmax=V/SA, #old
          zmax=zmax,
          V=V,
          kDOC=0.19320150,#new optim
          kA=0.00000461, #new optim
          lA=0.02285074,#new optim
          pA=1.73569800,#new optim
          hA=1621.98800000,#new optim
          mA=3.29913800,#new optim
          decay=0.00051373,#new optim
          cA=0.00506576,#new optim
          # v=0.05, #OLD
          v = 0.00005119, #new optim
          rec=0.94855630,#new optim
          Pin=ins[1],
          DOCin=ins[2],
          HRT=HRT,
          Qin=V/HRT)
  
  # starting state variables
  n<-c(A=100,P=lakeTP,DOC=lakeDOC)
  
  # simulate with ode
  run=ode(y=n,times=times,func=huisman,parms=parms)
  
  # store equilibrium values
  # Pstar<-run[nrow(run),3]
  TPstar<-run[nrow(run),3]+run[nrow(run),2]*0.015  # 0.015 is cA (P content of algae)
  DOCstar<-run[nrow(run),4]
  
  
  # calculate squared error of lake TP and DOC
  # return((Pstar-lakeTP)^2+(DOCstar-lakeDOC)^2)
  return((TPstar-lakeTP)^2+(DOCstar-lakeDOC)^2) #change made on 2021-05-13
  # return((Pstar*250-lakeTP*250)^2+(DOCstar-lakeDOC)^2) #Change suggested by SEJ-
  # the combined square error is really small and doesn’t actually matter much to the solver. 
  # The *250 scales the phosphorus concentrations to be closer in magnitude to the DOC concentrations. 
}

# initial guess for Pin (mg P m-3) and DOCin (g C m-3)



########ORIGINAL/OLD ##########
#Includs all the default Kelly model parameters from the paper
I0=350  #umol photons m-2 s-1 
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
    if(zmix>zmax | is.na(zmix)){zmix=zmax}
    
    V=SA*1e6*zmean #added this back in 2022-07-27
    
    Qin=V/HRT
    
    Izmix=lightAtten(z=zmix,I0=I0,kD=kD)
    
    # biomass specific growth integrated across mixed layer
    prod=(pA/(kD*zmix))*log((hA+I0)/(hA+Izmix))*(P/(P+mA))	# d-1
    
    dA.dt=A*prod-lA*A-v/zmix*A-Qin/(zmix*SA*1e6)*A	# mg C m-3
    dP.dt=Qin/(zmix*SA*1e6)*(Pin-P)+cA*lA*A*rec-cA*A*prod		# mg P m-3 (in epi)
    dDOC.dt=(Qin/(zmix*SA*1e6))*(DOCin-DOC)-decay*DOC				# g C m-3 (in epi)
    
    return(list(c(dA.dt,dP.dt,dDOC.dt)))
  })
}


# define function for estimating Cin and Pin
estInputs<-function(ins,SA,V,HRT,lakeDOC,zmax,lakeTP,I0=350){
  
  times=1:10000
  
  parms=c(SA=SA/1e6,# convert SA to km2
          # SA=SA, #OLD
          # zmax=V/SA, #old
          zmax=zmax,
          V=V,
          kDOC=0.42,
          kA=0.00022, 
          lA=0.1,
          pA=1,
          hA=55,
          mA=2,
          decay=0.001,
          cA=0.015,
          v = 0.1, #same as Kelly model predictions
          rec=0.95,
          Pin=ins[1],
          DOCin=ins[2],
          HRT=HRT,
          Qin=V/HRT)
  
  # starting state variables
  n<-c(A=100,P=lakeTP,DOC=lakeDOC)
  
  # simulate with ode
  run=ode(y=n,times=times,func=huisman,parms=parms)
  
  # store equilibrium values
  # Pstar<-run[nrow(run),3]
  TPstar<-run[nrow(run),3]+run[nrow(run),2]*0.015  # 0.015 is cA (P content of algae)
  DOCstar<-run[nrow(run),4]
  
  
  # calculate squared error of lake TP and DOC
  # return((Pstar-lakeTP)^2+(DOCstar-lakeDOC)^2)
  return((TPstar-lakeTP)^2+(DOCstar-lakeDOC)^2) #change made on 2021-05-13
  # return((Pstar*250-lakeTP*250)^2+(DOCstar-lakeDOC)^2) #Change suggested by SEJ-
  # the combined square error is really small and doesn’t actually matter much to the solver. 
  # The *250 scales the phosphorus concentrations to be closer in magnitude to the DOC concentrations. 
}




# Get Pin and Cin ---------------------------------------------------------



#Nastjarn # set lake parameters
guess=c(0.001, 1)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='Nastjarn']*10000         # m2
# V=metadata$`Volume (m3)`[metadata$`lakeName`=='Nastjarn']        # m3
HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='Nastjarn']*365        # days
lakeDOC = master_df$DOC_mgL[master_df$lakeName=='Nastjarn'] # g C m-3
lakeTP= master_df$TP_ugL[master_df$lakeName=='Nastjarn']  # mg P m-3
zmax = metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='Nastjarn']
zmean = metadata$`Mean lake depth (m)`[metadata$`lakeName`=='Nastjarn']
V=SA*zmean

# fit=optim(guess,estInputs,SA=SA,V=V,HRT=HRT,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350,
#           method="BFGS")
fit=optim(guess,estInputs,SA=SA,HRT=HRT,zmax=zmax, V=V,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350,
          method="BFGS")
nastjarn_fit<-fit$par
nastjarn_fit


#For Jordan
guess=c(0.0001,1)# initial guess for Pin (mg P m-3) and DOCin (g C m-3)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='Jordan']*10000         # m2
# V=metadata$`Volume (m3)`[metadata$`lakeName`=='Jordan']        # m3
HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='Jordan']*365        # days
lakeDOC = master_df$DOC_mgL[master_df$lakeName=='Jordan'] # g C m-3
lakeTP= master_df$TP_ugL[master_df$lakeName=='Jordan']  # mg P m-3
zmax = metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='Jordan']
zmean = metadata$`Mean lake depth (m)`[metadata$`lakeName`=='Jordan']
V=SA*zmean

# fit=optim(guess,estInputs,SA=SA,V=V,HRT=HRT,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350,
#           method="BFGS")
fit=optim(guess,estInputs,SA=SA,HRT=HRT,zmax=zmax, V=V,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350,
          method="BFGS")
Jordan_fit<-fit$par
Jordan_fit


#For Almberga
# set lake parameters

# initial guess for Pin (mg P m-3) and DOCin (g C m-3)
guess=c(0.0001,1)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='Almberga']*10000         # m2
# V=metadata$`Volume (m3)`[metadata$`lakeName`=='Almberga']        # m3
HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='Almberga']*365        # days
zmax=metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='Almberga']
lakeTP= 5    # mg P m-3 #It's a very oligotrophic lake. This is a GUESS
lakeDOC = master_df$DOC_mgL[master_df$lakeName=='Almberga'] # g C m-3
zmean = metadata$`Mean lake depth (m)`[metadata$`lakeName`=='Almberga']
V=SA*zmean


# fit=optim(guess,estInputs,SA=SA,V=V,HRT=HRT,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350,
#           method="BFGS")
fit=optim(guess,estInputs,SA=SA,HRT=HRT,zmax=zmax, V=V,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350,
          method="BFGS")
Almberga_fit<-fit$par
Almberga_fit


#For Bolger
guess=c(0.01,2)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='Bolger']*10000         # m2
V=metadata$`Volume (m3)`[metadata$`lakeName`=='Bolger']        # m3
HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='Bolger']*365        # days
lakeDOC = master_df$DOC_mgL[master_df$lakeName=='Bolger'] # g C m-3
lakeTP= master_df$TP_ugL[master_df$lakeName=='Bolger']  # mg P m-3
zmax=metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='Bolger']


fit=optim(guess,estInputs,SA=SA,HRT=HRT,zmax=zmax, V=V,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350,
          method="BFGS")
bolger_fit<-fit$par
bolger_fit

#For Brown
# set lake parameters

# initial guess for Pin (mg P m-3) and DOCin (g C m-3)
guess=c(0.01,10)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='Brown']*10000         # m2
V=metadata$`Volume (m3)`[metadata$`lakeName`=='Brown']        # m3
HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='Brown']*365        # days
lakeDOC = master_df$DOC_mgL[master_df$lakeName=='Brown'] # g C m-3
lakeTP= master_df$TP_ugL[master_df$lakeName=='Brown']  # mg P m-3
zmax=metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='Brown']


fit=optim(guess,estInputs,SA=SA,V=V,zmax=zmax,HRT=HRT,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350)
brown_fit<-fit$par
brown_fit

#For Bay
# set lake parameters

# initial guess for Pin (mg P m-3) and DOCin (g C m-3)
guess=c(0.01,10)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='Bay']*10000         # m2
V=metadata$`Volume (m3)`[metadata$`lakeName`=='Bay']        # m3
HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='Bay']*365        # days
lakeDOC = master_df$DOC_mgL[master_df$lakeName=='Bay'] # g C m-3
lakeTP= master_df$TP_ugL[master_df$lakeName=='Bay']  # mg P m-3
zmax=metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='Bay']


fit=optim(guess,estInputs,SA=SA,V=V,zmax=zmax,HRT=HRT,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350)
bay_fit<-fit$par
bay_fit


#For Cranberry
# set lake parameters

# initial guess for Pin (mg P m-3) and DOCin (g C m-3)
guess=c(10,1)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='Cranberry']*10000         # m2
V=metadata$`Volume (m3)`[metadata$`lakeName`=='Cranberry']        # m3
HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='Cranberry']*365        # days
lakeDOC = master_df$DOC_mgL[master_df$lakeName=='Cranberry'] # g C m-3
lakeTP= master_df$TP_ugL[master_df$lakeName=='Cranberry']  # mg P m-3
zmax=metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='Cranberry']

fit=optim(guess,estInputs,SA=SA,V=V,zmax=zmax,HRT=HRT,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350)
Cranberry_fit<-fit$par
Cranberry_fit


#For Erken
# set lake parameters

# initial guess for Pin (mg P m-3) and DOCin (g C m-3)
guess=c(20, 1)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='Erken']*10000         # m2
V=metadata$`Volume (m3)`[metadata$`lakeName`=='Erken']        # m3
HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='Erken']*365        # days
lakeDOC = master_df$DOC_mgL[master_df$lakeName=='Erken'] # g C m-3
lakeTP= master_df$TP_ugL[master_df$lakeName=='Erken']  # mg P m-3
zmax=metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='Erken']


fit=optim(guess,estInputs,SA=SA,V=V,zmax=zmax,HRT=HRT,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350)
Erken_fit<-fit$par
Erken_fit

#For Gollinsee
# set lake parameters

# initial guess for Pin (mg P m-3) and DOCin (g C m-3)
guess=c(10,1)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='Gollinsee']*10000         # m2
V=metadata$`Volume (m3)`[metadata$`lakeName`=='Gollinsee']        # m3
HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='Gollinsee']*365        # days
lakeDOC = master_df$DOC_mgL[master_df$lakeName=='Gollinsee'] # g C m-3
lakeTP= master_df$TP_ugL[master_df$lakeName=='Gollinsee']  # mg P m-3
zmax=metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='Gollinsee']


fit=optim(guess,estInputs,SA=SA,V=V,zmax=zmax,HRT=HRT,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350)
Gollinsee_fit<-fit$par
Gollinsee_fit

#For Hummingbird
# set lake parameters

# initial guess for Pin (mg P m-3) and DOCin (g C m-3)
guess=c(22,10)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='Hummingbird']*10000         # m2
V=metadata$`Volume (m3)`[metadata$`lakeName`=='Hummingbird']        # m3
HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='Hummingbird']*365        # days
lakeDOC = master_df$DOC_mgL[master_df$lakeName=='Hummingbird'] # g C m-3
lakeTP= master_df$TP_ugL[master_df$lakeName=='Hummingbird']  # mg P m-3
zmax=metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='Hummingbird']

fit=optim(guess,estInputs,SA=SA,V=V,zmax=zmax,HRT=HRT,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350)
Hummingbird_fit<-fit$par
Hummingbird_fit



#For Mirror
# set lake parameters

# initial guess for Pin (mg P m-3) and DOCin (g C m-3)
guess=c(1,0.5)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='Mirror']*10000         # m2
V=metadata$`Volume (m3)`[metadata$`lakeName`=='Mirror']        # m3
HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='Mirror']*365        # days
zmax=metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='Mirror']
lakeDOC = master_df$DOC_mgL[master_df$lakeName=='Mirror'] # g C m-3
lakeTP= master_df$TP_ugL[master_df$lakeName=='Mirror']  # mg P m-3

fit=optim(guess,estInputs,SA=SA,V=V,zmax=zmax,HRT=HRT,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350)
Mirror_fit<-fit$par
Mirror_fit


#For Mueggelsee
# set lake parameters

# initial guess for Pin (mg P m-3) and DOCin (g C m-3)
guess=c(1,5)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='Mueggelsee']*10000         # m2
V=metadata$`Volume (m3)`[metadata$`lakeName`=='Mueggelsee']        # m3
HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='Mueggelsee']*365        # days
lakeDOC = master_df$DOC_mgL[master_df$lakeName=='Mueggelsee'] # g C m-3
lakeTP= master_df$TP_ugL[master_df$lakeName=='Mueggelsee']  # mg P m-3
zmax=metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='Mueggelsee']
# zmax=V/SA

fit=optim(guess,estInputs,SA=SA,V=V,zmax=zmax,HRT=HRT,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350)
Mueggelsee_fit<-fit$par
Mueggelsee_fit


#For Suggs
# set lake parameters
# initial guess for Pin (mg P m-3) and DOCin (g C m-3)
guess=c(0.1,1)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='Suggs']*10000         # m2
V=metadata$`Volume (m3)`[metadata$`lakeName`=='Suggs']        # m3
# HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='Suggs']*365        # days
HRT=365
lakeDOC = master_df$DOC_mgL[master_df$lakeName=='Suggs'] # g C m-3
lakeTP= master_df$TP_ugL[master_df$lakeName=='Suggs']  # mg P m-3
zmax=metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='Suggs']


fit=optim(guess,estInputs,SA=SA,V=V,zmax=zmax,HRT=HRT,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350)
Suggs_fit<-fit$par
Suggs_fit

#For Sunapee
# set lake parameters

# initial guess for Pin (mg P m-3) and DOCin (g C m-3)
guess=c(0.001,1)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='Sunapee']*10000         # m2
V=metadata$`Volume (m3)`[metadata$`lakeName`=='Sunapee']        # m3
HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='Sunapee']*365        # days
lakeDOC = master_df$DOC_mgL[master_df$lakeName=='Sunapee'] # g C m-3
lakeTP= master_df$TP_ugL[master_df$lakeName=='Sunapee']  # mg P m-3
zmax=metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='Sunapee']


fit=optim(guess,estInputs,SA=SA,V=V,zmax=zmax,HRT=HRT,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350)
Sunapee_fit<-fit$par
Sunapee_fit


#For Acton
# set lake parameters

# initial guess for Pin (mg P m-3) and DOCin (g C m-3)
guess=c(20,4)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='Acton']*10000         # m2
V=metadata$`Volume (m3)`[metadata$`lakeName`=='Acton']        # m3
HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='Acton']*365        # days
lakeDOC = master_df$DOC_mgL[master_df$lakeName=='Acton'] # g C m-3
lakeTP= master_df$TP_ugL[master_df$lakeName=='Acton']  # mg P m-3
zmax=metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='Acton']
# zmax=V/SA

fit=optim(guess,estInputs,SA=SA,V=V,zmax=zmax,HRT=HRT,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350)
Acton_fit<-fit$par
Acton_fit


#For P1
guess=c(30,20)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='P1']*10000         # m2
V=metadata$`Volume (m3)`[metadata$`lakeName`=='P1']        # m3
HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='P1']*365        # days
# lakeDOC= 54.425000   # g C m-3
lakeDOC = master_df$DOC_mgL[master_df$lakeName=='P1'] # g C m-3
lakeTP= 35    # mg SRP m-3
zmax=metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='P1']

fit=optim(guess,estInputs,SA=SA,V=V,zmax=zmax,HRT=HRT,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350)
P1_fit<-fit$par
P1_fit


#For P8
guess=c(1,1)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='P8']*10000         # m2
V=metadata$`Volume (m3)`[metadata$`lakeName`=='P8']        # m3
HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='P8']*365        # days
# lakeDOC= 34.600000   # g C m-3
lakeTP= 30.000000    # mg SRP m-3
lakeDOC = master_df$DOC_mgL[master_df$lakeName=='P8'] # g C m-3
# lakeTP= master_df$SRP_ugL[master_df$lakeName=='P8']  # mg P m-3
zmax=metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='P8']

fit=optim(guess,estInputs,SA=SA,V=V,zmax=zmax,HRT=HRT,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350)
P8_fit<-fit$par
P8_fit



#For TheLoch
# set lake parameters

# initial guess for Pin (mg P m-3) and DOCin (g C m-3)
guess=c(0.01,1)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='TheLoch']*10000         # m2
V=metadata$`Volume (m3)`[metadata$`lakeName`=='TheLoch']        # m3
HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='TheLoch']*365        # days
# lakeDOC= 1.24   # g C m-3
# lakeTP= 9.46    # mg P m-3
lakeDOC = master_df$DOC_mgL[master_df$lakeName=='TheLoch'] # g C m-3
lakeTP= master_df$TP_ugL[master_df$lakeName=='TheLoch']  # mg P m-3
zmax=metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='TheLoch']


fit=optim(guess,estInputs,SA=SA,V=V,zmax=zmax,HRT=HRT,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350)
TheLoch_fit<-fit$par
TheLoch_fit


#For YunYang
guess=c(0.01,1)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='YunYang']*10000         # m2
V=metadata$`Volume (m3)`[metadata$`lakeName`=='YunYang']        # m3
HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='YunYang']*365        # days
lakeDOC = master_df$DOC_mgL[master_df$lakeName=='YunYang'] # g C m-3
lakeTP= master_df$TP_ugL[master_df$lakeName=='YunYang']  # mg P m-3
zmax=metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='YunYang']

fit=optim(guess,estInputs,SA=SA,V=V,zmax=zmax,HRT=HRT,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350)
YunYang_fit<-fit$par
YunYang_fit

#For Harp
guess=c(0.01,1)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='Harp']*10000         # m2
V=metadata$`Volume (m3)`[metadata$`lakeName`=='Harp']        # m3
HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='Harp']*365        # days
lakeDOC = master_df$DOC_mgL[master_df$lakeName=='Harp'] # g C m-3
lakeTP= master_df$TP_ugL[master_df$lakeName=='Harp']  # mg P m-3
zmax=metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='Harp']


fit=optim(guess,estInputs,SA=SA,V=V,zmax=zmax,HRT=HRT,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350)
Harp_fit<-fit$par
Harp_fit



#For Langtjern
guess=c(0.01,1)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='Langtjern']*10000         # m2
V=metadata$`Volume (m3)`[metadata$`lakeName`=='Langtjern']        # m3
HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='Langtjern']*365        # days
lakeDOC = master_df$DOC_mgL[master_df$lakeName=='Langtjern'] # g C m-3
lakeTP= master_df$TP_ugL[master_df$lakeName=='Langtjern']  # mg P m-3
zmax=metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='Langtjern']


fit=optim(guess,estInputs,SA=SA,V=V,zmax=zmax,HRT=HRT,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350)
Langtjern_fit<-fit$par
Langtjern_fit



#For Lillsjoliden
# set lake parameters

# initial guess for Pin (mg P m-3) and DOCin (g C m-3)
guess=c(17,10)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='Lillsjoliden']*10000         # m2
V=metadata$`Volume (m3)`[metadata$`lakeName`=='Lillsjoliden']        # m3
HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='Lillsjoliden']*365        # days
lakeDOC = master_df$DOC_mgL[master_df$lakeName=='Lillsjoliden'] # g C m-3
lakeTP= master_df$TP_ugL[master_df$lakeName=='Lillsjoliden']  # mg P m-3
zmax=metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='Lillsjoliden']


fit=optim(guess,estInputs,SA=SA,V=V,zmax=zmax,HRT=HRT,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350)
Lillsjoliden_fit<-fit$par
Lillsjoliden_fit


#For Feeagh
# set lake parameters

# initial guess for Pin (mg P m-3) and DOCin (g C m-3)
guess=c(5,10)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='Feeagh']*10000         # m2
V=metadata$`Volume (m3)`[metadata$`lakeName`=='Feeagh']        # m3
HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='Feeagh']*365        # days
lakeDOC = master_df$DOC_mgL[master_df$lakeName=='Feeagh'] # g C m-3
lakeTP= master_df$TP_ugL[master_df$lakeName=='Feeagh']  # mg P m-3
zmax=metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='Feeagh']



fit=optim(guess,estInputs,SA=SA,V=V,zmax=zmax,HRT=HRT,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350)
Feeagh_fit<-fit$par
Feeagh_fit


#For Mangstrettjarn
# set lake parameters

# initial guess for Pin (mg P m-3) and DOCin (g C m-3)
guess=c(0.001,10)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='Mangstrettjarn']*10000         # m2
V=metadata$`Volume (m3)`[metadata$`lakeName`=='Mangstrettjarn']        # m3
HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='Mangstrettjarn']*365        # days
lakeDOC = master_df$DOC_mgL[master_df$lakeName=='Mangstrettjarn'] # g C m-3
lakeTP= master_df$TP_ugL[master_df$lakeName=='Mangstrettjarn']  # mg P m-3
zmax=metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='Mangstrettjarn']
# zmax=V/SA

fit=optim(guess,estInputs,SA=SA,V=V,zmax=zmax,HRT=HRT,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350)
Mangstrettjarn_fit<-fit$par
Mangstrettjarn_fit



#For Mendota
# set lake parameters

# initial guess for Pin (mg P m-3) and DOCin (g C m-3)
guess=c(5,10)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='Mendota']*10000         # m2
V=metadata$`Volume (m3)`[metadata$`lakeName`=='Mendota']        # m3
HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='Mendota']*365        # days
lakeDOC = master_df$DOC_mgL[master_df$lakeName=='Mendota'] # g C m-3
lakeTP= master_df$TP_ugL[master_df$lakeName=='Mendota']  # mg P m-3
zmax=metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='Mendota']


fit=optim(guess,estInputs,SA=SA,V=V,zmax=zmax,HRT=HRT,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350)
Mendota_fit<-fit$par
Mendota_fit

#For Morris
guess=c(0.1,10)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='Morris']*10000         # m2
V=metadata$`Volume (m3)`[metadata$`lakeName`=='Morris']        # m3
HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='Morris']*365        # days
lakeDOC = master_df$DOC_mgL[master_df$lakeName=='Morris'] # g C m-3
lakeTP= master_df$TP_ugL[master_df$lakeName=='Morris']  # mg P m-3
zmax=metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='Morris']

fit=optim(guess,estInputs,SA=SA,V=V,zmax=zmax,HRT=HRT,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350)
Morris_fit<-fit$par
Morris_fit

#For Ovre
guess=c(0.1,10)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='Ovre']*10000         # m2
V=metadata$`Volume (m3)`[metadata$`lakeName`=='Ovre']        # m3
HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='Ovre']*365        # days
lakeDOC = master_df$DOC_mgL[master_df$lakeName=='Ovre'] # g C m-3
lakeTP= master_df$TP_ugL[master_df$lakeName=='Ovre']  # mg P m-3
zmax=metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='Ovre']

fit=optim(guess,estInputs,SA=SA,V=V,zmax=zmax,HRT=HRT,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350)
Ovre_fit<-fit$par
Ovre_fit

#For Struptjarn
guess=c(0.001,10)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='Struptjarn']*10000         # m2
V=metadata$`Volume (m3)`[metadata$`lakeName`=='Struptjarn']        # m3
HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='Struptjarn']*365        # days
lakeDOC = master_df$DOC_mgL[master_df$lakeName=='Struptjarn'] # g C m-3
lakeTP= master_df$TP_ugL[master_df$lakeName=='Struptjarn']  # mg P m-3
zmax=metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='Struptjarn']

fit=optim(guess,estInputs,SA=SA,V=V,zmax=zmax,HRT=HRT,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350)
Struptjarn_fit<-fit$par
Struptjarn_fit


#For Crampton
guess=c(0.001,10)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='Crampton']*10000         # m2
V=metadata$`Volume (m3)`[metadata$`lakeName`=='Crampton']        # m3
HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='Crampton']*365        # days
lakeDOC = master_df$DOC_mgL[master_df$lakeName=='Crampton'] # g C m-3
lakeTP= master_df$TP_ugL[master_df$lakeName=='Crampton']  # mg P m-3
zmax=metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='Crampton']


fit=optim(guess,estInputs,SA=SA,V=V,zmax=zmax,HRT=HRT,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350)
Crampton_fit<-fit$par
Crampton_fit


#For EastLong
guess=c(20,10)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='EastLong']*10000         # m2
V=metadata$`Volume (m3)`[metadata$`lakeName`=='EastLong']        # m3
HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='EastLong']*365        # days
lakeDOC = master_df$DOC_mgL[master_df$lakeName=='EastLong'] # g C m-3
lakeTP= master_df$TP_ugL[master_df$lakeName=='EastLong']  # mg P m-3
zmax=metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='EastLong']


fit=optim(guess,estInputs,SA=SA,V=V,zmax=zmax,HRT=HRT,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350)
EastLong_fit<-fit$par
EastLong_fit


#For Annie
guess=c(0.001,0.5)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='Annie']*10000         # m2
V=metadata$`Volume (m3)`[metadata$`lakeName`=='Annie']        # m3
HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='Annie']*365        # days
lakeDOC = master_df$DOC_mgL[master_df$lakeName=='Annie'] # g C m-3
lakeTP= master_df$TP_ugL[master_df$lakeName=='Annie']  # mg P m-3
zmax=metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='Annie']


fit=optim(guess,estInputs,SA=SA,V=V,zmax=zmax,HRT=HRT,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350)
Annie_fit<-fit$par
Annie_fit

#For Castle
guess=c(0.01,10)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='Castle']*10000         # m2
V=metadata$`Volume (m3)`[metadata$`lakeName`=='Castle']        # m3
HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='Castle']*365        # days
lakeDOC= 7.68   # g C m-3
lakeTP= 4.3    # mg P m-3
zmax=metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='Castle']


fit=optim(guess,estInputs,SA=SA,V=V,zmax=zmax,HRT=HRT,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350)
Castle_fit<-fit$par
Castle_fit


#For Simoncouche
guess=c(0.01,10)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='Simoncouche']*10000         # m2
V=metadata$`Volume (m3)`[metadata$`lakeName`=='Simoncouche']        # m3
HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='Simoncouche']*365        # days
lakeDOC = master_df$DOC_mgL[master_df$lakeName=='Simoncouche'] # g C m-3
lakeTP= master_df$TP_ugL[master_df$lakeName=='Simoncouche']  # mg P m-3
# zmax=metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='Simoncouche']
zmax=V/SA

fit=optim(guess,estInputs,SA=SA,V=V,zmax=zmax,HRT=HRT,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350)
Simoncouche_fit<-fit$par
Simoncouche_fit

#For CrystalBog
guess=c(0.01,10)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='CrystalBog']*10000         # m2
V=metadata$`Volume (m3)`[metadata$`lakeName`=='CrystalBog']        # m3
HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='CrystalBog']*365        # days
lakeDOC = master_df$DOC_mgL[master_df$lakeName=='CrystalBog'] # g C m-3
lakeTP= master_df$TP_ugL[master_df$lakeName=='CrystalBog']  # mg P m-3
zmax=metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='CrystalBog']

fit=optim(guess,estInputs,SA=SA,V=V,zmax=zmax,HRT=HRT,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350)
CrystalBog_fit<-fit$par
CrystalBog_fit


#For Rotorua
guess=c(0.01,1)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='Rotorua']*10000         # m2
V=metadata$`Volume (m3)`[metadata$`lakeName`=='Rotorua']        # m3
HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='Rotorua']*365        # days
lakeDOC = master_df$DOC_mgL[master_df$lakeName=='Rotorua'] # g C m-3
lakeTP= master_df$TP_ugL[master_df$lakeName=='Rotorua']  # mg P m-3
zmax=metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='Rotorua']


fit=optim(guess,estInputs,SA=SA,V=V,zmax=zmax,HRT=HRT,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350)
Rotorua_fit<-fit$par
Rotorua_fit

#For Sparkling
guess=c(0.01,1)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='Sparkling']*10000         # m2
V=metadata$`Volume (m3)`[metadata$`lakeName`=='Sparkling']        # m3
HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='Sparkling']*365        # days
lakeDOC = master_df$DOC_mgL[master_df$lakeName=='Sparkling'] # g C m-3
lakeTP= master_df$TP_ugL[master_df$lakeName=='Sparkling']  # mg P m-3
zmax=metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='Sparkling']


fit=optim(guess,estInputs,SA=SA,V=V,zmax=zmax,HRT=HRT,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350)
Sparkling_fit<-fit$par
Sparkling_fit

#For StGribso
guess=c(0.01,1)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='StGribso']*10000         # m2
V=metadata$`Volume (m3)`[metadata$`lakeName`=='StGribso']        # m3
HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='StGribso']*365        # days
lakeDOC = master_df$DOC_mgL[master_df$lakeName=='StGribso'] # g C m-3
lakeTP= master_df$TP_ugL[master_df$lakeName=='StGribso']  # mg P m-3
zmax=metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='StGribso']

fit=optim(guess,estInputs,SA=SA,V=V,zmax=zmax,HRT=HRT,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350)
StGribso_fit<-fit$par
StGribso_fit

#For Taihu
guess=c(10,1)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='Taihu']*10000         # m2
V=metadata$`Volume (m3)`[metadata$`lakeName`=='Taihu']        # m3
HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='Taihu']*365        # days
lakeDOC = master_df$DOC_mgL[master_df$lakeName=='Taihu'] # g C m-3
lakeTP= master_df$TP_ugL[master_df$lakeName=='Taihu']  # mg P m-3
zmax=metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='Taihu']


fit=optim(guess,estInputs,SA=SA,V=V,zmax=zmax,HRT=HRT,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350)
Taihu_fit<-fit$par
Taihu_fit

#For TroutBog
guess=c(1,1)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='TroutBog']*10000         # m2
V=metadata$`Volume (m3)`[metadata$`lakeName`=='TroutBog']        # m3
HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='TroutBog']*365        # days
lakeDOC = master_df$DOC_mgL[master_df$lakeName=='TroutBog'] # g C m-3
lakeTP= master_df$TP_ugL[master_df$lakeName=='TroutBog']  # mg P m-3
zmax=metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='TroutBog']


fit=optim(guess,estInputs,SA=SA,V=V,zmax=zmax,HRT=HRT,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350)
TroutBog_fit<-fit$par
TroutBog_fit


#For NorthGate
guess=c(0.01,1)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='Northgate']*10000         # m2
V=metadata$`Volume (m3)`[metadata$`lakeName`=='Northgate']        # m3
HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='Northgate']*365        # days
lakeDOC = master_df$DOC_mgL[master_df$lakeName=='Northgate'] # g C m-3
lakeTP= master_df$TP_ugL[master_df$lakeName=='Northgate']  # mg P m-3
zmax=metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='Northgate']


fit=optim(guess,estInputs,SA=SA,V=V,zmax=zmax,HRT=HRT,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350)
Northgate_fit<-fit$par
Northgate_fit

#For Oneida
guess=c(0.01,1)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='Oneida']*10000         # m2
V=metadata$`Volume (m3)`[metadata$`lakeName`=='Oneida']        # m3
HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='Oneida']*365        # days
lakeDOC = master_df$DOC_mgL[master_df$lakeName=='Oneida'] # g C m-3
lakeTP= master_df$TP_ugL[master_df$lakeName=='Oneida']  # mg P m-3
# zmax=metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='Oneida']
zmax=V/SA

fit=optim(guess,estInputs,SA=SA,V=V,zmax=zmax,HRT=HRT,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350)
Oneida_fit<-fit$par
Oneida_fit

#For Ward
# set lake parameters

# initial guess for Pin (mg P m-3) and DOCin (g C m-3)
guess=c(0.001,1)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='Ward']*10000         # m2
V=metadata$`Volume (m3)`[metadata$`lakeName`=='Ward']        # m3
HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='Ward']*365        # days
lakeDOC = master_df$DOC_mgL[master_df$lakeName=='Ward'] # g C m-3
lakeTP= master_df$TP_ugL[master_df$lakeName=='Ward']  # mg P m-3
zmax=metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='Ward']

fit=optim(guess,estInputs,SA=SA,V=V,zmax=zmax,HRT=HRT,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350)
Ward_fit<-fit$par
Ward_fit

#For WestLong
guess=c(0.001,1)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='WestLong']*10000         # m2
V=metadata$`Volume (m3)`[metadata$`lakeName`=='WestLong']        # m3
HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='WestLong']*365        # days
lakeDOC = master_df$DOC_mgL[master_df$lakeName=='WestLong'] # g C m-3
lakeTP= master_df$TP_ugL[master_df$lakeName=='WestLong']  # mg P m-3
zmax=metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='WestLong']


fit=optim(guess,estInputs,SA=SA,V=V,zmax=zmax,HRT=HRT,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350)
WestLong_fit<-fit$par
WestLong_fit

#For Kentucky
guess=c(0.1,1)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='Kentucky']*10000         # m2
V=metadata$`Volume (m3)`[metadata$`lakeName`=='Kentucky']        # m3
HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='Kentucky']*365        # days
lakeDOC = master_df$DOC_mgL[master_df$lakeName=='Kentucky'] # g C m-3
lakeTP= master_df$TP_ugL[master_df$lakeName=='Kentucky']  # mg P m-3
zmax=metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='Kentucky']


fit=optim(guess,estInputs,SA=SA,V=V,zmax=zmax,HRT=HRT,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350)
Kentucky_fit<-fit$par
Kentucky_fit

#For Pontchartrain
guess=c(0.001, 1)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='Pontchartrain']*10000         # m2
# V=metadata$`Volume (m3)`[metadata$`lakeName`=='Pontchartrain']        # m3
V=95000000 #From Wikipedia
HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='Pontchartrain']*365        # days
lakeDOC = master_df$DOC_mgL[master_df$lakeName=='Pontchartrain'] # g C m-3
lakeTP= master_df$TP_ugL[master_df$lakeName=='Pontchartrain']  # mg P m-3
zmax=metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='Pontchartrain']

fit=optim(guess,estInputs,SA=SA,V=V,zmax=zmax,HRT=HRT,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350)
Pontchartrain_fit<-fit$par
Pontchartrain_fit

#For Vedstedso
guess=c(1,1)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='Vedstedso']*10000         # m2
V=metadata$`Volume (m3)`[metadata$`lakeName`=='Vedstedso']        # m3
HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='Vedstedso']*365        # days
lakeDOC = master_df$DOC_mgL[master_df$lakeName=='Vedstedso'] # g C m-3
lakeTP= master_df$TP_ugL[master_df$lakeName=='Vedstedso']  # mg P m-3
zmax=metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='Vedstedso']

fit=optim(guess,estInputs,SA=SA,V=V,zmax=zmax,HRT=HRT,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350)
Vedstedso_fit<-fit$par
Vedstedso_fit


#For Hampenso
guess=c(20,10)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='Hampenso']*10000         # m2
V=metadata$`Volume (m3)`[metadata$`lakeName`=='Hampenso']        # m3
HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='Hampenso']*365        # days
lakeDOC = master_df$DOC_mgL[master_df$lakeName=='Hampenso'] # g C m-3
lakeTP= master_df$TP_ugL[master_df$lakeName=='Hampenso']  # mg P m-3
zmax=metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='Hampenso']

fit=optim(guess,estInputs,SA=SA,V=V,zmax=zmax,HRT=HRT,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350)
Hampenso_fit<-fit$par
Hampenso_fit


#For Rotoiti
guess=c(0.1,1)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='Rotoiti']*10000         # m2
V=metadata$`Volume (m3)`[metadata$`lakeName`=='Rotoiti']        # m3
HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='Rotoiti']*365        # days
lakeDOC= 1.35   # g C m-3
lakeTP= 30.3    # mg P m-3
zmax=metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='Rotoiti']

fit=optim(guess,estInputs,SA=SA,V=V,zmax=zmax,HRT=HRT,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350)
Rotoiti_fit<-fit$par
Rotoiti_fit

#For FredriksburgSlotso
guess=c(100,10)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='FredriksburgSlotso']*10000         # m2
V=metadata$`Volume (m3)`[metadata$`lakeName`=='FredriksburgSlotso']        # m3
HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='FredriksburgSlotso']*365        # days
lakeDOC = master_df$DOC_mgL[master_df$lakeName=='FredriksburgSlotso'] # g C m-3
lakeTP= master_df$TP_ugL[master_df$lakeName=='FredriksburgSlotso']  # mg P m-3
zmax=metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='FredriksburgSlotso']


fit=optim(guess,estInputs,SA=SA,V=V,zmax=zmax,HRT=HRT,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350)
FredriksburgSlotso_fit<-fit$par
FredriksburgSlotso_fit

#For Croche
guess=c(0.01,10)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='Croche']*10000         # m2
V=metadata$`Volume (m3)`[metadata$`lakeName`=='Croche']        # m3
HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='Croche']*365        # days
lakeDOC = master_df$DOC_mgL[master_df$lakeName=='Croche'] # g C m-3
lakeTP= master_df$TP_ugL[master_df$lakeName=='Croche']  # mg P m-3
# zmax=metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='Croche']
zmax=V/SA

fit=optim(guess,estInputs,SA=SA,V=V,zmax=zmax,HRT=HRT,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350)
Croche_fit<-fit$par
Croche_fit

#For Lillinonah
guess=c(0.0001,1)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='Lillinonah']*10000         # m2
V=metadata$`Volume (m3)`[metadata$`lakeName`=='Lillinonah']        # m3
HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='Lillinonah']*365        # days
lakeDOC= 4.213750   # g C m-3
lakeTP= master_df$TP_ugL[master_df$lakeName=='Lillinonah']  # mg P m-3
zmax=metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='Lillinonah']

fit=optim(guess,estInputs,SA=SA,V=V,zmax=zmax,HRT=HRT,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350)
Lillinonah_fit<-fit$par
Lillinonah_fit

#For LittleRock
# set lake parameters

# initial guess for Pin (mg P m-3) and DOCin (g C m-3)
guess=c(1,10)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='LittleRock']*10000         # m2
V=metadata$`Volume (m3)`[metadata$`lakeName`=='LittleRock']        # m3
HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='LittleRock']*365        # days
lakeDOC = master_df$DOC_mgL[master_df$lakeName=='LittleRock'] # g C m-3
# lakeTP= master_df$TP_ugL[master_df$lakeName=='LittleRock']  # mg P m-3
lakeTP = 5.87  #replace with TDP value otherwise we get an inflow concentration of 3000 ug/L...
# zmax=metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='LittleRock']
zmax=V/SA
zmean = metadata$`Mean lake depth (m)`[metadata$`lakeName`=='LittleRock']


fit=optim(guess,estInputs,SA=SA,V=V,zmax=zmax,HRT=HRT,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350)
LittleRock_fit<-fit$par
LittleRock_fit

#For Onondaga
# set lake parameters

# initial guess for Pin (mg P m-3) and DOCin (g C m-3)
guess=c(0.001,5)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='Onondaga']*10000         # m2
V=metadata$`Volume (m3)`[metadata$`lakeName`=='Onondaga']        # m3
HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='Onondaga']*365        # days
lakeDOC = master_df$DOC_mgL[master_df$lakeName=='Onondaga'] # g C m-3
lakeTP= master_df$TP_ugL[master_df$lakeName=='Onondaga']  # mg P m-3
zmax=metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='Onondaga']


fit=optim(guess,estInputs,SA=SA,V=V,zmax=zmax,HRT=HRT,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350)
Onondaga_fit<-fit$par
Onondaga_fit



#For Schulzensee
# set lake parameters

# initial guess for Pin (mg P m-3) and DOCin (g C m-3)
guess=c(0.001,1)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='Schulzensee']*10000         # m2
V=metadata$`Volume (m3)`[metadata$`lakeName`=='Schulzensee']        # m3
# HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='Schulzensee']*365        # days
HRT=3*365 #300 years seems outrageous, so trying this. 
lakeDOC = master_df$DOC_mgL[master_df$lakeName=='Schulzensee'] # g C m-3
lakeTP= master_df$TP_ugL[master_df$lakeName=='Schulzensee']  # mg P m-3
# zmax=metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='Schulzensee']
zmax=V/SA

fit=optim(guess,estInputs,SA=SA,V=V,zmax=zmax,HRT=HRT,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350)
Schulzensee_fit<-fit$par
Schulzensee_fit



#For SkyPond
# set lake parameters

# initial guess for Pin (mg P m-3) and DOCin (g C m-3)
guess=c(0.001,1)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='SkyPond']*10000         # m2
V=metadata$`Volume (m3)`[metadata$`lakeName`=='SkyPond']        # m3
HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='SkyPond']*365        # days
lakeDOC = master_df$DOC_mgL[master_df$lakeName=='SkyPond'] # g C m-3
lakeTP= master_df$TP_ugL[master_df$lakeName=='SkyPond']  # mg P m-3
zmax=metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='SkyPond']


fit=optim(guess,estInputs,SA=SA,V=V,zmax=zmax,HRT=HRT,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350)
SkyPond_fit<-fit$par
SkyPond_fit


#For Taupo
guess=c(0.01,10)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='Taupo']*10000         # m2
V=metadata$`Volume (m3)`[metadata$`lakeName`=='Taupo']        # m3
HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='Taupo']*365        # days
# lakeDOC= 0.613   # g C m-3
# lakeTP= 4.65    # mg P m-3
lakeDOC = master_df$DOC_mgL[master_df$lakeName=='Taupo'] # g C m-3
lakeTP= master_df$TP_ugL[master_df$lakeName=='Taupo']  # mg P m-3
# zmax=metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='Taupo']
zmax=V/SA

fit=optim(guess,estInputs,SA=SA,V=V,zmax=zmax,HRT=HRT,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350)
Taupo_fit<-fit$par
Taupo_fit


#For Utah
# set lake parameters

# initial guess for Pin (mg P m-3) and DOCin (g C m-3)
guess=c(25,1)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='Utah']*10000         # m2
V=metadata$`Volume (m3)`[metadata$`lakeName`=='Utah']        # m3
HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='Utah']*365        # days
# lakeDOC= 6.74   # g C m-3
lakeDOC = master_df$DOC_mgL[master_df$lakeName=='Utah'] # g C m-3
# lakeTP= 52.8    # mg P m-3
lakeTP= master_df$TP_ugL[master_df$lakeName=='Utah']  # mg P m-3
zmax=metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='Utah']


fit=optim(guess,estInputs,SA=SA,V=V,zmax=zmax,HRT=HRT,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350)
Utah_fit<-fit$par
Utah_fit



#For Trout
guess=c(0.001,10)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='Trout']*10000         # m2
V=metadata$`Volume (m3)`[metadata$`lakeName`=='Trout']        # m3
HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='Trout']*365        # days
lakeDOC = master_df$DOC_mgL[master_df$lakeName=='Trout'] # g C m-3
lakeTP= master_df$TP_ugL[master_df$lakeName=='Trout']  # mg P m-3
zmax=metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='Trout']

fit=optim(guess,estInputs,SA=SA,V=V,zmax=zmax,HRT=HRT,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350)
Trout_fit<-fit$par
Trout_fit


#For Vortsjarv
guess=c(20,10)
SA=metadata$`Surface area (ha)`[metadata$`lakeName`=='Vortsjarv']*10000         # m2
V=metadata$`Volume (m3)`[metadata$`lakeName`=='Vortsjarv']        # m3
HRT=metadata$`Lake residence time (year)`[metadata$`lakeName`=='Vortsjarv']*365        # days
lakeDOC = master_df$DOC_mgL[master_df$lakeName=='Vortsjarv'] # g C m-3
lakeTP= master_df$TP_ugL[master_df$lakeName=='Vortsjarv']  # mg P m-3
zmax=metadata$`Maximum lake depth (m)`[metadata$`lakeName`=='Vortsjarv']

fit=optim(guess,estInputs,SA=SA,V=V,zmax=zmax,HRT=HRT,lakeDOC=lakeDOC,lakeTP=lakeTP,I0=350)
Vortsjarv_fit<-fit$par
Vortsjarv_fit


# estimated loads dataframe -----------------------------------------------
variable<-c("TP_mgm3","DOC_gm3")

# str(uncertain_load_estimates)
#These lakes don't have actual inflows
# uncertain_load_estimates<-data.frame(variable,bolger_fit, Cranberry_fit, Hummingbird_fit,
#                                      Schulzensee_fit)
# write.csv(uncertain_load_estimates, here("data/load_Est/uncertain_load_estimates.txt"), row.names=FALSE)

#These lakes do have inflows, as far as I know
load_estimates<-data.frame(variable,nastjarn_fit,Jordan_fit,
                           brown_fit,Almberga_fit,Lillinonah_fit, bolger_fit,
                           bay_fit, Cranberry_fit, Erken_fit, Hummingbird_fit,
                           Gollinsee_fit,LittleRock_fit, Mirror_fit, Mueggelsee_fit,
                           Onondaga_fit, Pontchartrain_fit, Schulzensee_fit,SkyPond_fit,
                           Sunapee_fit, Acton_fit, P1_fit, P8_fit, Taupo_fit, TheLoch_fit,
                           Utah_fit, YunYang_fit, Harp_fit, Langtjern_fit,
                           Lillsjoliden_fit, Feeagh_fit, Mangstrettjarn_fit, Mendota_fit,
                           Morris_fit, Ovre_fit, Struptjarn_fit, Trout_fit,
                           Vortsjarv_fit, Crampton_fit, EastLong_fit, Annie_fit,
                           Croche_fit, Simoncouche_fit, CrystalBog_fit, FredriksburgSlotso_fit,
                           Hampenso_fit, Rotorua_fit, Sparkling_fit, StGribso_fit,
                           Taihu_fit, TroutBog_fit, Vedstedso_fit, Northgate_fit,
                           Oneida_fit, Ward_fit, WestLong_fit, Kentucky_fit)
# View(load_estimates)
write.csv(load_estimates, here("data/load_estimates/load_estimates_20220801_oleksyset.csv"), row.names=FALSE)

