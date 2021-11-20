#VERSION 3- April 14, 2021
#The biggest difference here is I'm not solving for in-lake TP and DOC with the Huisman function.



# FORCE model to use in-lake TP and DOC -----------------------------------



library(deSolve)
#Making GPP estimates from the Kelly Model from known data
sims<-left_join(master_df,load_estimates_huisman, by="lakeName")  %>% 
  select(lakeName, DOC_load_kgday_mean,meanzMix, zMax,TP_load_kgday_mean,
         SA_ha, HRT_days,DOC_mgL, TP_ugL, TP_mgm3, DOC_gm3, V_m3) %>%
  rename(TP_gm3=TP_mgm3)%>% #Mistakenly labeled mg/m3. Actually in units of g/m3 (mg/L). Refers to in-flow concentration of TP. 
  drop_na() %>%
  mutate(CP=DOC_load_kgday_mean/TP_load_kgday_mean,
         SA=SA_ha/100, #km2
         # TP_mgL=TP_ugL/1000, #mg/L
         TP_mgm3=TP_gm3*1000) %>% #The original is ACTUALLY g/m3.
  rename(DOC=DOC_mgL, #mg/L=g/m3, in-lake DOC concentrations
         TP=TP_ugL, #ug/L=mg/m3, in-lake TP concentrations
         Pin=TP_mgm3, #ug/L=mg/m3, estimated TP concentration of inflow
         DOCin=DOC_gm3) %>% #mg/L=g/m3, estimated DOC concentration of inflow
  distinct()
# sims<-sims[5:8,]

#make dailyPAR data - just use some random incident light data
I0<-300

lightAtten<-function(z,I0,kD){
  Iz=I0*exp(-kD*z)
  return(Iz)
}

# function for simulationg lake primary productivity as a function of DOC and P supply, lake surface area, and residence time
huisman<-function(t,y,params){
  with(as.list(c(y,params)),{
    
    # using Morris paper relationship, but symbols from Huisman
    ##Kelly et al., 2018 EQN 6
    kD=kA*A+kDOC*DOC	-0.05	#m-1; based on Morris paper
    
    # from a published paper -> I'll refind the paper eventually
    ##Kelly et al., 2018 EQN 7
    zmix=10^(-0.515*log10(DOC)+0.115*log10(2*sqrt(SA/pi))+0.991)
    if(zmix>zmax){zmix=zmax}

    
    # V=SA*1e6*zmax
    # Qin=V/365
    
    ##Kelly et al., EQN 8
    Izmix=lightAtten(z=zmix,I0=I0,kD=kD)
    
    # biomass specific growth integrated across mixed layer
    ##Kelly et al., EQN 2
    prod=(pA/(kD*zmix))*log((hA+I0)/(hA+Izmix))*(P/(P+mA))	# d-1
    
    ##Kelly et al., EQN 1
    dA.dt=A*prod-lA*A-v/zmix*A-Qin/(zmix*SA*1e6)*A	# mg C m-3
    
    ##Kelly et al., EQN 4
    # dP.dt=Qin/(zmix*SA*1e6)*(Pin-P)+cA*lA*A*rec-cA*A*prod		# mg P m-3 (in epi)
    
    ##Kelly et al., EQN 3
    # dDOC.dt=(Qin/(zmix*SA*1e6))*(DOCin-DOC)-decay*DOC				# g C m-3 (in epi)
    
    # return(list(c(dA.dt,dP.dt,dDOC.dt)))
    return(list(c(dA.dt)))
  })
}


# SAs=sims$SA_ha
# DOCs<-sims$DOC
# # CPs<-sims$CP
# CPs<-sims$DOCin/(sims$Pin*1000)
# zMax<-sims$zMax
# Pin<-sims$Pin
# DOCin<-sims$DOCin
# P<-sims$TP
# zmax<-sims$zMax
# 
# storing equilibrium state variables across simulations
# storeAs=array(NA,dim=c(length(DOCs),length(CPs),length(SAs)))
# storePs=storeAs
# storeDOCs=storeAs
times=1:2000
storeAs=matrix(NA,length(times),nrow(sims))
storePs=storeAs
storeDOCs=storeAs

# loop through different surface areas, DOC loads, and load stoichs
for(j in 1:nrow(sims)){
      #submittd
      #parms=c(SA=SAs[k],zmax=10,kDOC=0.22,kA=0.00015,lA=0.1,pA=1,hA=35,mA=3,decay=0.001,Qin=SAs[k]*1e6*zmax/365,Pin=DOCs[j]/CPs[i],DOCin=DOCs[j],cA=0.0045,v=0.1,rec=0.9)
      # used in jager & diehl
      #parms=c(SA=SAs[k],zmax=10,kDOC=0.22,kA=0.00015,lA=0.1,pA=1,hA=100,mA=3,decay=0.001,Qin=SAs[k]*1e6*zmax/365,Pin=DOCs[j]/CPs[i],DOCin=DOCs[j],cA=0.015,v=0.1,rec=0.9)
      # tweaking to get range of observations
      parms=c(SA=sims$SA[j],
              zmax=sims$zMax[j],
              kDOC=0.42,
              kA=0.00022,
              lA=0.1,
              pA=1.2,
              hA=55,
              mA=2,
              decay=0.001,
              Pin=sims$Pin[j],
              DOCin=sims$DOCin[j],
              cA=0.015,
              v=0.05,
              rec=0.99,
              V=sims$V_m3[j],
              Qin=sims$V_m3[j]/sims$HRT_days[j],
              # Qin=sims$SA[j]*1e6*sims$zMax[j]/365,
              # Qin=sims$V_m3[j]/365,
              P=sims$TP[j],
              DOC=sims$DOC[j])
      
      
      # starting state variables
      # n<-c(A=500,
      #      P=sims$TP[j],
      #      DOC=sims$DOC[j])
      n<-c(A=500)
      

      # 
      # n<-c(A=100,
      #      P=sims$Pin[j],
      #      DOC=sims$DOCin[j])
      
      
      # simulate with ode
      run=ode(y=n,times=times,func=huisman,parms=parms)

      storeAs[,j]<-run[,2]
      # storePs[,j]<-run[,3]
      storePs<-matrix(sims$TP, nrow=2000, ncol=24, byrow=TRUE)
      # storePs<-matrix(sims$TP, nrow=2000, ncol=24, byrow=TRUE)/(storeAs*parms[13]) #because we want dissolved P, not TP? 
      storeDOCs<-matrix(sims$DOC, nrow=2000, ncol=24, byrow=TRUE)
      # storeDOCs[,j]<-run[,4]
      
      print(j)
    }


# calculate other simulation characteristics from stored equilibrium state variables
store_kD=parms[4]*storeAs+parms[3]*storeDOCs-0.05 #ORIGINAL

# store_kD=parms[4]*storeAs+parms[3]*parms[18]-0.05 #ALT
store_zmix<-store_kD
for(i in 1:ncol(store_zmix)){ #ORIGINAL
  store_zmix[,i]<-10^(-0.515*log10(storeDOCs[,i])+0.115*log10(2*sqrt((sims$SA[i])/pi))+0.991)
}
# for(i in 1:ncol(store_zmix)){ #ALT
#   store_zmix[,i]<-10^(-0.515*log10(sims$DOC[i])+0.115*log10(2*sqrt((sims$SA[i])/pi))+0.991)
# }
# zmax=10
# store_zmix[store_zmix>zmax]=zmax


storeTP=storeAs*parms[13]+storePs #ORIGINAL
# storeTP=storePs ##I think we want to store TP as Ps because our input IS total P, not dissolved.
storePP=store_kD*0
light.limit.d<-store_kD*0
nutrient.limit.d<-store_kD*0
storer<-store_kD*0
for(i in 1:ncol(store_kD)){
  for(j in 1:nrow(store_kD)){

      cP=storePs[j,i]
      
      kD=store_kD[j,i]
      
      zmix=store_zmix[j,i]
      
      Izmix=lightAtten(z=zmix,I0=I0,kD=kD)
      
      pA=parms[6]
      hA=parms[7]
      mA=parms[8]
      
      storer[j,i]<-(pA/(kD*zmix))*log((hA+I0)/(hA+Izmix))*(cP/(cP+mA))
      storePP[j,i]<-storeAs[j,i]*(pA/(kD*zmix))*log((hA+I0)/(hA+Izmix))*(cP/(cP+mA))	
      light.limit.d[j,i]=log((hA+I0)/(hA+Izmix))
      nutrient.limit.d[j,i]<-(cP/(cP+mA))
      
    }
  }

store_arealPP=storePP*store_zmix

sims$eqDOC=storeDOCs[nrow(storeDOCs),]
sims$eqTP=storeTP[nrow(storeTP),]
sims$PPareal=store_arealPP[nrow(store_arealPP),]
sims$PPvol=storePP[nrow(storePP),]
sims$zMix_mod=store_zmix[nrow(store_zmix),]
sims$A=storeAs[nrow(storeAs),]

#PLOTS

sims %>%
  ggplot(aes(x=eqDOC, y=PPareal))+
  geom_point(shape=21, size=3)+
  theme_bw()+
  ggtitle("PREDICTED")+
  ylab(expression(paste('GPP (mg C m'^-2,' day'^-1,')')))+
  xlab(expression(paste('DOC mg L'^-1,)))+
  geom_text_repel(aes(label = lakeName), size = 3)

master_df %>%
  filter(lakeName %in% c( "Crampton","EastLong","Erken",   "Feeagh",  "Gollinsee" ,    "Harp",    "Langtjern" ,     "Lillsjoliden",  "Mangstrettjarn", "Mendota", "Morris", 
      "Mueggelsee" ,    "Nastjarn","Ovre",    "SkyPond", "Sparkling" ,     "Struptjarn" ,    "Sunapee", "Taupo",   "TheLoch", "Trout",   "Utah",   
      "Vortsjarv"   ,   "YunYang"      )) %>%
  ggplot(aes(x=DOC_mgL, y=meanGPP))+
  geom_point(shape=21, size=3)+
  theme_bw()+
  ggtitle("OBSERVED")+
  ylab(expression(paste('GPP (mg C m'^-2,' day'^-1,')')))+
  xlab(expression(paste('DOC mg L'^-1,)))+
  geom_text_repel(aes(label = lakeName), size = 3)


#OBSERVED VS PREDICTED
sims %>%
  select(lakeName, eqDOC, PPareal) %>%
  left_join(., master_df %>%
              select(lakeName, meanGPP, DOC_mgL), by="lakeName") %>%
  ggplot(aes(x=meanGPP, y=PPareal))+
  geom_point(shape=21, size=3)+
  theme_bw()+
  ylab(expression(paste('PRED. GPP (mg C m'^-2,' day'^-1,')')))+
  xlab(expression(paste('OBS. GPP (mg C m'^-2,' day'^-1,')')))+
  geom_text_repel(aes(label = lakeName), size = 3)+
  geom_abline(intercept = 0, slope = 1)+
  ggtitle("Force model to solve for DOC and TP concentrations")+
  xlim(c(0,8000))+ylim(c(0,8500))

#Predicted "equilibrium" DOC values are much lower than observed DOC? Why?
sims %>%
  ggplot(aes(x=eqDOC, y=DOC, fill=lakeName))+
  geom_point(shape=21, size=3)

sims %>%
  ggplot(aes(x=eqDOC, y=DOCin, fill=lakeName))+
  geom_point(shape=21, size=3)


sims %>%
  ggplot(aes(x=eqTP, y=PPareal, fill=lakeName))+
  geom_point(shape=21, size=3)

#Doesn't seem like there is any relationship at all between equilibrium TP and actual in-lake TP
sims %>%
  ggplot(aes(x=eqTP, y=TP, fill=lakeName))+
  geom_point(shape=21, size=3)
#Is it possible that TP is in the wrong units, resulting in extremely low PP estimates? 

#What do the zMix estimates look like compared to actual mean zMix observed values?
sims %>%
  ggplot(aes(x=meanzMix, y=zMix_mod, fill=lakeName))+
  geom_point(shape=21, size=3)

sims %>%
  select(DOC) %>%
  pivot_wider()
  filter(row_number() <= 2000) %>% 
  bind_rows(df)
  
  
  
  # FORCE model to GUESS in-lake TP and DOC -----------------------------------
  
  
  
  library(deSolve)
  #Making GPP estimates from the Kelly Model from known data
  sims<-left_join(master_df,load_estimates_huisman, by="lakeName")  %>% 
    select(lakeName, DOC_load_kgday_mean,meanzMix, zMax,TP_load_kgday_mean,
           SA_ha, HRT_days,DOC_mgL, TP_ugL, TP_mgm3, DOC_gm3, V_m3) %>%
    rename(TP_gm3=TP_mgm3)%>% #Mistakenly labeled mg/m3. Actually in units of g/m3. Refers to in-flow concentration of TP. 
    drop_na() %>%
    mutate(CP=DOC_load_kgday_mean/TP_load_kgday_mean,
           SA=SA_ha/100, #km2
           # TP_mgL=TP_ugL/1000, #mg/L
           TP_mgm3=TP_gm3*1000) %>% #The original is ACTUALLY g/m3.
    rename(DOC=DOC_mgL, #mg/L=g/m3, in-lake DOC concentrations
           TP=TP_ugL, #ug/L=mg/m3, in-lake TP concentrations
           Pin=TP_mgm3, #ug/L=mg/m3, estimated TP concentration of inflow
           DOCin=DOC_gm3) %>% #mg/L=g/m3, estimated DOC concentration of inflow
    distinct()
  # sims<-sims[5:8,]
  
  
  #make dailyPAR data - just use some random incident light data
  I0<-300
  
  lightAtten<-function(z,I0,kD){
    Iz=I0*exp(-kD*z)
    return(Iz)
  }
  
  # function for simulationg lake primary productivity as a function of DOC and P supply, lake surface area, and residence time
  huisman<-function(t,y,params){
    with(as.list(c(y,params)),{
      
      # using Morris paper relationship, but symbols from Huisman
      ##Kelly et al., 2018 EQN 6
      kD=kA*A+kDOC*DOC	-0.05	#m-1; based on Morris paper
      
      # from a published paper -> I'll refind the paper eventually
      ##Kelly et al., 2018 EQN 7
      zmix=10^(-0.515*log10(DOC)+0.115*log10(2*sqrt(SA/pi))+0.991)
      if(zmix>zmax){zmix=zmax}
      
      
      # V=SA*1e6*zmax
      # Qin=V/365
      
      ##Kelly et al., EQN 8
      Izmix=lightAtten(z=zmix,I0=I0,kD=kD)
      
      # biomass specific growth integrated across mixed layer
      ##Kelly et al., EQN 2
      prod=(pA/(kD*zmix))*log((hA+I0)/(hA+Izmix))*(P/(P+mA))	# d-1
      
      ##Kelly et al., EQN 1
      dA.dt=A*prod-lA*A-v/zmix*A-Qin/(zmix*SA*1e6)*A	# mg C m-3
      
      ##Kelly et al., EQN 4
      dP.dt=Qin/(zmix*SA*1e6)*(Pin-P)+cA*lA*A*rec-cA*A*prod		# mg P m-3 (in epi)
      
      ##Kelly et al., EQN 3
      dDOC.dt=(Qin/(zmix*SA*1e6))*(DOCin-DOC)-decay*DOC				# g C m-3 (in epi)
      
      return(list(c(dA.dt,dP.dt,dDOC.dt)))
      # return(list(c(dA.dt)))
    })
  }
  

  times=1:2000
  storeAs=matrix(NA,length(times),nrow(sims))
  storePs=storeAs
  storeDOCs=storeAs
  
  # loop through different surface areas, DOC loads, and load stoichs
  for(j in 1:nrow(sims)){
    #submittd
    #parms=c(SA=SAs[k],zmax=10,kDOC=0.22,kA=0.00015,lA=0.1,pA=1,hA=35,mA=3,decay=0.001,Qin=SAs[k]*1e6*zmax/365,Pin=DOCs[j]/CPs[i],DOCin=DOCs[j],cA=0.0045,v=0.1,rec=0.9)
    # used in jager & diehl
    #parms=c(SA=SAs[k],zmax=10,kDOC=0.22,kA=0.00015,lA=0.1,pA=1,hA=100,mA=3,decay=0.001,Qin=SAs[k]*1e6*zmax/365,Pin=DOCs[j]/CPs[i],DOCin=DOCs[j],cA=0.015,v=0.1,rec=0.9)
    # tweaking to get range of observations
    parms=c(SA=sims$SA[j],
            zmax=sims$zMax[j],
            kDOC=0.42,
            kA=0.00022,
            lA=0.1,
            pA=1.2,
            hA=55,
            mA=2,
            decay=0.001,
            Pin=sims$Pin[j],
            DOCin=sims$DOCin[j],
            cA=0.015,
            v=0.05,
            rec=0.99,
            V=sims$V_m3[j],
            # Qin=sims$Qin[j],
            Qin=sims$V_m3[j]/sims$HRT_days[j],
            # Qin=sims$V_m3[j]/365,
            # Qin=sims$SA[j]*1e6*sims$zMax[j]/365, #Could also try this.. [m3/day]
            P=sims$TP[j],
            DOC=sims$DOC[j])
    
    
    # starting state variables
    n<-c(A=500,
         P=sims$TP[j],
         DOC=sims$DOC[j])
    # n<-c(A=500)
    
    
    # 
    # n<-c(A=100,
    #      P=sims$Pin[j],
    #      DOC=sims$DOCin[j])
    
    
    # simulate with ode
    run=ode(y=n,times=times,func=huisman,parms=parms)
    
    storeAs[,j]<-run[,2]
    storePs[,j]<-run[,3]
    # storePs<-matrix(sims$TP, nrow=2000, ncol=13, byrow=TRUE)
    # storePs<-matrix(sims$TP, nrow=2000, ncol=24, byrow=TRUE)/(storeAs*parms[13]) #because we want dissolved P, not TP? 
    # storeDOCs<-matrix(sims$DOC, nrow=2000, ncol=13, byrow=TRUE)
    storeDOCs[,j]<-run[,4]
    
    print(j)
  }
  
  
  # calculate other simulation characteristics from stored equilibrium state variables
  store_kD=parms[4]*storeAs+parms[3]*storeDOCs-0.05 #ORIGINAL
  
  # store_kD=parms[4]*storeAs+parms[3]*parms[18]-0.05 #ALT
  store_zmix<-store_kD
  for(i in 1:ncol(store_zmix)){ #ORIGINAL
    store_zmix[,i]<-10^(-0.515*log10(storeDOCs[,i])+0.115*log10(2*sqrt((sims$SA[i])/pi))+0.991)
  }
  # for(i in 1:ncol(store_zmix)){ #ALT
  #   store_zmix[,i]<-10^(-0.515*log10(sims$DOC[i])+0.115*log10(2*sqrt((sims$SA[i])/pi))+0.991)
  # }
  # zmax=10
  # store_zmix[store_zmix>zmax]=zmax
  
  
  storeTP=storeAs*parms[13]+storePs #ORIGINAL
  # storeTP=storePs ##I think we want to store TP as Ps because our input IS total P, not dissolved. 
  storePP=store_kD*0
  light.limit.d<-store_kD*0
  nutrient.limit.d<-store_kD*0
  storer<-store_kD*0
  for(i in 1:ncol(store_kD)){
    for(j in 1:nrow(store_kD)){
      
      cP=storePs[j,i]
      
      kD=store_kD[j,i]
      
      zmix=store_zmix[j,i]
      
      Izmix=lightAtten(z=zmix,I0=I0,kD=kD)
      
      pA=parms[6]
      hA=parms[7]
      mA=parms[8]
      
      storer[j,i]<-(pA/(kD*zmix))*log((hA+I0)/(hA+Izmix))*(cP/(cP+mA))
      storePP[j,i]<-storeAs[j,i]*(pA/(kD*zmix))*log((hA+I0)/(hA+Izmix))*(cP/(cP+mA))	
      light.limit.d[j,i]=log((hA+I0)/(hA+Izmix))
      nutrient.limit.d[j,i]<-(cP/(cP+mA))
      
    }
  }
  
  store_arealPP=storePP*store_zmix
  
  sims$eqDOC=storeDOCs[nrow(storeDOCs),]
  sims$eqTP=storeTP[nrow(storeTP),]
  sims$PPareal=store_arealPP[nrow(store_arealPP),]
  sims$PPvol=storePP[nrow(storePP),]
  sims$zMix_mod=store_zmix[nrow(store_zmix),]
  sims$A=storeAs[nrow(storeAs),]
  
  #PLOTS
  
  sims %>%
    ggplot(aes(x=eqDOC, y=PPareal))+
    geom_point(shape=21, size=3)+
    theme_bw()+
    ggtitle("PREDICTED")+
    ylab(expression(paste('GPP (mg C m'^-2,' day'^-1,')')))+
    xlab(expression(paste('DOC mg L'^-1,)))+
    # scale_y_log10(expression(paste('GPP (mg C m'^-2,' day'^-1,')')), labels = scales::comma)+
    geom_text_repel(aes(label = lakeName), size = 3)
  
  master_df %>%
    filter(lakeName %in% c( "Crampton","EastLong","Erken",   "Feeagh",  "Gollinsee" ,    "Harp",    "Langtjern" ,     "Lillsjoliden",  "Mangstrettjarn", "Mendota", "Morris",   "Mueggelsee" ,    "Nastjarn","Ovre",    "SkyPond", "Sparkling" ,     "Struptjarn" ,    "Sunapee", "Taupo",   "TheLoch", "Trout",   "Utah",   "Vortsjarv"   ,   "YunYang" )) %>%
    ggplot(aes(x=DOC_mgL, y=meanGPP))+
    geom_point(shape=21, size=3)+
    theme_bw()+
    ggtitle("OBSERVED")+
    ylab(expression(paste('GPP (mg C m'^-2,' day'^-1,')')))+
    xlab(expression(paste('DOC mg L'^-1,)))+
    geom_text_repel(aes(label = lakeName), size = 3)
  
  
  #OBSERVED VS PREDICTED
  sims %>%
    select(lakeName, eqDOC, PPareal) %>%
    left_join(., master_df %>%
                select(lakeName, meanGPP, DOC_mgL), by="lakeName") %>%
    ggplot(aes(x=meanGPP, y=PPareal))+
    geom_point(shape=21, size=3)+
    theme_bw()+
    ylab(expression(paste('PRED. GPP (mg C m'^-2,' day'^-1,')')))+
    xlab(expression(paste('OBS. GPP (mg C m'^-2,' day'^-1,')')))+
    geom_text_repel(aes(label = lakeName), size = 3)+
    geom_abline(intercept = 0, slope = 1)+
    ggtitle("Force model to solve for DOC and TP concentrations")+
    xlim(c(0,1600))+ylim(c(0,1600))
  
  #Predicted "equilibrium" DOC values are much lower than observed DOC? Why?
  sims %>%
    ggplot(aes(x=DOC, y=eqDOC, fill=lakeName))+
    geom_point(shape=21, size=3)+
    geom_abline(intercept = 0, slope = 1)+
    ylab(expression(paste('PRED. DOC mg L'^-1,)))+
    ylab(expression(paste('OBS. DOC mg L'^-1,)))+
    ggtitle("Force model to solve for DOC and TP concentrations")+
    theme_bw()
  #Part of the reason this looks this good is because we modelled DOCin based on 
  #in-lake DOC concentrations. Kind of circular isn't it? 
  
  #Relationship between equilibrium TP and actual in-lake TP
  sims %>%
    ggplot(aes(y=eqTP, x=TP, fill=lakeName))+
    geom_point(shape=21, size=3)+
    geom_abline(intercept = 0, slope = 1)+
    ylab(expression(paste('PRED. TP ug L'^-1,)))+
    xlab(expression(paste('OBS. TP ug L'^-1,)))+
    ggtitle("Force model to solve for DOC and TP concentrations")+
    theme_bw()
  #Is it possible that TP is in the wrong units, resulting in extremely low PP estimates? 
  
  #What do the zMix estimates look like compared to actual mean zMix observed values?
  sims %>%
    ggplot(aes(x=meanzMix, y=zMix_mod, fill=lakeName))+
    geom_point(shape=21, size=3)+
    geom_abline(intercept = 0, slope = 1)+
    ylab("PRED. zMix (m)")+
    xlab("OBS. zMix (m)")+
    ggtitle("Force model to solve for DOC and TP concentrations")+
    theme_bw()
  