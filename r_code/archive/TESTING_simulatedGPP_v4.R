#VERSION 4- April 14, 2021
#Here I'm trying a different dataset. These are a set of Zwart's lakes
#where we know DOCin, TPin, Qin, TPlake, DOClake, Volume, SA, zMax

library(deSolve)

#Making GPP estimates from the Kelly Model from known data
rm(sims2)
glimpse(zwart_load)
sims2<-zwart_load %>%
  rename(lakeName=lake)%>%
  filter(doy >= 121 & doy <= 274) %>%
  group_by(lakeName) %>%
  summarize(DOC_load=mean(DOC_load, na.rm=TRUE), #kg/day ##2021-04-21 switched from MEAN to MEDIAN
            TP_load=mean(TP_load, na.rm=TRUE), #kg/day
            Qin=mean(inflow, na.rm=TRUE)) %>% #m3/s
  mutate(DOC_gm3=(DOC_load/Qin)*(1000/86400),
         TP_gm3=(TP_load/Qin)*(1000/86400)) 

mueggelsee_sum<-mueggelsee_load_manual %>%
  group_by(lakeName) %>%
  summarize(DOC_load=median(DOC_load_kgday, na.rm=TRUE), #kg/day ##2021-04-21 switched from MEAN to MEDIAN
            TP_load=median(TP_load_kgday, na.rm=TRUE), #kg/day
            Qin=median(flow, na.rm=TRUE)) %>% #m3/s
  mutate(DOC_gm3=(DOC_load/Qin)*(1000/86400),
         TP_gm3=(TP_load/Qin)*(1000/86400)) 

loch_sum <- loch_load_manual %>%
  group_by(lakeName) %>%
  summarize(DOC_load=median(DOC_load_kgday, na.rm=TRUE), #kg/day ##2021-04-21 switched from MEAN to MEDIAN
            TP_load=median(TP_load_kgday, na.rm=TRUE), #kg/day
            Qin=median(flow, na.rm=TRUE)) %>% #m3/s
  mutate(DOC_gm3=(DOC_load/Qin)*(1000/86400),
         TP_gm3=(TP_load/Qin)*(1000/86400)) 

taupo_sum<-bind_rows(Taupo_Hinemaiaia_load_manual,
          Taupo_Tauranga_load_manual,
          Taupo_Kuratau_load_manual,
          Taupo_TokaanuPowerStation_load_manual,
          Taupo_Tongariro_load_manual,
          Taupo_Whareroa_load_manual,
          Taupo_Whangamata_load_manual,
          Taupo_Mapara_load_manual,
          Taupo_Waihaha_load_manual,
          Taupo_Whanganui_load_manual,
          Taupo_Waitahanui_load_manual) %>%
  group_by(lakeName) %>%
  summarize(DOC_load=mean(DOC_load_kgday, na.rm=TRUE), #kg/day #calculating mean daily loads for each tributary
            TP_load=mean(TP_load_kgday, na.rm=TRUE), #kg/day
            Qin=mean(flow, na.rm=TRUE)) %>%#m3/s
  ungroup()%>%
  summarize(DOC_load=sum(DOC_load, na.rm=TRUE), #kg/day #summing mean daily loads for each tributary
            TP_load=sum(TP_load, na.rm=TRUE), #kg/day
            # DOC_gm3=sum(DOC_gm3, na.rm=TRUE), #g/m3
            # TP_gm3=sum(TP_gm3, na.rm=TRUE), #g/m3
            Qin=sum(Qin, na.rm=TRUE))%>% #m3/s
  mutate(lakeName="Taupo") %>%
  mutate(DOC_gm3=(DOC_load/Qin)*(1000/86400), #calculate mean daily inflow concentration
         TP_gm3=(TP_load/Qin)*(1000/86400)) 

# 
# sims2<- bind_rows(sims2, mueggelsee_sum, loch_sum,taupo_sum)%>%
#   left_join(.,metadata %>%
#               select(lakeName, `Lake residence time (year)`, `Surface area (ha)`,
#                      `Maximum lake depth (m)`, `Volume (m3)`, `Mean lake depth (m)`), by="lakeName") %>%
#   left_join(., newts_full %>%
#               select(lakeName, DOC_mgL, TP_ugL), by="lakeName") %>%
#   rename(HRT_days=`Lake residence time (year)`,
#          zMean=`Mean lake depth (m)`,
#          SA_ha=`Surface area (ha)`,
#          zMax=`Maximum lake depth (m)`, #maximum lake depth, m
#          V_m3=`Volume (m3)`, #volume, m3
#          DOC=DOC_mgL, #mg/L=g/m3, in-lake DOC concentrations
#          TP=TP_ugL, #ug/L=mg/m3, in-lake TP concentrations
#          DOCin=DOC_gm3) %>% #mg/L=g/m3, estimated DOC concentration of inflow
#   mutate(SA=SA_ha/100, #surface area in km2
#          Pin=TP_gm3*1000,#ug/L=mg/m3, estimated TP concentration of inflow
#          Qin=Qin*86400, #m3/day
#          HRT_days=HRT_days*365) %>%#HRT (days)
#   drop_na() 
#   # mutate(DOCin = replace(DOCin, lakeName=="Acton", 6.5907),
#   #        Pin = replace(Pin, lakeName=="Acton", 97.5))
# glimpse(sims2)
# n_distinct(sims2$lakeName)


sims2<-   left_join(inflow_conc_summary,metadata %>%
              select(lakeName, `Lake residence time (year)`, `Surface area (ha)`,
                     `Maximum lake depth (m)`, `Volume (m3)`, `Mean lake depth (m)`), by="lakeName") %>%
  left_join(., newts_full %>%
              select(lakeName, DOC_mgL, TP_ugL), by="lakeName") %>%
  rename(HRT_days=`Lake residence time (year)`,
         zMean=`Mean lake depth (m)`,
         SA_ha=`Surface area (ha)`,
         zMax=`Maximum lake depth (m)`, #maximum lake depth, m
         V_m3=`Volume (m3)`, #volume, m3
         DOC=DOC_mgL, #mg/L=g/m3, in-lake DOC concentrations
         TP=TP_ugL, #ug/L=mg/m3, in-lake TP concentrations
         DOCin=DOC_gm3) %>% #mg/L=g/m3, estimated DOC concentration of inflow
  mutate(SA=SA_ha/100, #surface area in km2
         Pin=TP_gm3*1000,#ug/L=mg/m3, estimated TP concentration of inflow
         Qin=Qin*86400, #m3/day
         HRT_days=HRT_days*365) %>%#HRT (days)
  drop_na() 
# mutate(DOCin = replace(DOCin, lakeName=="Acton", 6.5907),
#        Pin = replace(Pin, lakeName=="Acton", 97.5))
glimpse(sims2)
n_distinct(sims2$lakeName)


sims<-left_join(master_df,load_estimates_huisman, by="lakeName")  %>% 
  select(lakeName, DOC_load_kgday_mean,meanzMix, zMean,TP_load_kgday_mean,
         SA_ha, HRT_days,DOC_mgL, TP_ugL, TP_gm3, DOC_gm3, V_m3) %>% #exclude zMax for now bc lots of missing values
  mutate(CP=DOC_load_kgday_mean/TP_load_kgday_mean,
         SA=SA_ha/100, #km2
         # TP_mgL=TP_ugL/1000, #mg/L
         TP_mgm3=TP_gm3*1000) %>% #mg/m3 = ug/L
  rename(DOC=DOC_mgL, #mg/L=g/m3, in-lake DOC concentrations
         TP=TP_ugL, #ug/L=mg/m3, in-lake TP concentrations
         Pin=TP_mgm3, #ug/L=mg/m3, estimated TP concentration of inflow
         DOCin=DOC_gm3) %>% #mg/L=g/m3, estimated DOC concentration of inflow
  mutate(TP = replace(TP, lakeName=="Almberga", 5), #assumption, no TP data provided
         TP = replace(TP, lakeName=="P1", 35), #SRP, not TP
         TP = replace(TP, lakeName=="P8", 30)) %>%#SRP, not TP
  drop_na()

# This version of the script we predicted A, P, C and use known Qin as V/HRT  from zwart_load (sims2)-------------------------

# glimpse(sims2)
# sims2 %>%
#   ggplot(aes(x=Qin, y=V_m3/HRT_days))+
#   geom_point(shape=21, size=3, alpha=0.5)+
#   theme_bw()+
#   scale_y_log10(labels = scales::comma)+
#   scale_x_log10(labels = scales::comma)+
#   ggtitle("V/HRT is not a bad approximation for Qin")

# sims2<-sims2 %>%
#   filter(lakeName=="Acton")

#make dailyPAR data - just use some random incident light data
I0<-1000

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
    # zmix=abs(2.66-0.12*DOC+0.005*SA+zmax*0.113)
    # zmix=-0.155*DOC+0.00103*zmax+3.506 #multiple regression
    
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
    # dA.dt=A*prod-lA*A-v/zmix*A-Qin/(zmax*SA*1e6)*A	# mg C m-3
    
    ##Kelly et al., EQN 4
    dP.dt=Qin/(zmix*SA*1e6)*(Pin-P)+cA*lA*A*rec-cA*A*prod		# mg P m-3 (in epi)
    # dP.dt=Qin/(zmax*SA*1e6)*(Pin-P)+cA*lA*A*rec-cA*A*prod		# mg P m-3 (in epi)
    #What if we use full lake volume, not just epilimnetic volume? 
    
    
    ##Kelly et al., EQN 3
    dDOC.dt=(Qin/(zmix*SA*1e6))*(DOCin-DOC)-decay*DOC				# g C m-3 (in epi)
    # dDOC.dt=(Qin/(zmax*SA*1e6))*(DOCin-DOC)-decay*DOC				# g C m-3 (in epi) 
    #What if we use full lake volume, not just epilimnetic volume? 
    
    return(list(c(dA.dt,dP.dt,dDOC.dt)))
    # return(list(c(dA.dt)))
  })
}

times=1:2000
storeAs=matrix(NA,length(times),nrow(sims2))
storePs=storeAs
storeDOCs=storeAs

# loop through different surface areas, DOC loads, and load stoichs
for(j in 1:nrow(sims2)){
  #submittd
  # MY PARAMETERS
  parms=c(SA=sims2$SA[j],
          zmax=sims2$zMax[j],
          kDOC=0.42,#But it has to be 0.42 for the units to cancel out in the kD equation... kDOC=0.00042 in the paper
          # kA=0.00022, #value used in the paper
          kA=0.001, 
          lA=0.1,#mine
          # lA=0.05,
          pA=1.2,
          hA=55, #mine
          # ha=200,
          mA=2,
          decay=0.001,
          Pin=sims2$Pin[j],
          DOCin=sims2$DOCin[j],
          cA=0.015,
          v=0.01, #my value
          # v=0.001, #decreasing this value improves GPP est slightly, but not TP or zMix. 
          # v=0.05, #kelly value
          # v=0.1, #in the paper it's set to 0.1
          rec=0.99, #Improves GPP and TP estimates a bit! 
          # rec=0.95, #value used in the paper
          # V=sims2$V_m3[j],
          # Qin=sims2$Qin[j],
          Qin=sims2$V_m3[j]/sims2$HRT_days[j],
          # Qin=(sims2$SA[j]*1e6*sims2$zMax[j])/sims2$HRT_days[j], #Could also try this.. [m3/day]
          P=sims2$TP[j],
          DOC=sims2$DOC[j])
  # 
  ## Parameters used in "predictionsVSolomonObservations_Diehl.R"
  # parms=c(SA=sims2$SA[j],
  #         zmax=sims2$zMax[j],
  #         kDOC=0.42,
  #         kA=0.00022,
  #         lA=0.1,
  #         pA=1.2,
  #         hA=55,
  #         mA=2,
  #         decay=0.001,
  #         Qin=(sims2$SA[j]*1e6*sims2$zMax[j])/sims2$HRT_days[j],
  #         Pin=sims2$Pin[j],
  #         DOCin=sims2$DOCin[j],
  #         cA=0.015,
  #         v=0.05,
  #         rec=0.95,
  #         I0=300)

  
  # starting state variables
  n<-c(A=500,
       P=sims2$TP[j],
       DOC=sims2$DOC[j])
  # n<-c(A=500)
  
  
  # 
  # n<-c(A=100,
  #      P=sims2$Pin[j],
  #      DOC=sims2$DOCin[j])
  
  
  # simulate with ode
  run=ode(y=n,times=times,func=huisman,parms=parms)
  
  storeAs[,j]<-run[,2]
  storePs[,j]<-run[,3]
  # storePs<-matrix(sims2$TP, nrow=2000, ncol=13, byrow=TRUE)
  # storePs<-matrix(sims2$TP, nrow=2000, ncol=24, byrow=TRUE)/(storeAs*parms[13]) #because we want dissolved P, not TP? 
  # storeDOCs<-matrix(sims2$DOC, nrow=2000, ncol=13, byrow=TRUE)
  storeDOCs[,j]<-run[,4]
  
  print(j)
}


# calculate other simulation characteristics from stored equilibrium state variables
store_kD=parms[4]*storeAs+parms[3]*storeDOCs-0.05 #ORIGINAL

# store_kD=parms[4]*storeAs+parms[3]*parms[18]-0.05 #ALT
store_zmix<-store_kD
for(i in 1:ncol(store_zmix)){ #ORIGINAL
  store_zmix[,i]<-10^(-0.515*log10(storeDOCs[,i])+0.115*log10(2*sqrt((sims2$SA[i])/pi))+0.991)
  # if(store_zmix[,i]>zmax[,i]){zmix[,i]=store_zmix[,i]}
  # store_zmix[store_zmix>zmax]=zmax
  
}

#If zmix > zmax, replaces with zmax/zmean
#IAO: note that replacing with zmean does improve our fits a bit (R2=0.32 versus R2=0.35)
for(i in 1:ncol(store_zmix)){ #
  zmax=sims2$zMax
  # zmean=sims2$zMean
  store_zmix[,i][store_zmix[,i]>zmax[i]]=zmax[i]
}





storeTP=storeAs*parms[13]+storePs #ORIGINAL
# storeTP=storePs ##I think we want to store TP as Ps because our input IS total P, not dissolved. 
storePP=store_kD*0
light.limit.d<-store_kD*0
nutrient.limit.d<-store_kD*0
storer<-store_kD*0

## MY WAY
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

##KELLY'S WAY
# izmix<-c()
# kds<-c()
# for(i in 1:length(store_kD)){
# 
# 
#   cP=storePs[i]
# 
#   kD=store_kD[i]
#   kds[i]<-kD
#   zmix=store_zmix[i]
# 
#   Izmix=lightAtten(z=zmix,I0=I0,kD=kD)
#   izmix[i]<-Izmix
# 
#   pA=parms[6]
#   hA=parms[7]
#   mA=parms[8]
# 
#   storePP[i]=storeAs[i]/zmix*(pA/kD)*log((hA+I0)/(hA+Izmix))*(cP/(cP+mA))
# }


store_arealPP=storePP*store_zmix

sims2$eqDOC=storeDOCs[nrow(storeDOCs),]
sims2$eqTP=storeTP[nrow(storeTP),]
sims2$PPareal=store_arealPP[nrow(store_arealPP),]
sims2$PPvol=storePP[nrow(storePP),]
sims2$zMix_mod=store_zmix[nrow(store_zmix),]
sims2$A=storeAs[nrow(storeAs),]
sims2$light.limit.d=light.limit.d[nrow(light.limit.d),]
sims2$nutrient.limit.d=nutrient.limit.d[nrow(nutrient.limit.d),]


#PREDICTED VS OBSERVED
A<-sims2 %>%
  select(lakeName, eqDOC, PPareal) %>%
  left_join(., master_df %>%
              select(lakeName, meanGPP, DOC_mgL), by="lakeName") %>%
  # filter(!lakeName %in% c("Mendota","Acton"))%>%
  ggplot(aes(x=meanGPP, y=PPareal, fill=lakeName))+
  geom_point(shape=21, size=3)+
  theme_bw()+
  ylab(expression(paste('PRED. GPP (mg C m'^-2,' day'^-1,')')))+
  xlab(expression(paste('OBS. GPP (mg C m'^-2,' day'^-1,')')))+
  geom_text_repel(aes(label = lakeName), size = 3)+
  geom_abline(intercept = 0, slope = 1)

# ggtitle("This version of the script we predicted A, P, C and use known Qin  ")

# 
# PAR1000<-sims2$PPareal
# PAR600<-sims2$PPareal
# PAR300<-sims2$PPareal
# light_differences<-data.frame(sims2$lakeName, PAR1000, PAR600) %>%
#   rename(lakeName=sims2.lakeName) %>%
#   ggplot(aes(x=PAR300, y=PAR1000, fill=lakeName))+
#   geom_point(shape=21, size=3)+
#   theme_bw()+
#   ylab(expression(paste('PAR = 1000; GPP (mg C m'^-2,' day'^-1,')')))+
#   xlab(expression(paste('PAR = 300; OBS. GPP (mg C m'^-2,' day'^-1,')')))+
#   geom_text_repel(aes(label = lakeName), size = 3)+
#   geom_abline(intercept = 0, slope = 1)+
#   theme(legend.position="none")
# light_differences

#Predicted "equilibrium" DOC values are much lower than observed DOC? Why?
B<-sims2 %>%
  # filter(!lakeName=="EastLong")%>%
  ggplot(aes(x=DOC, y=eqDOC, fill=lakeName))+
  geom_point(shape=21, size=3)+
  geom_abline(intercept = 0, slope = 1)+
  ylab(expression(paste('PRED. lake DOC mg L'^-1,)))+
  xlab(expression(paste('OBS. lake DOC mg L'^-1,)))+
  geom_text_repel(aes(label = lakeName), size = 3)+
  theme_bw()

#Model tends to overpredict DOC, but it's especially bad for two lakes (EastLong, Vortsjarv)

#Relationship between equilibrium TP and actual in-lake TP
C<-sims2 %>%
  # filter(!lakeName=="EastLong")%>%
  ggplot(aes(y=eqTP, x=TP, fill=lakeName))+
  geom_point(shape=21, size=3)+
  geom_abline(intercept = 0, slope = 1)+
  ylab(expression(paste('PRED. lake TP ug L'^-1,)))+
  xlab(expression(paste('OBS. lake TP ug L'^-1,)))+
  theme_bw()+
  geom_text_repel(aes(label = lakeName), size = 3)


#What do the zMix estimates look like compared to actual mean zMix observed values?
D<-sims2 %>%
  left_join(., master_df %>%
              select(lakeName, meanzMix), by="lakeName") %>%
  ggplot(aes(x=meanzMix, y=zMix_mod, fill=lakeName))+
  geom_point(shape=21, size=3)+
  geom_abline(intercept = 0, slope = 1)+
  ylab("PRED. zMix (m)")+
  xlab("OBS. zMix (m)")+
  theme_bw()+
  geom_text_repel(aes(label = lakeName), size = 3)



(A+B)/(C+D)+
  plot_layout(guides = 'collect')
  

## How do the algal biomass estimates look? 
sims2 %>%
  ggplot(aes(x=DOC, y=A, fill=lakeName))+
  geom_point(shape=21, size=3)+
  # geom_abline(intercept = 0, slope = 1)+
  ylab(expression(paste('PRED. phyto. biomass ug C L'^-1,)))+
  xlab(expression(paste('OBS. lake DOC mg L'^-1,)))+
  geom_text_repel(aes(label = lakeName), size = 3)+
  theme_bw()

##Why is A so low in some lakes? 
sims2 %>%
  ggplot(aes(x=TP, y=A, fill=lakeName))+
  geom_point(shape=21, size=3)+
  # geom_abline(intercept = 0, slope = 1)+
  ylab(expression(paste('PRED. phyto. biomass ug C L'^-1,)))+
  xlab(expression(paste('OBS. lake TP ug L'^-1,)))+
  geom_text_repel(aes(label = lakeName), size = 3)+
  theme_bw()

sims2 %>%
  # filter(!lakeName %in% c("Vortsjarv","EastLong","Ovre","Struptjarn"))%>%
  ggplot(aes(x=eqTP, y=A, fill=lakeName))+
  geom_point(shape=21, size=3)+
  geom_smooth(method="lm",se=F)+
  # geom_abline(intercept = 0, slope = 1)+
  ylab(expression(paste('PRED. phyto. biomass ug C L'^-1,)))+
  xlab(expression(paste('PRED. lake TP ug L'^-1,)))+
  geom_text_repel(aes(label = lakeName), size = 3)+
  theme_bw()


(zwart_load %>%
  filter(lake=="EastLong") %>%
  ggplot(aes(x=doy, y=DOC_load))+
  geom_point()+
  theme_bw()+
    ylab("DOC loads (kg/day)")+
  ggtitle("East Long - DOC loads")+
    
    zwart_load %>%
    filter(lake=="EastLong") %>%
    ggplot(aes(x=doy, y=inflow))+
    geom_point()+
    theme_bw()+
    ylab("Discharge (m3/s)")+
    ggtitle("East Long - discharge") +

zwart_load %>%
  filter(lake=="EastLong") %>%
  mutate(DOC_gm3=(DOC_load/inflow)*(1000/86400),
         TP_gm3=(TP_load/inflow)*(1000/86400)) %>%
  ggplot(aes(x=doy, y=DOC_gm3))+
  geom_point()+
    theme_bw()+
    ylab("Inflow concentrations (mg/L  or g/m3)")+
  ggtitle("East Long - DOC inflow concentrations"))

#Check on the TPin and in-lake TP concentrations of each lake 
sims2 %>%
  ggplot(aes(y=TP, x=Pin, fill=lakeName))+
  geom_point(shape=21, size=3)+
  ylab(expression(paste('OBS lake TP ug L'^-1,)))+
  xlab(expression(paste('OBS. inflow TP ug L'^-1,)))+
  theme_bw()+
  geom_text_repel(aes(label = lakeName), size = 3)

######################Quantifying uncertainty##########################################
###As the square root of a variance, RMSE can be interpreted as the standard deviation#
###of the unexplained variance, and has the useful property of being in the same units#
###as the response variable. Lower values of RMSE indicate better fit.#################

# Function that returns Root Mean Squared Error
rmse <- function(error)
{
  sqrt(mean(error^2))
}
error_df<-sims2 %>%
  select(lakeName, eqDOC, PPareal, zMix_mod, SA) %>%
  left_join(., master_df %>%
              select(lakeName, meanGPP, meanzMix, DOC_mgL), by="lakeName") %>%
  filter(!lakeName=="Acton")

error_df<-error_df%>%
  group_by(lakeName)%>%
  mutate(error=meanGPP-PPareal,
         rmse_GPP=rmse(error))

install.packages("Metrics")
library(Metrics)
actual<-error_df$meanGPP
predicted<-error_df$PPareal
result = rmse(actual, predicted)

error_df %>%
  ggplot(aes(x=reorder(lakeName, (meanGPP-PPareal)), y=(meanGPP-PPareal), fill=meanGPP))+
  geom_bar(stat="identity",  color="black")+
  theme(
        axis.title.x=element_blank(),
        axis.text.x=element_text(angle = 45, hjust = 1, size=6))+
  ylab("ObsGPP-predGPP")+
  xlab("Lake")+
  scale_fill_continuous_sequential(palette = "Terrain", rev=TRUE)+
  ggtitle("How far off are observations for each lake?")+
  theme_classic()

error_df %>%
  ggplot(aes(x=reorder(lakeName, ((meanGPP-PPareal)/meanGPP)*100), y=((meanGPP-PPareal)/meanGPP)*100, fill=meanGPP))+
  geom_bar(stat="identity",  color="black")+
  theme(
    axis.title.x=element_blank(),
    axis.text.x=element_text(angle = 45, hjust = 1, size=6))+
  ylab("(ObsGPP-predGPP)/ObsGPP * 100")+
  xlab("Lake")+
  scale_fill_continuous_sequential(palette = "Terrain", rev=TRUE)+
  ggtitle("How far off are observations for each lake as a percentage of OBS GPP?")+
  theme_classic()


#test if slope is different from 1
ttest <- function(reg, coefnum, val){
  co <- coef(summary(reg))
  tstat <- (co[coefnum,1]-val)/co[coefnum,2]
  2 * pt(abs(tstat), reg$df.residual, lower.tail = FALSE)
}

mod<-lm(meanGPP~PPareal,data=error_df)
ttest(mod,2,0)
summary(lm(meanGPP~PPareal,data=error_df))

sims2 %>%
  left_join(., master_df %>%
              select(lakeName, meanGPP, DOC_mgL), by="lakeName")%>%
ggplot((aes(x=PPareal, y=meanGPP)))+
  geom_point(shape=21, fill="grey20", size=3)+
  geom_smooth(method="lm", se=F, color="black")+
  geom_abline(intercept = 0, slope = 1)+
  annotate(geom="text",x=500, y=2000,
           label=expression(paste('RMSE = 401 mg C m'^-2,' day'^-1,'')),
           # label="RMSE=401 mg C/m2/day",
           color="navy")+
  xlab(expression(paste('PRED. GPP (mg C m'^-2,' day'^-1,')')))+
  ylab(expression(paste('OBS. GPP (mg C m'^-2,' day'^-1,')')))+
  theme_classic()+
  xlim(c(0,2500))+ylim(c(0,2500))

sqrt(mean(mod$residuals^2))


mod2<-lm(meanzMix~zMix_mod, data=error_df)
summary(mod2)



# This version of the script we predicted A, P, C for all lakes  --------
#Both measured and modeled inflows 

#make dailyPAR data - just use some random incident light data
I0<-1000

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
    # zmix=abs(2.66-0.12*DOC+0.005*SA+zmax*0.113)
    # zmix=-0.155*DOC+0.00103*zmax+3.506 #multiple regression
    
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
    # dA.dt=A*prod-lA*A-v/zmix*A-Qin/(zmax*SA*1e6)*A	# mg C m-3
    
    ##Kelly et al., EQN 4
    dP.dt=Qin/(zmix*SA*1e6)*(Pin-P)+cA*lA*A*rec-cA*A*prod		# mg P m-3 (in epi)
    # dP.dt=Qin/(zmax*SA*1e6)*(Pin-P)+cA*lA*A*rec-cA*A*prod		# mg P m-3 (in epi)
    #What if we use full lake volume, not just epilimnetic volume? 
    
    
    ##Kelly et al., EQN 3
    dDOC.dt=(Qin/(zmix*SA*1e6))*(DOCin-DOC)-decay*DOC				# g C m-3 (in epi)
    # dDOC.dt=(Qin/(zmax*SA*1e6))*(DOCin-DOC)-decay*DOC				# g C m-3 (in epi) 
    #What if we use full lake volume, not just epilimnetic volume? 
    
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
  # MY PARAMETERS
  parms=c(SA=sims$SA[j],
          zmax=sims$zMax[j],
          kDOC=0.42,#But it has to be 0.42 for the units to cancel out in the kD equation... kDOC=0.00042 in the paper
          # kA=0.00022, #value used in the paper
          kA=0.001, 
          lA=0.1,#mine
          # lA=0.05,
          pA=1.2,
          hA=55, #mine
          # ha=200,
          mA=2,
          decay=0.001,
          Pin=sims$Pin[j],
          DOCin=sims$DOCin[j],
          cA=0.015,
          v=0.01, #my value
          # v=0.001, #decreasing this value improves GPP est slightly, but not TP or zMix. 
          # v=0.05, #kelly value
          # v=0.1, #in the paper it's set to 0.1
          rec=0.99, #Improves GPP and TP estimates a bit! 
          # rec=0.95, #value used in the paper
          # V=sims$V_m3[j],
          # Qin=sims$Qin[j],
          Qin=sims$V_m3[j]/sims$HRT_days[j],
          # Qin=(sims$SA[j]*1e6*sims$zMax[j])/sims$HRT_days[j], #Could also try this.. [m3/day]
          P=sims$TP[j],
          DOC=sims$DOC[j])
  # 
  ## Parameters used in "predictionsVSolomonObservations_Diehl.R"
  # parms=c(SA=sims$SA[j],
  #         zmax=sims$zMax[j],
  #         kDOC=0.42,
  #         kA=0.00022,
  #         lA=0.1,
  #         pA=1.2,
  #         hA=55,
  #         mA=2,
  #         decay=0.001,
  #         Qin=(sims$SA[j]*1e6*sims$zMax[j])/sims$HRT_days[j],
  #         Pin=sims$Pin[j],
  #         DOCin=sims$DOCin[j],
  #         cA=0.015,
  #         v=0.05,
  #         rec=0.95,
  #         I0=300)
  
  
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
  # if(store_zmix[,i]>zmax[,i]){zmix[,i]=store_zmix[,i]}
  # store_zmix[store_zmix>zmax]=zmax
  
}

#If zmix > zmax, replaces with zmax/zmean
#IAO: note that replacing with zmean does improve our fits a bit (R2=0.32 versus R2=0.35)
for(i in 1:ncol(store_zmix)){ #
  zmax=sims$zMax
  # zmean=sims$zMean
  store_zmix[,i][store_zmix[,i]>zmax[i]]=zmax[i]
}





storeTP=storeAs*parms[13]+storePs #ORIGINAL
# storeTP=storePs ##I think we want to store TP as Ps because our input IS total P, not dissolved. 
storePP=store_kD*0
light.limit.d<-store_kD*0
nutrient.limit.d<-store_kD*0
storer<-store_kD*0

## MY WAY
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

##KELLY'S WAY
# izmix<-c()
# kds<-c()
# for(i in 1:length(store_kD)){
# 
# 
#   cP=storePs[i]
# 
#   kD=store_kD[i]
#   kds[i]<-kD
#   zmix=store_zmix[i]
# 
#   Izmix=lightAtten(z=zmix,I0=I0,kD=kD)
#   izmix[i]<-Izmix
# 
#   pA=parms[6]
#   hA=parms[7]
#   mA=parms[8]
# 
#   storePP[i]=storeAs[i]/zmix*(pA/kD)*log((hA+I0)/(hA+Izmix))*(cP/(cP+mA))
# }


store_arealPP=storePP*store_zmix

sims$eqDOC=storeDOCs[nrow(storeDOCs),]
sims$eqTP=storeTP[nrow(storeTP),]
sims$PPareal=store_arealPP[nrow(store_arealPP),]
sims$PPvol=storePP[nrow(storePP),]
sims$zMix_mod=store_zmix[nrow(store_zmix),]
sims$A=storeAs[nrow(storeAs),]
sims$light.limit.d=light.limit.d[nrow(light.limit.d),]
sims$nutrient.limit.d=nutrient.limit.d[nrow(nutrient.limit.d),]


#PREDICTED VS OBSERVED
A<-sims %>%
  select(lakeName, eqDOC, PPareal) %>%
  left_join(., master_df %>%
              select(lakeName, meanGPP, DOC_mgL), by="lakeName") %>%
  # filter(!lakeName %in% c("Mendota","Acton"))%>%
  ggplot(aes(x=meanGPP, y=PPareal, fill=lakeName))+
  geom_point(shape=21, size=3)+
  theme_bw()+
  ylab(expression(paste('PRED. GPP (mg C m'^-2,' day'^-1,')')))+
  xlab(expression(paste('OBS. GPP (mg C m'^-2,' day'^-1,')')))+
  geom_text_repel(aes(label = lakeName), size = 3)+
  geom_abline(intercept = 0, slope = 1)

# ggtitle("This version of the script we predicted A, P, C and use known Qin  ")

# 
# PAR1000<-sims$PPareal
# PAR600<-sims$PPareal
# PAR300<-sims$PPareal
# light_differences<-data.frame(sims$lakeName, PAR1000, PAR600) %>%
#   rename(lakeName=sims.lakeName) %>%
#   ggplot(aes(x=PAR300, y=PAR1000, fill=lakeName))+
#   geom_point(shape=21, size=3)+
#   theme_bw()+
#   ylab(expression(paste('PAR = 1000; GPP (mg C m'^-2,' day'^-1,')')))+
#   xlab(expression(paste('PAR = 300; OBS. GPP (mg C m'^-2,' day'^-1,')')))+
#   geom_text_repel(aes(label = lakeName), size = 3)+
#   geom_abline(intercept = 0, slope = 1)+
#   theme(legend.position="none")
# light_differences

#Predicted "equilibrium" DOC values are much lower than observed DOC? Why?
B<-sims %>%
  # filter(!lakeName=="EastLong")%>%
  ggplot(aes(x=DOC, y=eqDOC, fill=lakeName))+
  geom_point(shape=21, size=3)+
  geom_abline(intercept = 0, slope = 1)+
  ylab(expression(paste('PRED. lake DOC mg L'^-1,)))+
  xlab(expression(paste('OBS. lake DOC mg L'^-1,)))+
  geom_text_repel(aes(label = lakeName), size = 3)+
  theme_bw()

#Model tends to overpredict DOC, but it's especially bad for two lakes (EastLong, Vortsjarv)

#Relationship between equilibrium TP and actual in-lake TP
C<-sims %>%
  # filter(!lakeName=="EastLong")%>%
  ggplot(aes(y=eqTP, x=TP, fill=lakeName))+
  geom_point(shape=21, size=3)+
  geom_abline(intercept = 0, slope = 1)+
  ylab(expression(paste('PRED. lake TP ug L'^-1,)))+
  xlab(expression(paste('OBS. lake TP ug L'^-1,)))+
  theme_bw()+
  geom_text_repel(aes(label = lakeName), size = 3)


#What do the zMix estimates look like compared to actual mean zMix observed values?
D<-sims %>%
  left_join(., master_df %>%
              select(lakeName, meanzMix), by="lakeName") %>%
  ggplot(aes(x=meanzMix, y=zMix_mod, fill=lakeName))+
  geom_point(shape=21, size=3)+
  geom_abline(intercept = 0, slope = 1)+
  ylab("PRED. zMix (m)")+
  xlab("OBS. zMix (m)")+
  theme_bw()+
  geom_text_repel(aes(label = lakeName), size = 3)



(A+B)/(C+D)+
  plot_layout(guides = 'collect')


## How do the algal biomass estimates look? 
sims %>%
  ggplot(aes(x=DOC, y=A, fill=lakeName))+
  geom_point(shape=21, size=3)+
  # geom_abline(intercept = 0, slope = 1)+
  ylab(expression(paste('PRED. phyto. biomass ug C L'^-1,)))+
  xlab(expression(paste('OBS. lake DOC mg L'^-1,)))+
  geom_text_repel(aes(label = lakeName), size = 3)+
  theme_bw()

##Why is A so low in some lakes? 
sims %>%
  ggplot(aes(x=TP, y=A, fill=lakeName))+
  geom_point(shape=21, size=3)+
  # geom_abline(intercept = 0, slope = 1)+
  ylab(expression(paste('PRED. phyto. biomass ug C L'^-1,)))+
  xlab(expression(paste('OBS. lake TP ug L'^-1,)))+
  geom_text_repel(aes(label = lakeName), size = 3)+
  theme_bw()

sims %>%
  # filter(!lakeName %in% c("Vortsjarv","EastLong","Ovre","Struptjarn"))%>%
  ggplot(aes(x=eqTP, y=A, fill=lakeName))+
  geom_point(shape=21, size=3)+
  geom_smooth(method="lm",se=F)+
  # geom_abline(intercept = 0, slope = 1)+
  ylab(expression(paste('PRED. phyto. biomass ug C L'^-1,)))+
  xlab(expression(paste('PRED. lake TP ug L'^-1,)))+
  geom_text_repel(aes(label = lakeName), size = 3)+
  theme_bw()



# This version of the script we predicted A, P, C and use  Qin as V/HRT  from master_df (sims)-------------------------

sims <- sims %>%
  filter(lakeName=="EastLong")

#make dailyPAR data - just use some random incident light data
I0<-1000

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
          zmax=sims$zMean[j],
          kDOC=0.42,#But it has to be 0.42 for the units to cancel out in the kD equation...
          # kDOC=0.00042, #value used in the paper
          kA=0.00022,
          lA=0.1,
          pA=1.2,
          hA=55,
          mA=2,
          decay=0.001,
          Pin=sims$Pin[j],
          DOCin=sims$DOCin[j],
          cA=0.015,
          v=0.1,
          # v=1, #in the paper it's set to 1
          # rec=0.99,
          rec=0.95, #value used in the paper
          # V=sims$V_m3[j],
          # Qin=sims$Qin[j],
          Qin=sims$V_m3[j]/sims$HRT_days[j],
          # Qin=(sims$SA[j]*1e6*sims$zMean[j])/sims$HRT_days[j], #Could also try this.. [m3/day]
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
#If zmix > zmax, replaces with zmax
for(i in 1:ncol(store_zmix)){ #
  zmax=sims2$zMax
  store_zmix[,i][store_zmix[,i]>zmax[i]]=zmax[i]
}


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

#PREDICTED VS OBSERVED
A<-sims %>%
  select(lakeName, eqDOC, PPareal) %>%
  left_join(., master_df %>%
              select(lakeName, meanGPP, DOC_mgL), by="lakeName") %>%
  ggplot(aes(x=meanGPP, y=PPareal, fill=lakeName))+
  geom_point(shape=21, size=3)+
  theme_bw()+
  ylab(expression(paste('PRED. GPP (mg C m'^-2,' day'^-1,')')))+
  xlab(expression(paste('OBS. GPP (mg C m'^-2,' day'^-1,')')))+
  geom_text_repel(aes(label = lakeName), size = 3)+
  geom_abline(intercept = 0, slope = 1)
# ggtitle("This version of the script we predicted A, P, C and use known Qin  ")
A

#Predicted "equilibrium" DOC values are much lower than observed DOC? Why?
B<-sims %>%
  ggplot(aes(x=DOC, y=eqDOC, fill=lakeName))+
  geom_point(shape=21, size=3)+
  geom_abline(intercept = 0, slope = 1)+
  ylab(expression(paste('PRED. lake DOC mg L'^-1,)))+
  xlab(expression(paste('OBS. lake DOC mg L'^-1,)))+
  geom_text_repel(aes(label = lakeName), size = 3)+
  theme_bw()
B
#Model tends to overpredict DOC, but it's especially bad for two lakes (EastLong, Vortsjarv)

#Relationship between equilibrium TP and actual in-lake TP
C<-sims %>%
  ggplot(aes(y=eqTP, x=TP, fill=lakeName))+
  geom_point(shape=21, size=3)+
  geom_abline(intercept = 0, slope = 1)+
  ylab(expression(paste('PRED. lake TP ug L'^-1,)))+
  xlab(expression(paste('OBS. lake TP ug L'^-1,)))+
  theme_bw()+
  geom_text_repel(aes(label = lakeName), size = 3)
C

#What do the zMix estimates look like compared to actual mean zMix observed values?
D<-sims %>%
  left_join(., master_df %>%
              select(lakeName, medianzMix), by="lakeName") %>%
  ggplot(aes(x=medianzMix, y=zMix_mod, fill=lakeName))+
  geom_point(shape=21, size=3)+
  geom_abline(intercept = 0, slope = 1)+
  ylab("PRED. zMix (m)")+
  xlab("OBS. zMix (m)")+
  theme_bw()+
  geom_text_repel(aes(label = lakeName), size = 3)
D

(A+B)/(C+D)+
  plot_layout(guides = 'collect')+
  plot_annotation(
    title = 'This version of the script we solve for A, P, C & Qin=V/HRT'
  )


##What are the "obs" DOCin and TPin?
sims %>%
  ggplot(aes(x=DOCin, y=Pin, fill=lakeName))+
  geom_point(shape=21, size=3)+
  # geom_abline(intercept = 0, slope = 1)+
  # ylab(expression(paste('PRED. lake DOC mg L'^-1,)))+
  # xlab(expression(paste('OBS. lake DOC mg L'^-1,)))+
  geom_text_repel(aes(label = lakeName), size = 3)+
  theme_bw()

