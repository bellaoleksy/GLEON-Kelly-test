#VERSION 5- April 26, 2021
#Building off Version4, but trying to streamline some of the input code a bit. 



#Making GPP estimates from the Kelly Model from known data
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
         Pin=TP_mgm3,#ug/L=mg/m3, estimated TP concentration of inflow
         Qin=Qin*86400, #m3/day
         HRT_days=HRT_days*365) %>%#HRT (days)
  mutate(DOCin = replace(DOCin, lakeName=="EastLong", 13.60287933), #replace with modeled estimates for DOCin
         Pin = replace(Pin, lakeName=="EastLong", 18.074)) %>% #replace with modeled estimates for TPin
  drop_na() 
# mutate(DOCin = replace(DOCin, lakeName=="Acton", 6.5907),
#        Pin = replace(Pin, lakeName=="Acton", 97.5))
glimpse(sims2)
n_distinct(sims2$lakeName)


# export<-left_join(sims2, master_df %>%
#             select(lakeName, medianGPP, meanGPP),
#           by="lakeName")
# write.csv(export, file = "data/gridSearchInput.csv", row.names = FALSE)

# sims<-left_join(master_df,load_estimates_huisman, by="lakeName")  %>% 
#   select(lakeName, DOC_load_kgday_mean,meanzMix, zMean,TP_load_kgday_mean,
#          SA_ha, HRT_days,DOC_mgL, TP_ugL, TP_gm3, DOC_gm3, V_m3) %>% #exclude zMax for now bc lots of missing values
#   mutate(CP=DOC_load_kgday_mean/TP_load_kgday_mean,
#          SA=SA_ha/100, #km2
#          # TP_mgL=TP_ugL/1000, #mg/L
#          TP_mgm3=TP_gm3*1000) %>% #mg/m3 = ug/L
#   rename(DOC=DOC_mgL, #mg/L=g/m3, in-lake DOC concentrations
#          TP=TP_ugL, #ug/L=mg/m3, in-lake TP concentrations
#          Pin=TP_mgm3, #ug/L=mg/m3, estimated TP concentration of inflow
#          DOCin=DOC_gm3) %>% #mg/L=g/m3, estimated DOC concentration of inflow
#   mutate(TP = replace(TP, lakeName=="Almberga", 5), #assumption, no TP data provided
#          TP = replace(TP, lakeName=="P1", 35), #SRP, not TP
#          TP = replace(TP, lakeName=="P8", 30)) %>%#SRP, not TP
#   drop_na()

glimpse(master_df)
# rm(sims)
sims<-master_df%>%
  select(lakeName, DOC_load_kgday_mean,meanzMix, zMean,TP_load_kgday_mean,
         SA_ha, HRT_days,DOC_mgL, TP_ugL, TP_mgm3, DOC_gm3, V_m3) %>% #exclude zMax for now bc lots of missing values
  mutate(CP=DOC_load_kgday_mean/TP_load_kgday_mean,
         SA=SA_ha/100) %>% 
  rename(DOC=DOC_mgL, #mg/L=g/m3, in-lake DOC concentrations
         TP=TP_ugL, #ug/L=mg/m3, in-lake TP concentrations
         Pin=TP_mgm3, #ug/L=mg/m3, estimated TP concentration of inflow
         DOCin=DOC_gm3) %>% #mg/L=g/m3, estimated DOC concentration of inflow
  mutate(TP = replace(TP, lakeName=="Almberga", 5), #assumption, no TP data provided
         TP = replace(TP, lakeName=="P1", 35), #SRP, not TP
         TP = replace(TP, lakeName=="P8", 30),#SRP, not TP
         TP = replace(TP, lakeName=="Suggs", 149),  #TDP, not TP
         TP = replace(TP, lakeName=="LittleRock", 23.8))%>% #orthoP, not TP
  mutate(Pin = replace(Pin, lakeName=="TroutBog", 29), #value from 2021-05-03 model run
         Pin = replace(Pin, lakeName=="LittleRock", 100), #value from 2021-05-03 model run
         Pin = replace(Pin, lakeName=="Hummingbird", 22),
         Pin = replace(Pin, lakeName=="Taihu", 1000))%>% #value from 2021-05-03 model run
  drop_na()
glimpse(sims)

sims_trim <- sims %>%
  filter(meanzMix<15)
mrl<-lm(meanzMix~DOC+zMean, data=sims_trim)
summary(mrl)


sims_trim<-sims_trim %>%
  mutate(zmix_predicted= mrl %>%
                            predict())
sims_trim %>%
ggplot(aes(x=zmix_predicted, y=meanzMix))+
  geom_point()

# ZWART LAKES-------------------------

# glimpse(sims2)
# sims2 %>%
#   ggplot(aes(x=Qin, y=V_m3/HRT_days))+
#   geom_point(shape=21, size=3, alpha=0.5)+
#   theme_bw()+
#   scale_y_log10(labels = scales::comma)+
#   scale_x_log10(labels = scales::comma)+
#   ggtitle("V/HRT is not a bad approximation for Qin")

# sims2<-sims2 %>%
#   filter(lakeName=="Taupo")

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
    # dA.dt=A*prod-lA*A-v/zmix*A-Qin/(V)*A	# mg C m-3
    
    ##Kelly et al., EQN 4
    dP.dt=Qin/(zmix*SA*1e6)*(Pin-P)+cA*lA*A*rec-cA*A*prod		# mg P m-3 (in epi)
    # dP.dt=Qin/(V)*(Pin-P)+cA*lA*A*rec-cA*A*prod		# mg P m-3 (in epi)
    # What if we use full lake volume, not just epilimnetic volume?
    
    
    ##Kelly et al., EQN 3
    dDOC.dt=(Qin/(zmix*SA*1e6))*(DOCin-DOC)-decay*DOC				# g C m-3 (in epi)
    # dDOC.dt=(Qin/(V))*(DOCin-DOC)-decay*DOC				# g C m-3 (in epi)
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
  # parms=c(SA=sims2$SA[j],
  #         zmax=sims2$zMax[j],
  #         kDOC=0.42,#But it has to be 0.42 for the units to cancel out in the kD equation... kDOC=0.00042 in the paper
  #         kA=0.00022, #value used in the paper
  #         lA=0.1,
  #         pA=1.2,
  #         hA=55, #mine
  #         mA=2,
  #         decay=0.001,
  #         Pin=sims2$Pin[j],
  #         DOCin=sims2$DOCin[j],
  #         cA=0.015,
  #         v=0.01, #my value
  #         rec=0.95, #Improves GPP and TP estimates a bit! 
  #         Qin=sims2$V_m3[j]/sims2$HRT_days[j],
  #         P=sims2$TP[j],
  #         DOC=sims2$DOC[j])

  ## Parameters from Maximum Likelihood approach IAO 2020-08-27
  parms=c(SA=sims2$SA[j],
          zmax=sims2$zMax[j],
          kDOC=0.42,#Value used in Kelly paper
          kA=0.00022,#Value used in Kelly paper
          lA=0.1,#Value used in Kelly paper
          pA=1, #Value used in Kelly paper
          hA=55,#Value used in Kelly paper
          mA=2, #Value used in Kelly paper
          decay=0.001, #Value used in Kelly paper
          Pin=sims$Pin[j],
          DOCin=sims$DOCin[j],
          cA=0.015, #Value used in Kelly paper
          v=0.1, #Value used in Kelly paper
          # rec=0.99, #Improves GPP and TP estimates a bit!
          rec=0.95, #Value used in Kelly paper
          Qin=sims2$V_m3[j]/sims2$HRT_days[j],
          P=sims2$TP[j],
          DOC=sims2$DOC[j])
  

  
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
    storePP[j,i]<-storeAs[j,i]*(pA/(kD*zmix))*log((hA+I0)/(hA+Izmix))*(cP/(cP+mA)) ##KEEP ME
    # storePP[j,i]<-storeAs[j,i]*zmix*(pA/kD)*log((hA+I0)/(hA+Izmix))*(cP/(cP+mA))
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


#PREDICTED VS OBSERVED - MEAN GPP
# A<-
  sims2 %>%
  select(lakeName, eqDOC, PPareal) %>%
  left_join(., master_df %>%
              select(lakeName, medianGPP, DOC_mgL), by="lakeName") %>%
  filter(!lakeName %in% c("Harp"))%>%
  ggplot(aes(x=medianGPP, y=PPareal*1.25, fill=lakeName))+
  geom_point(shape=21, size=3)+
  theme_bw()+
  ylab(expression(paste('PRED. GPP (mg C m'^-2,' day'^-1,')')))+
  xlab(expression(paste('OBS. GPP (mg C m'^-2,' day'^-1,')')))+
  geom_text_repel(aes(label = lakeName), size = 3)+
  geom_abline(intercept = 0, slope = 1)

#Predicted "equilibrium" DOC values are much lower than observed DOC? Why?
# B<-
  sims2 %>%
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
# C<-
  sims2 %>%
  # filter(!lakeName=="EastLong")%>%
  ggplot(aes(y=eqTP, x=TP, fill=lakeName))+
  geom_point(shape=21, size=3)+
  geom_abline(intercept = 0, slope = 1)+
  ylab(expression(paste('PRED. lake TP ug L'^-1,)))+
  xlab(expression(paste('OBS. lake TP ug L'^-1,)))+
  theme_bw()+
  geom_text_repel(aes(label = lakeName), size = 3)


#What do the zMix estimates look like compared to actual mean zMix observed values?
# D<-
  sims2 %>%
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
  

#PREDICTED VS OBSERVED - MEDIAN GPP
A<-sims2 %>%
  select(lakeName, eqDOC, PPareal) %>%
  left_join(., master_df %>%
              select(lakeName, medianGPP, DOC_mgL), by="lakeName") %>%
  # filter(!lakeName %in% c("Mendota","Acton"))%>%
  ggplot(aes(x=medianGPP, y=PPareal, fill=lakeName))+
  geom_point(shape=21, size=3)+
  theme_bw()+
  ylab(expression(paste('PRED. GPP (mg C m'^-2,' day'^-1,')')))+
  xlab(expression(paste('OBS. median GPP (mg C m'^-2,' day'^-1,')')))+
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


# volumetric GPP fits? ----------------------------------------------------


sims2 %>%
  left_join(., master_df %>%
              select(lakeName, meanGPP_vol), by="lakeName") %>%
  left_join(., metadata %>%
              select(lakeName, inflows_YN), by="lakeName") %>%
  # filter(dataset=="modelled")%>%
  filter(inflows_YN=="yes")%>%
  ggplot(aes(y=meanGPP_vol, x=PPvol, fill=lakeName, shape=inflows_YN))+
  geom_point(size=3, alpha=0.8)+
  geom_abline(intercept = 0, slope = 1)+
  # scale_fill_manual(values=c("#e63946","#457b9d"),
  #                   name="How were loads\ndeteremined?")+
  # scale_fill_manual(values=c("#457b9d","#457b9d"),
  #                   name="How were loads\ndeteremined?")+
  scale_shape_manual(values=c(21,25),
                     name="Does the lake\nhave surface inflow(s)?")+
  # ylab("PRED. GPP (mg C m-2 day-1)")+
  # xlab("PRED. algal biomass (mg C/m3)")+
  theme_bw()+
  geom_text_repel(aes(label = lakeName), size = 3)+
  # facet_grid(.~dataset)+
  theme(panel.spacing = unit(0, "lines"))+
  guides(fill = guide_legend(override.aes = list(shape = 21) ),
         shape = guide_legend(override.aes = list(fill = "black") ) )+
  xlim(c(0,3000))+ylim(c(0,3000))

# RMSE of model predictions -----------------------------------------------


library(caret)
preds<-sims2 %>%
  left_join(., master_df %>%
              select(lakeName, meanGPP, medianGPP, meanGPP_vol, medianGPP_vol, meanzMix, DOC_mgL, TP_ugL, dataset), by="lakeName") 
  # filter(!lakeName=="EastLong")

# Build the mean GPP model
model <- lm(meanGPP ~ PPareal, data = preds)
# Summarize the model
summary(model)
# Make predictions
predictions_meanGPP <- model %>% predict(preds)

# (a) Prediction error, RMSE
RMSE_meanGPP<-RMSE(predictions_meanGPP, preds$meanGPP)
# 1556 mg C m-2 day-1 
# 1606 mg C m-2 day-1 without EL
# (b) R-square
R2_meanGPP<-R2(predictions_meanGPP, preds$meanGPP)
#R2=0.58
#R2=0.57

# Build the median GPP model
model <- lm(medianGPP ~ PPareal, data = preds)
# Summarize the model
summary(model)
# Make predictions
predictions_medianGPP <- model %>% predict(preds)

# (a) Prediction error, RMSE
RMSE_medianGPP<-RMSE(predictions_medianGPP, preds$medianGPP)
# 1556 mg C m-2 day-1 
# 1606 mg C m-2 day-1 without EL
# (b) R-square
R2_medianGPP<-R2(predictions_medianGPP, preds$medianGPP)
#R2=0.58
#R2=0.57



# Build the mean volumetric GPP model
model <- lm(meanGPP_vol ~ PPvol, data = preds)
# Summarize the model
summary(model)
# Make predictions
predictions_meanGPP_vol <- model %>% predict(preds)

# (a) Prediction error, RMSE
RMSE_meanGPP_vol<-RMSE(predictions_meanGPP_vol, preds$meanGPP_vol)
#335
# (b) R-square
R2_meanGPP_vol<-R2(predictions_meanGPP_vol, preds$meanGPP_vol)
#0.65

# Build the median GPP model
model <- lm(medianGPP_vol ~ PPvol, data = preds)
# Summarize the model
summary(model)
# Make predictions
predictions_medianGPP_vol <- model %>% predict(preds)

# (a) Prediction error, RMSE
RMSE_medianGPP_vol<-RMSE(predictions_medianGPP_vol, preds$medianGPP_vol)
# 305 mg C L-2 day-1 
# (b) R-square
R2_medianGPP_vol<-R2(predictions_medianGPP_vol, preds$medianGPP_vol)
#R2=0.63




# Build the DOC model
model <- lm(DOC_mgL ~ eqDOC, data = preds)
# Summarize the model
summary(model)
# Make predictions
predictions_DOC <- model %>% predict(preds)

# (a) Prediction error, RMSE
RMSE_DOC<-RMSE(predictions_DOC, preds$DOC_mgL)
# 5.2 mg/L DOC
# 2.7 mg/L DOC without EL
# (b) R-square
R2_DOC<-R2(predictions_DOC, preds$DOC_mgL)
#R2=0.29
#R2=0.81 without EL

# Build the TP model
model <- lm(TP ~ eqTP, data = preds)
# Summarize the model
summary(model)
# Make predictions
predictions_TP <- model %>% predict(preds)

# (a) Prediction error, RMSE
RMSE_TP<-RMSE(predictions_TP, preds$TP)
# 12.5 ug/L...
# 11.6 ug/L without EL
# (b) R-square
R2_TP<-R2(predictions_TP, preds$TP)
#R2=0.74
#R2=0.79 without EL

# Build the zMix model
model <- lm(meanzMix ~ zMix_mod, data = preds)
# Summarize the model
summary(model)
# Make predictions
predictions_zMix <- model %>% predict(preds)

# (a) Prediction error, RMSE
RMSE_zMix<-RMSE(predictions_zMix, preds$meanzMix)
# 1.6 m
# 1.7 without EL
# (b) R-square
R2_zMix<-R2(predictions_zMix, preds$meanzMix)
#R2=0.91
#R2=0.91 without EL

variables<-c("meanGPP","medianGPP",
             "meanGPP_vol","medianGPP_vol",
             "DOC","TP","zMix")
R2s<-c(R2_meanGPP,
         R2_medianGPP,
       R2_meanGPP_vol,
       R2_medianGPP_vol,
         R2_DOC,
         R2_TP,
         R2_zMix)
RMSEs<-c(RMSE_meanGPP,
       RMSE_medianGPP,
       RMSE_meanGPP_vol,
       RMSE_medianGPP_vol,
       RMSE_DOC,
       RMSE_TP,
       RMSE_zMix)
inflow_type<- "measured"
#Combine to make a dataframe of model output 
Measured_inflow_fits<-data.frame(variables,inflow_type, RMSEs, R2s)


Measured_inflow_fits_table<-
  hux(Measured_inflow_fits) %>% 
  # add_colnames() %>% 
  set_bold(row = 1, col = everywhere, value = TRUE) %>% 
  set_all_borders(TRUE) %>%
  theme_article()


# ALL LAKES  --------
#Both measured and modeled inflows 
# sims<- sims %>% filter(lakeName=="FredriksburgSlotso")
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
    kD=kA*A+kDOC*DOC-0.05	#m-1; based on Morris paper
    
    # from a published paper -> I'll refind the paper eventually
    ##Kelly et al., 2018 EQN 7
    zmix=10^(-0.515*log10(DOC)+0.115*log10(2*sqrt(SA/pi))+0.991)

    if(zmix>zmax){zmix=zmax}
    
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
  })
}

times=1:1000
storeAs=matrix(NA,length(times),nrow(sims))
storePs=storeAs
storeDOCs=storeAs

# loop through different surface areas, DOC loads, and load stoichs
for(j in 1:nrow(sims)){
  #submittd
  parms=c(SA=sims$SA[j],
          zmax=sims$zMean[j],
          kDOC=0.42,#Value used in Kelly paper
          kA=0.00022,#Value used in Kelly paper
          lA=0.1,#Value used in Kelly paper
          pA=1, #Value used in Kelly paper
          hA=55,#Value used in Kelly paper
          mA=2, #Value used in Kelly paper
          decay=0.001, #Value used in Kelly paper
          Pin=sims$Pin[j],
          DOCin=sims$DOCin[j],
          cA=0.015, #Value used in Kelly paper
          v=0.1, #Value used in Kelly paper
          # rec=0.99, #Improves GPP and TP estimates a bit!
          rec=0.95, #Value used in Kelly paper
          Qin=(sims$SA[j]*1e6*sims$zMean[j])/sims$HRT_days[j], #Could also try this.. [m3/day]
          P=sims$TP[j],
          DOC=sims$DOC[j],
          # V=(sims$SA[j]*1e6*sims$zMean[j]),
          V=sims$V_m3[j]) #USED IN ZWART LAKES VERSION
# 
#   parms=c(SA=sims2$SA[j],
#           zmax=sims2$zMax[j],
#           kDOC=0.76,
#           kA=0.00005, 
#           lA=0.27,
#           pA=0.30,
#           hA=99.9,
#           mA=9.9,
#           decay=0.005746313,
#           Pin=sims2$Pin[j],
#           DOCin=sims2$DOCin[j],
#           cA=0.005,
#           v=0.008, 
#           rec=0.99, 
#           Qin=sims2$V_m3[j]/sims2$HRT_days[j],
#           P=sims2$TP[j],
#           DOC=sims2$DOC[j])
  
  
  # starting state variables
  n<-c(A=500,
       P=sims$TP[j],
       DOC=sims$DOC[j])

  # simulate with ode
  run=ode(y=n,times=times,func=huisman,parms=parms)
  
  storeAs[,j]<-run[,2]
  storePs[,j]<-run[,3]
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
  zmax=sims$zMean
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
    # storePP[j,i]<-storeAs[j,i]*zmix*(pA/kD)*log((hA+I0)/(hA+Izmix))*(cP/(cP+mA)) #This is how Patrick coded it, but that's wrong I think. Does match equation 2 in the paper
    light.limit.d[j,i]=log((hA+I0)/(hA+Izmix))
    nutrient.limit.d[j,i]<-(cP/(cP+mA))
    
  }
}



store_arealPP=storePP*store_zmix*1.25

sims$eqDOC=storeDOCs[nrow(storeDOCs),]
sims$eqTP=storeTP[nrow(storeTP),]
sims$PPareal=store_arealPP[nrow(store_arealPP),]
sims$PPvol=storePP[nrow(storePP),]
sims$zMix_mod=store_zmix[nrow(store_zmix),]
sims$A=storeAs[nrow(storeAs),]
sims$light.limit.d=light.limit.d[nrow(light.limit.d),]
sims$nutrient.limit.d=nutrient.limit.d[nrow(nutrient.limit.d),]

# ABCD with fill for dataset ----------------------------------------------

#PREDICTED VS OBSERVED
A<-sims %>%
  select(lakeName, eqDOC, PPareal) %>%
  left_join(., master_df %>%
              select(lakeName, meanGPP, medianGPP, DOC_mgL, dataset), by="lakeName") %>%
  left_join(., metadata %>%
              select(lakeName, inflows_YN), by="lakeName") %>%
  # filter(dataset=="modelled")%>%
  # filter(inflows_YN=="no")%>%
  # filter(!lakeName %in% c("Mendota","Acton"))%>%
  ggplot(aes(y=medianGPP, x=PPareal*1.25, color=dataset, group=NA, fill=dataset, shape=inflows_YN))+ #correct predictions for autrophic respiration
  geom_point(size=3, alpha=0.8)+
  theme_bw()+
  xlab(expression(paste('PRED. GPP (mg C m'^-2,' day'^-1,')')))+
  ylab(expression(paste('OBS. GPP (mg C m'^-2,' day'^-1,')')))+
  scale_fill_manual(values=c("#e63946","#457b9d"),
                    name="How were loads\ndeteremined?")+
  scale_color_manual(values=c("#e63946","#457b9d"),
                     name="How were loads\ndeteremined?")+
  scale_shape_manual(values=c(21,25),
                     name="Does the lake\nhave surface inflow(s)?")+
  # geom_text_repel(aes(label = lakeName), size = 3)+
  geom_abline(intercept = 0, slope = 1)+
  facet_grid(.~dataset)+
  geom_smooth(method="lm", se=T)+
  theme(panel.spacing = unit(0, "lines"))+
  guides(fill = guide_legend(override.aes = list(shape = 21) ),
         shape = guide_legend(override.aes = list(fill = "black") ) )+
  # xlim(c(0,10000))+ylim(c(0,10000))
  xlim(c(0,5000))+ylim(c(0,5000))


#Predicted "equilibrium" DOC values are much lower than observed DOC? Why?
B<-sims %>%
  left_join(., master_df %>%
              select(lakeName, meanGPP, DOC_mgL, dataset), by="lakeName") %>%
  left_join(., metadata %>%
              select(lakeName, inflows_YN), by="lakeName") %>%
  # filter(dataset=="modelled")%>%
  # filter(inflows_YN=="no")%>%
  ggplot(aes(y=DOC, x=eqDOC, fill=dataset, color=dataset, group=NA, shape=inflows_YN))+
  geom_point( size=3, alpha=0.8)+
  geom_abline(intercept = 0, slope = 1)+
  xlab(expression(paste('PRED. lake DOC mg L'^-1,)))+
  ylab(expression(paste('OBS. lake DOC mg L'^-1,)))+
  scale_fill_manual(values=c("#e63946","#457b9d"),
                    name="How were loads\ndeteremined?")+
  scale_color_manual(values=c("#e63946","#457b9d"),
                    name="How were loads\ndeteremined?")+
  scale_shape_manual(values=c(21,25),
                     name="Does the lake\nhave surface inflow(s)?")+
    # geom_text_repel(aes(label = lakeName), size = 3)+
  theme_bw()+
  facet_grid(.~dataset)+
  geom_smooth(method="lm", se=T)+
  theme(panel.spacing = unit(0, "lines"))+
  guides(fill = guide_legend(override.aes = list(shape = 21) ),
         shape = guide_legend(override.aes = list(fill = "black") ) )+
  xlim(c(0,75))+ylim(c(0,75))


#Model tends to overpredict DOC, but it's especially bad for two lakes (EastLong, Vortsjarv)

#Relationship between equilibrium TP and actual in-lake TP
C<-sims %>%
  left_join(., master_df %>%
              select(lakeName, meanGPP, dataset), by="lakeName") %>%
  left_join(., metadata %>%
              select(lakeName, inflows_YN), by="lakeName") %>%
  # filter(dataset=="modelled")%>%
  # filter(inflows_YN=="no")%>%
  ggplot(aes(x=eqTP, y=TP, fill=dataset, color=dataset, group=NA, shape=inflows_YN))+
  geom_point(size=3, alpha=0.8)+
  geom_abline(intercept = 0, slope = 1)+
  xlab(expression(paste('PRED. lake TP ug L'^-1,)))+
  ylab(expression(paste('OBS. lake TP ug L'^-1,)))+
  scale_fill_manual(values=c("#e63946","#457b9d"),
                    name="How were loads\ndeteremined?")+
  scale_color_manual(values=c("#e63946","#457b9d"),
                    name="How were loads\ndeteremined?")+
  scale_shape_manual(values=c(21,25),
                     name="Does the lake\nhave surface inflow(s)?")+
  theme_bw()+
  # geom_text_repel(aes(label = lakeName), size = 3)+
  facet_grid(.~dataset)+
  geom_smooth(method="lm", se=T)+
  theme(panel.spacing = unit(0, "lines"))+
  guides(fill = guide_legend(override.aes = list(shape = 21) ),
         shape = guide_legend(override.aes = list(fill = "black") ) )+
  xlim(c(0,350))+ylim(c(0,350))



#What do the zMix estimates look like compared to actual mean zMix observed values?
D<-sims %>%
  left_join(., master_df %>%
              select(lakeName, dataset), by="lakeName") %>%
  left_join(., metadata %>%
              select(lakeName, inflows_YN), by="lakeName") %>%
  # filter(dataset=="modelled")%>%
  # filter(inflows_YN=="no")%>%
  ggplot(aes(y=meanzMix, x=zMix_mod, fill=dataset, color=dataset, group=NA, shape=inflows_YN))+
  geom_point(size=3, alpha=0.8)+
  geom_abline(intercept = 0, slope = 1)+
  scale_fill_manual(values=c("#e63946","#457b9d"),
                    name="How were loads\ndeteremined?")+
  scale_color_manual(values=c("#e63946","#457b9d"),
                    name="How were loads\ndeteremined?")+
  # scale_fill_manual(values=c("#457b9d","#457b9d"),
  #                   name="How were loads\ndeteremined?")+
  scale_shape_manual(values=c(21,25),
                     name="Does the lake\nhave surface inflow(s)?")+
  xlab("PRED. zMix (m)")+
  ylab("OBS. zMix (m)")+
  theme_bw()+
  # geom_text_repel(aes(label = lakeName), size = 3)+
  geom_smooth(method="lm", se=T)+
  facet_grid(.~dataset)+
  theme(panel.spacing = unit(0, "lines"))+
  guides(fill = guide_legend(override.aes = list(shape = 21) ),
         shape = guide_legend(override.aes = list(fill = "black") ) )+
  xlim(c(0,10))+ylim(c(0,10))

E<-sims %>%
  left_join(., master_df %>%
              select(lakeName, dataset), by="lakeName") %>%
  left_join(., metadata %>%
              select(lakeName, inflows_YN), by="lakeName") %>%
  # filter(dataset=="modelled")%>%
  # filter(inflows_YN=="no")%>%
  ggplot(aes(y=A, x=eqTP, fill=dataset, shape=inflows_YN))+
  geom_point(size=3, alpha=0.8)+
  # geom_abline(intercept = 0, slope = 1)+
  scale_fill_manual(values=c("#e63946","#457b9d"),
                    name="How were loads\ndeteremined?")+
  # scale_fill_manual(values=c("#457b9d","#457b9d"),
  #                   name="How were loads\ndeteremined?")+
  scale_shape_manual(values=c(21,25),
                     name="Does the lake\nhave surface inflow(s)?")+
  xlab("PRED. TP (ug/L)")+
  ylab("PRED. algal biomass (mg C/m3)")+
  theme_bw()+
  # geom_text_repel(aes(label = lakeName), size = 3)+
  # facet_grid(.~dataset)+
  theme(panel.spacing = unit(0, "lines"))+
  guides(fill = guide_legend(override.aes = list(shape = 21) ),
         shape = guide_legend(override.aes = list(fill = "black") ) )+
  xlim(c(0,350))+ylim(c(0,20000))

EFF<-sims %>%
  left_join(., master_df %>%
              select(lakeName, dataset), by="lakeName") %>%
  left_join(., metadata %>%
              select(lakeName, inflows_YN), by="lakeName") %>%
  # filter(dataset=="modelled")%>%
  # filter(inflows_YN=="no")%>%
  ggplot(aes(y=PPareal, x=A, fill=dataset, shape=inflows_YN))+
  geom_point(size=3, alpha=0.8)+
  # geom_abline(intercept = 0, slope = 1)+
  scale_fill_manual(values=c("#e63946","#457b9d"),
                    name="How were loads\ndeteremined?")+
  # scale_fill_manual(values=c("#457b9d","#457b9d"),
  #                   name="How were loads\ndeteremined?")+
  scale_shape_manual(values=c(21,25),
                     name="Does the lake\nhave surface inflow(s)?")+
  ylab("PRED. GPP (mg C m-2 day-1)")+
  xlab("PRED. algal biomass (mg C/m3)")+
  theme_bw()+
  # geom_text_repel(aes(label = lakeName), size = 3)+
  # facet_grid(.~dataset)+
  theme(panel.spacing = unit(0, "lines"))+
  guides(fill = guide_legend(override.aes = list(shape = 21) ),
         shape = guide_legend(override.aes = list(fill = "black") ) )+
  xlim(c(0,20000))+ylim(c(0,10000))


(A+B)/(C+D)+
# /(E+EFF)+
  plot_layout(guides = 'collect')+
  plot_annotation(tag_levels = 'A',
                  title = 'Observations vs. predictions of GPP, lake DOC, lake TP, and zMix',
                  subtitle = 'The predicted values using the published Kelly model parameters; predictions adjusted for autotrophic respiration (*1.25)',
                  caption = 'Plot generated 2020-08-25 IAO')

#Compare areal mean GPP, areal median GPP, and volumetric mean GPP, volumetric median GPP


sims %>%
  select(lakeName, eqDOC, PPareal) %>%
  left_join(., master_df %>%
              select(lakeName, meanGPP, medianGPP, DOC_mgL, dataset), by="lakeName") %>%
  left_join(., metadata %>%
              select(lakeName, inflows_YN), by="lakeName") %>%
  # filter(dataset=="modelled")%>%
  # filter(inflows_YN=="no")%>%
  # filter(!lakeName %in% c("Mendota","Acton"))%>%
  ggplot(aes(y=meanGPP, x=DOC_mgL, fill=dataset, shape=inflows_YN, group=NA))+
  geom_point(size=3, alpha=0.8)+
  theme_bw()+
  ylab(expression(paste('PRED. GPP (mg C m'^-2,' day'^-1,')')))+
  # ylab(expression(paste('OBS. GPP (mg C m'^-2,' day'^-1,')')))+
  scale_fill_manual(values=c("#e63946","#457b9d"),
                    name="How were loads\ndeteremined?")+
  geom_smooth(method="lm", se=T, color="black")+
  scale_shape_manual(values=c(21,25),
                     name="Does the lake\nhave surface inflow(s)?")+
  geom_text_repel(aes(label = lakeName), size = 3)+
  geom_abline(intercept = 0, slope = 1)+
  # facet_grid(.~dataset)+
  theme(panel.spacing = unit(0, "lines"))+
  guides(fill = guide_legend(override.aes = list(shape = 21) ),
         shape = guide_legend(override.aes = list(fill = "black") ) )



# ggsave(here("results/draft MS figs/predVSmeasuredGPP-DOC-TP-zMix.png"), width=9, height=6,units="in", dpi=600)

##Relationship between measured and modelled TP concentrations in lake and inflow 
X<-sims %>%
  left_join(., master_df %>%
              select(lakeName, dataset), by="lakeName") %>%
  left_join(., metadata %>%
              select(lakeName, inflows_YN), by="lakeName") %>%
  filter(dataset=="modelled")%>%
  filter(inflows_YN=="yes")%>%
  ggplot(aes(y=eqTP, x=Pin, fill=dataset, shape=inflows_YN))+
  geom_point(size=3, alpha=0.8)+
  geom_abline(intercept = 0, slope = 1)+
  scale_fill_manual(values=c("#e63946","#457b9d"),
                    name="How were loads\ndeteremined?")+
  scale_shape_manual(values=c(21,25),
                     name="Does the lake\nhave surface inflow(s)?")+
  facet_grid(.~dataset)+
  ylab(expression(paste('PRED. lake TP ug L'^-1,)))+
  xlab(expression(paste('Inflow TP ug L'^-1,)))+
  theme_bw()+
  theme(panel.spacing = unit(0, "lines"))+
  geom_text_repel(aes(label = lakeName), size = 3)+
  guides(fill = guide_legend(override.aes = list(shape = 21) ),
         shape = guide_legend(override.aes = list(fill = "black") ) )

Y<-sims %>%
  left_join(., master_df %>%
              select(lakeName, dataset), by="lakeName") %>%
  left_join(., metadata %>%
              select(lakeName, inflows_YN), by="lakeName") %>%
  filter(dataset=="modelled")%>%
  filter(inflows_YN=="yes")%>%
  ggplot(aes(y=TP, x=Pin, fill=dataset, shape=inflows_YN))+
  geom_point(size=3, alpha=0.8)+
  geom_abline(intercept = 0, slope = 1)+
  ylab(expression(paste('Known lake TP ug L'^-1,)))+
  xlab(expression(paste('Inflow TP ug L'^-1,)))+
  scale_shape_manual(values=c(21,25),
                     name="Does the lake\nhave surface inflow(s)?")+
  scale_fill_manual(values=c("#e63946","#457b9d"),
                    name="How were loads\ndeteremined?")+
  facet_grid(.~dataset)+
  theme_bw()+
  theme(panel.spacing = unit(0, "lines"))+
  geom_text_repel(aes(label = lakeName), size = 3)+
  guides(fill = guide_legend(override.aes = list(shape = 21) ),
         shape = guide_legend(override.aes = list(fill = "black") ) )

(C/X/Y)+
  plot_layout(guides = 'collect')+
  plot_annotation(tag_levels = 'A')

##Relationship between measured and modelled DOC concentrations in lake and inflow 
Q<-sims %>%
  left_join(., master_df %>%
              select(lakeName, dataset), by="lakeName") %>%
  left_join(., metadata %>%
              select(lakeName, inflows_YN), by="lakeName") %>%
  filter(dataset=="modelled")%>%
  filter(inflows_YN=="yes")%>%
  ggplot(aes(y=eqDOC, x=DOCin, fill=dataset))+
  geom_point(shape=21, size=3)+
  geom_abline(intercept = 0, slope = 1)+
  ylab(expression(paste('PRED. lake DOC mg L'^-1,)))+
  xlab(expression(paste('Modelled Inflow DOC mg L'^-1,)))+
  theme_bw()+
  geom_text_repel(aes(label = lakeName), size = 3)+
  xlim(c(0,60))+ylim(c(0,60))




R<-sims %>%
  left_join(., master_df %>%
              select(lakeName, dataset), by="lakeName") %>%
  left_join(., metadata %>%
              select(lakeName, inflows_YN), by="lakeName") %>%
  filter(!lakeName=="EastLong")%>%
  # filter(dataset=="modelled")%>%
  # filter(inflows_YN=="yes")%>%
  ggplot(aes(y=DOC, x=DOCin, fill=dataset, shape=inflows_YN))+
  geom_point(size=3, alpha=0.8)+
  geom_abline(intercept = 0, slope = 1)+
  ylab(expression(paste('Known lake DOC mg L'^-1,)))+
  xlab(expression(paste('Modelled Inflow DOC mg L'^-1,)))+
  scale_shape_manual(values=c(21,25),
                     name="Does the lake\nhave surface inflow(s)?")+
  scale_fill_manual(values=c("#e63946","#457b9d"),
                    name="How were loads\ndeteremined?")+
  facet_grid(.~dataset)+
  theme_bw()+
  theme(panel.spacing = unit(0, "lines"))+
  geom_text_repel(aes(label = lakeName), size = 3)+
  guides(fill = guide_legend(override.aes = list(shape = 21) ),
         shape = guide_legend(override.aes = list(fill = "black") ) )

(B+Q+R)+
  plot_layout(guides = 'collect')



# Relationship btwn obs & pred volumetric GPP -----------------------------

sims %>%
  left_join(., master_df %>%
              select(lakeName, meanGPP_vol, dataset), by="lakeName") %>%
  left_join(., metadata %>%
              select(lakeName, inflows_YN), by="lakeName") %>%
  filter(dataset=="modelled")%>%
  # filter(inflows_YN=="no")%>%
  ggplot(aes(y=meanGPP_vol, x=PPvol, fill=dataset, shape=inflows_YN))+
  geom_point(size=3, alpha=0.8)+
  geom_abline(intercept = 0, slope = 1)+
  scale_fill_manual(values=c("#e63946","#457b9d"),
                    name="How were loads\ndeteremined?")+
  # scale_fill_manual(values=c("#457b9d","#457b9d"),
  #                   name="How were loads\ndeteremined?")+
  scale_shape_manual(values=c(21,25),
                     name="Does the lake\nhave surface inflow(s)?")+
  # ylab("PRED. GPP (mg C m-2 day-1)")+
  # xlab("PRED. algal biomass (mg C/m3)")+
  theme_bw()+
  geom_text_repel(aes(label = lakeName), size = 3)+
  # facet_grid(.~dataset)+
  theme(panel.spacing = unit(0, "lines"))+
  guides(fill = guide_legend(override.aes = list(shape = 21) ),
         shape = guide_legend(override.aes = list(fill = "black") ) )+
  xlim(c(0,3000))+ylim(c(0,3000))


# Just measured inflow lakes ----------------------------------------------


#Calculate logSSE
SSEdf_measured<-sims %>%
  select(lakeName, eqDOC, PPareal) %>%
  left_join(., master_df %>%
              select(lakeName, meanGPP, medianGPP, DOC_mgL, dataset), by="lakeName") %>%
  left_join(., metadata %>%
              select(lakeName, inflows_YN), by="lakeName") %>%
  filter(dataset=="measured")
SSEdf_modeled<-sims %>%
  select(lakeName, eqDOC, PPareal) %>%
  left_join(., master_df %>%
              select(lakeName, meanGPP, medianGPP, DOC_mgL, dataset), by="lakeName") %>%
  left_join(., metadata %>%
              select(lakeName, inflows_YN), by="lakeName")%>%
  filter(dataset=="modelled")

err_measured=log(SSEdf_measured$PPareal*1.25+0.1)-log(SSEdf_measured$medianGPP)
logSSE_measured=sum(err_measured*err_measured)

err_modeled=log(SSEdf_modeled$PPareal*1.25+0.1)-log(SSEdf_modeled$medianGPP)
logSSE_modeled=sum(err_modeled*err_modeled)

logSSE_measured
logSSE_modeled

#PREDICTED VS OBSERVED
measured<-sims %>%
  select(lakeName, eqDOC, PPareal) %>%
  left_join(., master_df %>%
              select(lakeName, meanGPP, medianGPP, DOC_mgL, dataset), by="lakeName") %>%
  left_join(., metadata %>%
              select(lakeName, inflows_YN), by="lakeName") %>%
  filter(dataset=="measured")%>%
  # filter(inflows_YN=="no")%>%
  # filter(!lakeName %in% c("Mendota","Acton"))%>%
  ggplot(aes(y=medianGPP, x=PPareal*1.25, fill=dataset, shape=inflows_YN))+
  geom_point(size=3, alpha=0.8)+
  theme_bw()+
  xlab(expression(paste('PRED. GPP (mg C m'^-2,' day'^-1,')')))+
  ylab(expression(paste('OBS. GPP (mg C m'^-2,' day'^-1,')')))+
  scale_fill_manual(values=c("#e63946"),
                    name="How were loads\ndeteremined?")+
  geom_smooth(method="lm", se=T, color="#e63946", fill="#e63946")+
  scale_shape_manual(values=c(21,25),
                     name="Does the lake\nhave surface inflow(s)?")+
  # geom_text_repel(aes(label = lakeName), size = 3)+
  geom_abline(intercept = 0, slope = 1)+
  # facet_grid(.~dataset)+
  theme(panel.spacing = unit(0, "lines"))+
  guides(fill = guide_legend(override.aes = list(shape = 21) ),
         shape = "none"  )+
  xlim(c(0,3000))+ylim(c(0,4000))+
  annotate("text", x = 500, y = 4000, label = paste('logSSE=',round(logSSE_measured,1)))

modeled<-sims %>%
  select(lakeName, eqDOC, PPareal) %>%
  left_join(., master_df %>%
              select(lakeName, meanGPP, medianGPP, DOC_mgL, dataset), by="lakeName") %>%
  left_join(., metadata %>%
              select(lakeName, inflows_YN), by="lakeName") %>%
  filter(dataset=="modelled")%>%
  # filter(inflows_YN=="no")%>%
  # filter(!lakeName %in% c("Mendota","Acton"))%>%
  ggplot(aes(y=medianGPP, x=PPareal*1.25, group=NA, fill=dataset, shape=inflows_YN))+
  geom_point(size=3, alpha=0.8)+
  theme_bw()+
  xlab(expression(paste('PRED. GPP (mg C m'^-2,' day'^-1,')')))+
  ylab(expression(paste('OBS. GPP (mg C m'^-2,' day'^-1,')')))+
  scale_fill_manual(values=c("#457b9d"),
                    name="")+
  geom_smooth(method="lm", se=T, color="#457b9d", fill="#457b9d")+
  scale_shape_manual(values=c(21,25),
                     name="Does the lake\nhave surface inflow(s)?")+
  # geom_text_repel(aes(label = lakeName), size = 3)+
  geom_abline(intercept = 0, slope = 1)+
  # facet_grid(.~dataset)+
  theme(panel.spacing = unit(0, "lines"))+
  guides(fill = guide_legend(override.aes = list(shape = 21), order=1 ),
         shape = guide_legend(override.aes = list(fill = "black"), order=2 ) )+
  # xlim(c(0,10000))+ylim(c(0,10000))
  xlim(c(0,3000))+ylim(c(0,4000))+
  annotate("text", x = 500, y = 4000, label = paste('logSSE=',round(logSSE_modeled,1)))

(measured+modeled)+
  # /(E+EFF)+
  plot_layout(guides = 'collect')+
  plot_annotation(tag_levels = 'A',
                  title = 'Observed vs. predicted GPP',
                  subtitle = 'The predicted values using the published Kelly model parameters; predictions adjusted for autotrophic respiration (*1.25)',
                  caption = 'Plot generated 2020-08-25 IAO')





# RMSE of model predictions -----------------------------------------------


library(caret)
preds<-sims %>%
  left_join(., master_df %>%
              select(lakeName, meanGPP, medianGPP, meanGPP_vol, medianGPP_vol, DOC_mgL, TP_ugL, dataset), by="lakeName") %>%
  left_join(., metadata %>%
              select(lakeName, inflows_YN), by="lakeName") %>%
  filter(dataset=="modelled")%>%
  # filter(inflows_YN=="yes")%>%
  mutate(TP = replace(TP, lakeName=="Almberga", 5), #assumption, no TP data provided
         TP = replace(TP, lakeName=="P1", 35), #SRP, not TP
         TP = replace(TP, lakeName=="P8", 30),#SRP, not TP
         TP = replace(TP, lakeName=="Suggs", 149),  #TDP, not TP
         TP = replace(TP, lakeName=="LittleRock", 23.8))%>% #orthoP, not TP
  mutate(Pin = replace(Pin, lakeName=="TroutBog", 29), #value from 2021-05-03 model run
         Pin = replace(Pin, lakeName=="LittleRock", 100), #value from 2021-05-03 model run
         Pin = replace(Pin, lakeName=="Hummingbird", 22),
         Pin = replace(Pin, lakeName=="Taihu", 1000))#value from 2021-05-03 model run

# Build the mean GPP model
model <- lm(meanGPP ~ PPareal, data = preds)
# Summarize the model
summary(model)
# Make predictions
predictions_meanGPP <- model %>% predict(preds)

# (a) Prediction error, RMSE
RMSE_meanGPP<-RMSE(predictions_meanGPP, preds$meanGPP)
# 1216 mg C m-2 day-1 
# (b) R-square
R2_meanGPP<-R2(predictions_meanGPP, preds$meanGPP)
#R2=0.48

# Build the median GPP model
model <- lm(medianGPP ~ PPareal, data = preds)
# Summarize the model
summary(model)
# Make predictions
predictions_medianGPP <- model %>% predict(preds)

# (a) Prediction error, RMSE
RMSE_medianGPP<-RMSE(predictions_medianGPP, preds$meanGPP)
# 1216 mg C m-2 day-1 
# (b) R-square
R2_medianGPP<-R2(predictions_medianGPP, preds$meanGPP)
#R2=0.48



# Build the mean volumetric GPP model
model <- lm(meanGPP_vol ~ PPvol, data = preds)
# Summarize the model
summary(model)
# Make predictions
predictions_meanGPP_vol <- model %>% predict(preds)

# (a) Prediction error, RMSE
RMSE_meanGPP_vol<-RMSE(predictions_meanGPP_vol, preds$meanGPP_vol)
#335
# (b) R-square
R2_meanGPP_vol<-R2(predictions_meanGPP_vol, preds$meanGPP_vol)
#0.65

# Build the median GPP model
model <- lm(medianGPP_vol ~ PPvol, data = preds)
# Summarize the model
summary(model)
# Make predictions
predictions_medianGPP_vol <- model %>% predict(preds)

# (a) Prediction error, RMSE
RMSE_medianGPP_vol<-RMSE(predictions_medianGPP_vol, preds$medianGPP_vol)
# 305 mg C L-2 day-1 
# (b) R-square
R2_medianGPP_vol<-R2(predictions_medianGPP_vol, preds$medianGPP_vol)
#R2=0.63






# Build the DOC model
model <- lm(DOC_mgL ~ eqDOC, data = preds)
# Summarize the model
summary(model)
# Make predictions
predictions_DOC <- model %>% predict(preds)

# (a) Prediction error, RMSE
RMSE_DOC<-RMSE(predictions_DOC, preds$DOC_mgL)
# 4.2 mg/L DOC
# (b) R-square
R2_DOC<-R2(predictions_DOC, preds$DOC_mgL)
#R2=0.88

# Build the TP model
model <- lm(TP ~ eqTP, data = preds)
# Summarize the model
summary(model)
# Make predictions
predictions_TP <- model %>% predict(preds)

# (a) Prediction error, RMSE
RMSE_TP<-RMSE(predictions_TP, preds$TP)
# 19.5 ug/L...
# (b) R-square
R2_TP<-R2(predictions_TP, preds$TP)
#R2=0.72


# Build the zMix model
model <- lm(meanzMix ~ zMix_mod, data = preds)
# Summarize the model
summary(model)
# Make predictions
predictions_zMix <- model %>% predict(preds)

# (a) Prediction error, RMSE
RMSE_zMix<- RMSE(predictions_zMix, preds$meanzMix)
# 1.7 m
# (b) R-square
R2_zMix<-R2(predictions_zMix, preds$meanzMix)
#R2=0.56



variables<-c("meanGPP","medianGPP",
             "meanGPP_vol","medianGPP_vol",
             "DOC","TP","zMix")
R2s<-c(R2_meanGPP,
       R2_medianGPP,
       R2_meanGPP_vol,
       R2_medianGPP_vol,
       R2_DOC,
       R2_TP,
       R2_zMix)
RMSEs<-c(RMSE_meanGPP,
         RMSE_medianGPP,
         RMSE_meanGPP_vol,
         RMSE_medianGPP_vol,
         RMSE_DOC,
         RMSE_TP,
         RMSE_zMix)
# inflow_type<- "modeled"
# #Combine to make a dataframe of model output 
# Modeled_inflow_fits<-data.frame(variables, inflow_type, RMSEs, R2s)

#Combine to make a dataframe of model output 
Modeled_fits<-data.frame(variables, RMSEs, R2s)


Modeled_fits_table<-
  hux(Modeled_fits) %>% 
  # add_colnames() %>%
  # mutate_each(funs(prettyNum(., big.mark=",")))%>%
  set_bold(row = 1, col = everywhere, value = TRUE) %>% 
  set_all_borders(TRUE) %>%
  theme_article()

##Export table with all the 
obs_vs_pred_results<-
bind_rows(Modeled_inflow_fits, Measured_inflow_fits)%>%
  hux() %>% 
  # add_colnames() %>%
  # mutate_each(funs(prettyNum(., big.mark=",")))%>%
  set_bold(row = 1, col = everywhere, value = TRUE) %>% 
  set_all_borders(TRUE) %>%
  theme_article()

library(openxlsx)
# multiple sheets in a single workbook:
wb <- openxlsx::createWorkbook()
wb <- as_Workbook(Modeled_fits_table,
                  Workbook = wb, sheet = "sheet1")

saveWorkbook(wb, "results/draft MS figs/obs_v_pred_table_defaultKellyParameters_includingDefaultParmsLoadEst.xlsx", overwrite = TRUE)

# quick_docx(obs_vs_pred_results, file = 'results/draft MS figs/obs_v_pred_table.docx')

# >> export plots & tables ------------------------------------------------


GPPareal_mean<-sims %>%
  select(lakeName, eqDOC, PPareal) %>%
  left_join(., master_df %>%
              select(lakeName, meanGPP, medianGPP, DOC_mgL, dataset), by="lakeName") %>%
  left_join(., metadata %>%
              select(lakeName, inflows_YN), by="lakeName") %>%
  ggplot(aes(y=meanGPP, x=PPareal, fill=dataset, shape=inflows_YN, group=NA))+
  geom_point(size=3, alpha=0.8)+
  theme_bw()+
  xlab(expression(paste('PRED. GPP (mg C m'^-2,' day'^-1,')')))+
  ylab(expression(paste('OBS. mean GPP (mg C m'^-2,' day'^-1,')')))+
  scale_fill_manual(values=c("#e63946","#457b9d"),
                    name="How were loads\ndeteremined?")+
  scale_shape_manual(values=c(21,25),
                     name="Does the lake\nhave surface inflow(s)?")+
  geom_smooth(method="lm", se=TRUE, color="black")+
  geom_abline(intercept = 0, slope = 1)+
  theme(panel.spacing = unit(0, "lines"))+
  guides(fill = guide_legend(override.aes = list(shape = 21) ),
         shape = guide_legend(override.aes = list(fill = "black") ) )+
  xlim(c(0,5000))+ylim(c(0,5000))


GPPareal_median<-sims %>%
  select(lakeName, eqDOC, PPareal) %>%
  left_join(., master_df %>%
              select(lakeName, meanGPP, medianGPP, DOC_mgL, dataset), by="lakeName") %>%
  left_join(., metadata %>%
              select(lakeName, inflows_YN), by="lakeName") %>%
  ggplot(aes(y=medianGPP, x=PPareal, fill=dataset, shape=inflows_YN, group=NA))+
  geom_point(size=3, alpha=0.8)+
  theme_bw()+
  xlab(expression(paste('PRED. GPP (mg C m'^-2,' day'^-1,')')))+
  ylab(expression(paste('OBS. median GPP (mg C m'^-2,' day'^-1,')')))+
  scale_fill_manual(values=c("#e63946","#457b9d"),
                    name="How were loads\ndeteremined?")+
  scale_shape_manual(values=c(21,25),
                     name="Does the lake\nhave surface inflow(s)?")+
  geom_smooth(method="lm", se=TRUE, color="black")+
  geom_abline(intercept = 0, slope = 1)+
  theme(panel.spacing = unit(0, "lines"))+
  guides(fill = guide_legend(override.aes = list(shape = 21) ),
         shape = guide_legend(override.aes = list(fill = "black") ) )+
  xlim(c(0,5000))+ylim(c(0,5000))


GPPvol_mean<-sims %>%
  select(lakeName, eqDOC, PPvol) %>%
  left_join(., master_df %>%
              select(lakeName, meanGPP, medianGPP, meanGPP_vol, medianGPP_vol, DOC_mgL, TP_ugL, dataset), by="lakeName") %>%
  
  left_join(., metadata %>%
              select(lakeName, inflows_YN), by="lakeName") %>%
  ggplot(aes(y=meanGPP_vol, x=PPvol, fill=dataset, shape=inflows_YN, group=NA))+
  geom_point(size=3, alpha=0.8)+
  theme_bw()+
  xlab(expression(paste('PRED. GPP (mg C m'^-3,' day'^-1,')')))+
  ylab(expression(paste('OBS. mean volumetric GPP (mg C m'^-3,' day'^-1,')')))+
  scale_fill_manual(values=c("#e63946","#457b9d"),
                    name="How were loads\ndeteremined?")+
  scale_shape_manual(values=c(21,25),
                     name="Does the lake\nhave surface inflow(s)?")+
  geom_smooth(method="lm", se=TRUE, color="black")+
  geom_abline(intercept = 0, slope = 1)+
  theme(panel.spacing = unit(0, "lines"))+
  guides(fill = guide_legend(override.aes = list(shape = 21) ),
         shape = guide_legend(override.aes = list(fill = "black") ) )+
  xlim(c(0,3000))+ylim(c(0,3000))

GPPvol_median<-sims %>%
  select(lakeName, eqDOC, PPvol) %>%
  left_join(., master_df %>%
              select(lakeName, meanGPP, medianGPP, meanGPP_vol, medianGPP_vol, DOC_mgL, TP_ugL, dataset), by="lakeName") %>%
  
  left_join(., metadata %>%
              select(lakeName, inflows_YN), by="lakeName") %>%
  ggplot(aes(y=medianGPP_vol, x=PPvol, fill=dataset, shape=inflows_YN, group=NA))+
  geom_point(size=3, alpha=0.8)+
  theme_bw()+
  xlab(expression(paste('PRED. GPP (mg C m'^-3,' day'^-1,')')))+
  ylab(expression(paste('OBS. median volumetric GPP (mg C m'^-3,' day'^-1,')')))+
  scale_fill_manual(values=c("#e63946","#457b9d"),
                    name="How were loads\ndeteremined?")+
  scale_shape_manual(values=c(21,25),
                     name="Does the lake\nhave surface inflow(s)?")+
  geom_smooth(method="lm", se=TRUE, color="black")+
  geom_abline(intercept = 0, slope = 1)+
  theme(panel.spacing = unit(0, "lines"))+
  guides(fill = guide_legend(override.aes = list(shape = 21) ),
         shape = guide_legend(override.aes = list(fill = "black") ) )+
  xlim(c(0,3000))+ylim(c(0,3000))

(GPPareal_mean+GPPareal_median)/(GPPvol_mean+GPPvol_median)+
  gridExtra::tableGrob(Modeled_fits[1:4,1:3])+
  # /(E+EFF)+
  plot_layout(guides = 'collect')+
  plot_annotation(tag_levels = 'A',
                  title="Predictions with default Kelly model parameters for AND default parameters for obtaining load estimates")



DOCpreds<-sims %>%
  left_join(., master_df %>%
              select(lakeName, meanGPP, DOC_mgL, dataset), by="lakeName") %>%
  left_join(., metadata %>%
              select(lakeName, inflows_YN), by="lakeName") %>%
  ggplot(aes(y=DOC, x=eqDOC, fill=dataset, shape=inflows_YN, group=NA))+
  geom_point( size=3, alpha=0.8)+
  geom_abline(intercept = 0, slope = 1)+
  xlab(expression(paste('PRED. lake DOC mg L'^-1,)))+
  ylab(expression(paste('OBS. lake DOC mg L'^-1,)))+
  scale_fill_manual(values=c("#e63946","#457b9d"),
                    name="How were loads\ndeteremined?")+
  scale_shape_manual(values=c(21,25),
                     name="Does the lake\nhave surface inflow(s)?")+
  geom_smooth(method="lm", se=TRUE, color="black")+
  theme_bw()+
  theme(panel.spacing = unit(0, "lines"))+
  guides(fill = guide_legend(override.aes = list(shape = 21) ),
         shape = guide_legend(override.aes = list(fill = "black") ) )+
  xlim(c(0,75))+ylim(c(0,75))


TPpreds<-sims %>%
  left_join(., master_df %>%
              select(lakeName, meanGPP, dataset), by="lakeName") %>%
  left_join(., metadata %>%
              select(lakeName, inflows_YN), by="lakeName") %>%
  # filter(dataset=="modelled")%>%
  # filter(inflows_YN=="no")%>%
  ggplot(aes(x=eqTP, y=TP, fill=dataset, shape=inflows_YN, group=NA))+
  geom_point(size=3, alpha=0.8)+
  geom_abline(intercept = 0, slope = 1)+
  xlab(expression(paste('PRED. lake TP ug L'^-1,)))+
  ylab(expression(paste('OBS. lake TP ug L'^-1,)))+
  scale_fill_manual(values=c("#e63946","#457b9d"),
                    name="How were loads\ndeteremined?")+
  geom_smooth(method="lm", se=TRUE, color="black")+
  scale_shape_manual(values=c(21,25),
                     name="Does the lake\nhave surface inflow(s)?")+
  theme_bw()+
  theme(panel.spacing = unit(0, "lines"))+
  guides(fill = guide_legend(override.aes = list(shape = 21) ),
         shape = guide_legend(override.aes = list(fill = "black") ) )+
  xlim(c(0,350))+ylim(c(0,350))



zMixpreds<-sims %>%
  left_join(., master_df %>%
              select(lakeName, dataset), by="lakeName") %>%
  left_join(., metadata %>%
              select(lakeName, inflows_YN), by="lakeName") %>%
  # filter(dataset=="modelled")%>%
  # filter(inflows_YN=="no")%>%
  ggplot(aes(y=meanzMix, x=zMix_mod, fill=dataset, shape=inflows_YN, group=NA))+
  geom_point(size=3, alpha=0.8)+
  geom_abline(intercept = 0, slope = 1)+
  scale_fill_manual(values=c("#e63946","#457b9d"),
                    name="How were loads\ndeteremined?")+
  scale_shape_manual(values=c(21,25),
                     name="Does the lake\nhave surface inflow(s)?")+
  xlab("PRED. zMix (m)")+
  ylab("OBS. zMix (m)")+
  theme_bw()+
  geom_smooth(method="lm", se=TRUE, color="black")+
  theme(panel.spacing = unit(0, "lines"))+
  guides(fill = guide_legend(override.aes = list(shape = 21) ),
         shape = guide_legend(override.aes = list(fill = "black") ) )+
  xlim(c(0,10))+ylim(c(0,10))


(DOCpreds+TPpreds)/(zMixpreds+gridExtra::tableGrob(Modeled_fits[5:7,1:3]))+
  plot_layout(guides = 'collect')+
  plot_annotation(tag_levels = 'A',
                  title="Predictions with default Kelly model parameters for AND default parameters for obtaining load estimates")


# Export dfs -> merge with master_df ---------------------------------------


preds_modeled<-sims %>%
  left_join(., master_df %>%
              select(lakeName, meanGPP, medianGPP, DOC_mgL, TP_ugL, dataset), by="lakeName") %>%
  left_join(., metadata %>%
              select(lakeName, inflows_YN), by="lakeName") %>%
  filter(dataset=="modelled") %>%
  select(lakeName,eqDOC:nutrient.limit.d)

preds_measured<-sims2 %>%
  select(lakeName,eqDOC:nutrient.limit.d)

preds_full<- bind_rows(preds_modeled,preds_measured)  


sims %>%
  left_join(., master_df %>%
              select(lakeName, meanGPP, medianGPP, DOC_mgL, TP_ugL, dataset), by="lakeName") %>%
  left_join(., metadata %>%
              select(lakeName, inflows_YN), by="lakeName") %>%
  filter(dataset=="modelled") %>%
  ggplot(aes(x=PPareal, y=medianGPP))+
  geom_point()


preds_full %>%
  ggplot(aes(x=PPareal, y=nutrient.limit.d, fill=light.limit.d))+
  geom_point(shape=21, size=3)

preds_full %>%
  ggplot(aes(y=PPareal, x=eqDOC, fill=light.limit.d))+
  geom_point(shape=21, size=3)

preds_full %>%
  ggplot(aes(y=PPareal, x=eqDOC, fill=nutrient.limit.d))+
  geom_point(shape=21, size=3)

