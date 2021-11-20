#Making GPP estimates from the Kelly Model from known data
master_df_NAfree<-left_join(master_df,load_estimates_huisman, by="lakeName")  %>% 
  select(lakeName, DOC_load_kgday_mean, meanGPP, TP_load_kgday_mean,
         SA_ha, HRT_days, DOC_mgL, TP_ugL, zMax, TP_mgm3, DOC_gm3) %>%
  drop_na() %>%
  mutate(CP=DOC_load_kgday_mean/TP_load_kgday_mean,
         SA=SA_ha/100,
         TP_mgL=TP_ugL/1000) %>%
  rename(DOC=DOC_mgL,
         TP=TP_mgL,
         Pin=TP_mgm3,
         DOCin=DOC_gm3)
# master_df_NAfree<-master_df_NAfree[12,]


# install.packages("deSolve")
library(deSolve)
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
    
    V=SA*1e6*zmax
    Qin=V/365
    
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

SAs=master_df_NAfree$SA_ha #km
DOCs<-master_df_NAfree$DOC #mg/L 
# CPs<-master_df_NAfree$CP
CPs<-master_df_NAfree$DOCin/(master_df_NAfree$Pin*1000)
zMax<-master_df_NAfree$zMax
Pin<-master_df_NAfree$Pin
DOCin<-master_df_NAfree$DOCin
P<-master_df_NAfree$TP
zmax<-master_df_NAfree$zMax
# 
parms=c(SA=SAs[12],
        zmax=zMax[12],
        kDOC=0.42,
        kA=0.00022,
        lA=0.1,
        pA=1.2,
        hA=55,
        mA=2,
        decay=0.001,
        Pin=Pin[12],
        DOCin=DOCin[12],
        cA=0.015,
        v=0.05,rec=0.99)
# 
# # starting state variables
n<-c(A=100,P=P[12],DOC=DOCs[12])
n
times=1:10000
# 
run=ode(y=n,times=times,func=huisman,parms=parms)
storeA<-run[nrow(run),2]
storeP<-run[nrow(run),3]
storeDOC<-run[nrow(run),4]
storekD=parms[4]*storeA+parms[3]*storeDOC-0.05
storezmix<-10^(-0.515*log10(storeDOC)+0.115*log10(2*sqrt(SAs[2]/pi))+0.991)
storezmix[storezmix>zmax[2]]=zmax
storeTP=storeA*parms[13]+storeP
# storePP=storekD*0
# light.limit.d<-storekD*0
# nutrient.limit.d<-storekD*0


# storer<-storekD*0
cP=storeP
kD=storekD
zmix=storezmix
Izmix=lightAtten(z=zmix,I0=I0,kD=kD)
pA=parms[6]
hA=parms[7]
mA=parms[8]

storer<-(pA/(kD*zmix))*log((hA+I0)/(hA+Izmix))*(cP/(cP+mA))
storePP<-storeA*(pA/(kD*zmix))*log((hA+I0)/(hA+Izmix))*(cP/(cP+mA))	
light.limit.d=log((hA+I0)/(hA+Izmix))
nutrient.limit.d<-(cP/(cP+mA))
store_arealPP=storePP*storezmix



plot(storeDOCs[,1,1],store_arealPP[,1,1],
     ylim=c(0,max(store_arealPP)),
     xlab="DOC (g C m-3)",
     ylab="Areal GPP (mg C m-2 day-1)",
     xlim=c(0,40))


