#VERSION 1- April 12, 2021

#Making GPP estimates from the Kelly Model from known data
master_df_NAfree<-left_join(master_df,load_estimates_huisman, by="lakeName")  %>% 
  select(lakeName, DOC_load_kgday_mean, TP_load_kgday_mean,
         SA_ha, HRT_days, DOC_mgL, TP_ugL, zMax, TP_mgm3, DOC_gm3) %>%
  drop_na() %>%
  mutate(CP=DOC_load_kgday_mean/TP_load_kgday_mean,
         SA=SA_ha/100,
         TP_mgL=TP_ugL/1000) %>%
  rename(DOC=DOC_mgL,
         TP=TP_mgL,
         Pin=TP_mgm3,
         DOCin=DOC_gm3)
master_df_NAfree<-master_df_NAfree[1:3,]


glimpse(metadata)
n_distinct(metadata$lakeName)

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

SAs=master_df_NAfree$SA_ha
DOCs<-master_df_NAfree$DOC
# CPs<-master_df_NAfree$CP
CPs<-master_df_NAfree$DOCin/(master_df_NAfree$Pin*1000)
zMax<-master_df_NAfree$zMax
Pin<-master_df_NAfree$Pin
DOCin<-master_df_NAfree$DOCin
P<-master_df_NAfree$TP
zmax<-master_df_NAfree$zMax
# 
# storing equilibrium state variables across simulations
storeAs=array(NA,dim=c(length(DOCs),length(CPs),length(SAs)))
storePs=storeAs
storeDOCs=storeAs
times=1:10000
# loop through different surface areas, DOC loads, and load stoichs
for(k in 1:length(SAs)){
  for(j in 1:length(DOCs)){
    for(i in 1:length(CPs)){
      
      #submittd
      #parms=c(SA=SAs[k],zmax=10,kDOC=0.22,kA=0.00015,lA=0.1,pA=1,hA=35,mA=3,decay=0.001,Qin=SAs[k]*1e6*zmax/365,Pin=DOCs[j]/CPs[i],DOCin=DOCs[j],cA=0.0045,v=0.1,rec=0.9)
      # used in jager & diehl
      #parms=c(SA=SAs[k],zmax=10,kDOC=0.22,kA=0.00015,lA=0.1,pA=1,hA=100,mA=3,decay=0.001,Qin=SAs[k]*1e6*zmax/365,Pin=DOCs[j]/CPs[i],DOCin=DOCs[j],cA=0.015,v=0.1,rec=0.9)
      # tweaking to get range of observations
      parms=c(SA=SAs[k],zmax=10,kDOC=0.42,kA=0.00022,lA=0.1,pA=1.2,hA=55,mA=2,decay=0.001,Pin=DOCs[j]/CPs[i],DOCin=DOCs[j],cA=0.015,v=0.05,rec=0.99)
      
      
      # starting state variables
      n<-c(A=100,P=(DOCs[j]/CPs[i]),DOC=DOCs[j])
      
      # simulate with ode
      run=ode(y=n,times=times,func=huisman,parms=parms)
      
      # store equilibrium values
      storeAs[j,i,k]<-run[nrow(run),2]
      storePs[j,i,k]<-run[nrow(run),3]
      storeDOCs[j,i,k]<-run[nrow(run),4]
      
      print(c(k,i,j))
    }
  }
}





# calculate other simulation characteristics from stored equilibrium state variables
store_kD=parms[4]*storeAs+parms[3]*storeDOCs-0.05
store_zmix<-store_kD
for(i in 1:dim(store_zmix)[1]){
  for(j in 1:dim(store_zmix)[2]){
    for(k in 1:dim(store_zmix)[3]){
      store_zmix[i,j,k]<-10^(-0.515*log10(storeDOCs[i,j,k])+0.115*log10(2*sqrt(SAs[k]/pi))+0.991)
      
    }
  }
}
store_zmix[store_zmix>zmax]=zmax
storeTP=storeAs*parms[13]+storePs
storePP=store_kD*0
light.limit.d<-store_kD*0
nutrient.limit.d<-store_kD*0
storer<-store_kD*0
for(i in 1:dim(store_kD)[1]){
  for(j in 1:dim(store_kD)[2]){
    for(k in 1:dim(store_kD)[3]){	
      
      cP=storePs[i,j,k]
      
      kD=store_kD[i,j,k]
      
      zmix=store_zmix[i,j,k]
      
      Izmix=lightAtten(z=zmix,I0=I0,kD=kD)
      
      pA=parms[6]
      hA=parms[7]
      mA=parms[8]
      
      storer[i,j,k]<-(pA/(kD*zmix))*log((hA+I0)/(hA+Izmix))*(cP/(cP+mA))
      storePP[i,j,k]<-storeAs[i,j,k]*(pA/(kD*zmix))*log((hA+I0)/(hA+Izmix))*(cP/(cP+mA))	
      light.limit.d[i,j,k]=log((hA+I0)/(hA+Izmix))
      nutrient.limit.d[i,j,k]<-(cP/(cP+mA))
      
    }
  }
}

store_arealPP=storePP*store_zmix





plot(storeDOCs[,1,1],store_arealPP[,1,1],
     ylim=c(0,max(store_arealPP)),
     xlab="DOC (g C m-3)",
     ylab="Areal GPP (mg C m-2 day-1)",
     xlim=c(0,40))

plot(storeDOCs[,1,1],storeAs[,1,1],
     ylim=c(0,max(storeAs)),
     xlab="DOC (g C m-3)",
     ylab="Algal biomass (mg C m-3)",
     xlim=c(0,40))





### From Diehl_suggestionsFigureV2.R file
#Convert the 3D values into a dataframe
PP.data<-c()
for(i in 1:dim(storePP)[1]){
  z<-c()
  for(j in 1:dim(storePP)[2]){
    y<-c()
    for(k in 1:dim(storePP)[3]){
      x<-data.frame(DOC.load=DOCs[i],
                    DOC=storeDOCs[i,j,k],
                    CP=CPs[j],
                    Pload=DOCs[i]/CPs[j],
                    SA=SAs[k],
                    Izmix=integrate(lightAtten,0,store_zmix[i,j,k],
                                    I0=I0,
                                    kD=store_kD[i,j,k])$value/store_zmix[i,j,k],
                    zmix=store_zmix[i,j,k],
                    P=storePs[i,j,k],
                    TP=storeTP[i,j,k],
                    kD=store_kD[i,j,k],
                    volPP=storePP[i,j,k],
                    A=storeAs[i,j,k],
                    arealPP=store_arealPP[i,j,k])
      y<-rbind(y,x)
    }
    z<-rbind(z,y)
  }
  PP.data<-rbind(PP.data,z)
}
PP.data$C.load<-((PP.data$SA/365))*PP.data$DOC.load
PP.data$P.load<-((PP.data$SA/365))*PP.data$Pload

PP.data$Qin=(PP.data$SA*1e6*10)/365
PP.data$Cload<-(PP.data$Qin*PP.data$DOC.load)/(PP.data$SA*1e6*PP.data$zmix)
View(PP.data)


PP.data %>%
  ggplot(aes(x=DOC, y=arealPP))+
  geom_point()+
  facet_wrap(.~factor(SA), scales="free")

##Get lake names, join by SA
PP.data.lakeNames<-left_join(PP.data, master_df_NAfree %>%
                               select(lakeName, SA_ha), by=c("SA"="SA_ha"))


#All lakes
PP.data.lakeNames %>%
  ggplot(aes(x=DOC, y=arealPP))+
  geom_point()+
  facet_wrap(.~lakeName, scales="free")

#All lakes plus fill for... C:P? 
PP.data.lakeNames %>%
  ggplot(aes(x=DOC, y=arealPP, fill=CP))+
  geom_point(shape=21, size=3)+
  facet_wrap(.~lakeName, scales="free")+
  scale_fill_continuous_sequential(palette = "Lajolla", rev=FALSE)


#Examine just one lake to try to see what each of these curves means..
PP.data.lakeNames %>%
  filter(lakeName=="Crampton") %>%
  # filter(DOC.load==0.455100)%>%
  ggplot(aes(x=DOC, y=arealPP, fill=CP))+
  geom_point(shape=21, size=3, alpha=0.9)+
  scale_fill_continuous_sequential(palette = "Lajolla", rev=FALSE)


PP.data.lakeNames %>%
  filter(lakeName=="SkyPond") %>%
  ggplot(aes(x=DOC.load, y=C.load))+
  geom_point(shape=21, size=3)

PP.data.Sky<-PP.data.lakeNames %>%
  filter(lakeName=="SkyPond") 
PP.data.Harp<-PP.data.lakeNames %>%
  filter(lakeName=="Harp") 


######SAVE WORKSPACE
outName="2021-04-12 Kelly model Predictions"
save(list=ls(),file=paste(outName,'.RData',sep=""))
