#Long Lake observations and predictions
#CTS 17 March 2021, borrowing bits from SEJ 'ConsideringAquashade_3-15-2021.r'

#####
#Setup

Sys.setenv(tz='America/Chicago')

##
#Load libraries
library(dplyr)
library(lubridate)
library(deSolve)
library(ggplot2)

##
#Load MFE db utilities
# source('C:/Users/solomonc/OneDrive/Documents/R files and functions/R custom functions/MFEh20/db/dbUtil.r')
source("/Users/solomonlab/Google Drive/Research (common)/Research/Data/R/MFE/000_dbUtil.R")

#Directory for db
# dbdir <- 'C:/Users/solomonc/OneDrive/Documents/Research projects/Microbes to Micropterus/Database/Current'
#File name for db
# db <- 'MFEdb_20210305.db'
dbdir=file.path("/Users/solomonlab/Google Drive/Research (common)/Research/Data/R/MFE")
# db="MFEdb_20200218.db"
db="MFEdb_20210112.db"

##
#Load data

#Directory with data files from Stuart
setwd('C:/Users/solomonc/OneDrive/Documents/Research projects/Microbes to Micropterus/2021 field season/aquashade2021')

#Curtain scenarios from Stuart's GIS work
gis=read.csv("LongLakeCurtainBathysFromGIS_3-3-2018.csv",header=TRUE,stringsAsFactors=FALSE)

#Average daily water budget for each lake-year (I think: start from daily values, average across days)
avgH2O=read.csv("avg_dailyH2Obudgets.csv",header=TRUE,stringsAsFactors=FALSE)
avgH2O=avgH2O[avgH2O$lakeID%in%c("WL","EL","FE"),]

#Average daily C and P budget
avgC=read.csv("avg_dailyCbudgets.csv",header=TRUE,stringsAsFactors=FALSE)
avgC=avgC[avgC$lakeID%in%c("WL","EL","FE"),]

#Observed pelagic and benthic GPP
obsGPP=read.csv("longGPPAll.csv",header=TRUE,row.names=1,colClasses=c(solarDay='Date'))
obsGPP$areal_mgCm2=obsGPP$GPP/32*12*1000
obsBPP=read.csv("longBPPAll.csv",header=TRUE,row.names=1)

#Water chem data
wc<-dbTable('WATER_CHEM',lakeID=c("WL","EL","FE"))
#Get DOC measurements from PML
pmlDOC=wc[wc$parameter=="DOC",]
pmlDOC=pmlDOC[pmlDOC$flag==0,]
pmlDOC=pmlDOC[grepl("PML",pmlDOC$sampleID),]
pmlDOC$year=substr(pmlDOC$dateSample,1,4)
#Get TP measurements from PML
pmlTP=wc[wc$parameter=="TP",]
pmlTP=pmlTP[pmlTP$flag==0,]
pmlTP=pmlTP[grepl("PML",pmlTP$sampleID),]
pmlTP$year=substr(pmlTP$dateSample,1,4)

##
#Load functions

#Directory for functions; also working directory
setwd('C:/Users/solomonc/OneDrive/Documents/Research projects/Microbes to Micropterus/2021 field season/Long obs and pred')
source('calcVol.r')
source('lightAtten.r')
source('hybrid.r')



#####
#Set up for model simulations

#Data.frame to hold simulation inputs and outputs for each lake-year
sims=data.frame(year=c(2011,2012,rep(2013:2020,each=2)),
                lakeID=c("LL","LL",rep(c("WL","EL"),6),rep(c("WL","FE"),2)),
                SA_m2=NA,
                V_m3=NA)

#Define surface area for each lake-year
WLsa=4.9*1e4 # m2
ELsa=gis[1,4] # m2
FEsa=gis[33,4] # m2
sims[sims[,2]=="LL",3]=WLsa+ELsa
sims[sims[,2]=="WL",3]=WLsa
sims[sims[,2]=="EL",3]=ELsa
sims[sims[,2]=="FE",3]=FEsa

#Define volume for each lake-year
#First calculate East Long volumes for scenarios
scenarios=unique(gis$Scenario)
volumes=numeric(length(scenarios))
for(i in 1:length(scenarios)){
  volumes[i]=calcVol(gis[gis$Scenario==scenarios[i],c(2,4)])
}
names(volumes)=scenarios
#Now define volume for each lake-year
Vs=c(LL=3.8*WLsa+volumes[1],WL=3.8*WLsa,EL=volumes[1],FE=volumes[5]) # m3
names(Vs)=c("LL","WL","EL","FE")
sims[sims[,2]=="LL",4]=Vs[1]
sims[sims[,2]=="WL",4]=Vs[2]
sims[sims[,2]=="EL",4]=Vs[3]
sims[sims[,2]=="FE",4]=Vs[4]

#Define Qin for each lake-year (m3 d-1) and merge into sims
#Sum inputs for each lake-year
avgH2O <- mutate(avgH2O,Qin_m3day=precip_m3+streamIn_m3+surfaceFlow_m3)
sims <- merge(sims,avgH2O[,c('year','lakeID','Qin_m3day')],by=c('year','lakeID'),all.x=T)

#Define Cin for each lake-year (g C m-3) and merge into sims
#Calculate average daily total mass of DOC input for each lake-year
avgC <- mutate(avgC,DOCin=docSTin+docSFin+docPrecip+docWetland)
#Merge in Qin and divide DOC input mass by Qin to get average daily concentration of DOC input, Cin
avgC <- merge(avgC,avgH2O[,c("lakeID","year","Qin_m3day")],by=c('year','lakeID'),all.x=T)
avgC <- mutate(avgC,Cin_gCm3=DOCin/Qin_m3day)
sims <- merge(sims,avgC[,c('year','lakeID','Cin_gCm3')],by=c('year','lakeID'),all.x=T)

#Define Pin for each lake-year (g P m-3) and merge into sims
#Calculate average daily total mass of TP input for each lake-year
avgC <- mutate(avgC,TPin=tpSTin+tpPrecip)
#Divide TP input mass by Qin to get average daily concentration of TP input, Pin
avgC <- mutate(avgC,Pin_gPm3=TPin/Qin_m3day)
sims <- merge(sims,avgC[,c('year','lakeID','Pin_gPm3')],by=c('year','lakeID'),all.x=T)

#Define average depth for each lake-year
sims$zbar <- sims$V_m3/sims$SA_m2

#Show sims
sims

#Drop rows of sims with NA inputs for now
sims <- sims[complete.cases(sims),]

#####
#Run model simulations

#Notes from Stuart:
# pretty sure we're assuming that each depth has the same area with the current benthic-pelagic model
# not sure this is exactly accurate for Long, but using it for now...

# with Kelly model parameterization (pars0 below) 2011-2012 LL DOC is >14 gC m-3
# I adjusted decay to align that with observed DOC = 8 gC m-3

times=1:2000

zmax=6.2 # trying this for all lakes because easier to store output; could simulate specific zbars...
zs=seq(0.1,zmax,0.1)

storeDOCs=matrix(NA,length(times),nrow(sims))
storeAs=storeDOCs
storeRpels=storeAs
storeBs=array(NA,dim=c(length(times),nrow(sims),length(zs)))
storeRbents=storeBs

for(j in 1:nrow(sims)){
  
  #pars0=c(SA=sims$SA_m2[j]/1e 6,cA=0.008,cB=0.015,Dbent=0.05,Dpel=0.05,fB=0.3,hA=80,hB=40,I0=300,kA=0.0003,kB=0.0005,kDOC=0.22,lA=0.1,lB=0.1,mA=3,mB=5,pA=1,pB=1,Rsed=30,v=0.08,zbent=0.01,zmax=zmax,rec=0.9,decay=0.001,Qin=sims$Qin_m3day[j],Rin=sims$Pin_gPm3[j]*1000,DOCin=sims$Cin_gCm3[j])
  pars=c(SA=sims$SA_m2[j]/1e6,cA=0.008,cB=0.015,Dbent=0.05,Dpel=0.05,fB=0.3,hA=80,hB=40,I0=300,kA=0.0003,kB=0.0005,kDOC=0.22,lA=0.1,lB=0.1,mA=3,mB=5,pA=1,pB=1,Rsed=30,v=0.08,zbent=0.01,zmax=zmax,rec=0.9,decay=0.007,Qin=sims$Qin_m3day[j],Rin=sims$Pin_gPm3[j]*1000,DOCin=sims$Cin_gCm3[j],V=sims$V_m3[j])
  
  N0=c(10,1,1,rep(1,length(zs)),rep(1,length(zs)))
  
  run=ode(y=N0,times=times,func=hybrid,parms=pars)
  
  # store equilibrium values
  storeDOCs[,j]<-run[,2]
  storeAs[,j]<-run[,3]
  storeRpels[,j]<-run[,4]
  storeBs[,j,]<-run[nrow(run),(5:(4+length(zs)))]
  storeRbents[,j,]<-run[nrow(run),((5+length(zs)):ncol(run))]
  
  print(j)
}


# calculate other simulation characteristics from stored equilibrium state variables
store_kD=pars[10]*storeAs+pars[12]*storeDOCs
store_zmix<-store_kD
for(i in 1:ncol(store_zmix)){
  store_zmix[,i]<-10^(-0.515*log10(storeDOCs[,i])+0.115*log10(2*sqrt((sims$SA_m2[i]/1e6)/pi))+0.991)
}
store_zmix[store_zmix>zmax]=zmax

storeTP=storeAs*pars[2]+storeRpels

storePP=store_kD*0
storeBP=storeBs*0

storeRlimA=storePP
storeIlimA=storePP
for(i in 1:ncol(store_kD)){
  for(j in 1:nrow(store_kD)){
    I0=pars[9]
    pA=pars[17]
    hA=pars[7]
    mA=pars[15]
    pB=pars[18]
    hB=pars[8]
    mB=pars[16]
    kB=pars[11]
    kDOC=pars[12]
    zbent=pars[21]
    
    A=storeAs[j,i]
    Bs=storeBs[j,i,]
    DOC=storeDOCs[j,i]
    Rpel=storeRpels[j,i]
    Rbents=storeRbents[j,i,]
    
    kD=store_kD[j,i]
    
    zmix=store_zmix[j,i]
    
    Izmix=lightAtten(z=zmix,I0=I0,kD=kD)
    Izbents=lightAtten(z=zs,I0=I0,kD=kD)
    Izseds=lightAtten(z=zbent,I0=Izbents,kD=kB*Bs+kDOC*DOC)
    
    # biomass specific growth integrated across mixed layer
    PAt=(pA/(kD*zmix))*log((hA+I0)/(hA+Izmix))*(Rpel/(Rpel+mA))	# d-1
    PBts=(pB/((kB*Bs+kDOC*DOC)*zbent))*log((hB+Izbents)/(hB+Izseds))*(Rbents/(Rbents+mB)) # d-1
    
    storePP[j,i]=A*PAt
    storeBP[j,i,]=Bs*PBts	
    
    storeRlimA[j,i]=(Rpel/(Rpel+mA))
    storeIlimA[j,i]=(log((hA+I0)/(hA+Izmix)))/kD
  }
}

store_arealPP=storePP*store_zmix

total_arealPP=store_arealPP*0
for(i in 1:(dim(storeBP)[1])){
  for(j in 1:(dim(storeBP)[2])){
    total_arealPP[i,j]=store_arealPP[i,j]+sum(storeBP[i,j,]*zbent)/length(storeBP[i,j,])
  }
}

fracPelagic=store_arealPP/total_arealPP


sims$eqDOC=storeDOCs[nrow(storeDOCs),]
sims$eqTP=storeTP[nrow(storeTP),]
sims$PPpel=store_arealPP[nrow(store_arealPP),]
sims$BPP=total_arealPP[nrow(total_arealPP),]-store_arealPP[nrow(store_arealPP),]


#####
#Plot model predictions and observations

#Calculate mean DOC, mean TP for each lake-year

#DOC
#Any NAs? Any flags? All data are TP?
any(is.na(pmlDOC$parameterValue))
unique(pmlDOC$flag)
unique(pmlDOC$parameter)
#Calculate mean (And convert to mg/L)
meanDOC <- pmlDOC %>%
  group_by(lakeID,year) %>%
  summarize(meanDOC=mean(parameterValue)/1000)

#TP
#Any NAs? Any flags? All data are TP?
any(is.na(pmlTP$parameterValue))
unique(pmlTP$flag)
unique(pmlTP$parameter)
#Calculate mean
meanTP <- pmlTP %>%
  group_by(lakeID,year) %>%
  summarize(meanTP=mean(parameterValue))

#Plot mean observed DOC
par(mfrow=c(2,1),mar=c(3.1,4.1,1.1,1.1))
plot(meanDOC~year,data=meanDOC,subset=lakeID=='WL',xlab="",ylab="DOC (mg L-1)",xlim=c(2011,2020),ylim=range(c(meanDOC,sims$eqDOC)),type='b',pch=16,col='black',lwd=2)
points(meanDOC~year,data=meanDOC,subset=lakeID%in%c('EL','FE'),type='b',pch=16,col='red',lwd=2)
#Plot model-predicted DOC
points(eqDOC~year,data=sims,subset=lakeID%in%c('LL','WL'),type='b',pch=1,col='black')
points(eqDOC~year,data=sims,subset=lakeID%in%c('LL','EL','FE'),type='b',pch=1,col='red')
#Legend
legend('topleft',legend=c('WL obs','WL pred','EL/FE obs','EL/FE pred'),pch=c(16,1,16,1),lwd=c(2,1,2,1),col=c('black','black','red','red'),bty='n')
#Plot mean observed TP
plot(meanTP~year,data=meanTP,subset=lakeID=='WL',xlab="",ylab="TP (ug L-1)",xlim=c(2011,2020),ylim=range(c(meanTP,sims$eqTP)),type='b',pch=16,col='black',lwd=2)
points(meanTP~year,data=meanTP,subset=lakeID%in%c('EL','FE'),type='b',pch=16,col='red',lwd=2)
#Plot model-predicted TP
points(eqTP~year,data=sims,subset=lakeID%in%c('LL','WL'),type='b',pch=1,col='black')
points(eqTP~year,data=sims,subset=lakeID%in%c('LL','EL','FE'),type='b',pch=1,col='red')

#Plot observed and predicted pelagic GPP
#Mean observed GPP by lake, year
meanGPP <- obsGPP %>%
  group_by(year,lakeID) %>%
  summarize(meanGPP = mean(areal_mgCm2,na.rm=T),
            nGPP=n())
meanGPP <- as.data.frame(meanGPP)
meanGPP$lakeID <- factor(meanGPP$lakeID,levels=c("WL","EL","FE"))
#Plot observed
par(mfrow=c(2,1))
plot(meanGPP~year,data=meanGPP,subset=lakeID=='WL',ylim=range(meanGPP),ylab="obs GPP (mg C m-2 d-1)",type='b',pch=16,col='black',lwd=2)
points(meanGPP~year,data=meanGPP,subset=lakeID!='WL',type='b',pch=16,col='red',lwd=2)
legend('topleft',legend=c('WL','EL/FE'),pch=16,col=c('black','red'),lwd=2,bty='n')
#Plot predicted
plot(PPpel~year,data=sims,subset=lakeID=='WL',xlim=c(2011,2020),ylim=range(PPpel),ylab="pred GPP (mg C m-2 d-1)",type='b',col='black')
points(PPpel~year,data=sims,subset=lakeID=='EL',type='b',col='red')

#Plot observed benthic GPP
#Mean observed GPP by lake, year
meanBenthicGPP <- obsBPP %>%
  group_by(year,lakeID) %>%
  summarize(meanGPP = mean(GPP,na.rm=T),
            nGPP=n())
meanBenthicGPP <- as.data.frame(meanBenthicGPP)
#Quick plot
plot(meanGPP~year,data=meanBenthicGPP,subset=lakeID=='WL',xlim=c(2011,2020),ylim=range(meanGPP),ylab="Mean benthic GPP",type='b',pch=16,col='black',lwd=2)
points(meanGPP~year,data=meanBenthicGPP,subset=lakeID=='FE',type='b',pch=16,col='red',lwd=2)
legend('topleft',legend=c('WL','EL/FE'),pch=16,col=c('black','red'),lwd=2,bty='n')


#Plot time series of pelagic GPP for each lake-year
#First add a DOY column to obsGPP to facilitate plotting
obsGPP$doy <- yday(obsGPP$solarDay)
#Add a new lakeID column, collapsing EL and FE to just EL; convert to factor, levels are WL, EL
obsGPP$lakeID2 <- obsGPP$lakeID
obsGPP$lakeID2[obsGPP$lakeID2=='FE'] <- 'EL'
obsGPP$lakeID2 <- factor(obsGPP$lakeID2,levels=c('WL','EL'))
#Plot the observations
ggplot(data=obsGPP,aes(x=doy,y=areal_mgCm2,col=lakeID2))+
  scale_color_manual(values=c('black','red')) +
  geom_point() +
  facet_grid(cols=vars(year)) +
  labs(x="Day of year",y="Gross primary production (mg C m-2 d-1)",color="lakeID")
#Now plot loess-smoothed line with 95% CI
ggplot(data=obsGPP,aes(x=doy,y=areal_mgCm2,col=lakeID2))+
  scale_color_manual(values=c('black','red')) +
  geom_smooth() +
  facet_grid(cols=vars(year)) +
  labs(x="Day of year",y="Gross primary production (mg C m-2 d-1)",color="lakeID")
#Same but try a smaller 'span' on the loess smoother - so smoothed line goes through the data more, with less smoothing
ggplot(data=obsGPP,aes(x=doy,y=areal_mgCm2,col=lakeID2))+
  scale_color_manual(values=c('black','red')) +
  geom_smooth(span=0.5) +
  facet_grid(cols=vars(year)) +
  labs(x="Day of year",y="Gross primary production (mg C m-2 d-1)",color="lakeID")

##
#Try Welch's t-test on GPP data
#Hypotheses:  
#  1) Paired difference becomes larger after first manipulation: paired difference of (2011-2012) < paired difference of (2013-2018) 
#  2) Paired difference becomes smaller after second manipulation: paired difference of (2013-2018) > paired difference of (2019-2020)
#Calculate paired differences, (treatment-reference)
#First reshape data - column for date, column for treatment GPP, column for reference GPP
treatment <- obsGPP[obsGPP$lakeID%in%c('EL','FE'),c('solarDay','areal_mgCm2')]
reference <- obsGPP[obsGPP$lakeID==c('WL'),c('solarDay','areal_mgCm2')]
pairedDif <- merge(treatment,reference,by='solarDay',all=T,suffixes=c('treatment','reference'))
pairedDif$pairedDif <- pairedDif$areal_mgCm2treatment-pairedDif$areal_mgCm2reference
pairedDif$year <- year(pairedDif$solarDay)
#Do t-test for H1. One-sided test 
t.test(pairedDif$pairedDif[pairedDif$year<=2012],pairedDif$pairedDif[pairedDif$year>=2013&pairedDif$year<=2018],alternative='less',var.equal=F)
#Do t-test for H2. One-sided test
t.test(pairedDif$pairedDif[pairedDif$year>=2013&pairedDif$year<=2018],pairedDif$pairedDif[pairedDif$year>=2019],alternative='greater',var.equal=F)


##
#Looking at benthic primary production data in a little more detail
#Note that the structure of these data is a little funny. Deployments are not always on the same day for the two lakes (particularly
#in early years, perhaps); these seem to be the whole-lake numbers rather than the raw numbers; it's not clear what the nObs colum
#indicates (presumably replicate domes or something).
str(obsBPP)
unique(obsBPP$lakeID)
#Convert lakeID to factor with levels (WL, EL)
obsBPP$lakeID[obsBPP$lakeID=='FE'] <- 'EL'
obsBPP$lakeID <- factor(obsBPP$lakeID,levels=c('WL','EL'))
#Convert datetime to Date
obsBPP$datetime <- as.Date(obsBPP$datetime)
#Add a column for DOY
obsBPP$doy <- yday(obsBPP$datetime)
#Plot the observations
ggplot(data=obsBPP,aes(x=doy,y=GPP,col=lakeID))+
  scale_color_manual(values=c('black','red')) +
  geom_point() +
  facet_grid(cols=vars(year)) +
  labs(x="Day of year",y="Benthic gross primary production (mg C m-2 d-1)",color="lakeID")

plot(GPP~datetime,data=obsBPP,subset=lakeID=='WL',col='black',type='b')

