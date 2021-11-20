#This script uses the Husiman (maybe now Diehl?) model to predict HPP in Solomon lakes. Try to match TP, residence time, lake size, etc.
#Patrick Kelly REVISED 10 July 2017

rm(list=ls())
setwd('~')
source('.Rprofile')

library(gridExtra)
library(grid)

#Need to source model script to run it
setwd('~/Documents/Notre Dame/TP_DOCgppModel/Post review')
source('DiehlSuggestion_6-9-17.R')

lightAtten<-function(z,I0,kD){
	Iz=I0*exp(-kD*z)
	return(Iz)
	}

#Run model with Solomon lake specific parameters
setwd('~/Documents/Notre Dame/TP_DOCgppModel/Huisman figures')
solomon<-read.csv('solomonData.csv')

setwd('~/Documents/Notre Dame/TP_DOCgppModel/Huisman figures')
load('consolidated results for analysis 2015-03-19.RData')

lightClimate<-c()
for(i in 1:nrow(solomon)){
	lightClimate[i]<-mean(as.numeric(get(paste(solomon$lake[i],'daily',sep=''))$lightClimate),na.rm=T)*1000
}

incomingLight<-c()
for(i in 1:nrow(solomon)){
	incomingLight[i]<-mean(as.numeric(get(paste(solomon$lake[i],'daily',sep=''))$dailyPAR),na.rm=T)*1000
}

solomon$volGPP<-solomon$GPP*1000
solomon$zmix<-10^(-0.515*log10(solomon$DOC)+0.115*log10(2*sqrt(solomon$area.km2/pi))+0.991)

solomon$arealGPP<-solomon$volGPP*solomon$zmix
solomon$arealGPP<-(solomon$arealGPP/32)*12

times=1:1500

solomon<-solomon[!is.na(solomon$HRT),]

SAs<-solomon$area.km2
#SAs=1
DOCs<-c(4,8,10,15,8,5.8,3,3,6,9,3.7,1.5,3,3.8,14,2.5,7.8,3,18.5,5.5,10)
# (g C m-3)*(mg P m-3)-1; rather than running DOC and P loads independently running three trajectories that differ in their load stoich
#CPs<-c(70,4,25,1.7,30,14,2.5,12,85,20,4.5,80,6,1.8,10,2.5,40,5.5,5.45,2.5,1.3)
#CPs<-c(73,5,24,6,33,70,8,60,190,75,40,300,50,4,65,3,58,10,12.5,18.5,2.3)
CPs<-c(500,12.5,450,280,8,400,70,120,150,380,70,40,70,40,180,12.5,1550,30,100,70,12)
zmax=solomon$maxDepth
HRT=solomon$HRT.days	
Qin=(SAs*1e6*zmax)/HRT


# storing equilibrium state variables across simulations
storeAs=c()
storePs=storeAs
storeDOCs=storeAs

# loop through different surface areas, DOC loads, and load stoichs
for(k in 1:length(SAs)){
#parms=c(SA=SAs[k],zmax=zmax[k],kDOC=0.22,kA=0.00015,lA=0.1,pA=1,hA=35,mA=3,decay=0.001,Qin=Qin[k],Pin=CPs[k],DOCin=DOCs[k],cA=0.0045,v=0.1,I0=600)
parms=c(SA=SAs[k],zmax=zmax[k],kDOC=0.42,kA=0.00022,lA=0.1,pA=1.2,hA=55,mA=2,decay=0.001,Qin=Qin[k],Pin=CPs[k],DOCin=DOCs[k],cA=0.015,v=0.05,rec=0.95,I0=300)


	# starting state variables
	n<-c(A=100,P=CPs[k],DOC=DOCs[k])
			
	# simulate with ode
	run=ode(y=n,times=times,func=huisman,parms=parms)
			
	# store equilibrium values
	storeAs[k]<-run[nrow(run),2]
	storePs[k]<-run[nrow(run),3]
	storeDOCs[k]<-run[nrow(run),4]

}

# calculate other simulation characteristics from stored equilibrium state variables
store_kD=parms[4]*storeAs+parms[3]*storeDOCs

store_zmix<-c()
for(k in 1:length(SAs)){
store_zmix[k]<-10^(-0.515*log10(storeDOCs[k])+0.115*log10(2*sqrt(SAs[k]/pi))+0.991)
}


storeTP=storeAs*parms[13]+storePs
storePP=store_kD*0
izmix<-c()
kds<-c()
for(i in 1:length(store_kD)){

				
	cP=storePs[i]
		
	kD=store_kD[i]
	kds[i]<-kD
	zmix=store_zmix[i]
				
	Izmix=integrate(lightAtten,0,zmix,I0=incomingLight[i],kD=kD)$value/zmix
	izmix[i]<-Izmix
		
	pA=parms[6]
	hA=parms[7]
	mA=parms[8]
		
	storePP[i]=storeAs[i]/zmix*(pA/kD)*log((hA+incomingLight[i])/(hA+Izmix))*(cP/(cP+mA))		
}


store_arealPP=storePP*store_zmix
solomon$predGPP<-(store_arealPP)
solomon$predVolGPP<-(storePP)
solomon$predTP<-storeTP
solomon$predDOC<-storeDOCs


data.frame(CPs,storeTP,solomon$TP,predGPP=round(solomon$predGPP,2),solomon$GPP,storeDOCs,solomon$DOC,SAs,solomon$maxDepth)

plot(solomon$DOC,storeDOCs,cex=0)
text(solomon$DOC, storeDOCs,labels=1:nrow(solomon))
abline(a=0,b=1)

plot(solomon$TP,storeTP,cex=0)
text(solomon$TP, storeTP,labels=1:nrow(solomon))
abline(a=0,b=1)

quartz()
ggplot(data=solomon,aes(x=predGPP,y=arealGPP,label=1:nrow(solomon)))+geom_text()+theme_bw()+geom_abline(slope=1,intercept=0)+xlab('Modeled GPP')+ylab('Observed GPP')+theme(text=element_text(size=30))+scale_x_continuous(limits=c(0,max(c(solomon$arealGPP,solomon$predGPP))))+scale_y_continuous(limits=c(0,max(c(solomon$arealGPP,solomon$predGPP))))

cor(solomon$TP,storeTP)
cor(solomon$DOC,storeDOCs)


# Function that returns Root Mean Squared Error
rmse <- function(error)
{
    sqrt(mean(error^2))
}
error<-solomon$GPP-solomon$predGPP
rmse(error)

tp<-ggplot(data=solomon,aes(x=predTP,y=TP))+geom_point(size=6)+theme_bw()+geom_abline(slope=1,intercept=0)+ylab(expression(paste('Observed TP (',mu,'g L'^-1,')')))+xlab(expression(paste('Modeled TP (',mu,'g L'^-1,')')))+theme(text=element_text(size=18))

doc<-ggplot(data=solomon,aes(x=predDOC,y=DOC))+geom_point(size=6)+theme_bw()+geom_abline(slope=1,intercept=0)+ylab(expression(paste('Observed DOC (mg L'^-1,')')))+xlab(expression(paste('Modeled DOC (mg L'^-1,')')))+theme(text=element_text(size=18))

gpp<-ggplot(data=solomon,aes(x=predGPP,y=arealGPP))+geom_point(size=6)+theme_bw()+geom_abline(slope=1,intercept=0)+ylab(expression(paste('Observed GPP (mg C m'^-2,'d'^-1,')')))+xlab(expression(paste('Modeled GPP (mg C m'^-2,'d'^-1,')')))+theme(text=element_text(size=18))+scale_x_continuous(limits=c(0,max(solomon$arealGPP)))+scale_y_continuous(limits=c(0,max(solomon$arealGPP)))
grid.arrange(doc,tp,gpp,ncol=3)

setwd('~/Documents/Notre Dame/TP_DOCgppModel/post review/figures')
jpeg('solomonPredictionsV4.jpeg',height=400,width=1000)
grid.arrange(doc,tp,gpp,ncol=3)
dev.off()

jpeg('solomonPredictions_GPP.jpeg',height=400,width=400)
gpp
dev.off()


#test if slope is different from 1
ttest <- function(reg, coefnum, val){
  co <- coef(summary(reg))
  tstat <- (co[coefnum,1]-val)/co[coefnum,2]
  2 * pt(abs(tstat), reg$df.residual, lower.tail = FALSE)
}

mod<-lm(arealGPP~predGPP,data=solomon)
ttest(mod,2,0)

summary(lm(arealGPP~predGPP,data=solomon))

#pick random DOC load concentrations and P load concentrations, and lake sizes. Will keep all RTs at 1 year. DOC concentrations can range across the range I did before, same with P, and lake size

#pick 50 lakes

lakes<-data.frame(P=c(140,12.5,115,14.8,13.5,120,13,19,125,130,16,16,14.6,13.6,115,12.7,160,13.5,1100,13.45,11.5),DOC=c(3.5,14,8,19.5,30,5.4,6,4,18,8,40,3,5,14,24,6,6,10,50,15,12),SA=c(solomon$area.km2),zmax=solomon$maxDepth,HRT=solomon$HRT.days,Qin=(solomon$area.km2*1e6*solomon$maxDepth)/HRT)

#Ps<-seq(1,100,length.out=20)
#DOCs<-seq(1,40,length.out=20)
#SAs<-c(0.01,0.1,1,10)

#lakes<-c()
#for(i in 1:length(DOCs)){
#	y<-c()
#	for(j in 1:length(SAs)){
#		x<-data.frame(P=Ps,DOC=rep(DOCs[i],length(Ps)),SA=rep(SAs[j],length(Ps)))
#		y<-rbind(y,x)
#	}
#	lakes<-rbind(lakes,y)
#}

maxdepth=10

# storing equilibrium state variables across simulations
storeAs=c()
storePs=storeAs
storeDOCs=storeAs

# loop through different surface areas, DOC loads, and load stoichs
for(k in 1:nrow(lakes)){
parms=c(SA=lakes$SA[k],zmax=lakes$zmax[k],kDOC=0.42,kA=0.00022,lA=0.1,pA=1.2,hA=55,mA=2,decay=0.001,Qin=lakes$Qin[k],Pin=lakes$P[k],DOCin=lakes$DOC[k],cA=0.0045,v=0.05,rec=0.95,I0=600)


	# starting state variables
	n<-c(A=100,P=CPs[k],DOC=DOCs[k])
			
	# simulate with ode
	run=ode(y=n,times=times,func=huisman,parms=parms)
			
	# store equilibrium values
	storeAs[k]<-run[nrow(run),2]
	storePs[k]<-run[nrow(run),3]
	storeDOCs[k]<-run[nrow(run),4]

}

# calculate other simulation characteristics from stored equilibrium state variables
store_kD=parms[4]*storeAs+parms[3]*storeDOCs

store_zmix<-c()
for(k in 1:nrow(lakes)){
store_zmix[k]<-10^(-0.515*log10(storeDOCs[k])+0.115*log10(2*sqrt(lakes$SA[k]/pi))+0.991)
}


storeTP=storeAs*parms[13]+storePs
storePP=store_kD*0
izmix<-c()
kds<-c()
for(i in 1:length(store_kD)){

				
	cP=storePs[i]
		
	kD=store_kD[i]
	kds[i]<-kD
	zmix=store_zmix[i]
				
	Izmix=integrate(lightAtten,0,zmix,I0=600,kD=kD)$value/zmix
	izmix[i]<-Izmix
		
	pA=parms[6]
	hA=parms[7]
	mA=parms[8]
		
	storePP[i]=storeAs[i]/zmix*(pA/kD)*log((hA+600)/(hA+Izmix))*(cP/(cP+mA))		
}


store_arealPP=storePP*store_zmix

setwd('~/Documents/Notre Dame/TP_DOCgppModel/Post review')
write.csv(lakes,'spatialLakeSet.csv')
d<-data.frame(doc=storeDOCs,pp=store_arealPP,tp=storeTP,SA=lakes$SA)

#plot Hanson data
setwd('~/Documents/Notre Dame/TP_DOCgppModel/post review/figures')

#ask<-read.xlsx('hansonData.xlsx',1)

ask.plot<-ggplot(data=solomon,aes(x=DOC,y=arealGPP,color=log10(DOCs/CPs),size=area.km2))+geom_point()+scale_color_gradient(expression(paste('Load DOC:P')),low='light grey',high='black',limits=c(log10(0.001),log10(1)),breaks=c(-3, -2, -1, 0), labels=c(0.001,0.01,0.1,1))+theme_bw()+theme(text=element_text(size=20))+ylab(expression(paste('GPP (mg C m'^-2,' day'^-1,')')))+xlab(expression(paste('Lake DOC (g m'^-3,')')))+scale_y_continuous(limits=c(0,17000))+annotate('text',x=15,y=15500,label='d',size=15)+scale_size(name=expression(paste('SA km'^2)),range=c(2,10),breaks=c(0.1,1,10,100,1000),labels=c('0.1','1','10','100','1000'))

mod.plot<-ggplot(data=solomon,aes(x=DOC,y=predGPP,color=log10(DOCs/CPs),size=area.km2))+geom_point()+scale_color_gradient(expression(paste('Load DOC:P')),low='light grey',high='black',limits=c(log10(0.001),log10(1)),breaks=c(-3, -2, -1, 0), labels=c(0.001,0.01,0.1,1))+theme_bw()+theme(text=element_text(size=20))+xlab(expression(paste('Lake DOC (g m'^-3,')')))+ylab(expression(paste('GPP (mg C m'^-2,'day'^-1,')')))+annotate('text',x=15,y=4500,label='c',size=15)+scale_size(name=expression(paste('SA km'^2)),range=c(2,10),breaks=c(0.1,1,10,100,1000),labels=c('0.1','1','10','100','1000')) 


jpeg('spatialPlot.jpeg',height=400,width=1000)
grid.arrange(mod.plot,ask.plot,ncol=2)
dev.off()


