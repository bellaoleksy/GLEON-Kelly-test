#Code for grid search
#IO, SEJ, CTS 18 AUg 2021

library(deSolve)
library(foreach)
library(doParallel)

#----Helper functions----

lightAtten<-function(z,I0,kD){
  Iz=I0*exp(-kD*z)
  return(Iz)
}

# function for simulationg lake primary productivity as a function of DOC and P supply, lake surface area, and residence time
huisman<-function(t,y,params){
  with(as.list(c(y,params)),{
    
    # using Morris paper relationship, but symbols from Huisman
    kD=kA*A+kDOC*DOC	-0.05	#m-1; based on Morris paper
    
    # from a published paper -> I'll refind the paper eventually
    zmix=10^(-0.515*log10(DOC)+0.115*log10(2*sqrt(SA/pi))+0.991)
    if(zmix>zmax){zmix=zmax}
    
    V=SA*1e6*zmax
    Qin=V/365
    
    Izmix=lightAtten(z=zmix,I0=I0,kD=kD)
    
    # biomass specific growth integrated across mixed layer
    prod=(pA/(kD*zmix))*log((hA+I0)/(hA+Izmix))*(P/(P+mA))	# d-1
    
    dA.dt=A*prod-lA*A-v/zmix*A-Qin/(zmix*SA*1e6)*A	# mg C m-3
    dP.dt=Qin/(zmix*SA*1e6)*(Pin-P)+cA*lA*A*rec-cA*A*prod		# mg P m-3 (in epi)
    dDOC.dt=(Qin/(zmix*SA*1e6))*(DOCin-DOC)-decay*DOC				# g C m-3 (in epi)
    
    return(list(c(dA.dt,dP.dt,dDOC.dt)))
  })
}

#Read in data on lakes
d <- read.csv('data/gridSearchInput.csv')

lakes <- d$lakeName

#<<Don't do this over a grid at first, try just doing the optimization at one parameter guess>>
#Over a grid of possible parameter values,

#Guess parameter values
#Including nuisane parameter sigma, sd of the residuals

#Parameters for which initial guess should be constant for each lake

#Parameters set constant
I0 <- 1000 #or something like that. Average incoming light at the surface



#----Function to run the Kelly model for each lake, then gather up the predicted GPPs and compare them to observed GPPs----
kellyLoss <- function(p,GPPObs,TPObs, DOCObs) {
  
  GPPHat=numeric(length(lakes))
  PHat=numeric(length(lakes))
  CHat=numeric(length(lakes))
  
  #For each lake in the data set, run the Kelly model to generate GPP predictions
  for (i in 1:length(lakes)) {
    
    #Set vector of initial conditions for state variables
    initConds <- c(A=500,P=d$TP[i],DOC=d$DOC[i])
    
    #Vector of parameter guesses
    parGuess <- c(SA=d$SA[i],zmax=d$zMax[i],kDOC=p[1],kA=p[2],lA=p[3],pA=p[4],hA=p[5],mA=p[6],decay=p[7],Qin=d$Qin[i],Pin=d$Pin[i],DOCin=d$DOCin[i],cA=p[8],V=d$V_m3[i],v=p[9],rec=p[10])
    
    curRun=ode(y=initConds,times=c(1:2000),func=huisman,parms=parGuess)  
    Astar=curRun[nrow(curRun),2]
    Pstar=curRun[nrow(curRun),3]
    Cstar=curRun[nrow(curRun),4]
      
    kD=p[2]*Astar+p[1]*Cstar-0.05	#m-1; based on Morris paper
    
    # from a published paper -> I'll refind the paper eventually
    zmix=10^(-0.515*log10(Cstar)+0.115*log10(2*sqrt(d$SA[i]/pi))+0.991)
    if(zmix>d$zMax[i]){zmix=d$zMax[i]}
    
    V=d$V_m3
    Qin=d$Qin
    
    Izmix=lightAtten(z=zmix,I0=I0,kD=kD)
    
    prod=(p[4]/(kD*zmix))*log((p[5]+I0)/(p[5]+Izmix))*(Pstar/(Pstar+p[6]))	# d-1
    
    GPPHat[i] <- prod*Astar*zmix   # areal predicted GPP
    PHat[i] <- Astar*p[8]+Pstar # predicted lake TP; algal biomass * Phosphorus to carbon quota of phytoplankton + dissolvedP
    CHat[i] <- Cstar # predicted lake DOC
  }

  #Compare observations to predictions and calculate SSE or NLL
  #Calculate residuals
  sigmaGPP=exp(p[11])
  sigmaP=exp(p[12])
  sigmaC=exp(p[13])
  
  
  resGPP <- GPPHat - GPPObs
  resP <- PHat - TPObs
  resC <- CHat - DOCObs
  nll <- -sum(dnorm(resGPP,0,sigmaGPP,log=T),
              dnorm(resP,0,sigmaP,log=T),
              dnorm(resC,0,sigmaC,log=T))
  
  return(nll)
}

#>>>>>>>>>>>>>>>>>>>>>>
#optimGuess <- c(kDOC=0.42,kA=0.00022,lA=0.1,pA=1.2,hA=55,mA=2,decay=0.001,cA=0.015,v=0.01,rec=0.95)
optimGuess <- c(0.42,0.00022,0.1,1.2,55,2,0.001,0.015,0.01,0.95,1,1,1)


lower=c(0.05, #kDOC
        0.00005, #kA
        0.01, #lA
        0.1, #pA
        20, #hA
        0.1, #mA
        0.0001, #decay
        0.005, #cA
        0.001, #v
        0.2, #rec
        -10,
        -10,
        -10)
upper=c(0.8, #kDOC
        0.005, #kA
        0.3, #lA
        2, #pA
        100, #hA
        10, #mA
        0.01, #decay
        0.025, #cA
        0.05, #v
        0.995, #rec
        100,
        100,
        100)
#Optimize to minimize the NLL
optimOut <- optim(par=optimGuess,fn=kellyLoss,lower=lower,upper=upper,GPPObs=d$medianGPP,TPObs=d$TP,DOCObs=d$DOC) #the ... are any other arguments required by KellyLoss function
# check if any maximum liklihood estimates run into the lower or upper bound


#Is this the best set of parameter estimates?
optimOutParams<-optimOut$par[1:10]
parmNames<-c("kDOC","kA","lA","pA","hA","mA","decay","cA","v","rec")
data.frame(parmNames,optimOutParams)

# run manual grid search of SSE
kellyLossSSE <- function(p,GPPObs, TPObs, DOCObs) {
  
  GPPHat=numeric(length(lakes))
  PHat=numeric(length(lakes))
  CHat=numeric(length(lakes))
  
  #For each lake in the data set, run the Kelly model to generate GPP predictions
  for (i in 1:length(lakes)) {
    
    #Set vector of initial conditions for state variables
    initConds <- c(A=500,P=d$TP[i],DOC=d$DOC[i])
    
    #Vector of parameter guesses
    parGuess <- c(SA=d$SA[i],zmax=d$zMax[i],kDOC=p[1],kA=p[2],lA=p[3],pA=p[4],hA=p[5],mA=p[6],decay=p[7],Qin=d$Qin[i],Pin=d$Pin[i],DOCin=d$DOCin[i],cA=p[8],V=d$V_m3[i],v=p[9],rec=p[10])
    
    curRun=ode(y=initConds,times=c(1:2000),func=huisman,parms=parGuess)  
    Astar=curRun[nrow(curRun),2]
    Pstar=curRun[nrow(curRun),3]
    Cstar=curRun[nrow(curRun),4]
    
    kD=p[2]*Astar+p[1]*Cstar-0.05	#m-1; based on Morris paper
    
    # from a published paper -> I'll refind the paper eventually
    zmix=10^(-0.515*log10(Cstar)+0.115*log10(2*sqrt(d$SA[i]/pi))+0.991)
    
    V=d$V_m3
    Qin=d$Qin
    
    Izmix=lightAtten(z=zmix,I0=I0,kD=kD)
    
    prod=(p[4]/(kD*zmix))*log((p[5]+I0)/(p[5]+Izmix))*(Pstar/(Pstar+p[6]))	# d-1
    
    GPPHat[i] <- prod*Astar*zmix   # areal predicted GPP
    PHat[i] <- Astar*p[8]+Pstar # predicted lake TP; algal biomass * Phosphorus to carbon quota of phytoplankton + dissolvedP
    CHat[i] <- Cstar # predicted lake DOC
  }
  
  #Compare observations to predictions and calculate SSE or NLL
  #Calculate residuals
  resGPP <- GPPHat - GPPObs
  resP <- PHat - TPObs
  resC <- CHat - DOCObs
  
  return(sum(resGPP*resGPP,
             resP*resP,
             resC*resC))
}


#### not actually doing grid search; instead doing monte carlo approach

#Number of iterations
Niters=1e4 # might need to be more like 1e8

#Lower and upper bounds of range to search for each parameter
lower=c(0.05,0.00005,0.01,0.1,20,0.1,0.0001,0.005,0.001,0.2)
upper=c(0.8,0.005,0.3,2,100,10,0.01,0.025,0.05,0.995)

#Set up matrix of parameter sets to try. In each set (row), values for each parameter are pulled randomly from a uniform distribution between lower and upper
parDF=matrix(0,Niters,10)
for(i in 1:10){
  parDF[,i]=runif(Niters,lower[i],upper[i])
}

#Set up cores for parallel processing
numCores <- detectCores()
registerDoParallel(numCores)

#Parallelized for loop over all the parameter combinations
#Each row of output has the set of parameter values used and then the resulting SSE
MCoutput <- foreach(i=1:Niters, .inorder=F, .combine=rbind, .packages=c("deSolve")) %dopar% {
  c(parDF[i,],kellyLossSSE(parDF[i,],GPPObs=d$medianGPP,TPObs=d$TP,DOCObs=d$DOC))
}
head(MCoutput)
colnames(MCoutput)=c("kDOC","kA","lA","pA","hA","mA","decay","cA","v","rec","SSE")

#Write out MCoutput
write.csv(MCoutput,'MCoutput_20200826_IAO.csv')

#Ways to look at outputs:
#-What is the lowest SSE, what set of parameters does it occur at?
#-What are the 10 or 100 lowest SSE, what set of parameters
#-Look at plots of SSE as heat map on two parameters at a time. Maybe focus in particular on the ones that Carly and Brittni identified in sensitivity analysis/

MCoutput=read.csv("MCoutput.csv",row.names=1)
colnames(MCoutput)=c("kDOC","kA","lA","pA","hA","mA","decay","cA","v","rec","SSE")



#Parameter values at lowest SSE
MCoutput[which.min(MCoutput[,11]),]

#Parameter values at lowest 100 SSE
best100 <- MCoutput[order(MCoutput[,11]),][1:100,]
head(best100)

# look for correlation amongst parameters (equifinality)
cor(best100[,1:10])
# kDOC and kA, r=-0.23
# kA and pA,  r=0.65
# cA and lA,  r=0.30
# kA and hA,  r=-0.29
# rec and cA, r=0.48
# everything else <0.2 magnitude


# 1:1 with  best parameters -->  looks like many best 100 parameters  severly underestimate  low GPP lakes; issue with SSE?
# 4th set do the best on low GPP lakes...

j=1  # parameter set to use

GPPHat=numeric(length(lakes))

#For each lake in the data set, run the Kelly model to generate GPP predictions
for (i in 1:length(lakes)) {
  
  #Set vector of initial conditions for state variables
  initConds <- c(A=500,P=d$TP[i],DOC=d$DOC[i])
  
  #Vector of parameter guesses
  p=best100[j,1:10]
  #parGuess <- unlist(c(SA=d$SA[i],zmax=d$zMax[i],kDOC=p[1],kA=p[2],lA=p[3],pA=p[4],hA=p[5],mA=p[6],decay=p[7],Qin=d$Qin[i],Pin=d$Pin[i],DOCin=d$DOCin[i],cA=p[8],V=d$V_m3[i],v=p[9],rec=p[10]))
  parGuess <- unlist(c(SA=d$SA[i],zmax=d$zMax[i],p[1],p[2],p[3],p[4],p[5],p[6],p[7],Qin=d$Qin[i],Pin=d$Pin[i],DOCin=d$DOCin[i],p[8],V=d$V_m3[i],p[9],p[10]))
  
  curRun=ode(y=initConds,times=c(1:2000),func=huisman,parms=parGuess)  
  Astar=curRun[nrow(curRun),2]
  Pstar=curRun[nrow(curRun),3]
  Cstar=curRun[nrow(curRun),4]
  
  kD=p[2]*Astar+p[1]*Cstar-0.05	#m-1; based on Morris paper
  
  # from a published paper -> I'll refind the paper eventually
  zmix=10^(-0.515*log10(Cstar)+0.115*log10(2*sqrt(d$SA[i]/pi))+0.991)
  if(zmix>d$zMax[i]){zmix=d$zMax[i]}
  
  V=d$V_m3
  Qin=d$Qin
  
  Izmix=lightAtten(z=zmix,I0=I0,kD=kD)
  
  prod=(p[4]/(kD*zmix))*log((p[5]+I0)/(p[5]+Izmix))*(Pstar/(Pstar+p[6]))	# d-1
  
  GPPHat[i] <- prod*Astar*zmix   # areal predicted GPP
}

plot(GPPHat,d$medianGPP,xlim=c(0,8000),ylim=c(0,8000),xlab="predicted",ylab="observed",main=j)
abline(a=0,b=1,lwd=2)

#Plot "likelihood" surface in two dimensions


# save.image(file = "IAO_workspace_20200826.RData")




# IAO - Generate predictions from kellyLoss function ----------------------------

#make dailyPAR data - just use some random incident light data
I0<-1000

lightAtten<-function(z,I0,kD){
  Iz=I0*exp(-kD*z)
  return(Iz)
}


# function for simulationg lake primary productivity as a function of DOC and P supply, lake surface area, and residence time
huisman<-function(t,y,params){
  with(as.list(c(y,params)),{
    
    kD=kA*A+kDOC*DOC	-0.05	#m-1; based on Morris paper
    
    zmix=10^(-0.515*log10(DOC)+0.115*log10(2*sqrt(SA/pi))+0.991)

    if(zmix>zmax){zmix=zmax}
    
    Izmix=lightAtten(z=zmix,I0=I0,kD=kD)
    
    # biomass specific growth integrated across mixed layer
    prod=(pA/(kD*zmix))*log((hA+I0)/(hA+Izmix))*(P/(P+mA))	# d-1

    dA.dt=A*prod-lA*A-v/zmix*A-Qin/(zmix*SA*1e6)*A	# mg C m-3

    dP.dt=Qin/(zmix*SA*1e6)*(Pin-P)+cA*lA*A*rec-cA*A*prod		# mg P m-3 (in epi)
   
    dDOC.dt=(Qin/(zmix*SA*1e6))*(DOCin-DOC)-decay*DOC				# g C m-3 (in epi)

    return(list(c(dA.dt,dP.dt,dDOC.dt,prod)))
  })
}

times=1:2000
storeAs=matrix(NA,length(times),nrow(d))
storePs=storeAs
storeDOCs=storeAs
storeprod=storeAs

# loop through different surface areas, DOC loads, and load stoichs
for(j in 1:nrow(d)){

  ## Parameters from Maximum Likelihood approach IAO 2020-08-27
  ## from optimOut$par object
  parms=c(SA=d$SA[j],
          zmax=d$zMax[j],
          kDOC=0.76,
          kA=0.00005, 
          lA=0.27,
          pA=0.30,
          hA=99.9,
          mA=9.9,
          decay=0.005746313,
          Pin=d$Pin[j],
          DOCin=d$DOCin[j],
          cA=0.005,
          v=0.008, 
          rec=0.99, 
          Qin=d$V_m3[j]/d$HRT_days[j],
          P=d$TP[j],
          DOC=d$DOC[j])
  
  
  # starting state variables
  n<-c(A=500,
       P=d$TP[j],
       DOC=d$DOC[j],
       prod=10)

  run=ode(y=n,times=times,func=huisman,parms=parms)
  
  storeAs[,j]<-run[,2]
  storePs[,j]<-run[,3]
  storeDOCs[,j]<-run[,4]
  storeprod[,j]<-run[,5]
  
  print(j)
}


# calculate other simulation characteristics from stored equilibrium state variables
store_kD=parms[4]*storeAs+parms[3]*storeDOCs-0.05 

store_zmix<-store_kD
for(i in 1:ncol(store_zmix)){ 
  store_zmix[,i]<-10^(-0.515*log10(storeDOCs[,i])+0.115*log10(2*sqrt((d$SA[i])/pi))+0.991)

}

#If zmix > zmax, replaces with zmax/zmean
for(i in 1:ncol(store_zmix)){ #
  zmax=d$zMean
  store_zmix[,i][store_zmix[,i]>zmax[i]]=zmax[i]
}

storeTP=storeAs*parms[13]+storePs 
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
    light.limit.d[j,i]=log((hA+I0)/(hA+Izmix))
    nutrient.limit.d[j,i]<-(cP/(cP+mA))
    
  }
}



store_arealPP=storePP*store_zmix

d$eqDOC=storeDOCs[nrow(storeDOCs),]
d$eqTP=storeTP[nrow(storeTP),]
d$PPareal=store_arealPP[nrow(store_arealPP),]
d$PPvol=storePP[nrow(storePP),]
d$zMix_mod=store_zmix[nrow(store_zmix),]
d$A=storeAs[nrow(storeAs),]
d$light.limit.d=light.limit.d[nrow(light.limit.d),]
d$nutrient.limit.d=nutrient.limit.d[nrow(nutrient.limit.d),]



# Plot fits ---------------------------------------------------------------
library(ggplot2)
library(patchwork)

#Troubleshooting IAO 2021-08-30
#Why are we getting unreasonably small values of GPPareal? Is it because of A?
d %>%
  ggplot(aes(y=A, x=eqTP))+
  geom_point(size=3, alpha=0.8)+
  xlab("PRED. TP (ug/L)")+
  ylab("PRED. algal biomass (mg C/m3)")+
  theme_bw()+
  theme(panel.spacing = unit(0, "lines"))+
  guides(fill = guide_legend(override.aes = list(shape = 21) ),
         shape = guide_legend(override.aes = list(fill = "black") ) )


#PREDICTED VS OBSERVED - MEDIAN GPP
A<-d %>%
  ggplot(aes(x=medianGPP, y=PPvol*1.25))+
  geom_point(shape=21, size=3)+
  theme_bw()+
  ylab(expression(paste('PRED. GPP (mg C m'^-2,' day'^-1,')')))+
  xlab(expression(paste('OBS. median GPP (mg C m'^-2,' day'^-1,')')))+
  geom_abline(intercept = 0, slope = 1)

#Predicted "equilibrium" DOC values are much lower than observed DOC? Why?
B<-d %>%
  ggplot(aes(x=DOC, y=eqDOC))+
  geom_point(shape=21, size=3)+
  geom_abline(intercept = 0, slope = 1)+
  ylab(expression(paste('PRED. lake DOC mg L'^-1,)))+
  xlab(expression(paste('OBS. lake DOC mg L'^-1,)))+
  theme_bw()

#Relationship between equilibrium TP and actual in-lake TP
C<-d %>%
  ggplot(aes(y=eqTP, x=TP))+
  geom_point(shape=21, size=3)+
  geom_abline(intercept = 0, slope = 1)+
  ylab(expression(paste('PRED. lake TP ug L'^-1,)))+
  xlab(expression(paste('OBS. lake TP ug L'^-1,)))+
  theme_bw()


(A+B+C)+
  plot_layout(guides = 'collect')
