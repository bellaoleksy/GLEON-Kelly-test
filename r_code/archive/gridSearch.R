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
    kD=kA*A+kDOC*DOC-0.05	#m-1; based on Morris paper
    
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
kellyLoss <- function(p,GPPObs) {
  
  GPPHat=numeric(length(lakes))
  
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
  }

  #Compare observations to predictions and calculate SSE or NLL
  #Calculate residuals
  sigma=exp(p[11])
  
  res <- GPPHat - GPPObs
  nll <- -sum(dnorm(res,0,sigma,log=T))
  
  return(nll)
}

#>>>>>>>>>>>>>>>>>>>>>>
#optimGuess <- c(kDOC=0.42,kA=0.00022,lA=0.1,pA=1.2,hA=55,mA=2,decay=0.001,cA=0.015,v=0.01,rec=0.95)
optimGuess <- c(0.42,0.00022,0.1,1.2,55,2,0.001,0.015,0.01,0.95,1)


# lower=c(0.05,0.00005,0.01,0.1,20,0.1,0.0001,0.005,0.001,0.2)
# upper=c(0.8,0.005,0.3,2,100,10,0.01,0.025,0.05,0.995)
#Optimize to minimize the NLL

lower=optimGuess[1:10]*0.90 #IAO What if we keep the bounds within 10% of the published values? 
upper=optimGuess[1:10]*1.10
upper[10]<-0.995 #replace upper bound of rec to by 0.995

optimOut <- optim(par=optimGuess,fn=kellyLoss,lower=lower,upper=upper,GPPObs=d$medianGPP) #the ... are any other arguments required by KellyLoss function


# check if any maximum liklihood estimates run into the lower or upper bound


# run manual grid search of SSE
kellyLossSSE <- function(p,GPPObs) {
  
  GPPHat=numeric(length(lakes))
  
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
  }
  
  #Compare observations to predictions and calculate SSE or NLL
  #Calculate residuals
  res <- GPPHat - GPPObs
  
  return(sum(res*res))
}


#### not actually doing grid search; instead doing monte carlo approach

#Number of iterations
Niters=1e4 # might need to be more like 1e8

#Lower and upper bounds of range to search for each parameter
# lower=c(0.05,0.00005,0.01,0.1,20,0.1,0.0001,0.005,0.001,0.2)
# upper=c(0.8,0.005,0.3,2,100,10,0.01,0.025,0.05,0.995)

lower=optimGuess[1:10]*0.90 #IAO What if we keep the bounds within 10% of the published values? 
upper=optimGuess[1:10]*1.10
upper[10]<-0.995 #replace upper bound of rec to by 0.995


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
start_time <- Sys.time()
MCoutput <- foreach(i=1:Niters, .inorder=F, .combine=rbind, .packages=c("deSolve")) %dopar% {
  c(parDF[i,],kellyLossSSE(parDF[i,],GPPObs=d$medianGPP))
}
end_time <- Sys.time()
end_time - start_time
head(MCoutput)

#Write out MCoutput
write.csv(MCoutput,'MCoutput_20210819_IAO.csv')
# MCoutput<-read.csv('r_code/MCoutput.csv')

#Ways to look at outputs:
#-What is the lowest SSE, what set of parameters does it occur at?
#-What are the 10 or 100 lowest SSE, what set of parameters
#-Look at plots of SSE as heat map on two parameters at a time. Maybe focus in particular on the ones that Carly and Brittni identified in sensitivity analysis/

#Parameter values at lowest SSE
MCoutput[which.min(MCoutput[,11]),]

#Parameter values at lowest 100 SSE
best100 <- MCoutput[order(MCoutput[,11]),][1:100,]
head(best100)
best100 %>%
  arrange(SSE)
#Plot "likelihood" surface in two dimensions

#Histograms of all the parameters, top 100 best iterations
hist(best100$kDOC)
hist(best100$kA)
hist(best100$lA)
hist(best100$pA)
hist(best100$hA)
hist(best100$mA)
hist(best100$decay)
hist(best100$cA)
hist(best100$v)
hist(best100$rec)

library(ggplot2)
library(patchwork)
#cA tile plot where x some arbitrary observation number
best100 %>% 
  select(cA, SSE) %>%
  rowid_to_column(var="X") %>%
  ggplot(aes(x=X,y=cA,fill=SSE))+
  geom_tile(width=1,height=0.01)+
  scale_fill_gradient(low="lightgreen",high="red")

#v tile plot where x some arbitrary observation number
best100 %>% 
  select(v, SSE) %>%
  rowid_to_column(var="X") %>%
  ggplot(aes(x=X,y=v,fill=SSE))+
  geom_tile(width=1,height=0.01)+
  scale_fill_gradient(low="lightgreen",high="red")

#rec tile plot where x some arbitrary observation number
best100 %>% 
  select(rec, SSE) %>%
  rowid_to_column(var="X") %>%
  ggplot(aes(x=X,y=rec,fill=SSE))+
  geom_tile(width=1,height=0.01)+
  scale_fill_gradient(low="lightgreen",high="red")

#Plot against each other
#Raw data
A<-best100 %>%
  ggplot(aes(x=cA,y=v,fill=SSE))+
  geom_point(shape=21,size=5,alpha=0.7)+
  geom_point(aes(x=0.015,y=0.01), fill="grey50",shape=21, size=5)+ #Reference for the parameters currently in the model
  scale_fill_gradient(low="lightgreen",high="red")+
  theme(legend.position="none")

B<-best100 %>%
  ggplot(aes(x=cA,y=rec,fill=SSE))+
  geom_point(shape=21,size=5,alpha=0.7)+
  geom_point(aes(x=0.015,y=0.95), fill="grey50",shape=21, size=5)+ #Reference for the parameters currently in the model
  scale_fill_gradient(low="lightgreen",high="red")+
  theme(legend.position="none")


C<-best100 %>%
  ggplot(aes(x=v,y=rec,fill=SSE))+
  geom_point(shape=21,size=5,alpha=0.7)+
  geom_point(aes(x=0.01,y=0.95), fill="grey50",shape=21, size=5)+ #Reference for the parameters currently in the model
  scale_fill_gradient(low="lightgreen",high="red")+
  theme(legend.position="none")

A+B+C


#Scaled data
Ascaled<-best100 %>%
  ggplot(aes(x=scale(cA),y=scale(v),fill=scale(SSE)))+
  # geom_point(shape=21,size=3)+
  geom_tile(width=0.1,height=0.1)+
  scale_fill_gradient(low="lightgreen",high="red")+
  theme(legend.position="none")

Bscaled<-best100 %>%
  ggplot(aes(x=scale(cA),y=scale(rec),fill=scale(SSE)))+
  # geom_point(shape=21,size=3)+
  geom_tile(width=0.1,height=0.1)+
  scale_fill_gradient(low="lightgreen",high="red")+
  theme(legend.position="none")

Cscaled<-best100 %>%
  ggplot(aes(x=scale(v),y=scale(rec),fill=scale(SSE)))+
  # geom_point(shape=21,size=3)+
  geom_tile(width=0.1,height=0.1)+
  scale_fill_gradient(low="lightgreen",high="red")+
  theme(legend.position="none")


