#### processing Monte Carlo parameter  "grid search" results
#### on 8/23/2021 SEJ ran 100,000 monte carlo simulations of the Kelly model for 16 GLEON lakes where load data was available
#### parameters were drawn from uniform distributions as in gridSearch.R
#### MCoutput.csv contains 100,000 rows and 26 columns - the first 10 rows are the parameters as in gridSearch.R; 
#### the last 16 columns are equilibrium areal NPP for the 16 GLEON lakes in the order of the "lakes" vector in gridSearch.R
library(tidyverse)
d=read.csv("data/gridSearchInput.csv")

lakes=d$lakeName

mcout=read.csv("data/MCoutput.csv")

# forcing any areal GPP less than 0.1 to zero
# this maybe keeps this quantity from being biased by very low GPP estimates because
# if the predicted GPP is 1e-80, this will contribute a lot to SSE of logged GPP...
for(i in 11:26){
  mcout[mcout[,i]<0.1,i]=0
}

# originally tried  SSE, but  this biased fits to high GPP lakes; trying SSE of  log  GPP
#  also correcting  model output to be GPP, rather than NPP; based  on some lit.  review  Chris and Stuart  decided autotrophic  respiration is ~25% of GPP

SSElogCalc<-function(x){
  err=log(x*1.25+0.1)-log(d$medianGPP)
  return(sum(err*err))
}

mcout$SSElog=apply(mcout[,11:26],1,SSElogCalc)

hist(mcout$SSElog)

best100=mcout[order(mcout$SSElog,decreasing=FALSE),][1:100,]

head(best100)


# 1:1 with  best parameters 

j=1  # parameter set to use

GPPHat=numeric(length(lakes))
PHat=numeric(length(lakes))
CHat=numeric(length(lakes))


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
  PHat[i] <- Astar*p[8]+Pstar # predicted lake TP; algal biomass * Phosphorus to carbon quota of phytoplankton + dissolvedP
  CHat[i] <- Cstar # predicted lake DOC
}

plot(GPPHat,d$medianGPP,xlim=c(0,8000),ylim=c(0,8000),xlab="predicted",ylab="observed",main="GPP")
abline(a=0,b=1,lwd=2)

plot(PHat,d$TP,xlim=c(0,100),ylim=c(0,100),xlab="predicted",ylab="observed",main="TP")
abline(a=0,b=1,lwd=2)


plot(CHat,d$DOC,xlim=c(0,40),ylim=c(0,40),xlab="predicted",ylab="observed",main="DOC")
abline(a=0,b=1,lwd=2)

kellyParams<-data.frame(optimGuess) 
kellyParams<-kellyParams %>%
  rownames_to_column() %>%
  mutate(rowname=as.numeric(rowname))%>%
  filter(rowname <= 10) %>%
  rename(kellyParam=optimGuess)
secondBest<-best100[2,1:10]%>%
  rownames_to_column() %>%
  pivot_longer(-1) %>%
  select(-rowname)%>%
  rename(parameter=name,
         secondBestCombo=value)%>%
  rownames_to_column() %>%
  mutate(rowname=as.numeric(rowname))
  
comparison<-p %>%
  rownames_to_column() %>%
  pivot_longer(-1) %>%
  select(-rowname)%>%
  rename(parameter=name,
         bestCombo=value)%>%
  rownames_to_column() %>%
  mutate(rowname=as.numeric(rowname))%>%
  left_join(.,kellyParams, by="rowname") %>%
  left_join(.,secondBest, by=c("rowname","parameter"))%>%
  select(-rowname) %>%
  relocate(secondBestCombo, .before = kellyParam) #rearranging column names

comparison
