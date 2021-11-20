### power analysis for lake size effect on critical productivity relationship
### ecosystems proposal 2016
### 7-9-16
### SEJ

# assume linear relationship between tDOC load and critical tDOC level
b=20
m=-(10/6)		# presumably smaller this is the harder it is to detect differences

plot(0:8,b+m*(0:8),type='l',xlab="log10 lake area",lwd=2)

### deriving maximum and critical value from quadratic coefficients a, b, and c
# a must be negative to have a maximum
# maxProd=c-b^2/(4*a)
# crit=-b/(2*a)

maxProd=80		#mmol O2 m-3 d-1

LAs=10^c(4,5,6,7)
crits=b+m*log10(LAs)
C=10

as=(C-maxProd)/crits^2
bs=-2*as*crits
cs=as*crits^2+maxProd

tDOCpred=seq(0,30,0.1)

plot(tDOCpred,as[1]*tDOCpred^2+bs[1]*tDOCpred+cs[1],type='l',xlim=c(0,30),ylim=c(0,maxProd),col=1)
for(i in 2:length(crits)){
  lines(tDOCpred,as[i]*tDOCpred^2+bs[i]*tDOCpred+cs[i],col=i)
}


# assuming C.V. of GPP is ~25%
# pick lakes within 25% of target size

### how many lakes to get significant quadratic fit
# function to get p-value from lm object
lmp<-function(m){
  f=summary(m)$fstatistic
  p=pf(f[1],f[2],f[3],lower.tail=F)
  return(p)
}

# function to generate DOC-GPP survey data with noise and report performance vs. non-noise model fit
fitGPPvDOC<-function(N,LA,b=20,m=-10/6,C=10,maxProd=80){
  doc=runif(N,min=seq(0,(20-20/N),length.out=N),max=seq((20/N),20,length.out=N))
  doc2=doc*doc
  LAs=runif(N,LA*0.75,LA*1.25)
  
  crits=b+m*log10(LAs)
  as=(C-maxProd)/crits^2
  bs=-2*as*crits
  cs=as*crits^2+maxProd

  meanGPPs=as*doc^2+bs*doc+cs
  GPPs=rnorm(N,meanGPPs,0.25*meanGPPs)
  
  meanFit=lm(meanGPPs~doc2+doc)
  mfc=coef(meanFit)
  meanV=-mfc[3]/(2*mfc[2])
  meanMax=mfc[2]*meanV^2+mfc[3]*meanV+mfc[1]
  
  fit=lm(GPPs~doc2+doc)
  fc=coef(fit)
  fitV=-fc[3]/(2*fc[2])
  fitMax=fc[2]*fitV^2+fc[3]*fitV+fc[1]
  
  return(c(meanV-fitV,meanMax-fitMax,lmp(fit)))
}

Ns=seq(4,20)
reps=100
  
#Set up a bunch of blank matrices, I think - IAO
Vdiff=matrix(NA,length(Ns),reps)
MAXdiff=Vdiff
pvalue=Vdiff

#fill with predicted values
for(i in 1:length(Ns)){
  for(j in 1:reps){
    print(c(i,j))
    cur=fitGPPvDOC(N=Ns[i],LA=1e6)
    Vdiff[i,j]=cur[1]
    MAXdiff[i,j]=cur[2]
    pvalue[i,j]=cur[3]	
  }
}

meanVdiff=rowMeans(abs(Vdiff))
sdVdiff=apply(Vdiff,1,sd)
meanMAXdiff=rowMeans(abs(MAXdiff))
sdMAXdiff=apply(MAXdiff,1,sd)
meanP=rowMeans(pvalue)
sdP=apply(pvalue,1,sd)

dev.new()
par(mfrow=c(2,2))
plot(Ns,meanVdiff,xlab="# of lakes sampled",ylab="error in critical tDOC",ylim=c(min(meanVdiff-sdVdiff),max(meanVdiff+sdVdiff)))
arrows(Ns,meanVdiff-sdVdiff,Ns,meanVdiff+sdVdiff,code=0)
abline(h=0,lty=2)
plot(Ns,meanMAXdiff,xlab="# of lakes sampled",ylab="error in max GPP",ylim=c(min(meanMAXdiff-sdMAXdiff),max(meanMAXdiff+sdMAXdiff)))
arrows(Ns,meanMAXdiff-sdMAXdiff,Ns,meanMAXdiff+sdMAXdiff,code=0)
abline(h=0,lty=2)
plot(Ns,meanP,xlab="# of lakes sampled",ylab="pvalue of quadratic fit",ylim=c(min(meanP-sdP),max(meanP+sdP)))
arrows(Ns,meanP-sdP,Ns,meanP+sdP,code=0)
abline(h=0.05,lty=2)


### how many lakes to detect size effect on critical DOC level when using lake size classes
LAclasses<-function(N,meanLAs=10^(4:7),maxProd=80,b=20,m=-10/6,C=10){
  LAs=runif((N*length(meanLAs)),rep(meanLAs*0.75,each=N),rep(meanLAs*1.25,each=N))
  doc=runif((N*length(meanLAs)),min=rep(seq(0,(20-20/N),length.out=N),length(meanLAs)),max=rep(seq((20/N),20,length.out=N),length(meanLAs)))
  doc2=doc*doc
  
  crits=b+m*log10(LAs)
  as=(C-maxProd)/crits^2
  bs=-2*as*crits
  cs=as*crits^2+maxProd
  
  meanGPPs=as*doc^2+bs*doc+cs
  GPPs=rnorm((N*length(meanLAs)),meanGPPs,0.25*meanGPPs)
  
  #plot(doc,GPPs,col=rep(1:length(meanLAs),each=N))
  fit=lm(GPPs~doc2+doc2:LAs+doc+doc:LAs)
  
  return(summary(fit)$coefficients[4:5,4])
}

Ns=4:20
reps=100

p2=matrix(NA,length(Ns),reps)
p1=p2

for(i in 1:length(Ns)){
  for(j in 1:reps){
    print(c(i,j))
    cur=LAclasses(Ns[i])
    p2[i,j]=cur[1]
    p1[i,j]=cur[2]	
  }
}

meanP2=rowMeans(p2)
sdP2=apply(p2,1,sd)
meanP1=rowMeans(p1)
sdP1=apply(p1,1,sd)

dev.new()
par(mfrow=c(2,1))
plot(Ns,meanP2,xlab="# of lakes sampled per size class",ylab="p-value of LA:doc^2",ylim=c(min(meanP2-sdP2),max(meanP2+sdP2)))
arrows(Ns,meanP2-sdP2,Ns,meanP2+sdP2,code=0)
abline(h=0.05,lty=2)
plot(Ns,meanP1,xlab="# of lakes sampled per size class",ylab="p-value of LA:doc",ylim=c(min(meanP1-sdP1),max(meanP1+sdP1)))
arrows(Ns,meanP1-sdP1,Ns,meanP1+sdP1,code=0)
abline(h=0.05,lty=2)


### how many lakes to detect size effect on critical DOC level when using lake size classes
LAunif<-function(N,minLA=1e4,maxLA=1e7,maxProd=80,b=20,m=-10/6,C=10){
  LAs=10^runif(N,log10(minLA),log10(maxLA))
  doc=runif(N,min=rep(seq(0,16,4),ceiling(N/5)),max=rep(seq(4,20,4),ceiling(N/5)))
  doc2=doc*doc
  
  crits=b+m*log10(LAs)
  as=(C-maxProd)/crits^2
  bs=-2*as*crits
  cs=as*crits^2+maxProd
  
  meanGPPs=as*doc^2+bs*doc+cs
  GPPs=rnorm(N,meanGPPs,0.25*meanGPPs)
  
  #plot(doc,GPPs,col=rep(1:length(meanLAs),each=N))
  fit=lm(GPPs~doc2+doc2:LAs+doc+doc:LAs)
  
  return(summary(fit)$coefficients[4:5,4])
}

Ns=seq(10,80,5)
reps=100

p2=matrix(NA,length(Ns),reps)
p1=p2

for(i in 1:length(Ns)){
  for(j in 1:reps){
    print(c(i,j))
    cur=LAunif(Ns[i])
    p2[i,j]=cur[1]
    p1[i,j]=cur[2]	
  }
}

meanP2=rowMeans(p2)
sdP2=apply(p2,1,sd)
meanP1=rowMeans(p1)
sdP1=apply(p1,1,sd)

dev.new()

par(mfrow=c(2,1))
plot(Ns,meanP2,xlab="# of lakes sampled",ylab="p-value of LA:doc^2",ylim=c(min(meanP2-sdP2),max(meanP2+sdP2)))
arrows(Ns,meanP2-sdP2,Ns,meanP2+sdP2,code=0)
abline(h=0.05,lty=2)
plot(Ns,meanP1,xlab="# of lakes sampled ",ylab="p-value of LA:doc",ylim=c(min(meanP1-sdP1),max(meanP1+sdP1)))
arrows(Ns,meanP1-sdP1,Ns,meanP1+sdP1,code=0)
abline(h=0.05,lty=2)



## how does minimum N per class change with slope of log-linear relationship between LA and critical tDOC
Ns=4:20
reps=100

p2=matrix(NA,length(Ns),reps)
p1=p2

# m = -4/6
for(i in 1:length(Ns)){
  for(j in 1:reps){
    print(c(i,j))
    cur=LAclasses(Ns[i],m=-4/6)
    p2[i,j]=cur[1]
    p1[i,j]=cur[2]	
  }
}

meanP2=rowMeans(p2)
sdP2=apply(p2,1,sd)
meanP1=rowMeans(p1)
sdP1=apply(p1,1,sd)

dev.new()
plot(Ns,meanP2,xlab="# of lakes sampled per size class",ylab="p-value of LA:doc^2",ylim=c(-0.2,1))
arrows(Ns,meanP2-sdP2,Ns,meanP2+sdP2,code=0)
abline(h=0.05,lty=2)

# m = -6/6
for(i in 1:length(Ns)){
  for(j in 1:reps){
    print(c(i,j))
    cur=LAclasses(Ns[i],m=-6/6)
    p2[i,j]=cur[1]
    p1[i,j]=cur[2]	
  }
}

meanP2=rowMeans(p2)
sdP2=apply(p2,1,sd)
meanP1=rowMeans(p1)
sdP1=apply(p1,1,sd)

points(Ns,meanP2,col=2)
arrows(Ns,meanP2-sdP2,Ns,meanP2+sdP2,code=0,col=2)

# m = -8/6
for(i in 1:length(Ns)){
  for(j in 1:reps){
    print(c(i,j))
    cur=LAclasses(Ns[i],m=-8/6)
    p2[i,j]=cur[1]
    p1[i,j]=cur[2]	
  }
}

meanP2=rowMeans(p2)
sdP2=apply(p2,1,sd)
meanP1=rowMeans(p1)
sdP1=apply(p1,1,sd)

points(Ns,meanP2,col=3)
arrows(Ns,meanP2-sdP2,Ns,meanP2+sdP2,code=0,col=3)

# m = -10/6
for(i in 1:length(Ns)){
  for(j in 1:reps){
    print(c(i,j))
    cur=LAclasses(Ns[i],m=-10/6)
    p2[i,j]=cur[1]
    p1[i,j]=cur[2]	
  }
}

meanP2=rowMeans(p2)
sdP2=apply(p2,1,sd)
meanP1=rowMeans(p1)
sdP1=apply(p1,1,sd)

points(Ns,meanP2,col=4)
arrows(Ns,meanP2-sdP2,Ns,meanP2+sdP2,code=0,col=4)

# m = -12/6
for(i in 1:length(Ns)){
  for(j in 1:reps){
    print(c(i,j))
    cur=LAclasses(Ns[i],m=-12/6)
    p2[i,j]=cur[1]
    p1[i,j]=cur[2]	
  }
}

meanP2=rowMeans(p2)
sdP2=apply(p2,1,sd)
meanP1=rowMeans(p1)
sdP1=apply(p1,1,sd)

points(Ns,meanP2,col=5)
arrows(Ns,meanP2-sdP2,Ns,meanP2+sdP2,code=0,col=5)

# m = -14/6
for(i in 1:length(Ns)){
  for(j in 1:reps){
    print(c(i,j))
    cur=LAclasses(Ns[i],m=-14/6)
    p2[i,j]=cur[1]
    p1[i,j]=cur[2]	
  }
}

meanP2=rowMeans(p2)
sdP2=apply(p2,1,sd)
meanP1=rowMeans(p1)
sdP1=apply(p1,1,sd)

points(Ns,meanP2,col=6)
arrows(Ns,meanP2-sdP2,Ns,meanP2+sdP2,code=0,col=6)

# m = -14/3
for(i in 1:length(Ns)){
  for(j in 1:reps){
    print(c(i,j))
    cur=LAclasses(Ns[i],m=-14/6)
    p2[i,j]=cur[1]
    p1[i,j]=cur[2]	
  }
}

meanP2=rowMeans(p2)
sdP2=apply(p2,1,sd)
meanP1=rowMeans(p1)
sdP1=apply(p1,1,sd)

points(Ns,meanP2,col=7)
arrows(Ns,meanP2-sdP2,Ns,meanP2+sdP2,code=0,col=7)

legend('topright',legend=round(c(-4/6,-6/6,-8/6,-10/6,-12/6,-14/6,-14/3),2),pch=21,col=1:7)


#################################################################
####Here I'm messing around with simulating data#################
# and attempting a power analysis for the Kelly model test#######
#################################################################

library(ggplot2)
library(tidyverse)

#####
DOC<-seq(0,30,0.5)

population_mean<-mean(DOC)
population_sd<-sd(DOC)

GPP<- dnorm(DOC, population_mean, population_sd)*10000

#Make it noisy
noise <- rnorm(length(GPP), mean=10, sd=50)
noisy.GPP <- GPP + noise

noise <- rnorm(length(DOC), mean=15, sd=0.5)
noisy.DOC <- DOC + abs(noise)



data2<-data.frame(DOC,GPP,noisy.DOC,noisy.GPP)
# data3<-data.frame(noisy.DOC,noisy.GPP)


#Fit a polynomial model 
mod<- lm(GPP ~ poly(DOC, 2))
summary(mod)

mod2<- lm(noisy.GPP ~ poly(noisy.DOC, 2))
summary(mod2)
c<-as.numeric(mod$coefficients[1])
b<-as.numeric(mod$coefficients[2])
a<-as.numeric(mod$coefficients[3])
maxGPP<-(c-b^2/(4*a))
crit<-(-b/(2*a))


plot(GPP ~ DOC, data = data2, 
     xlab = "DOC load", ylab = "GPP (mg C/m2/day)",
     pch = 20, cex = 2)
xplot = seq(0, 30, by = 1.5)
lines(xplot, predict(mod2, newdata = data.frame(advert = xplot)),
      col = "blue", lwd = 2)
abline(h=maxGPP, col="green")
abline(h=crit, col="pink")



#Simulate lake size categories
A<-"0.001-0.01"
B<-"0.01-0.1"
C<-"0.1-1"
D<-"1-10"
E<-"10-100"
lakecategories<-c(A,B,C,D,E)
lakeletters<-c("A","B","C","D","E")

lakesizes<-data.frame(lakecategories,lakeletters) %>%
  rename(randomLetters=lakeletters)
randomLetters<-rep(LETTERS[1:5], times = 1, length.out=21)

data2<-data.frame(data2, randomLetters) %>%
  left_join(., lakesizes, by = "randomLetters") 

#Power analysis on mod3, a LMM with lake size as a random effect
mod3 <- lmer(noisy.GPP ~ poly(noisy.DOC, 2) + (1|lakecategories), data=data2)
summary(model1)
plot(model1)

data2 %>%
  ggplot(aes(x=noisy.DOC, y=noisy.GPP, color=lakecategories))+
  geom_point()+geom_smooth(se=F)


set.seed(123)
powerSim(mod3)
#given this particular set up,
#The power to reject the null hypothesis of zero trend in x is about 33%

##################################################
####Power analysis a different way##########
##################################################
library(pwr)
pwr.f2.test(u=2,f2=1.94, sig.level=0.05, power=0.80)
#f2=1.94 is equivelent to R2=0.66

gam1 <- gam(noisy.GPP ~  s(DOC)+ s(lakecategories,k=11,bs="re"),
             data = data2, method = "REML", family="gaussian")
summary(gam1)
appraise(gam1)
draw(gam1)
gam.check(gam1)


##################################################
####Power analysis a different way################
####power.trend###################################
##################################################
library(sjmisc)
data(efc)
# linear fit. loess-smoothed line indicates a more
# or less cubic curve
sjp.poly(efc$c160age, efc$quol_5, 1)

# quadratic fit
sjp.poly(efc$c160age, efc$quol_5, 2)

# linear to cubic fit
sjp.poly(efc$c160age, efc$quol_5, 1:4, show.scatter = FALSE)


# fit sample model
fit <- lm(tot_sc_e ~ c12hour + e17age + e42dep, data = efc)
# inspect relationship between predictors and response
plot_model(fit, type = "slope")
# "e17age" does not seem to be linear correlated to response
# try to find appropiate polynomial. Grey line (loess smoothed)
# indicates best fit. Looks like x^4 has the best fit,
# however, only x^3 has significant p-values.
sjp.poly(fit, "e17age", 2:4, show.scatter = FALSE)

## Not run: 
# fit new model
fit <- lm(tot_sc_e ~ c12hour + e42dep + e17age + I(e17age^2) + I(e17age^3),
          data = efc)
# plot marginal effects of polynomial term
plot_model(fit, type = "pred", terms = "e17age")
