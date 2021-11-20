#Wrapper script for fitting Kelly model

#Load libraries ----
library(deSolve)

#Load functions ----
# source('kellyLoss.r')
# source('huisman.r')
# source('lightAtten.r')
# source('kellyPredix.r')
source('r_code/scripts/paramOptim/kellyLoss.r')
source('r_code/scripts/paramOptim/huisman.r')
source('r_code/scripts/paramOptim/lightAtten.r')
source('r_code/scripts/paramOptim/kellyPredix.r')

#Load data ----
# d <- read.csv('gridSearchInput.csv')
d <- read.csv('data/gridSearchInput.csv')

#Add a column to d for incident light - this is just a constant made up number right now
d$I0 <- 1000

#Initial guesses of parameters ----

#Log-transform these because they will be exponentiated within kellyLoss.r
parGuess <- log(c(kDOC=0.42,kA=0.00022,lA=0.1,pA=1.2,hA=55,mA=2,decay=0.001,cA=0.015,v=0.1,rec=0.95,scaleGPP=1,scaleP=1,scaleDOC=1, scalezMix=1))

#Lower and upper bounds; again, log-transform
lower <- log(c(kDOC=0.05,kA=0.00005,lA=0.01,pA=0.1,hA=20,mA=0.1,decay=0.0001,cA=0.005,v=0.01,rec=0.2,scaleGPP=0,scaleP=0,scaleDOC=0, scalezMix=0))
upper <- log(c(kDOC=0.8,kA=0.005,lA=0.3,pA=2,hA=100,mA=10,decay=0.01,cA=0.025,v=0.5,rec=0.995,scaleGPP=100,scaleP=100,scaleDOC=100, scalezMix=100))
# 

#What if we keep the lower and upper bounds with +/- 20% of the published values?
#2021-11-03 IAO
# lower <- log(c(kDOC=0.336,kA=0.000176,lA=0.08,pA=0.96,hA=44,mA=1.6,decay=8e-04,cA=0.012,v=0.08,rec=0.76,scaleGPP=0,scaleP=0,scaleDOC=0, scalezMix=0))
# upper <- log(c(kDOC=0.504,kA=0.000264,lA=0.12,pA=1.44,hA=66,mA=2.4,decay=0.0012,cA=0.018,v=0.12,rec=0.995,scaleGPP=100,scaleP=100,scaleDOC=100, scalezMix=100))
# 



#Find ML estimates of parameters ----
#(by minimizing the NLL of residuals between observations and predictions)

#Test that loss function works
# kellyLoss(parGuess,d)

#Find ML estimates
start_time <- Sys.time()
optimOut <- optim(par=parGuess,fn=kellyLoss,lower=lower,upper=upper,d=d)
end_time <- Sys.time()
end_time - start_time
optimOut
# Time difference of 48 minutes IAO 2021-11-02

exp(optimOut$par)

#Plot fits vs observations ----

#Calculate the fits
fits <- kellyPredix(optimOut$par,d)

#Plot fits vs observations - three panels
par(mfrow=c(2,2),mar=c(4.1,4.1,1.1,1.1))

#GPP
plot(fits[,1]~d$medianGPP, ylab="GPP pred", xlab="GPP obs")
abline(0,1)

#TP
plot(fits[,2]~d$TP, , ylab="TP pred", xlab="TP obs")
abline(0,1)

#DOC
plot(fits[,3]~d$DOC, , ylab="DOC pred", xlab="DOC obs")
abline(0,1)

#zMix
plot(fits[,4]~d$meanzMix, , ylab="zMix pred", xlab="zMix obs")
abline(0,1)

#What are the parameters?
optimOut$par
params<-optimOut$par[1:10]


#Look at the residuals
GPP.mod<-lm(fits[,1]~d$medianGPP)
GPP.resid<-resid(GPP.mod)
plot(fits[,1], GPP.resid, 
          ylab="Residuals",
     xlab="Pred. GPP") 
abline(0, 0)

TP.mod<-lm(fits[,2]~d$TP)
TP.resid<-resid(TP.mod)
plot(fits[,2], TP.resid, 
     ylab="Residuals",
     xlab="Pred. TP") 
abline(0, 0)

DOC.mod<-lm(fits[,3]~d$DOC)
DOC.resid<-resid(DOC.mod)
plot(fits[,3], DOC.resid, 
     ylab="Residuals",
     xlab="Pred. DOC") 
abline(0, 0)


zMix.mod<-lm(fits[,4]~d$meanzMix)
zMix.resid<-resid(zMix.mod)
plot(fits[,4], zMix.resid, 
     ylab="Residuals",
     xlab="Pred. zMix") 
abline(0, 0)




library(tidyverse)
kelly_set<-data.frame(c(kDOC=0.42,kA=0.00022,lA=0.1,pA=1.2,hA=55,mA=2,decay=0.001,cA=0.015,v=0.1,rec=0.95))
colnames(kelly_set)<-c("kelly_set")
kelly_set <- kelly_set %>%
  rownames_to_column()

compare_params<-data.frame(params) %>%
  rownames_to_column() %>%
  pivot_longer(-1) %>%
  select(-name) %>%
  mutate(value=exp(value)) %>%
  rename("max. likelihood set"=value) %>%
  left_join(.,kelly_set,by=c("rowname")) %>%
  rename("Kelly set"=kelly_set)


compare_params
# write_csv(compare_params, "data/export/compareParams_20211102_correctedGPP.csv")
# write_csv(compare_params, "data/export/compareParams_20211104_correctedGPP_freeRange.csv")
write_csv(compare_params, "data/export/compareParams_20211104_correctedGPP_constrainedLimits20perc.csv")



# How do the fits compare with the default params? ------------------------

#Pull in parameter estimates from 11/04/2021 - free range
params1<-read_csv("data/export/compareParams_20211104_correctedGPP_freeRange.csv") %>%
  dplyr::select(-`Kelly set`) %>%
  pivot_wider(names_from=`max. likelihood set`, values_from="rowname") %>%
  names()
params1<-as.numeric(as.character(params1))
paramNames<-c("kDOC",
  "kA",
  "lA",
  "pA",
  "hA",
  "mA",
  "decay",
  "cA",
  "v",
  "rec")
names(params1)<-paramNames

#Pull in parameter estimates from 11/04/2021 - constrained
params2<-read_csv("data/export/compareParams_20211104_correctedGPP_constrainedLimits20perc.csv") %>%
  dplyr::select(-`Kelly set`) %>%
  pivot_wider(names_from=`max. likelihood set`, values_from="rowname") %>%
  names()
params2<-as.numeric(as.character(params2))
paramNames<-c("kDOC",
              "kA",
              "lA",
              "pA",
              "hA",
              "mA",
              "decay",
              "cA",
              "v",
              "rec")
names(params2)<-paramNames


#Calculate the fits
fits_default <- kellyPredix(parGuess[1:10],d)
fits_default<-data.frame(fits_default) %>%
  rownames_to_column() %>%
  mutate(parameterset='default')

fits_freeRange <- kellyPredix(log(params1),d)
fits_freeRange<-data.frame(fits_freeRange) %>%
  rownames_to_column() %>%
  mutate(parameterset='free')

fits_constrained <-  kellyPredix(log(params2),d)
fits_constrained<-data.frame(fits_constrained) %>%
  rownames_to_column() %>%
  mutate(parameterset='constrained')

#Create master dataframe
fits_master<-bind_rows(fits_default,
                       fits_freeRange,
                       fits_constrained)

#Create table with all the parameter sets too
kelly_set<-data.frame(c(kDOC=0.42,kA=0.00022,lA=0.1,pA=1.2,hA=55,mA=2,decay=0.001,cA=0.015,v=0.1,rec=0.95))
colnames(kelly_set)<-c("kelly_set")
kelly_set <- kelly_set %>%
  rownames_to_column()

# constrained_set<-data.frame(exp(optimOut$par[1:10])) 
# colnames(constrained_set)<-"max. likelihood set - +/- 20%"
# constrained_set <- constrained_set %>%
#   rownames_to_column()

compare_params<-data.frame(params1) %>%
  rownames_to_column() %>%
  pivot_longer(-1) %>%
  select(-name) %>%
  rename("max. likelihood set - free Range"=value) %>%
  left_join(.,kelly_set,by=c("rowname")) %>%
  rename("Kelly defaults"=kelly_set) %>%
  left_join(., data.frame(params2) %>%
              rownames_to_column() %>%
              pivot_longer(-1) %>%
              select(-name) %>%
              rename("max. likelihood set - constrained +/- 20%"=value), by=c("rowname")) %>%
  mutate_if(is.numeric, round, 4) %>%
  relocate(`Kelly defaults`, .after = rowname) %>% #rearranging in case this presents a problem in modeling
  rename("Parameter"=rowname)

library(kableExtra)
# webshot::install_phantomjs()

#Export table of parameter sets
kable(compare_params, "html") %>%
  kable_styling("striped") %>%
  save_kable("figures/coauthor update Nov 5,  2021/Table1_parametersSets.pdf")





######################Quantifying uncertainty##########################################
###As the square root of a variance, RMSE can be interpreted as the standard deviation#
###of the unexplained variance, and has the useful property of being in the same units#
###as the response variable. Lower values of RMSE indicate better fit.#################

# install.packages("Metrics")
library(Metrics)
library(patchwork)


# ~ Default parameters ----------------------------------------------------


error_default<-d %>%
  select(lakeName, DOC, TP, meanzMix, medianGPP, meanGPP) %>%
  rownames_to_column() %>%
  left_join(., fits_default, by="rowname")

error_default<-error_default%>%
  # group_by(rowname)%>%
  mutate(rmse_GPPmean=round(rmse(meanGPP, GPPHat),1),
         rmse_GPPmedian=round(rmse(medianGPP, GPPHat),1),
         rmse_TP=round(rmse(TP,PHat),1),
         rmse_zMix=round(rmse(meanzMix, zMixHat),1),
         rmse_DOC=round(rmse(DOC,CHat),1)) 

rmse_defaults <- error_default[1,13:17]

##Using the default parameters, how far off are the obsevations for each lake?
##MEAN GPP
meanGPP_bar<-error_default %>%
  ggplot(aes(x=reorder(lakeName, (meanGPP)), y=(GPPHat-meanGPP)))+
  geom_bar(stat="identity",  color="black", fill="white")+
  theme_pubr(base_size = 18)+
  theme(
    axis.title.x=element_blank(),
    axis.text.x=element_text(angle = 45, hjust = 1, size=12))+
  ylab("Pred - Obs")+
  xlab("Lake")+
  scale_fill_continuous_sequential(palette = "Terrain", rev=TRUE)
  # ggtitle("How far off are GPP observations for each lake?")+

meanGPP_1to1<-error_default %>%
  ggplot(aes(x=meanGPP, y=GPPHat))+
  geom_point(shape=21, size=2.5, fill="grey70")+
  geom_abline(slope=1, intercept=0)+
  annotate(
    "text",
    x = 1200,
    y = 1500 * 1 * 1.1,
    label = "1:1"
  ) +
  geom_text(data=rmse_defaults,
            aes(label= paste0("RMSE=", rmse_GPPmean)),
            x=5000,
            y=3000)+
  geom_smooth(method="lm", se=F, linetype="longdash", color="black")+
  ylab("Pred. GPP")+
  xlab("Mean obs. GPP")+
  theme_pubr(base_size = 18)



##MEDIAN GPP
medianGPP_bar<-error_default %>%
  ggplot(aes(x=reorder(lakeName, (medianGPP)), y=(GPPHat-medianGPP)))+
  geom_bar(stat="identity",  color="black", fill="white")+
  theme_pubr(base_size = 18)+
  theme(
    axis.title.x=element_blank(),
    axis.text.x=element_text(angle = 45, hjust = 1, size=12))+
  ylab("Pred - Obs")+
  xlab("Lake")
  # ggtitle("How far off are GPP observations for each lake?")+
  
medianGPP_1to1<-error_default %>%
  ggplot(aes(x=medianGPP, y=GPPHat))+
  geom_point(shape=21, size=2.5, fill="grey70")+
  geom_abline(slope=1, intercept=0)+
  geom_text(data=rmse_defaults,
            aes(label= paste0("RMSE=", rmse_GPPmedian)),
            x=4500,
            y=3000)+
  annotate(
    "text",
    x = 850,
    y = 1500 * 1 * 1.1,
    label = "1:1"
  ) +
  geom_smooth(method="lm", se=F, linetype="longdash", color="black")+
  ylab("Pred. GPP")+
  xlab("Median obs. GPP")+
  theme_pubr(base_size = 18)

#Plot the GPP parameters together
(meanGPP_bar+meanGPP_1to1)/
(medianGPP_bar+medianGPP_1to1)+
  plot_annotation(title = 'Default (published) Kelly model parameters')



##DOC
DOC_bar<-error_default %>%
  ggplot(aes(x=reorder(lakeName, (DOC-CHat)), y=(DOC-CHat)))+
  geom_bar(stat="identity",  color="black", fill="white")+
  theme_pubr()+
  theme(
    axis.title.x=element_blank(),
    axis.text.x=element_text(angle = 45, hjust = 1, size=6))+
  ylab("Obs mean DOC - DOC Hat")+
  xlab("Lake")
  # ggtitle("How far off are GPP observations for each lake?")+
  
DOC_1to1<-  error_default %>%
  ggplot(aes(x=DOC, y=CHat))+
  geom_point(shape=21, size=2.5, fill="white")+
  geom_abline(slope=1, intercept=0)+
  geom_text(data=rmse_defaults,
            aes(label= paste0("RMSE=", rmse_DOC)),
            x=5,
            y=30)+
  ylab("DOC hat")+
  xlab("Mean obs DOC")+
  theme_pubr()


##TP
TP_bar<-error_default %>%
  ggplot(aes(x=reorder(lakeName, (TP-PHat)), y=(TP-PHat)))+
  geom_bar(stat="identity",  color="black", fill="white")+
  theme_pubr()+
  theme(
    axis.title.x=element_blank(),
    axis.text.x=element_text(angle = 90, size=12))+
  ylab("Obs mean TP - TP Hat")+
  xlab("Lake")

# ggtitle("How far off are GPP observations for each lake?")+

TP_1to1<-  error_default %>%
  ggplot(aes(x=TP, y=PHat))+
  geom_point(shape=21, size=2.5, fill="white")+
  geom_abline(slope=1, intercept=0)+
  geom_text(data=rmse_defaults,
            aes(label= paste0("RMSE=", rmse_TP)),
            x=25,
            y=60)+
  ylab("TP hat")+
  xlab("Mean obs TP")+
  theme_pubr()



##zMix
zMix_bar<-error_default %>%
  ggplot(aes(x=reorder(lakeName, (meanzMix-zMixHat)), y=(meanzMix-zMixHat)))+
  geom_bar(stat="identity",  color="black", fill="white")+
  theme_pubr()+
  theme(
    axis.title.x=element_blank(),
    axis.text.x=element_text(angle = 45, hjust = 1, size=6))+
  ylab("Obs mean zMix - zMix Hat")+
  xlab("Lake")
# ggtitle("How far off are GPP observations for each lake?")+

zMix_1to1<-  error_default %>%
  ggplot(aes(x=meanzMix, y=zMixHat))+
  geom_point(shape=21, size=2.5, fill="white")+
  geom_abline(slope=1, intercept=0)+
  geom_text(data=rmse_defaults,
            aes(label= paste0("RMSE=", rmse_zMix)),
            x=5,
            y=25)+
  ylab("zMix hat")+
  xlab("Mean obs zMix")+
  theme_pubr()


##plot TP, DOC, and zMix together
(DOC_bar+DOC_1to1)/
  (TP_bar+TP_1to1)/
    (zMix_bar+zMix_1to1) +
  plot_annotation(title = 'Default (published) Kelly model parameters')






# ~ Free range parameters ----------------------------------------------------


error_freeRange<-d %>%
  select(lakeName, DOC, TP, meanzMix, medianGPP, meanGPP) %>%
  rownames_to_column() %>%
  left_join(., fits_freeRange, by="rowname")

error_freeRange<-error_freeRange%>%
  # group_by(rowname)%>%
  mutate(rmse_GPPmean=round(rmse(meanGPP, GPPHat),1),
         rmse_GPPmedian=round(rmse(medianGPP, GPPHat),1),
         rmse_TP=round(rmse(TP,PHat),1),
         rmse_zMix=round(rmse(meanzMix, zMixHat),1),
         rmse_DOC=round(rmse(DOC,CHat),1)) 

rmse_freeRange <- error_freeRange[1,13:17]

##Using the "free range" parameters, how far off are the obsevations for each lake?
##MEAN GPP
meanGPP_bar<-error_freeRange %>%
  ggplot(aes(x=reorder(lakeName, (meanGPP-GPPHat)), y=(meanGPP-GPPHat)))+
  geom_bar(stat="identity",  color="black", fill="white")+
  theme_pubr()+
  theme(
    axis.title.x=element_blank(),
    axis.text.x=element_text(angle = 45, hjust = 1, size=6))+
  ylab("Obs mean GPP - GPP Hat")+
  xlab("Lake")+
  scale_fill_continuous_sequential(palette = "Terrain", rev=TRUE)
# ggtitle("How far off are GPP observations for each lake?")+

meanGPP_1to1<-error_freeRange %>%
  ggplot(aes(x=meanGPP, y=GPPHat))+
  geom_point(shape=21, size=2.5, fill="white")+
  geom_abline(slope=1, intercept=0)+
  geom_text(data=rmse_freeRange,
            aes(label= paste0("RMSE=", rmse_GPPmean)),
            x=1500,
            y=5000)+
  ylab("GPP hat")+
  xlab("Mean obs GPP")+
  theme_pubr()



##MEDIAN GPP
medianGPP_bar<-error_freeRange %>%
  ggplot(aes(x=reorder(lakeName, (medianGPP-GPPHat)), y=(medianGPP-GPPHat)))+
  geom_bar(stat="identity",  color="black", fill="white")+
  theme_pubr()+
  theme(
    axis.title.x=element_blank(),
    axis.text.x=element_text(angle = 45, hjust = 1, size=6))+
  ylab("Obs median GPP - GPP Hat")+
  xlab("Lake")
# ggtitle("How far off are GPP observations for each lake?")+

medianGPP_1to1<-error_freeRange %>%
  ggplot(aes(x=medianGPP, y=GPPHat))+
  geom_point(shape=21, size=2.5, fill="white")+
  geom_abline(slope=1, intercept=0)+
  geom_text(data=rmse_freeRange,
            aes(label= paste0("RMSE=", rmse_GPPmedian)),
            x=750,
            y=5000)+
  ylab("GPP hat")+
  xlab("Median obs GPP")+
  theme_pubr()

#Plot the GPP parameters together
(meanGPP_bar+meanGPP_1to1)/
  (medianGPP_bar+medianGPP_1to1)+
  plot_annotation(title = 'Optimized Kelly model parameters - "free range"')



##DOC
DOC_bar<-error_freeRange %>%
  ggplot(aes(x=reorder(lakeName, (DOC-CHat)), y=(DOC-CHat)))+
  geom_bar(stat="identity",  color="black", fill="white")+
  theme_pubr()+
  theme(
    axis.title.x=element_blank(),
    axis.text.x=element_text(angle = 45, hjust = 1, size=6))+
  ylab("Obs mean DOC - DOC Hat")+
  xlab("Lake")
# ggtitle("How far off are GPP observations for each lake?")+

DOC_1to1<-  error_freeRange %>%
  ggplot(aes(x=DOC, y=CHat))+
  geom_point(shape=21, size=2.5, fill="white")+
  geom_abline(slope=1, intercept=0)+
  geom_text(data=rmse_freeRange,
            aes(label= paste0("RMSE=", rmse_DOC)),
            x=5,
            y=30)+
  ylab("DOC hat")+
  xlab("Mean obs DOC")+
  theme_pubr()


##TP
TP_bar<-error_freeRange %>%
  ggplot(aes(x=reorder(lakeName, (TP-PHat)), y=(TP-PHat)))+
  geom_bar(stat="identity",  color="black", fill="white")+
  theme_pubr()+
  theme(
    axis.title.x=element_blank(),
    axis.text.x=element_text(angle = 45, hjust = 1, size=6))+
  ylab("Obs mean TP - TP Hat")+
  xlab("Lake")
# ggtitle("How far off are GPP observations for each lake?")+

TP_1to1<-  error_freeRange %>%
  ggplot(aes(x=TP, y=PHat))+
  geom_point(shape=21, size=2.5, fill="white")+
  geom_abline(slope=1, intercept=0)+
  geom_text(data=rmse_freeRange,
            aes(label= paste0("RMSE=", rmse_TP)),
            x=25,
            y=120)+
  ylab("TP hat")+
  xlab("Mean obs TP")+
  theme_pubr()



##zMix
zMix_bar<-error_freeRange %>%
  ggplot(aes(x=reorder(lakeName, (meanzMix-zMixHat)), y=(meanzMix-zMixHat)))+
  geom_bar(stat="identity",  color="black", fill="white")+
  theme_pubr()+
  theme(
    axis.title.x=element_blank(),
    axis.text.x=element_text(angle = 45, hjust = 1, size=6))+
  ylab("Obs mean zMix - zMix Hat")+
  xlab("Lake")
# ggtitle("How far off are GPP observations for each lake?")+

zMix_1to1<-  error_freeRange %>%
  ggplot(aes(x=meanzMix, y=zMixHat))+
  geom_point(shape=21, size=2.5, fill="white")+
  geom_abline(slope=1, intercept=0)+
  geom_text(data=rmse_freeRange,
            aes(label= paste0("RMSE=", rmse_zMix)),
            x=5,
            y=15)+
  ylab("zMix hat")+
  xlab("Mean obs zMix")+
  theme_pubr()


##plot TP, DOC, and zMix together
(DOC_bar+DOC_1to1)/
  (TP_bar+TP_1to1)/
  (zMix_bar+zMix_1to1) +
  plot_annotation(title = 'Optimized Kelly model parameters - "free range"')





# ~ Constrained parameters ----------------------------------------------------


error_constrained<-d %>%
  select(lakeName, DOC, TP, meanzMix, medianGPP, meanGPP) %>%
  rownames_to_column() %>%
  left_join(., fits_constrained, by="rowname")

error_constrained<-error_constrained%>%
  # group_by(rowname)%>%
  mutate(rmse_GPPmean=round(rmse(meanGPP, GPPHat),1),
         rmse_GPPmedian=round(rmse(medianGPP, GPPHat),1),
         rmse_TP=round(rmse(TP,PHat),1),
         rmse_zMix=round(rmse(meanzMix, zMixHat),1),
         rmse_DOC=round(rmse(DOC,CHat),1)) 

rmse_constrained <- error_constrained[1,13:17]

##Using the "free range" parameters, how far off are the obsevations for each lake?
##MEAN GPP
meanGPP_bar<-error_constrained %>%
  ggplot(aes(x=reorder(lakeName, (meanGPP-GPPHat)), y=(meanGPP-GPPHat)))+
  geom_bar(stat="identity",  color="black", fill="white")+
  theme_pubr()+
  theme(
    axis.title.x=element_blank(),
    axis.text.x=element_text(angle = 45, hjust = 1, size=6))+
  ylab("Obs mean GPP - GPP Hat")+
  xlab("Lake")+
  scale_fill_continuous_sequential(palette = "Terrain", rev=TRUE)
# ggtitle("How far off are GPP observations for each lake?")+

meanGPP_1to1<-error_constrained %>%
  ggplot(aes(x=meanGPP, y=GPPHat))+
  geom_point(shape=21, size=2.5, fill="white")+
  geom_abline(slope=1, intercept=0)+
  geom_text(data=rmse_constrained,
            aes(label= paste0("RMSE=", rmse_GPPmean)),
            x=1800,
            y=5000)+
  ylab("GPP hat")+
  xlab("Mean obs GPP")+
  theme_pubr()



##MEDIAN GPP
medianGPP_bar<-error_constrained %>%
  ggplot(aes(x=reorder(lakeName, (medianGPP-GPPHat)), y=(medianGPP-GPPHat)))+
  geom_bar(stat="identity",  color="black", fill="white")+
  theme_pubr()+
  theme(
    axis.title.x=element_blank(),
    axis.text.x=element_text(angle = 45, hjust = 1, size=6))+
  ylab("Obs median GPP - GPP Hat")+
  xlab("Lake")
# ggtitle("How far off are GPP observations for each lake?")+

medianGPP_1to1<-error_constrained %>%
  ggplot(aes(x=medianGPP, y=GPPHat))+
  geom_point(shape=21, size=2.5, fill="white")+
  geom_abline(slope=1, intercept=0)+
  geom_text(data=rmse_constrained,
            aes(label= paste0("RMSE=", rmse_GPPmedian)),
            x=1000,
            y=5000)+
  ylab("GPP hat")+
  xlab("Median obs GPP")+
  theme_pubr()

#Plot the GPP parameters together
(meanGPP_bar+meanGPP_1to1)/
  (medianGPP_bar+medianGPP_1to1)+
  plot_annotation(title = 'Optimized Kelly model parameters - constrained +/- 20%')



##DOC
DOC_bar<-error_constrained %>%
  ggplot(aes(x=reorder(lakeName, (DOC-CHat)), y=(DOC-CHat)))+
  geom_bar(stat="identity",  color="black", fill="white")+
  theme_pubr()+
  theme(
    axis.title.x=element_blank(),
    axis.text.x=element_text(angle = 45, hjust = 1, size=6))+
  ylab("Obs mean DOC - DOC Hat")+
  xlab("Lake")
# ggtitle("How far off are GPP observations for each lake?")+

DOC_1to1<-  error_constrained %>%
  ggplot(aes(x=DOC, y=CHat))+
  geom_point(shape=21, size=2.5, fill="white")+
  geom_abline(slope=1, intercept=0)+
  geom_text(data=rmse_constrained,
            aes(label= paste0("RMSE=", rmse_DOC)),
            x=5,
            y=30)+
  ylab("DOC hat")+
  xlab("Mean obs DOC")+
  theme_pubr()


##TP
TP_bar<-error_constrained %>%
  ggplot(aes(x=reorder(lakeName, (TP-PHat)), y=(TP-PHat)))+
  geom_bar(stat="identity",  color="black", fill="white")+
  theme_pubr()+
  theme(
    axis.title.x=element_blank(),
    axis.text.x=element_text(angle = 45, hjust = 1, size=6))+
  ylab("Obs mean TP - TP Hat")+
  xlab("Lake")
# ggtitle("How far off are GPP observations for each lake?")+

TP_1to1<-  error_constrained %>%
  ggplot(aes(x=TP, y=PHat))+
  geom_point(shape=21, size=2.5, fill="white")+
  geom_abline(slope=1, intercept=0)+
  geom_text(data=rmse_constrained,
            aes(label= paste0("RMSE=", rmse_TP)),
            x=25,
            y=80)+
  ylab("TP hat")+
  xlab("Mean obs TP")+
  theme_pubr()



##zMix
zMix_bar<-error_constrained %>%
  ggplot(aes(x=reorder(lakeName, (meanzMix-zMixHat)), y=(meanzMix-zMixHat)))+
  geom_bar(stat="identity",  color="black", fill="white")+
  theme_pubr()+
  theme(
    axis.title.x=element_blank(),
    axis.text.x=element_text(angle = 45, hjust = 1, size=6))+
  ylab("Obs mean zMix - zMix Hat")+
  xlab("Lake")
# ggtitle("How far off are GPP observations for each lake?")+

zMix_1to1<-  error_constrained %>%
  ggplot(aes(x=meanzMix, y=zMixHat))+
  geom_point(shape=21, size=2.5, fill="white")+
  geom_abline(slope=1, intercept=0)+
  geom_text(data=rmse_constrained,
            aes(label= paste0("RMSE=", rmse_zMix)),
            x=5,
            y=15)+
  ylab("zMix hat")+
  xlab("Mean obs zMix")+
  theme_pubr()


##plot TP, DOC, and zMix together
(DOC_bar+DOC_1to1)/
  (TP_bar+TP_1to1)/
  (zMix_bar+zMix_1to1) +
  plot_annotation(title = 'Optimized Kelly model parameters - constrained +/- 20%')

