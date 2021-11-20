# install.packages("ggplot2")
# install.packages("tidyverse")
# install.packages("lubridate") #To tell R that my dates are dates
# install.packages("colorspace") # for scale_color_discrete_qualitative() ftn
# install.packages("rLakeAnalyzer") #For temperature heatmaps 
# install.packages("skimr")
# install.packages("hablar") #retype() and convert() functions
# install.packages("viridis")
# install.packages("hrbrthemes") #has nice fonts

# install.packages("remotes")
library(breakerofchains)
remotes::install_github("MilesMcBain/breakerofchains", force=TRUE)

library(readxl)
library(ggplot2)
library(tidyverse)
library(lubridate) #To tell R that my dates are dates
library(colorspace) # for scale_color_discrete_qualitative() ftn
library(rLakeAnalyzer) #For temperature heatmaps 
library(skimr)
library(hablar) #retype() and convert() functions
library(viridis)
library(hrbrthemes) #has nice fonts
# hrbrthemes::import_roboto_condensed()
source('r_code/scripts/fillHoles.R')


Sys.setenv(TZ='GMT')
# RUN: Handy functions ----------------------------------------------------
all_na <- function(x) any(!is.na(x))
#Removes any columns where EVERY row is an NA



#Function to find duplicate datetime stamps
findNotDupRows <- function(dataInName)
{
  #This function returns the indexes of rows where the datetime is NOT a duplicate
  #of the datetime in a previous row
  #dataInName is character, e.g. "dataPAR"
  dataIn <- eval(parse(text=dataInName))
  #Find duplicated time stamps
  dups <- duplicated(dataIn$datetime)
  #If no duplicated time stamps, notDupRows=all rows in dataIn
  if (all(dups==FALSE))
  {
    notDupRows <- c(1:dim(dataIn)[1])
  } else
    #If at least one time stamp is duplicated, warn, and notDupRows is indexes
    #of rows where datetime is not duplicate of the datetime in a previous row
  {
    notDupRows <- which(dups==FALSE)
    nDups <- dim(dataIn)[1]-length(notDupRows)
    print(paste("Warning:",nDups,"rows with duplicate time stamps in",dataInName,"will be removed"))
  }
  #Return dupRows
  return(notDupRows)
}

#Round all time down to nearest timeStep (e.g. if timeStep is 5, round 00:07 to 00:05)
floorMins <- function(dataIn)
{
  #Pull out datetime column and name it x
  x <- dataIn$dateTime
  nRows <- length(x)
  #Truncate each datetime to hour; convert to class numeric
  floorHour <- as.POSIXct(trunc(x[1:nRows],"hour"))
  floorNumeric <- as.numeric(floorHour)
  #Create sequence from floorNumeric to next hour by timeStep (in seconds)
  seqSec <- seq(0,3600,60*timeStep)
  #Create matrix where each row is floorNumeric + the elements of seqSec
  matSec <- matrix(rep(seqSec,nRows),nrow=nRows,byrow=T)
  matSec <- floorNumeric + matSec
  #Calculate abs(time difference) between each element of x and the timeStep intervals
  difs <- abs(as.numeric(x) - matSec)
  #Find the minimum absolute difference in each row and select the corresponding time from matSec
  whichMin <- apply(difs,1,which.min)
  rowNames <- as.numeric(rownames(data.frame(whichMin)))
  matIndex <- (whichMin-1)*nRows + rowNames
  matSecFlat <- matrix(matSec,ncol=1)
  outTime <- as.POSIXct(matSecFlat[matIndex],origin="1970-01-01")
  #Return outTime
  return(outTime)
}

# > Jordan Pond -------------------------------------------------------------

jordanmetadata <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Jordan/JordanPond_metadata.txt")
jordanDOobs <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Jordan/JordanPond_doobs.txt")
jordanwnd <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Jordan/JordanPond_wnd.txt")
jordantemp <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Jordan/JordanPond_temp.txt")
jordanpar <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Jordan/JordanPond_par.txt")


jordannuts <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Jordan/JordanPond_nutrient.txt")

jordan_full <- left_join(jordanDOobs, jordanwnd, by = c("dateTime", "localTZ", "daylightSavings"))
jordan_full <- left_join(jordan_full, jordantemp, by = c("dateTime", "localTZ", "daylightSavings"))
jordan_full <- left_join(jordan_full, jordanpar, by = c("dateTime", "localTZ", "daylightSavings"))

#Add lake ID
jordan_full$lakeID <- 'JORDAN'
glimpse(jordan_full)

####DATE CONVERSION###

# jordan_full$dateTime <- ymd_hms(as.factor(jordan_full$dateTime))
jordan_full <- jordan_full %>%
  mutate(dateTime = as_datetime(dateTime, tz="America/New_York"),
         dateTime_GMT= with_tz(dateTime, "GMT")) %>%
  dplyr::select(-localTZ, -daylightSavings)




####plot DO###########
ggplot(jordan_full, aes(x=dateTime, y=doObs)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%m-%d") +
  ylab("DO obs")+
  ggtitle("Jordan Pond 2018")


####plot WIND###########
ggplot(jordan_full, aes(x=dateTime, y=wind)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%m-%d") +
  ylab("wind speed")+
  ggtitle("Jordan Pond 2018")


####plot PAR###########
ggplot(jordan_full, aes(x=dateTime, y=PAR)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%m-%d") +
  ylab("par")+
  ggtitle("Jordan Pond 2018")

#Mean PAR May1-Sep30?
jordan_full %>%
  mutate(solarDay=date(dateTime))%>%
  filter(dateTime < "2018-09-30")%>%
  filter(PAR>100) %>%
  group_by(solarDay) %>%
  summarize(PARmean=mean(PAR, na.rm=TRUE),
            PARmax=max(PAR, na.rm=TRUE)) %>%
  ungroup() %>%
  summarize(PARmean=mean(PARmean, na.rm=TRUE),
            PARmax=mean(PARmax, na.rm=TRUE))

####plot TEMPS###########

jordan_full %>%
  dplyr::select(dateTime, wtr1.0:wtr16.0)%>%
  pivot_longer(-(dateTime), names_sep = "r", names_to = c("variable","depth")) %>%
  dplyr::select(-variable)%>%
  mutate(depth=as.numeric(as.character(depth))) %>%
  rename(temp=value)%>%
  ggplot(aes(x=dateTime, y=temp, color=depth)) + 
  geom_point(size=2)+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%m-%d") +
  scale_color_continuous_sequential(palette = "Dark Mint", rev=TRUE)+
  ylab("water temperature")+
  ggtitle("Jordan Pond 2018")

#plot denstity profile 
#commented out because this only works if you have partially run the metabolism model
dataDensProfile %>%
  dplyr::select(dateTime, wtr1.0:wtr16.0)%>%
  # filter(dateTime > "2018-07-07" & dateTime < "2018-07-08")%>%
  pivot_longer(-(dateTime), names_sep = "r", names_to = c("variable","depth")) %>%
  dplyr::select(-variable)%>%
  mutate(depth=as.numeric(as.character(depth))) %>%
  rename(temp=value)%>%
  ggplot(aes(x=dateTime, y=temp, color=depth)) + 
  geom_point(size=2)+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%m-%d") +
  scale_color_continuous_sequential(palette = "Dark Mint", rev=TRUE)+
  ylab("water density")+
  ggtitle("Jordan Pond 2018")


# Jordan.wtr = load.ts("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Jordan/JordanPond_temp.txt")
Jordan.wtr = load.ts("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Jordan/dataTempProfile.txt")
Jordan.wtr<- Jordan.wtr %>%
  # dplyr::select(-localtz,-daylightsavings) %>%
  rename_all(function(x) gsub("wtr", "wtr_", x)) #Add an underscore to all heads to read wtr_#.#
wtr.heat.map(Jordan.wtr, plot.title="Jordan Water Temp (C)")

#calculate and plot the metalimnion depths
m.d = ts.meta.depths(Jordan.wtr)

plot(m.d$datetime, m.d$top, type='l', ylab='Meta Depths (m)', xlab='Date', col='blue')
lines(m.d$datetime, m.d$bottom, col='red')

#calculate and plot the thermocline depth
t.d = ts.thermo.depth(Jordan.wtr)

plot(t.d$datetime, t.d$thermo.depth, type='l', ylab='Thermocline Depth (m)', xlab='Date')

JordanzMix<-t.d %>%
  mutate(solarDay=date(datetime))%>%
  rename(zMix=thermo.depth) %>%
  group_by(solarDay) %>%
  summarize(zMix=mean(zMix, na.rm=TRUE)) %>%
  filter(solarDay > "2018-05-01" & solarDay < "2018-10-01") %>%
  summarize(meanzMix=mean(zMix, na.rm=TRUE),
            medianzMix=median(zMix, na.rm=TRUE))
# write.table(KentuckyzMix, "results/model_output_raw/Kentucky/KentuckyzMix.txt", row.names=FALSE)




####plot nutrients###########
jordannuts %>%
  mutate(dateTime = as_datetime(dateTime)) %>%
  dplyr::select(dateTime, DOC_mgL, TP_ugL, TN_ugL, chla_ugL, secchi_m)%>%
  pivot_longer(-dateTime, names_to = "nutrient") %>%
  ggplot(aes(x=dateTime, y=value, color=nutrient)) + 
  geom_point()+geom_line()+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%m-%d") +
  ggtitle("Jordan Pond 2018") +
  facet_wrap(.~nutrient, scales="free_y", nrow=3)


####export for modelling###########
jordanDOobs <- jordan_full %>%
  dplyr::select(dateTime, doObs) %>%
  rename(DO=doObs) 
write.csv(jordanDOobs, "data/metab_data_clean/jordan/jordan_2018_DO.txt", row.names=FALSE)

#DO sensor depth = 1 m
jordansensorTemp <- jordan_full %>%
  dplyr::select(dateTime, wtr1.0) %>%
  rename(sensorTemp=wtr1.0) 
write.csv(jordansensorTemp, "data/metab_data_clean/jordan/Jordan_2018_sensorTemp.txt", row.names=FALSE)

#PAR
jordanPAR <- jordan_full %>%
  dplyr::select(dateTime, PAR) 
write.csv(jordanPAR, "data/metab_data_clean/jordan/jordan_2018_PAR.txt", row.names=FALSE)

#wind height = 9m
jordanwind <- jordan_full %>%
  dplyr::select(dateTime, wind) %>%
  rename(windSpeed=wind) 
write.csv(jordanwind, "data/metab_data_clean/jordan/jordan_2018_windSpeed.txt", row.names=FALSE)

#temp profile
jordantempprofile<- jordan_full %>%
  dplyr::select(dateTime, wtr1.0:wtr16.0)
write.csv(jordantempprofile, "data/metab_data_clean/jordan/jordan_2018_tempProfile.txt", row.names=FALSE)






# > Castle Lake -------------------------------------------------------------



castleDOobs <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Castle/Castle_doobs.txt")
###DO sensor is at 5m.
castlewnd <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Castle/Castle_wnd.txt")
castlePAR <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Castle/Castle_par.txt")
castletemp <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Castle/Castle_wtr.txt") %>%
  mutate(LocalTZ="America/LosAngeles")




# glimpse(castleDOobs)
# glimpse(castlewnd)
glimpse(castlePAR)
# glimpse(castletemp)


castle_full <- left_join(castleDOobs, castlewnd, by = c("lakeID","dateTime", "LocalTZ", "daylightSavings"))
castle_full <- left_join(castle_full, castletemp, by = c("lakeID","dateTime", "LocalTZ"))
castle_full <- left_join(castle_full, castlePAR, by = c("lakeID","dateTime", "LocalTZ", "daylightSavings"))

glimpse(castle_full)



####DATE CONVERSION###

castle_full <- castle_full %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Los_Angeles"),
         dateTime = with_tz(dateTime, "GMT")) %>%
  dplyr::select(-LocalTZ, -daylightSavings) %>%
  rename(wtr10.0=wtr10)

tz(castle_full$dateTime)

glimpse(castle_full)


####plot DO###########
ggplot(castle_full, aes(x=dateTime, y=doObs)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%m-%d") +
  ylab("DO obs")+
  ggtitle("castle lake 2019")


####plot WIND###########
ggplot(castle_full, aes(x=dateTime, y=wind)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%m-%d") +
  ylab("wind speed")+
  ggtitle("castle lake 2019")

####plot PAR###########
ggplot(castle_full, aes(x=dateTime, y=PAR)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%m-%d") +
  ylab("PAR")+
  ggtitle("castle lake 2019")

castle_full %>%
  filter(dateTime >="2019-09-05" & dateTime <="2019-09-08") %>%
  ggplot(aes(x=dateTime, y=PAR)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "4 hours",
                   date_labels = "%m-%d\n%H:%M")+
  ylab("PAR")+
  ggtitle("castle lake 2019")

####plot TEMPS###########

castle_full %>%
  dplyr::select(dateTime, wtr1.0:wtr30.0)%>%
  pivot_longer(-(dateTime), names_sep = "r", names_to = c("variable","depth")) %>%
  dplyr::select(-variable)%>%
  # mutate(depth=as.numeric(as.character(depth))) %>%
  rename(temp=value)%>%
  ggplot(aes(x=dateTime, y=temp, color=factor(depth,
                                              levels=c("1.0","2.0",
                                                       "5.0","7.5",
                                                       "9.0","10",
                                                       "11.0","12.5",
                                                       "14.0","15.0",
                                                       "17.5","20.0",
                                                       "22.0","25.0",
                                                       "30.0")))) + 
  geom_point(size=2)+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%m-%d") +
  # scale_color_continuous_sequential(palette = "Dark Mint", rev=TRUE)+
  ylab("water temperature")+
  ggtitle("castle lake 2019")+
  scale_color_discrete_sequential(palette="Purples 3",
                                  name="Depth")+
  theme(panel.background = element_rect(fill="black"))

#There is a break in the record I think. Is it 5.0?
castle_full %>%
  dplyr::select(dateTime, wtr1.0:wtr30.0)%>%
  pivot_longer(-(dateTime), names_sep = "r", names_to = c("variable","depth")) %>%
  dplyr::select(-variable)%>%
  # mutate(depth=as.numeric(as.character(depth))) %>%
  rename(temp=value)%>%
  filter(depth %in% c("1.0","2.0", "5.0")) %>%
  ggplot(aes(x=dateTime, y=temp, color=factor(depth,
                                              levels=c("1.0","2.0",
                                                       "5.0","7.5",
                                                       "9.0","10",
                                                       "11.0","12.5",
                                                       "14.0","15.0",
                                                       "17.5","20.0",
                                                       "22.0","25.0",
                                                       "30.0")))) + 

  geom_point(size=2)+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%m-%d") +
  # scale_color_continuous_sequential(palette = "Dark Mint", rev=TRUE)+
  ylab("water temperature")+
  ggtitle("castle lake 2019")+
  scale_color_discrete_sequential(palette="Purples 3",
                                  name="Depth")+
  theme(panel.background = element_rect(fill="black"))


# ##Some of these late summer temps are kind of off. Divide the dataset in 2.
# #Remove wtr12.5 and wtr20.0 which just hold one value most of the season. weird. 
# castle_wtr_earlysummer <- castle_full %>%
#   select(dateTime, wtr1.0:wtr30.0) %>%
#   filter(dateTime<"2019-08-05") %>%
#   select(-wtr12.5, -wtr20.0)
# castle_wtr_latesummer <- castle_full %>%
#   select(dateTime, wtr1.0:wtr30.0) %>%
#   filter(dateTime>="2019-08-05")%>%
#   select(-wtr12.5, -wtr20.0)
# 
# castletemp %>%
#   mutate(doy=yday(dateTime))%>%
#   filter(doy<209)%>%
#   ggplot(aes(x=wtr7.5, y=wtr5.0,fill=doy))+
#   geom_point(shape=21, size=2)+
#   scale_fill_continuous_sequential(palette = "Lajolla", rev=TRUE)
# #These late summer wtr temps are not reliable for wtr5.0
# #Could we use the relationship between the two depth at the beginning of the season
# #to impute some values?
# 
# temp_lm_data<-castletemp %>%
#   mutate(doy=yday(dateTime))%>%
#   filter(doy<209) %>%
#   mutate(wtr5.0_predict=wtr7.5*1.6-0.98)
# 
# lm1<-lm(wtr5.0~wtr7.5,
#         data=temp_lm_data)
# summary(lm1)
# 
# temp_lm_data %>%
#   ggplot(aes(x=wtr7.5, y=wtr5.0_predict,fill=doy))+
#   geom_point(shape=21, size=2)+
#   geom_abline(slope=1, intercept = 0)+
#   scale_fill_continuous_sequential(palette = "Lajolla", rev=TRUE)
# 
# castle_wtr_latesummer <- castle_wtr_latesummer %>%
#   mutate(wtr5.0_predict=wtr7.5*1.6-0.98) %>%
#   select(-wtr5.0) %>% #remove old wtr5.0
#   rename(wtr5.0=wtr5.0_predict) %>% #rename
#   relocate(wtr5.0, .before = wtr7.5)
# 
# #combine the split up datasets
# castletemp<-bind_rows(castle_wtr_earlysummer,castle_wtr_latesummer)
# 
# #Does this improve things in late summer?
# castletemp %>%
#   filter(dateTime >= "2019-08-05" & dateTime <= "2019-09-20") %>%
#   pivot_longer(-(dateTime), names_sep = "r", names_to = c("variable","depth")) %>%
#   dplyr::select(-variable)%>%
#   mutate(depth=as.numeric(as.character(depth))) %>%
#   rename(temp=value)%>%
#   ggplot(aes(x=dateTime, y=temp, color=depth)) + 
#   geom_point(size=2)+
#   scale_x_datetime(date_breaks = "3 weeks", date_labels = "%m-%d") +
#   scale_color_continuous_sequential(palette = "Lajolla", rev=TRUE)
# ##NO!! because then the lake surface temperature is cooler prior to stratification. 
# 

####plot nutrients###########
castlenuts %>%
  mutate(dateTime = as_datetime(dateTime)) %>%
  dplyr::select(dateTime, DOC_mgL, TP_ugL, TN_ugL, chla_ugL, secchi_m)%>%
  pivot_longer(-dateTime, names_to = "nutrient") %>%
  ggplot(aes(x=dateTime, y=value, color=nutrient)) + 
  geom_point()+geom_line()+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%m-%d") +
  ggtitle("castle Pond 2018") +
  facet_wrap(.~nutrient, scales="free_y", nrow=3)


####export for modelling###########
castleDOobs <- castle_full %>%
  dplyr::select(dateTime, doObs) %>%
  rename(DO=doObs) 
write.csv(castleDOobs, "data/metab_data_clean/castle/castle_2019_DO.txt", row.names=FALSE)

#DO sensor depth = 5 m
castle_sensorTemp_earlysummer <- castle_full %>%
  select(dateTime, wtr5.0) %>%
  filter(dateTime<"2019-08-05") %>%
  rename(sensorTemp=wtr5.0) 
castle_sensorTemp_latesummer <- castle_full %>%
  select(dateTime, wtr7.5) %>%
  filter(dateTime>="2019-08-05")%>%
  rename(sensorTemp=wtr7.5) 
castlesensorTemp <- bind_rows(castle_sensorTemp_earlysummer,castle_sensorTemp_latesummer)
#Set to 7.5 because at the end of summer, there is a huge gap of missing data at that depth.. 
write.csv(castlesensorTemp, "data/metab_data_clean/castle/castle_2019_sensorTemp.txt", row.names=FALSE)

#PAR
castlePAR <- castle_full %>%
  dplyr::select(dateTime, PAR) 
tz(castlePAR$dateTime)
write.csv(castlePAR, "data/metab_data_clean/castle/castle_2019_PAR.txt", row.names=FALSE)

#wind height = 19m above lake surface, 15m above ground
castlewind <- castle_full %>%
  dplyr::select(dateTime, wind) %>%
  rename(windSpeed=wind) 
write.csv(castlewind, "data/metab_data_clean/castle/castle_2019_windSpeed.txt", row.names=FALSE)

#temp profile
castletempprofile<- castle_full %>%
  dplyr::select(dateTime, wtr1.0:wtr30.0)%>%
  rename_all(function(x) gsub("wtr", "wtr_", x)) #Add an underscore to all heads to read wtr_#.#
write.csv(castletempprofile, "data/metab_data_clean/castle/castle_2019_tempProfile.txt", row.names=FALSE)










# > YunYang Pond -------------------------------------------------------------


YunYangDOobs <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/YunYang/yyl_doobs.txt") %>% dplyr::select(-lakeID) %>%
  rename(DO=doObs)
YunYangwnd <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/YunYang/yyl_wind.txt")%>% dplyr::select(-lakeID) %>%
  rename(windSpeed=wind)
YunYangtemp <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/YunYang/yyl_wtr.txt")%>% dplyr::select(-lakeID)
YunYangPAR <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/YunYang/yyl_par.txt")%>% dplyr::select(-lakeID) %>%
  rename(PAR=q)

YunYangnuts <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/YunYang/yyl_nutrient.txt")%>% dplyr::select(-lakeID)



YunYang_full <- left_join(YunYangDOobs, YunYangwnd, by = c("dateTime", "localTZ", "daylightSavings"))
YunYang_full <- left_join(YunYang_full, YunYangtemp, by = c("dateTime", "localTZ", "daylightSavings"))
YunYang_full <- left_join(YunYang_full, YunYangPAR, by = c("dateTime", "localTZ", "daylightSavings")) %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="Asia/Taipei"),
         dateTime = with_tz(dateTime, "GMT")) 
glimpse(YunYang_full)
tz(YunYang_full$dateTime)


####DATE CONVERSION###

YunYang_full$dateTime <- ymd_hms(as.factor(YunYang_full$dateTime))
YunYang_full <- YunYang_full %>%
  mutate(dateTime = as_datetime(dateTime))

str(YunYang_full$dateTime)


####plot DO###########
ggplot(YunYang_full, aes(x=dateTime, y=doObs)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%m-%d") +
  ylab("DO obs")+
  ggtitle("YunYang Lake 2017")


####plot WIND###########
ggplot(YunYang_full, aes(x=dateTime, y=wind)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%m-%d") +
  ylab("wind speed")+
  ggtitle("YunYang Lake 2017")

####plot TEMPS###########
YunYang_full %>%
  dplyr::select(dateTime, wtr0.0:wtr3.5)%>%
  pivot_longer(-(dateTime), names_sep = "r", names_to = c("variable","depth")) %>%
  dplyr::select(-variable)%>%
  mutate(depth=as.numeric(as.character(depth))) %>%
  rename(temp=value)%>%
  ggplot(aes(x=dateTime, y=temp, color=depth)) + 
  geom_point(size=2)+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%m-%d") +
  scale_color_continuous_sequential(palette = "Dark Mint", rev=TRUE)+
  ylab("water temperature")+
  ggtitle("YunYang Lake 2017")

str(YunYang.wtr)
YunYang.wtr = load.ts("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/YunYang/yyl_wtr.txt")
YunYang.wtr<- YunYang.wtr %>%
  dplyr::select(-lakeid, -localtz,-daylightsavings) %>%
  rename_all(function(x) gsub("wtr", "wtr_", x)) #Add an underscore to all heads to read wtr_#.#
wtr.heat.map(YunYang.wtr, plot.title="Yun Yang Water Temp (C)")

#calculate and plot the thermocline depth
t.d = ts.thermo.depth(YunYang.wtr)
plot(t.d$datetime, t.d$thermo.depth, type='l', ylab='Thermocline Depth (m)', xlab='Date')
str(t.d)
mean(t.d$thermo.depth, na.rm=TRUE)
median(t.d$thermo.depth, na.rm=TRUE)
t.d$thermo.depth[is.nan(t.d$thermo.depth)]<-NA
t.d

YunYangzMix<- t.d %>%
  mutate(solarDay=date(datetime))%>%
  rename(zMix=thermo.depth) %>%
  group_by(solarDay) %>%
  summarize(zMix=mean(zMix, na.rm=TRUE))

SunapeezMix$zMix[is.nan(SunapeezMix$zMix)]<-NA
SunapeezMix

YunYangzMix %>%
  # filter(solarDay > "2018-05-01" & solarDay < "2018-10-01") %>%
  # mutate(if_nan(NA))%>%
  summarize(meanzMix=mean(zMix, na.rm=TRUE),
            medianzMix=median(zMix, na.rm=TRUE))
# write.table(SunapeezMix, "results/model_output_raw/Sunapee/SunapeezMix.txt", row.names=FALSE)




####plot PAR###########
ggplot(YunYang_full, aes(x=dateTime, y=q)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%m-%d") +
  ylab("PAR")+
  ggtitle("YunYang Lake 2017")

####plot nutrients###########
YunYangnuts %>%
  mutate(dateTime = as_datetime(dateTime)) %>%
  dplyr::select(dateTime, DOC_mgL, TP_ugL, TN_ugL)%>%
  pivot_longer(-dateTime, names_to = "nutrient") %>%
  ggplot(aes(x=dateTime, y=value, color=nutrient)) + 
  geom_point()+geom_line()+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%m-%d") +
  ggtitle("YunYang Lake 2017") +
  facet_wrap(.~nutrient, scales="free_y", nrow=3)




####export for modelling###########
YunYangDOobs <- YunYang_full %>%
  dplyr::select(dateTime, DO)
write.csv(YunYangDOobs, "data/metab_data_clean/yunyang/YunYang_DO.txt", row.names=FALSE)

#DO sensor depth = 4.9 m
YunYangsensorTemp <- YunYang_full %>%
  dplyr::select(dateTime, wtr0.5) %>%
  rename(sensorTemp=wtr0.5) 
write.csv(YunYangsensorTemp, "data/metab_data_clean/yunyang/YunYang_sensorTemp.txt", row.names=FALSE)

#PAR
#convert W m-2 to PAR: conversion factor = 0.217 
YunYangPAR <- YunYang_full %>%
  dplyr::select(dateTime, PAR)
write.csv(YunYangPAR, "data/metab_data_clean/yunyang/YunYang_PAR.txt", row.names=FALSE)

#wind height = 19m above lake surface, 15m above ground
YunYangwind <- YunYang_full %>%
  dplyr::select(dateTime, windSpeed)
write.csv(YunYangwind, "data/metab_data_clean/yunyang/YunYang_windSpeed.txt", row.names=FALSE)

#temp profile
YunYangtempprofile<- YunYang_full %>%
  dplyr::select(dateTime, wtr0.0:wtr3.5) %>%
  rename_all(function(x) gsub("wtr", "wtr_", x)) %>% #Add an underscore to all heads to read wtr_#.#
  rename(wtr_0.1=wtr_0.0)
write.csv(YunYangtempprofile, "data/metab_data_clean/yunyang/YunYang_tempProfile.txt", row.names=FALSE)






# > Lake Taupo -------------------------------------------------------------


TaupoDOobs <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Taupo/Taupo_doobs.txt") 
Taupownd <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Taupo/Taupo_wnd.txt")
Taupotemp <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Taupo/Taupo_wtr.txt")
TaupoPAR <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Taupo/Taupo_SolRad(Wm-2).txt")

str(TaupoPAR)
str(TaupoDOobs)
str(Taupownd)
Taupo_full <- left_join(TaupoDOobs, Taupownd, by = c("DateTime", "Local.TZ", "daylightSavings","LakeID"))
Taupo_full <- left_join(Taupo_full, Taupotemp, by = c("DateTime", "Local.TZ", "daylightSavings","LakeID"))
Taupo_full <- left_join(Taupo_full, TaupoPAR, by = c("DateTime", "Local.TZ", "daylightSavings","LakeID"))
Taupo_full<- Taupo_full%>%
  rename(lakeID=LakeID,
         dateTime=DateTime,
         localTZ=Local.TZ,
         doObs=doObs4.9,
         solRad_Wm2=SolRad.W.m2.) %>%
  mutate(doObs_depth_m="4.9") 

####DATE CONVERSION###

Taupo_full <- Taupo_full %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="Etc/GMT+12"),
         dateTime = with_tz(dateTime, "GMT"),
         year = year(dateTime)) %>%
  dplyr::select(-localTZ, -daylightSavings)



TaupoPAR <- TaupoPAR %>%
  mutate(dateTime = ymd_hms(as.factor(DateTime)),
         dateTime = force_tz(dateTime, tz="Pacific/Auckland"),
         dateTime = with_tz(dateTime, "GMT"),
         year = year(dateTime))


glimpse(Taupo_full)
OlsonNames()
tz(Taupo_full$dateTime)
####plot DO###########
Taupo_full %>%
  mutate(year=year(dateTime))%>%
  filter(year %in% c("2017","2018"))%>%
  filter(dateTime >= "2017-05-01")%>%
  ggplot(aes(x=dateTime, y=doObs)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%m-%d") +
  ylab("DO obs")+
  ggtitle("Lake Taupo 2017-2018")+
  facet_wrap(.~year, scales="free_x")


####plot WIND###########
Taupo_full %>%
  mutate(year=year(dateTime))%>%
  filter(year %in% c("2017","2018"))%>%
  filter(dateTime >= "2017-05-01")%>%
  ggplot(aes(x=dateTime, y=wind)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "6 months", date_labels = "%y-%m-%d") +
  ylab("Wind (m/s)")+
  ggtitle("Lake Taupo 2015-2019")


####plot TEMPS###########
Taupo_full %>%
  mutate(year=year(dateTime))%>%
  filter(year %in% c("2017","2018"))%>%
  filter(dateTime >= "2017-05-01" & dateTime <= "2018-08-01")%>%
  dplyr::select(dateTime, wtr1.0:wtr150)%>%
  pivot_longer(-(dateTime), names_sep = "r", names_to = c("variable","depth")) %>%
  dplyr::select(-variable)%>%
  mutate(depth=as.numeric(as.character(depth)),
         year=year(dateTime)) %>%
  rename(temp=value)%>%
  ggplot(aes(x=dateTime, y=temp, color=depth)) + 
  geom_point(size=2)+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%y-%m-%d") +
  ylab("Temp C")+
  ggtitle("Lake Taupo 2015-2019")+
  scale_color_continuous_sequential(palette = "Dark Mint", rev=TRUE)
    # facet_wrap(.~year, scales="free_x")

Taupo.wtr = load.ts("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Taupo/Taupo_wtr.txt")
Taupo.wtr<- Taupo.wtr %>%
  dplyr::select(-lakeid, -local.tz,-daylightsavings) %>%
  rename_all(function(x) gsub("wtr", "wtr_", x)) #Add an underscore to all heads to read wtr_#.#
wtr.heat.map(Taupo.wtr)
##Commented out becausse it takes a long time to load (6 years!)



####plot Energy (W/m2)###########
Taupo_full %>%
  mutate(year=year(dateTime))%>%
  filter(year %in% c("2017","2018"))%>%
  filter(dateTime >= "2017-05-01" & dateTime <= "2018-08-01")%>%
ggplot(aes(x=dateTime, y=solRad_Wm2)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "6 months", date_labels = "%y-%m") +
  ylab("Solar radiation (W/m2)")+
  ggtitle("Lake Taupo 2015-2019")


Taupo_full %>%
  filter(dateTime >="2018-03-03" & dateTime <="2018-03-05") %>%
  mutate(date=date(dateTime))%>%
  mutate(PAR=solRad_Wm2*2.15) %>% #See "Estimating PAR from solar flux data" Word Doc in Troubleshooting folder
  ggplot(aes(x=dateTime, y=PAR, color=factor(date)))+
  geom_point()+
  ggtitle("Taupo")+
  scale_x_datetime(date_breaks = "4 hours",
                   date_labels = "%m-%d\n%H:%M")+
  theme(panel.grid.minor = element_line(color="lightgrey"),
        axis.text.x= element_text(size=8))



####export for modelling###########
taupoDOobs <- Taupo_full %>%
  dplyr::select(dateTime, doObs) %>%
  rename(DO=doObs) 
write.csv(taupoDOobs, "data/metab_data_clean/taupo/taupo_2015-2020_DO.txt", row.names=FALSE)

#DO sensor depth = 4.9 m
tauposensorTemp <- Taupo_full %>%
  dplyr::select(dateTime, wtr4.9) %>%
  rename(sensorTemp=wtr4.9) 
write.csv(tauposensorTemp, "data/metab_data_clean/taupo/taupo_2015-2020_sensorTemp.txt", row.names=FALSE)

#PAR
#convert W m-2 to PAR: conversion factor = 0.217 
taupoPAR <- Taupo_full %>%
  dplyr::select(dateTime, solRad_Wm2) %>%
  mutate(PAR=solRad_Wm2*2.15) %>% #See "Estimating PAR from solar flux data" Word Doc in Troubleshooting folder
  dplyr::select(-solRad_Wm2)
write.csv(taupoPAR, "data/metab_data_clean/taupo/taupo_2015-2020_PAR.txt", row.names=FALSE)

#wind height = 19m above lake surface, 15m above ground
taupowind <- Taupo_full %>%
  dplyr::select(dateTime, wind) %>%
  rename(windSpeed=wind) 
write.csv(taupowind, "data/metab_data_clean/taupo/taupo_2015-2020_windSpeed.txt", row.names=FALSE)

#temp profile
taupotempprofile<- Taupo_full %>%
  dplyr::select(dateTime, wtr1.0:wtr150) %>%
  rename_all(function(x) gsub("wtr", "wtr_", x)) #Add an underscore to all heads to read wtr_#.#
write.csv(taupotempprofile, "data/metab_data_clean/taupo/taupo_2015-2020_tempProfile.txt", row.names=FALSE)








# > Lake Almberga -------------------------------------------------------------


AlmbergaDOobs <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Almberga/Almberga_doobs.txt", stringsAsFactors = FALSE) %>%
  dplyr::select(-X)
Almbergawnd <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Almberga/Almberga_wnd.txt", stringsAsFactors = FALSE)
Almbergatemp <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Almberga/Almberga_wtr.txt", stringsAsFactors = FALSE)
AlmbergaPAR <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Almberga/Almberga_par.txt", stringsAsFactors = FALSE)


str(AlmbergaDOobs)
str(Almbergawnd)
Almbergawnd$wind<-as.numeric(as.character(Almbergawnd$wind))
str(Almbergatemp)
#And this is going to be annoying because they have 2 sets of temperature arrays
str(AlmbergaPAR)

#Max depth == 5.3m
#Sensors #1-12 keep all
#Can remove sensors 13,14,15 because of overlap with top-down thermistor
#Rename the columns to be relative to depth from lake surface
# Sensor 16 1.6m below surface
# Sensor 17 1.9m below surface
# Sensor 18 2.3m below surface
# Sensor 19 2.8m below surface
# Sensor 20 3.3m below surface
# Sensor 21 4.3m below surface
# Sensor 22 4.8m below surface
# Sensor 23 5.3m belove surface
Almbergatemp_short <- Almbergatemp %>%
  dplyr::select(-(wtr_13_.0.9.:wtr_15_.1.3.),
         -wtr_24_.Sed.) %>% #This last point is embedded in the sediment-- don't need.
  rename(wtr0.1=wtr_1_.0.1.,        
         wtr0.2=wtr_2_.0.2.,        
         wtr0.3=wtr_3_.0.3.,        
         wtr0.4=wtr_4_.0.4.,        
         wtr0.5=wtr_5_.0.5.,        
         wtr0.6=wtr_6_.0.6.,        
         wtr0.7=wtr_7_.0.7.,        
         wtr0.8=wtr_8_.0.8.,        
         wtr0.9=wtr_9_.0.9.,        
         wtr1.0=wtr_10_.1.0.,       
         wtr1.1=wtr_11_.1.1.,       
         wtr1.4=wtr_12_.1.4.,       
         wtr1.6=wtr_16_.1.6.,       
         wtr1.9=wtr_17_.1.9.,       
         wtr2.3=wtr_18_.2.3.,       
         wtr2.8=wtr_19_.2.8.,       
         wtr3.3=wtr_20_.3.3.,       
         wtr4.3=wtr_21_.4.3.,       
         wtr4.8=wtr_22_.4.8.,       
         wtr5.3=wtr_23_.Bottom_5.3.)
glimpse(Almbergatemp_short)



####DATE CONVERSION###
# Almberga_full$dateTime <- ymd_hm(as.factor(Almberga_full$dateTime))
# str(Almberga_full$dateTime)
# Almbergatemp_short$dateTime <- ymd_hm(as.factor(Almbergatemp_short$dateTime))
# force_tz(Almbergatemp_short$dateTime, tz="Etc/GMT-1") #This is confusing, because if you specify -1 it prints as GMT+1... 
# Almbergatemp_short$dateTimeGMT<-with_tz(dateTime, "GMT")
# AlmbergaDOobs$dateTime <- ymd_hm(as.factor(AlmbergaDOobs$dateTime))
# force_tz(AlmbergaDOobs$dateTime, tz="Etc/GMT-1")
# Almbergawnd$dateTime <- ymd_hm(as.factor(Almbergawnd$dateTime))
# force_tz(Almbergawnd$dateTime, tz="Etc/GMT-1")
# AlmbergaPAR$dateTime <- ymd_hm(as.factor(AlmbergaPAR$dateTime))
# force_tz(AlmbergaPAR$dateTime, tz="Etc/GMT-1")

Almbergatemp_short <- Almbergatemp_short %>%
  mutate(dateTime = ymd_hm(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="Etc/GMT-1"),
         dateTime = with_tz(dateTime, "GMT")) 
AlmbergaDOobs <- AlmbergaDOobs %>%
  mutate(dateTime = ymd_hm(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="Etc/GMT-1"),
         dateTime = with_tz(dateTime, "GMT")) 
Almbergawnd <- Almbergawnd %>%
  mutate(dateTime = ymd_hm(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="Etc/GMT-1"),
         dateTime = with_tz(dateTime, "GMT")) 
AlmbergaPAR <- AlmbergaPAR %>%
  mutate(dateTime = ymd_hm(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="Etc/GMT-1"),
         dateTime = with_tz(dateTime, "GMT")) 

tz(AlmbergaPAR$dateTime)

#Print first five time readings for each variable
firstFiveTimes <- data.frame(DO=AlmbergaDOobs$dateTime[1:5],
                             PAR=AlmbergaPAR$dateTime[1:5],
                             wind=Almbergawnd$dateTime[1:5],
                             temp=Almbergatemp_short$dateTime[1:5])
print('First five time readings for each variable'); print(firstFiveTimes)
#Ok this is going to be annoying because the times don't line up...

#Calculate first differences of time readings, display unique values
cat('\n','First differences of datetime','\n')
difTimesDO <- diff(AlmbergaDOobs$dateTime); print(table(difTimesDO))
difTimesPAR <- diff(AlmbergaPAR$dateTime); print(table(difTimesPAR))
difTimesWindSpeed <- diff(Almbergawnd$dateTime); print(table(difTimesWindSpeed))
difTimesSensorTemp <- diff(Almbergatemp_short$dateTime); print(table(difTimesSensorTemp))
timeStep=10	# in minutes


notDupRows <- findNotDupRows("AlmbergaDOobs")
AlmbergaDOobs <- AlmbergaDOobs[notDupRows,]

notDupRows <- findNotDupRows("AlmbergaPAR")
AlmbergaPAR <- AlmbergaPAR[notDupRows,]

notDupRows <- findNotDupRows("Almbergawnd")
Almbergawnd <- Almbergawnd[notDupRows,]

notDupRows <- findNotDupRows("Almbergatemp_short")
Almbergatemp_short <- Almbergatemp_short[notDupRows,]

#Make all data sets extend from startTime to endTime by timeStep
#Note that for some lakes it may be necessary to aggregate some variables to coarser time scale to get match up


AlmbergaDOobs$dateTime <- floorMins(AlmbergaDOobs)
AlmbergaPAR$dateTime <- floorMins(AlmbergaPAR)
Almbergawnd$dateTime <- floorMins(Almbergawnd)
Almbergatemp_short$dateTime <- floorMins(Almbergatemp_short)

tz(AlmbergaPAR$dateTime)

#Repeat check for dup rows in case any introduced during floorMins
notDupRows <- findNotDupRows("AlmbergaDOobs")
AlmbergaDOobs <- AlmbergaDOobs[notDupRows,]
notDupRows <- findNotDupRows("AlmbergaPAR")
AlmbergaPAR <- AlmbergaPAR[notDupRows,]
notDupRows <- findNotDupRows("Almbergawnd")
Almbergawnd <- Almbergawnd[notDupRows,]
notDupRows <- findNotDupRows("Almbergatemp_short")
Almbergatemp_short <- Almbergatemp_short[notDupRows,]

#Find the latest first time point and the earliest last time point of all the data
startTime <- max(min(AlmbergaDOobs$dateTime),min(AlmbergaPAR$dateTime),min(Almbergawnd$dateTime),min(Almbergatemp_short$dateTime))
endTime <- min(max(AlmbergaDOobs$dateTime),max(AlmbergaPAR$dateTime),max(Almbergawnd$dateTime),max(Almbergatemp_short$dateTime))

#Data.frame with one column "dateTime" which is sequence of times at time interval of timeStep, from startTime to endTime
completeTimes <- data.frame(dateTime=seq(startTime,endTime,paste(timeStep,"mins")))

#Merge all of input data.frames with completeTimes, so that they now all extend from startTime to endTime by timeStep
AlmbergaDOobs <- merge(completeTimes,AlmbergaDOobs,by="dateTime",all.x=T)
AlmbergaPAR <- merge(completeTimes,AlmbergaPAR,by="dateTime",all.x=T)
Almbergawnd <- merge(completeTimes,Almbergawnd,by="dateTime",all.x=T)
Almbergatemp_short <- merge(completeTimes,Almbergatemp_short,by="dateTime",all.x=T)


#Is it a problem that there are NAs in the PAR data? I guess we will find out
##IAO - 2021-02-12 --  I just checked and it shouldn't be because of the fillHoles helper function. 

Almberga_full <- left_join(AlmbergaDOobs, Almbergawnd, by = c("dateTime", "localTZ", "daylightSavings","lakeID"))
Almberga_full <- left_join(Almberga_full, Almbergatemp_short, by = c("dateTime", "localTZ", "daylightSavings","lakeID"))
Almberga_full <- left_join(Almberga_full, AlmbergaPAR, by = c("dateTime", "localTZ", "daylightSavings","lakeID"))

tz(Almberga_full$dateTime)
Almberga_full$dateTime


####plot TEMPS###########
#Based on the readme, it seems like there are two sensor arrays, and some of the depths overlap
#Specifically, sensor 9 and sensor 13 both measure 0.9m below surface (or 4.4m above sediment)
#Do they match? 
head(Almberga_full)
Almberga_full %>%
  dplyr::select(dateTime, wtr1.0:wtr5.3)%>%
  pivot_longer(-(dateTime), names_sep = "r", names_to = c("variable","depth")) %>%
  dplyr::select(-variable)%>%
  mutate(depth=as.numeric(as.character(depth)),
         year=year(dateTime)) %>%
  rename(temp=value)%>%
  ggplot(aes(x=dateTime, y=temp, color=depth)) + 
  geom_point(size=2)+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%y-%m-%d") +
  ylab("Temp C")+
  ggtitle("Lake Almberga 2019")+
  scale_color_continuous_sequential(palette = "Dark Mint", rev=TRUE)+
  facet_wrap(.~year, scales="free_x")

#plot heatmap
Almberga.wtr = load.ts("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Almberga/Almberga_wtr.txt")
Almberga.wtr <- Almberga.wtr %>%
  dplyr::select(-lakeid,-localtz,-daylightsavings,
         -(wtr_13_.0.9.:wtr_15_.1.3.),
         -wtr_24_.sed.,
         -wtr_20_.3.3.) %>% #This last point is embedded in the sediment-- don't need.
  rename(wtr_0.1=wtr_1_.0.1.,        
         wtr_0.2=wtr_2_.0.2.,        
         wtr_0.3=wtr_3_.0.3.,        
         wtr_0.4=wtr_4_.0.4.,        
         wtr_0.5=wtr_5_.0.5.,        
         wtr_0.6=wtr_6_.0.6.,        
         wtr_0.7=wtr_7_.0.7.,        
         wtr_0.8=wtr_8_.0.8.,        
         wtr_0.9=wtr_9_.0.9.,        
         wtr_1.0=wtr_10_.1.0.,       
         wtr_1.1=wtr_11_.1.1.,       
         wtr_1.4=wtr_12_.1.4.,       
         wtr_1.6=wtr_16_.1.6.,       
         wtr_1.9=wtr_17_.1.9.,       
         wtr_2.3=wtr_18_.2.3.,       
         wtr_2.8=wtr_19_.2.8.,       
         # wtr_3.3=wtr_20_.3.3.,       
         wtr_4.3=wtr_21_.4.3.,       
         wtr_4.8=wtr_22_.4.8.,       
         wtr_5.3=wtr_23_.bottom_5.3.)
# wtr.lineseries(Almberga.wtr, ylab = "Temperature C")
wtr.heat.map(Almberga.wtr)



####plot DO###########
ggplot(Almberga_full, aes(x=dateTime, y=doObs)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%y-%m") +
  ylab("DO obs")+
  ggtitle("Lake Almberga 2019")


####plot WIND###########
ggplot(Almberga_full, aes(x=dateTime, y=wind)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%y-%m") +
  ylab("Wind (m/s)")+
  ggtitle("Lake Almberga 2019")

####plot PAR###########
ggplot(Almberga_full, aes(x=dateTime, y=PAR)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%y-%m") +
  ylab("PAR")+
  ggtitle("Lake Almberga 2019")


####export for modelling###########
almbergaDOobs <- Almberga_full %>%
  dplyr::select(dateTime, doObs) %>%
  rename(DO=doObs) 
write.csv(almbergaDOobs, "data/metab_data_clean/almberga/almberga_2019_DO.txt", row.names=FALSE)

#DO sensor depth = 0.5
almbergasensorTemp <- Almberga_full %>%
  dplyr::select(dateTime, wtr0.5) %>%
  rename(sensorTemp=wtr0.5) 
write.csv(almbergasensorTemp, "data/metab_data_clean/almberga/almberga_2019_sensorTemp.txt", row.names=FALSE)

#PAR
almbergaPAR <- Almberga_full %>%
  dplyr::select(dateTime, PAR) 
write.csv(almbergaPAR, "data/metab_data_clean/almberga/almberga_2019_PAR.txt", row.names=FALSE)

#wind height = 2.16m above lake surface
almbergawind <- Almberga_full %>%
  dplyr::select(dateTime, wind) %>%
  rename(windSpeed=wind) 
write.csv(almbergawind, "data/metab_data_clean/almberga/almberga_2019_windSpeed.txt", row.names=FALSE)

#temp profile
almbergatempprofile<- Almberga_full %>%
  dplyr::select(dateTime, wtr0.1:wtr5.3) %>%
  rename_all(function(x) gsub("wtr", "wtr_", x)) #Add an underscore to all heads to read wtr_#.#
write.csv(almbergatempprofile, "data/metab_data_clean/almberga/almberga_2019_tempProfile.txt", row.names=FALSE)

### 2021-04-30 Checking on Simoncouche wtr profiles because the estimated zMix from the model
### is 30m which doesn't seem right. What does lakeAnalyzer/Metabolizer say? 
library(LakeMetabolizer)
Almberga_wtr<-almbergatempprofile %>%
  # mutate(datetime=ymd_hm(dateTime)) %>%
  filter(dateTime > "2019-05-01" & dateTime < "2019-10-01") %>%
  rename_all(function(x) gsub("temp", "wtr_", x)) %>% #Add an underscore to all heads to read wtr_#.#
select(-wtr_3.3)

wtr.heat.map(almbergatempprofile, plot.title="Almberga Water Temp (C)")
View(almbergatempprofile)

#calculate and plot the metalimnion depths
m.d = ts.meta.depths(Almberga_wtr)

plot(m.d$datetime, m.d$top, type='l', ylab='Meta Depths (m)', xlab='Date', col='blue')
lines(m.d$datetime, m.d$bottom, col='red')

#calculate and plot the thermocline depth
t.d = ts.thermo.depth(Almberga_wtr)
plot(t.d$datetime, t.d$thermo.depth, type='l', ylab='Thermocline Depth (m)', xlab='Date')
str(t.d)
mean(t.d$thermo.depth, na.rm=TRUE)
median(t.d$thermo.depth, na.rm=TRUE)
t.d$thermo.depth[is.nan(t.d$thermo.depth)]<-NA
t.d

AlmbergazMix<- t.d %>%
  mutate(solarDay=date(datetime))%>%
  rename(zMix=thermo.depth) %>%
  group_by(solarDay) %>%
  summarize(zMix=mean(zMix, na.rm=TRUE))

# AlmbergazMix$zMix[is.nan(AlmbergazMix$zMix)]<-NA
# AlmbergazMix

AlmbergazMix %>%
  # filter(solarDay > "2018-05-01" & solarDay < "2018-10-01") %>%
  # mutate(if_nan(NA))%>%
  summarize(meanzMix=mean(zMix, na.rm=TRUE),
            medianzMix=median(zMix, na.rm=TRUE))
write.table(AlmbergazMix, "results/model_output_raw/Almberga/AlmbergazMix.txt", row.names=FALSE)








# > Barco Lake (BARC, NEON) -------------------------------------------------------------


BarcDOobs <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/NEON sites/BARC/BARC_DO_2018-2019.csv") %>% dplyr::select(-X)
# Barcwnd <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/NEON sites/BARC/BARC_wind_2018-2019.csv")%>% dplyr::select(-X)
Barctemp <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/NEON sites/BARC/BARC_wtr_2018-2019.csv")
# BarcPAR <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/NEON sites/BARC/BARC_PAR_2018-2019.csv")%>% dplyr::select(-X)
Barcnuts <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/NEON sites/BARC/BARC_surfChem_2018-2019.csv")%>% dplyr::select(-X)

#Barco wind data sucks so pulling this from Ordway-Swisher Biological Station (OSBS)
#29.689282
#-81.993431
#2.02 km away from Barco lake in a straight line. 
#Located in the SUGG folder, because I did the same for that lake. 
Barcwnd <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/NEON sites/SUGG/SUGG_wind_2019.csv")%>% 
  dplyr::select(-X)

#Barco PAR data are also bad so pulling from  Ordway-Swisher Biological Station (OSBS)
#29.689282
#-81.993431
#2.02 km away from Barco lake in a straight line. 
BarcPAR <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/NEON sites/BARC/OSBS_PAR_2019.csv")%>% 
  dplyr::select(-X)


# Barctemp<-Barctemp %>%
  # mutate_all(funs(replace(, .<0, NA))) #every once in a while there are negative temp values... no idea why. Delete
  

# Data collation/cleaning -------------------------------------------------
#Recgonize dateTimes as datese 
####DATE CONVERSION###


Barctemp <- Barctemp %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="UTC"),
         dateTime = with_tz(dateTime, "GMT")) %>%
  dplyr::select(-tz, -lakeID)
BarcDOobs <- BarcDOobs %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="UTC"),
         dateTime = with_tz(dateTime, "GMT"))%>%
  dplyr::select(-tz, -lakeID)
Barcwnd <- Barcwnd %>%
  rename(dateTime = startDateTime)%>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="UTC"),
         dateTime = with_tz(dateTime, "GMT")) %>%
  dplyr::select(-siteID)
BarcPAR <- BarcPAR %>%
  rename(dateTime = startDateTime)%>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="UTC"),
         dateTime = with_tz(dateTime, "GMT")) %>%
  dplyr::select(-siteID)
Barcnuts <- Barcnuts %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="UTC"),
         dateTime = with_tz(dateTime, "GMT")) 

# Barctemp_alt$dateTime <- ymd_hms(Barctemp_alt$dateTime)
# Barctemp$dateTime <- ymd_hms(Barctemp$dateTime)
# BarcDOobs$dateTime <- ymd_hms(BarcDOobs$dateTime)
# Barcwnd$dateTime <- ymd_hms(Barcwnd$dateTime)
# BarcPAR$dateTime <- ymd_hms(BarcPAR$dateTime)
# Barcnuts$dateTime <- ymd_hms(Barcnuts$dateTime)


#Print first five time readings for each variable
firstFiveTimes <- data.frame(DO=BarcDOobs$dateTime[1:5],
                             PAR=BarcPAR$dateTime[1:5],
                             wind=Barcwnd$dateTime[1:5],
                             temp=Barctemp$dateTime[1:5])
print('First five time readings for each variable'); print(firstFiveTimes)
#Ok this is going to be annoying because the times don't line up...

#Calculate first differences of time readings, display unique values
cat('\n','First differences of datetime','\n')
difTimesDO <- diff(BarcDOobs$dateTime); print(table(difTimesDO))
difTimesPAR <- diff(BarcPAR$dateTime); print(table(difTimesPAR))
difTimesWindSpeed <- diff(Barcwnd$dateTime); print(table(difTimesWindSpeed))
difTimesSensorTemp <- diff(Barctemp$dateTime); print(table(difTimesSensorTemp))
# difTimesSensorTemp_alt <- diff(Barctemp_alt$dateTime); print(table(difTimesSensorTemp))

timeStep=15	# in minutes
#DOs kind of all over the place, but I think if we truncate to 15 minute timeSteps we'll be ok



notDupRows <- findNotDupRows("BarcDOobs")
BarcDOobs <- BarcDOobs[notDupRows,]
notDupRows <- findNotDupRows("BarcPAR")
BarcPAR <- BarcPAR[notDupRows,]
notDupRows <- findNotDupRows("Barcwnd")
Barcwnd <- Barcwnd[notDupRows,]
notDupRows <- findNotDupRows("Barctemp")
Barctemp <- Barctemp[notDupRows,]
# notDupRows <- findNotDupRows("Barctemp_alt")
# Barctemp_alt <- Barctemp_alt[notDupRows,]

#Make all data sets extend from startTime to endTime by timeStep
#Note that for some lakes it may be necessary to aggregate some variables to coarser time scale to get match up

#Round all time down to nearest timeStep (e.g. if timeStep is 5, round 00:07 to 00:05)
BarcDOobs$dateTime <- floorMins(BarcDOobs)
BarcPAR$dateTime <- floorMins(BarcPAR)
Barcwnd$dateTime <- floorMins(Barcwnd)
Barctemp$dateTime <- floorMins(Barctemp)
# Barctemp_alt$dateTime <- floorMins(Barctemp_alt)

#Repeat check for dup rows in case any introduced during floorMins
notDupRows <- findNotDupRows("BarcDOobs")
BarcDOobs <- BarcDOobs[notDupRows,]
notDupRows <- findNotDupRows("BarcPAR")
BarcPAR <- BarcPAR[notDupRows,]
notDupRows <- findNotDupRows("Barcwnd")
Barcwnd <- Barcwnd[notDupRows,]
notDupRows <- findNotDupRows("Barctemp")
Barctemp <- Barctemp[notDupRows,]
# notDupRows <- findNotDupRows("Barctemp_alt")
# Barctemp_alt <- Barctemp_alt[notDupRows,]

#Find the latest first time point and the earliest last time point of all the data
startTime <- max(min(BarcDOobs$dateTime),min(BarcPAR$dateTime),min(Barcwnd$dateTime),min(Barctemp$dateTime))
endTime <- min(max(BarcDOobs$dateTime),max(BarcPAR$dateTime),max(Barcwnd$dateTime),max(Barctemp$dateTime))

#Data.frame with one column "dateTime" which is sequence of times at time interval of timeStep, from startTime to endTime
completeTimes <- data.frame(dateTime=seq(startTime,endTime,paste(timeStep,"mins")))

#Merge all of input data.frames with completeTimes, so that they now all extend from startTime to endTime by timeStep
BarcDOobs <- merge(completeTimes,BarcDOobs,by="dateTime",all.x=T)
BarcPAR <- merge(completeTimes,BarcPAR,by="dateTime",all.x=T)
Barcwnd <- merge(completeTimes,Barcwnd,by="dateTime",all.x=T)
Barctemp <- merge(completeTimes,Barctemp,by="dateTime",all.x=T)
# Barctemp_alt <- merge(completeTimes,Barctemp_alt,by="dateTime",all.x=T)

# #Compare Barctemp and Barctemp_alt
# skim(Barctemp)
# skim(Barctemp_alt)

head(Barcwnd)
Barc_full <- left_join(BarcDOobs, Barcwnd, by = c("dateTime"))
Barc_full <- left_join(Barc_full, Barctemp, by = c("dateTime"))
Barc_full <- left_join(Barc_full, BarcPAR, by = c("dateTime"))

#For some reason (maybe can de-bug later) there are lots of duplicate rows.
#Use distinct function to pull out only distinct dateTimes.
Barc_full<-Barc_full %>%
  distinct(dateTime, .keep_all = TRUE)

#Plot data to see which year looks better. 2018 or 2019?


####plot DO###########
Barc_full %>%
  mutate(year=year(dateTime),
         DOY=yday(dateTime)) %>%
  filter(year=="2019")%>%
  filter(dateTime > "2019-05-06" & dateTime<"2019-06-10")%>%
  ggplot(aes(x=dateTime, y=DO)) + 
  geom_point()+
  # scale_x_datetime(date_breaks = "1 week", date_labels = "%m-%d") +
  scale_x_datetime(date_breaks = "4 hours") +
  ylab("DO obs")+
  facet_wrap(.~DOY, scales="free")+
  ggtitle("Barc Lake 2019")
#2019 looks best

####plot WIND###########
Barc_full %>%
  mutate(year=year(dateTime)) %>%
  ggplot(aes(x=dateTime, y=wind)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "2 months", date_labels = "%m-%d") +
  ylab("Wind (m/s)")+
  facet_wrap(.~year, scales="free")+
  ggtitle("Barc Lake 2018-2019")
#2019 looks best but some gaps in summer still... 

####plot TEMPS###########
Barc_full %>%
  dplyr::select(dateTime, wtr_0.05:wtr_3.05)%>%
  pivot_longer(-(dateTime), names_sep = "_", names_to = c("variable","depth")) %>%
  dplyr::select(-variable)%>%
  mutate(year=year(dateTime)) %>%
  mutate(depth=as.numeric(as.character(depth))) %>%
  rename(temp=value)%>%
  ggplot(aes(x=dateTime, y=temp, color=depth)) + 
  geom_point(size=2)+
  scale_x_datetime(date_breaks = "2 months", date_labels = "%m-%d") +
  scale_color_continuous_sequential(palette = "Dark Mint", rev=TRUE)+
  ylab("water temperature")+
  facet_wrap(.~year, scales="free")+
  ggtitle("Barc Lake 2018-2019")
#2018 is trash

str(Barc.wtr)
Barc.wtr = load.ts("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/NEON sites/BARC/BARC_wtr_2018-2019.txt")
Barc.wtr.2018<- Barc.wtr %>%
  dplyr::select(-lakeid, -tz) %>%
  mutate(datetime=ymd_hms(datetime),
         year=year(datetime))%>%
  filter(year=="2018") %>%
  dplyr::select(-year) %>%
  mutate_all(funs(replace(., .<0, NA))) %>%#every once in a while there are negative temp values... no idea why. Delete
  drop_na() 
Barc.wtr.2019<- Barc.wtr %>%
  dplyr::select(-lakeid, -tz) %>%
  mutate(datetime=ymd_hms(datetime),
         year=year(datetime))%>%
  filter(year=="2019") %>%
  dplyr::select(-year) %>%
  mutate_all(funs(replace(., .<0, NA))) %>% #every once in a while there are negative temp values... no idea why. Delete
  drop_na() 
wtr.heat.map(Barc.wtr.2018, plot.title="Barc Water Temp (C) - 2018")
wtr.heat.map(Barc.wtr.2019, plot.title="Barc Water Temp (C) - 2019")

####plot PAR###########
Barc_full %>%
  mutate(year=year(dateTime)) %>%
  ggplot(aes(x=dateTime, y=PAR)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "2 months", date_labels = "%m-%d") +
  ylab("PAR")+
  facet_wrap(.~year, scales="free")+
  ggtitle("Barc Lake 2018-2019")

Barc_full %>%
  mutate(year=year(dateTime)) %>%
  filter(dateTime > "2019-05-01" & dateTime < "2019-05-30") %>%
  ggplot(aes(x=dateTime, y=PAR)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "4 days", date_labels = "%m-%d") +
  ylab("PAR")+
  facet_wrap(.~year, scales="free")+
  ggtitle("Barc Lake 2018-2019")




####plot nutrients###########
Barcnuts %>%
  mutate(dateTime = as_datetime(dateTime), year=year(dateTime)) %>%
  dplyr::select(year, dateTime, DOC_mgL, TP_mgL, TN_mgL)%>%
  pivot_longer(-(1:2), names_to = "nutrient") %>%
  ggplot(aes(x=dateTime, y=value, color=nutrient)) + 
  geom_point()+geom_line()+
  scale_x_datetime(date_breaks = "2 months", date_labels = "%m-%d") +
  ggtitle("Barc Lake 2018-2019") +
  facet_wrap(year~nutrient, scales="free", nrow=2)


# FINAL Barc dataset ------------------------------------------------------
Barc_full_2019<-Barc_full %>%
  mutate(year=year(dateTime)) %>%
  filter(year=="2019")
glimpse(Barc_full_2019)


barc_summary<-Barc_full_2019 %>%
  mutate(date=yday(dateTime))%>%
  group_by(date) %>%
  summarize(count_PAR=n_unique(PARMean),
            count_wind=n_unique(windSpeedMean),
            count_DO=n_unique(DO))

barc_summary<-Barc_full_2019 %>%
  mutate(date=yday(dateTime))%>%
  group_by(date) %>%
  summarize(count_PAR=n_unique(PARMean),
            count_wind=n_unique(windSpeedMean),
            count_DO=n_unique(DO))

####export for modelling###########
barcoDOobs <- Barc_full_2019 %>%
  dplyr::select(dateTime, DO) 
write.csv(barcoDOobs, "data/metab_data_clean/barco/barco_2019_DO.txt", row.names=FALSE)

#DO sensor depth = 0.5m
barcosensorTemp <- Barc_full_2019 %>%
  dplyr::select(dateTime, wtr_0.55) %>%
  rename(sensorTemp=wtr_0.55) 
write.csv(barcosensorTemp, "data/metab_data_clean/barco/barco_2019_sensorTemp.txt", row.names=FALSE)

#PAR
barcoPAR <- Barc_full_2019 %>%
  dplyr::select(dateTime, PARMean) %>%
  rename(PAR = PARMean)
write.csv(barcoPAR, "data/metab_data_clean/barco/barco_2019_PAR.txt", row.names=FALSE)

#wind height = 3 m above ground
#from Ordway-Swisher Biological Station (OSBS), 2km to the north
barcowind <- Barc_full_2019 %>%
  dplyr::select(dateTime, windSpeedMean) %>%
  rename(windSpeed=windSpeedMean) 
write.csv(barcowind, "data/metab_data_clean/barco/barco_2019_windSpeed.txt", row.names=FALSE)

#temp profile
barcotempprofile<- Barc_full_2019 %>%
  dplyr::select(dateTime, wtr_0.05:wtr_3.05)
write.csv(barcotempprofile, "data/metab_data_clean/barco/barco_2019_tempProfile.txt", row.names=FALSE)














# > Little Rock Lake (LIRO, NEON) -------------------------------------------------------------


LiroDOobs <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/NEON sites/LIRO/LIRO_DO_2018-2019.csv") %>% dplyr::select(-X)
Lirownd <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/NEON sites/LIRO/LIRO_wind_2018-2019.csv")%>% dplyr::select(-X)
Lirotemp <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/NEON sites/LIRO/LIRO_wtr_2018-2019.csv")
LiroPAR <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/NEON sites/LIRO/LIRO_PAR_2018-2019.csv")%>% dplyr::select(-X)
Lironuts <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/NEON sites/LIRO/LIRO_surfChem_2018-2019.csv")%>% dplyr::select(-X)


# Data collation/cleaning -------------------------------------------------
#Recgonize dateTimes as datese 
####DATE CONVERSION###

Lirotemp <- Lirotemp %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="UTC"),
         dateTime = with_tz(dateTime, "GMT")) 
LiroDOobs <- LiroDOobs %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="UTC"),
         dateTime = with_tz(dateTime, "GMT"))
Lirownd <- Lirownd %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="UTC"),
         dateTime = with_tz(dateTime, "GMT")) 
LiroPAR <- LiroPAR %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="UTC"),
         dateTime = with_tz(dateTime, "GMT")) 
Lironuts <- Lironuts %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="UTC"),
         dateTime = with_tz(dateTime, "GMT")) 


#Print first five time readings for each variable
firstFiveTimes <- data.frame(DO=LiroDOobs$dateTime[1:5],
                             PAR=LiroPAR$dateTime[1:5],
                             wind=Lirownd$dateTime[1:5],
                             temp=Lirotemp$dateTime[1:5])
print('First five time readings for each variable'); print(firstFiveTimes)
#Ok this is going to be annoying because the times don't line up...

#Calculate first differences of time readings, display unique values
cat('\n','First differences of datetime','\n')
difTimesDO <- diff(LiroDOobs$dateTime); print(table(difTimesDO))
difTimesPAR <- diff(LiroPAR$dateTime); print(table(difTimesPAR))
difTimesWindSpeed <- diff(Lirownd$dateTime); print(table(difTimesWindSpeed))
difTimesSensorTemp <- diff(Lirotemp$dateTime); print(table(difTimesSensorTemp))
# difTimesSensorTemp_alt <- diff(Lirotemp_alt$dateTime); print(table(difTimesSensorTemp))

timeStep=15	# in minutes
#DOs kind of all over the place, but I think if we truncate to 15 minute timeSteps we'll be ok



notDupRows <- findNotDupRows("LiroDOobs")
LiroDOobs <- LiroDOobs[notDupRows,]
notDupRows <- findNotDupRows("LiroPAR")
LiroPAR <- LiroPAR[notDupRows,]
notDupRows <- findNotDupRows("Lirownd")
Lirownd <- Lirownd[notDupRows,]
notDupRows <- findNotDupRows("Lirotemp")
Lirotemp <- Lirotemp[notDupRows,]
# notDupRows <- findNotDupRows("Lirotemp_alt")
# Lirotemp_alt <- Lirotemp_alt[notDupRows,]

#Make all data sets extend from startTime to endTime by timeStep
#Note that for some lakes it may be necessary to aggregate some variables to coarser time scale to get match up

#Round all time down to nearest timeStep (e.g. if timeStep is 5, round 00:07 to 00:05)
LiroDOobs$dateTime <- floorMins(LiroDOobs)
LiroPAR$dateTime <- floorMins(LiroPAR)
Lirownd$dateTime <- floorMins(Lirownd)
Lirotemp$dateTime <- floorMins(Lirotemp)
# Lirotemp_alt$dateTime <- floorMins(Lirotemp_alt)

#Repeat check for dup rows in case any introduced during floorMins
notDupRows <- findNotDupRows("LiroDOobs")
LiroDOobs <- LiroDOobs[notDupRows,]
notDupRows <- findNotDupRows("LiroPAR")
LiroPAR <- LiroPAR[notDupRows,]
notDupRows <- findNotDupRows("Lirownd")
Lirownd <- Lirownd[notDupRows,]
notDupRows <- findNotDupRows("Lirotemp")
Lirotemp <- Lirotemp[notDupRows,]
# notDupRows <- findNotDupRows("Lirotemp_alt")
# Lirotemp_alt <- Lirotemp_alt[notDupRows,]

#Find the latest first time point and the earliest last time point of all the data
startTime <- max(min(LiroDOobs$dateTime),min(LiroPAR$dateTime),min(Lirownd$dateTime),min(Lirotemp$dateTime))
endTime <- min(max(LiroDOobs$dateTime),max(LiroPAR$dateTime),max(Lirownd$dateTime),max(Lirotemp$dateTime))

#Data.frame with one column "dateTime" which is sequence of times at time interval of timeStep, from startTime to endTime
completeTimes <- data.frame(dateTime=seq(startTime,endTime,paste(timeStep,"mins")))

#Merge all of input data.frames with completeTimes, so that they now all extend from startTime to endTime by timeStep
LiroDOobs <- merge(completeTimes,LiroDOobs,by="dateTime",all.x=T)
LiroPAR <- merge(completeTimes,LiroPAR,by="dateTime",all.x=T)
Lirownd <- merge(completeTimes,Lirownd,by="dateTime",all.x=T)
Lirotemp <- merge(completeTimes,Lirotemp,by="dateTime",all.x=T)
# Lirotemp_alt <- merge(completeTimes,Lirotemp_alt,by="dateTime",all.x=T)


Liro_full <- left_join(LiroDOobs, Lirownd, by = c("dateTime", "tz","lakeID"))
Liro_full <- left_join(Liro_full, Lirotemp, by = c("dateTime", "tz","lakeID"))
Liro_full <- left_join(Liro_full, LiroPAR, by = c("dateTime", "tz","lakeID"))

#For some reason (maybe can de-bug later) there are lots of duplicate rows.
#Use distinct function to pull out only distinct dateTimes.
Liro_full<-Liro_full %>%
  distinct(dateTime, .keep_all = TRUE)

#Plot data to see which year looks better. 2018 or 2019?


####plot DO###########
Liro_full %>%
  mutate(year=year(dateTime)) %>%
  filter(year=="2018")%>%
  ggplot(aes(x=dateTime, y=DO)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%m-%d") +
  ylab("DO obs")+
  facet_wrap(.~year, scales="free")+
  ggtitle("Liro Lake 2018-2019")
#2018 looks best

####plot WIND###########
Liro_full %>%
  mutate(year=year(dateTime)) %>%
  ggplot(aes(x=dateTime, y=wind)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "2 months", date_labels = "%m-%d") +
  ylab("Wind (m/s)")+
  facet_wrap(.~year, scales="free")+
  ggtitle("Liro Lake 2018-2019")
#2018 looks best

####plot TEMPS###########
Liro_full %>%
  dplyr::select(dateTime, wtr_0.05:wtr_4.5)%>%
  pivot_longer(-(dateTime), names_sep = "_", names_to = c("variable","depth")) %>%
  dplyr::select(-variable)%>%
  mutate(year=year(dateTime)) %>%
  mutate(depth=as.numeric(as.character(depth))) %>%
  rename(temp=value)%>%
  ggplot(aes(x=dateTime, y=temp, color=depth)) + 
  geom_point(size=2)+
  scale_x_datetime(date_breaks = "2 months", date_labels = "%m-%d") +
  scale_color_continuous_sequential(palette = "Dark Mint", rev=TRUE)+
  ylab("water temperature")+
  facet_wrap(.~year, scales="free")+
  ggtitle("Liro Lake 2018-2019")
#2019 is ok but 2018 is best.
str(Liro.wtr)
# Liro.wtr = load.ts("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/NEON sites/Liro/Liro_wtr_2018-2019.txt")
# Liro.wtr.2018<- Liro.wtr %>%
#   dplyr::select(-lakeid, -tz) %>%
#   mutate(datetime=ymd_hms(datetime),
#          year=year(datetime))%>%
#   filter(year=="2018") %>%
#   dplyr::select(-year) %>%
#   mutate_all(funs(replace(., .<0, NA))) %>%#every once in a while there are negative temp values... no idea why. Delete
#   drop_na() 
# Liro.wtr.2019<- Liro.wtr %>%
#   dplyr::select(-lakeid, -tz) %>%
#   mutate(datetime=ymd_hms(datetime),
#          year=year(datetime))%>%
#   filter(year=="2019") %>%
#   dplyr::select(-year) %>%
#   mutate_all(funs(replace(., .<0, NA))) %>% #every once in a while there are negative temp values... no idea why. Delete
#   drop_na() 
# wtr.heat.map(Liro.wtr.2018, plot.title="Liro Water Temp (C) - 2018")
# wtr.heat.map(Liro.wtr.2019, plot.title="Liro Water Temp (C) - 2019")

####plot PAR###########
Liro_full %>%
  mutate(year=year(dateTime)) %>%
  ggplot(aes(x=dateTime, y=PAR)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "2 months", date_labels = "%m-%d") +
  ylab("PAR")+
  facet_wrap(.~year, scales="free")+
  ggtitle("Liro Lake 2018-2019")
#2018 is best

####plot nutrients###########
Lironuts %>%
  mutate(dateTime = as_datetime(dateTime), year=year(dateTime)) %>%
  dplyr::select(year, dateTime, DOC_mgL, TP_mgL, TN_mgL)%>%
  pivot_longer(-(1:2), names_to = "nutrient") %>%
  ggplot(aes(x=dateTime, y=value, color=nutrient)) + 
  geom_point()+geom_line()+
  scale_x_datetime(date_breaks = "2 months", date_labels = "%m-%d") +
  ggtitle("Liro Lake 2018-2019") +
  facet_wrap(year~nutrient, scales="free", nrow=2)
#A big TP spike in 2018! holy cow


# FINAL Liro dataset ------------------------------------------------------
Liro_full_2018<-Liro_full %>%
  mutate(year=year(dateTime)) %>%
  filter(year=="2018")
head(Liro_full_2018)

liro_summary<-Liro_full_2018 %>%
  mutate(date=yday(dateTime))%>%
  group_by(date) %>%
  summarize(count_PAR=n_unique(PAR),
            count_wind=n_unique(wind),
            count_DO=n_unique(DO))

head(data1)
data2_summary<-data2 %>%
  mutate(date=yday(dateTime))%>%
  group_by(date) %>%
  summarize(count_DOobs=n_unique(DOObs),
            count_DOsat=n_unique(DOSat),
            count_irr=n_unique(irr))
data1_summary<-data1 %>%
  mutate(date=yday(dateTime))%>%
  group_by(date) %>%
  summarize(count_DO=n_unique(DO),
            count_PAR=n_unique(PAR),
            count_wind=n_unique(windSpeed),
            count_temp=n_unique(sensorTemp))

####export for modelling###########
liroDOobs <- Liro_full_2018 %>%
  dplyr::select(dateTime, DO) 
write.csv(liroDOobs, "data/metab_data_clean/littlerock/littlerock_2018_DO.txt", row.names=FALSE)

#DO sensor depth = 0.5m
lirosensorTemp <- Liro_full_2018 %>%
  dplyr::select(dateTime, wtr_0.5) %>%
  rename(sensorTemp=wtr_0.5) 
write.csv(lirosensorTemp, "data/metab_data_clean/littlerock/littlerock_2018_sensorTemp.txt", row.names=FALSE)

#PAR
liroPAR <- Liro_full_2018 %>%
  dplyr::select(dateTime, PAR) 
write.csv(liroPAR, "data/metab_data_clean/littlerock/littlerock_2018_PAR.txt", row.names=FALSE)

#wind height = 2.85 m above lake surface
lirowind <- Liro_full_2018 %>%
  dplyr::select(dateTime, wind) %>%
  rename(windSpeed=wind) 
write.csv(lirowind, "data/metab_data_clean/littlerock/littlerock_2018_windSpeed.txt", row.names=FALSE)

#temp profile
lirotempprofile<- Liro_full_2018 %>%
  dplyr::select(dateTime, wtr_0.05:wtr_4.5)
write.csv(lirotempprofile, "data/metab_data_clean/littlerock/littlerock_2018_tempProfile.txt", row.names=FALSE)






# > Prairie lake (PRLA, NEON) -------------------------------------------------------------


PrlaDOobs <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/NEON sites/PRLA/PRLA_DO_2018-2019.csv") %>% dplyr::select(-X)
Prlawnd <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/NEON sites/PRLA/PRLA_wind_2018-2019.csv")%>% dplyr::select(-X)
Prlatemp <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/NEON sites/PRLA/PRLA_wtr_2018-2019.csv")
PrlaPAR <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/NEON sites/PRLA/PRLA_PAR_2018-2019.csv")%>% dplyr::select(-X)
Prlanuts <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/NEON sites/PRLA/PRLA_surfChem_2018-2019.csv")%>% dplyr::select(-X)


# Data collation/cleaning -------------------------------------------------
#Recgonize dateTimes as datese 
####DATE CONVERSION###


Prlatemp <- Prlatemp %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="UTC"),
         dateTime = with_tz(dateTime, "GMT")) 
PrlaDOobs <- PrlaDOobs %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="UTC"),
         dateTime = with_tz(dateTime, "GMT"))
Prlawnd <- Prlawnd %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="UTC"),
         dateTime = with_tz(dateTime, "GMT")) 
PrlaPAR <- PrlaPAR %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="UTC"),
         dateTime = with_tz(dateTime, "GMT")) 
Prlanuts <- Prlanuts %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="UTC"),
         dateTime = with_tz(dateTime, "GMT")) 


#Print first five time readings for each variable
firstFiveTimes <- data.frame(DO=PrlaDOobs$dateTime[1:5],
                             PAR=PrlaPAR$dateTime[1:5],
                             wind=Prlawnd$dateTime[1:5],
                             temp=Prlatemp$dateTime[1:5])
print('First five time readings for each variable'); print(firstFiveTimes)
#Ok this is going to be annoying because the times don't line up...

#Calculate first differences of time readings, display unique values
cat('\n','First differences of datetime','\n')
difTimesDO <- diff(PrlaDOobs$dateTime); print(table(difTimesDO))
difTimesPAR <- diff(PrlaPAR$dateTime); print(table(difTimesPAR))
difTimesWindSpeed <- diff(Prlawnd$dateTime); print(table(difTimesWindSpeed))
difTimesSensorTemp <- diff(Prlatemp$dateTime); print(table(difTimesSensorTemp))
# difTimesSensorTemp_alt <- diff(Prlatemp_alt$dateTime); print(table(difTimesSensorTemp))

timeStep=15	# in minutes
#DOs kind of all over the place, but I think if we truncate to 15 minute timeSteps we'll be ok



notDupRows <- findNotDupRows("PrlaDOobs")
PrlaDOobs <- PrlaDOobs[notDupRows,]
notDupRows <- findNotDupRows("PrlaPAR")
PrlaPAR <- PrlaPAR[notDupRows,]
notDupRows <- findNotDupRows("Prlawnd")
Prlawnd <- Prlawnd[notDupRows,]
notDupRows <- findNotDupRows("Prlatemp")
Prlatemp <- Prlatemp[notDupRows,]
# notDupRows <- findNotDupRows("Prlatemp_alt")
# Prlatemp_alt <- Prlatemp_alt[notDupRows,]

#Make all data sets extend from startTime to endTime by timeStep
#Note that for some lakes it may be necessary to aggregate some variables to coarser time scale to get match up

#Round all time down to nearest timeStep (e.g. if timeStep is 5, round 00:07 to 00:05)
PrlaDOobs$dateTime <- floorMins(PrlaDOobs)
PrlaPAR$dateTime <- floorMins(PrlaPAR)
Prlawnd$dateTime <- floorMins(Prlawnd)
Prlatemp$dateTime <- floorMins(Prlatemp)
# Prlatemp_alt$dateTime <- floorMins(Prlatemp_alt)

#Repeat check for dup rows in case any introduced during floorMins
notDupRows <- findNotDupRows("PrlaDOobs")
PrlaDOobs <- PrlaDOobs[notDupRows,]
notDupRows <- findNotDupRows("PrlaPAR")
PrlaPAR <- PrlaPAR[notDupRows,]
notDupRows <- findNotDupRows("Prlawnd")
Prlawnd <- Prlawnd[notDupRows,]
notDupRows <- findNotDupRows("Prlatemp")
Prlatemp <- Prlatemp[notDupRows,]
# notDupRows <- findNotDupRows("Prlatemp_alt")
# Prlatemp_alt <- Prlatemp_alt[notDupRows,]

#Find the latest first time point and the earliest last time point of all the data
startTime <- max(min(PrlaDOobs$dateTime),min(PrlaPAR$dateTime),min(Prlawnd$dateTime),min(Prlatemp$dateTime))
endTime <- min(max(PrlaDOobs$dateTime),max(PrlaPAR$dateTime),max(Prlawnd$dateTime),max(Prlatemp$dateTime))

#Data.frame with one column "dateTime" which is sequence of times at time interval of timeStep, from startTime to endTime
completeTimes <- data.frame(dateTime=seq(startTime,endTime,paste(timeStep,"mins")))

#Merge all of input data.frames with completeTimes, so that they now all extend from startTime to endTime by timeStep
PrlaDOobs <- merge(completeTimes,PrlaDOobs,by="dateTime",all.x=T)
PrlaPAR <- merge(completeTimes,PrlaPAR,by="dateTime",all.x=T)
Prlawnd <- merge(completeTimes,Prlawnd,by="dateTime",all.x=T)
Prlatemp <- merge(completeTimes,Prlatemp,by="dateTime",all.x=T)
# Prlatemp_alt <- merge(completeTimes,Prlatemp_alt,by="dateTime",all.x=T)

# #Compare Prlatemp and Prlatemp_alt
# skim(Prlatemp)
# skim(Prlatemp_alt)


#Is it a problem that there are NAs in the PAR data? I guess we will find out

Prla_full <- left_join(PrlaDOobs, Prlawnd, by = c("dateTime", "tz","lakeID"))
Prla_full <- left_join(Prla_full, Prlatemp, by = c("dateTime", "tz","lakeID"))
Prla_full <- left_join(Prla_full, PrlaPAR, by = c("dateTime", "tz","lakeID"))

#For some reason (maybe can de-bug later) there are lots of duplicate rows.
#Use distinct function to pull out only distinct dateTimes.
Prla_full<-Prla_full %>%
  distinct(dateTime, .keep_all = TRUE)

#Plot data to see which year looks better. 2018 or 2019?


####plot DO###########
Prla_full %>%
  mutate(year=year(dateTime)) %>%
  ggplot(aes(x=dateTime, y=DO)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%m-%d") +
  ylab("DO obs")+
  facet_wrap(.~year, scales="free")+
  ggtitle("Prla Lake 2018-2019")
#2018 looks best

####plot WIND###########
Prla_full %>%
  mutate(year=year(dateTime)) %>%
  ggplot(aes(x=dateTime, y=wind)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "2 months", date_labels = "%m-%d") +
  ylab("Wind (m/s)")+
  facet_wrap(.~year, scales="free")+
  ggtitle("Prla Lake 2018-2019")
#2018 looks best

####plot TEMPS###########
Prla_full %>%
  dplyr::select(dateTime, wtr_0.05:wtr_4.5)%>%
  pivot_longer(-(dateTime), names_sep = "_", names_to = c("variable","depth")) %>%
  dplyr::select(-variable)%>%
  mutate(year=year(dateTime)) %>%
  mutate(depth=as.numeric(as.character(depth))) %>%
  rename(temp=value)%>%
  ggplot(aes(x=dateTime, y=temp, color=depth)) + 
  geom_point(size=2)+
  scale_x_datetime(date_breaks = "2 months", date_labels = "%m-%d") +
  scale_color_continuous_sequential(palette = "Dark Mint", rev=TRUE)+
  ylab("water temperature")+
  facet_wrap(.~year, scales="free")+
  ggtitle("Prla Lake 2018-2019")
#2019 is ok but 2018 is best.
str(Prla.wtr)
# Prla.wtr = load.ts("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/NEON sites/Prla/Prla_wtr_2018-2019.txt")
# Prla.wtr.2018<- Prla.wtr %>%
#   dplyr::select(-lakeid, -tz) %>%
#   mutate(datetime=ymd_hms(datetime),
#          year=year(datetime))%>%
#   filter(year=="2018") %>%
#   dplyr::select(-year) %>%
#   mutate_all(funs(replace(., .<0, NA))) %>%#every once in a while there are negative temp values... no idea why. Delete
#   drop_na() 
# Prla.wtr.2019<- Prla.wtr %>%
#   dplyr::select(-lakeid, -tz) %>%
#   mutate(datetime=ymd_hms(datetime),
#          year=year(datetime))%>%
#   filter(year=="2019") %>%
#   dplyr::select(-year) %>%
#   mutate_all(funs(replace(., .<0, NA))) %>% #every once in a while there are negative temp values... no idea why. Delete
#   drop_na() 
# wtr.heat.map(Prla.wtr.2018, plot.title="Prla Water Temp (C) - 2018")
# wtr.heat.map(Prla.wtr.2019, plot.title="Prla Water Temp (C) - 2019")

####plot PAR###########
Prla_full %>%
  mutate(year=year(dateTime)) %>%
  ggplot(aes(x=dateTime, y=PAR)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "2 months", date_labels = "%m-%d") +
  ylab("PAR")+
  facet_wrap(.~year, scales="free")+
  ggtitle("Prla Lake 2018-2019")
#2018 is best

####plot nutrients###########
Prlanuts %>%
  mutate(dateTime = as_datetime(dateTime), year=year(dateTime)) %>%
  dplyr::select(year, dateTime, DOC_mgL, TP_mgL, TN_mgL)%>%
  pivot_longer(-(1:2), names_to = "nutrient") %>%
  ggplot(aes(x=dateTime, y=value, color=nutrient)) + 
  geom_point()+geom_line()+
  scale_x_datetime(date_breaks = "2 months", date_labels = "%m-%d") +
  ggtitle("Prla Lake 2018-2019") +
  facet_wrap(year~nutrient, scales="free", nrow=2)
#A big TP spike in 2018! holy cow


# FINAL Prla dataset ------------------------------------------------------


Prla_full_2018<-Prla_full %>%
  mutate(year=year(dateTime)) %>%
  filter(year=="2018")




# export for modeling -----------------------------------------------------


prlaDOobs <- Prla_full_2018 %>%
  dplyr::select(dateTime, DO) 
write.csv(prlaDOobs, "data/metab_data_clean/prairielake/prairielake_2018_DO.txt", row.names=FALSE)

#DO sensor depth = 0.5m
prlasensorTemp <- Prla_full_2018 %>%
  dplyr::select(dateTime, wtr_0.55) %>%
  rename(sensorTemp=wtr_0.55) 
write.csv(prlasensorTemp, "data/metab_data_clean/prairielake/prairielake_2018_sensorTemp.txt", row.names=FALSE)

#PAR
prlaPAR <- Prla_full_2018 %>%
  dplyr::select(dateTime, PAR) 
write.csv(prlaPAR, "data/metab_data_clean/prairielake/prairielake_2018_PAR.txt", row.names=FALSE)

#wind height = 2.84 m above lake surface
prlawind <- Prla_full_2018 %>%
  dplyr::select(dateTime, wind) %>%
  rename(windSpeed=wind) 
write.csv(prlawind, "data/metab_data_clean/prairielake/prairielake_2018_windSpeed.txt", row.names=FALSE)

#temp profile
prlatempprofile<- Prla_full_2018 %>%
  dplyr::select(dateTime, wtr_0.05:wtr_2.05)
write.csv(prlatempprofile, "data/metab_data_clean/prairielake/prairielake_2018_tempProfile.txt", row.names=FALSE)







# > Prairie pothole (PRPO, NEON) -------------------------------------------------------------


PRPODOobs <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/NEON sites/PRPO/PRPO_DO_2018-2019.csv") %>% dplyr::select(-X)
PRPOwnd <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/NEON sites/PRPO/PRPO_wind_2018-2019.csv")%>% dplyr::select(-X)
PRPOtemp <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/NEON sites/PRPO/PRPO_wtr_2018-2019.csv")
PRPOPAR <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/NEON sites/PRPO/PRPO_PAR_2018-2019.csv")%>% dplyr::select(-X)
PRPOnuts <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/NEON sites/PRPO/PRPO_surfChem_2018-2019.csv")%>% dplyr::select(-X)


# Data collation/cleaning -------------------------------------------------
#Recgonize dateTimes as datese 
####DATE CONVERSION###


PRPOtemp <- PRPOtemp %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="UTC"),
         dateTime = with_tz(dateTime, "GMT")) 
PRPODOobs <- PRPODOobs %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="UTC"),
         dateTime = with_tz(dateTime, "GMT"))
PRPOwnd <- PRPOwnd %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="UTC"),
         dateTime = with_tz(dateTime, "GMT")) 
PRPOPAR <- PRPOPAR %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="UTC"),
         dateTime = with_tz(dateTime, "GMT")) 
PRPOnuts <- PRPOnuts %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="UTC"),
         dateTime = with_tz(dateTime, "GMT")) 


#Print first five time readings for each variable
firstFiveTimes <- data.frame(DO=PRPODOobs$dateTime[1:5],
                             PAR=PRPOPAR$dateTime[1:5],
                             wind=PRPOwnd$dateTime[1:5],
                             temp=PRPOtemp$dateTime[1:5])
print('First five time readings for each variable'); print(firstFiveTimes)
#Ok this is going to be annoying because the times don't line up...

#Calculate first differences of time readings, display unique values
cat('\n','First differences of datetime','\n')
difTimesDO <- diff(PRPODOobs$dateTime); print(table(difTimesDO))
difTimesPAR <- diff(PRPOPAR$dateTime); print(table(difTimesPAR))
difTimesWindSpeed <- diff(PRPOwnd$dateTime); print(table(difTimesWindSpeed))
difTimesSensorTemp <- diff(PRPOtemp$dateTime); print(table(difTimesSensorTemp))
# difTimesSensorTemp_alt <- diff(PRPOtemp_alt$dateTime); print(table(difTimesSensorTemp))

timeStep=15	# in minutes
#DOs kind of all over the place, but I think if we truncate to 15 minute timeSteps we'll be ok



notDupRows <- findNotDupRows("PRPODOobs")
PRPODOobs <- PRPODOobs[notDupRows,]
notDupRows <- findNotDupRows("PRPOPAR")
PRPOPAR <- PRPOPAR[notDupRows,]
notDupRows <- findNotDupRows("PRPOwnd")
PRPOwnd <- PRPOwnd[notDupRows,]
notDupRows <- findNotDupRows("PRPOtemp")
PRPOtemp <- PRPOtemp[notDupRows,]
# notDupRows <- findNotDupRows("PRPOtemp_alt")
# PRPOtemp_alt <- PRPOtemp_alt[notDupRows,]

#Make all data sets extend from startTime to endTime by timeStep
#Note that for some lakes it may be necessary to aggregate some variables to coarser time scale to get match up

#Round all time down to nearest timeStep (e.g. if timeStep is 5, round 00:07 to 00:05)
PRPODOobs$dateTime <- floorMins(PRPODOobs)
PRPOPAR$dateTime <- floorMins(PRPOPAR)
PRPOwnd$dateTime <- floorMins(PRPOwnd)
PRPOtemp$dateTime <- floorMins(PRPOtemp)
# PRPOtemp_alt$dateTime <- floorMins(PRPOtemp_alt)

#Repeat check for dup rows in case any introduced during floorMins
notDupRows <- findNotDupRows("PRPODOobs")
PRPODOobs <- PRPODOobs[notDupRows,]
notDupRows <- findNotDupRows("PRPOPAR")
PRPOPAR <- PRPOPAR[notDupRows,]
notDupRows <- findNotDupRows("PRPOwnd")
PRPOwnd <- PRPOwnd[notDupRows,]
notDupRows <- findNotDupRows("PRPOtemp")
PRPOtemp <- PRPOtemp[notDupRows,]
# notDupRows <- findNotDupRows("PRPOtemp_alt")
# PRPOtemp_alt <- PRPOtemp_alt[notDupRows,]

#Find the latest first time point and the earliest last time point of all the data
startTime <- max(min(PRPODOobs$dateTime),min(PRPOPAR$dateTime),min(PRPOwnd$dateTime),min(PRPOtemp$dateTime))
endTime <- min(max(PRPODOobs$dateTime),max(PRPOPAR$dateTime),max(PRPOwnd$dateTime),max(PRPOtemp$dateTime))

#Data.frame with one column "dateTime" which is sequence of times at time interval of timeStep, from startTime to endTime
completeTimes <- data.frame(dateTime=seq(startTime,endTime,paste(timeStep,"mins")))

#Merge all of input data.frames with completeTimes, so that they now all extend from startTime to endTime by timeStep
PRPODOobs <- merge(completeTimes,PRPODOobs,by="dateTime",all.x=T)
PRPOPAR <- merge(completeTimes,PRPOPAR,by="dateTime",all.x=T)
PRPOwnd <- merge(completeTimes,PRPOwnd,by="dateTime",all.x=T)
PRPOtemp <- merge(completeTimes,PRPOtemp,by="dateTime",all.x=T)
# PRPOtemp_alt <- merge(completeTimes,PRPOtemp_alt,by="dateTime",all.x=T)

# #Compare PRPOtemp and PRPOtemp_alt
# skim(PRPOtemp)
# skim(PRPOtemp_alt)


#Is it a problem that there are NAs in the PAR data? I guess we will find out

PRPO_full <- left_join(PRPODOobs, PRPOwnd, by = c("dateTime", "tz","lakeID"))
PRPO_full <- left_join(PRPO_full, PRPOtemp, by = c("dateTime", "tz","lakeID"))
PRPO_full <- left_join(PRPO_full, PRPOPAR, by = c("dateTime", "tz","lakeID"))

#For some reason (maybe can de-bug later) there are lots of duplicate rows.
#Use distinct function to pull out only distinct dateTimes.
PRPO_full<-PRPO_full %>%
  distinct(dateTime, .keep_all = TRUE)

#Plot data to see which year looks better. 2018 or 2019?


####plot DO###########
PRPO_full %>%
  mutate(year=year(dateTime)) %>%
  ggplot(aes(x=dateTime, y=DO)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%m-%d") +
  ylab("DO obs")+
  facet_wrap(.~year, scales="free")+
  ggtitle("PRPO Lake 2018-2019")
#2019 is more complete, but still have gaps

####plot WIND###########
PRPO_full %>%
  mutate(year=year(dateTime)) %>%
  ggplot(aes(x=dateTime, y=wind)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "2 months", date_labels = "%m-%d") +
  ylab("Wind (m/s)")+
  facet_wrap(.~year, scales="free")+
  ggtitle("PRPO Lake 2018-2019")
#2019 looks best

####plot TEMPS###########
PRPO_full %>%
  dplyr::select(dateTime, wtr_0.05:wtr_1.05)%>%
  pivot_longer(-(dateTime), names_sep = "_", names_to = c("variable","depth")) %>%
  dplyr::select(-variable)%>%
  mutate(year=year(dateTime)) %>%
  mutate(depth=as.numeric(as.character(depth))) %>%
  rename(temp=value)%>%
  ggplot(aes(x=dateTime, y=temp, color=depth)) + 
  geom_point(size=2)+
  scale_x_datetime(date_breaks = "2 months", date_labels = "%m-%d") +
  scale_color_continuous_sequential(palette = "Dark Mint", rev=TRUE)+
  ylab("water temperature")+
  facet_wrap(.~year, scales="free")+
  ggtitle("PRPO Lake 2018-2019")
#2019 is best

####plot PAR###########
PRPO_full %>%
  mutate(year=year(dateTime)) %>%
  ggplot(aes(x=dateTime, y=PAR)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "2 months", date_labels = "%m-%d") +
  ylab("PAR")+
  facet_wrap(.~year, scales="free")+
  ggtitle("PRPO Lake 2018-2019")
#2019 is best

####plot nutrients###########
PRPOnuts %>%
  mutate(dateTime = as_datetime(dateTime), year=year(dateTime)) %>%
  dplyr::select(year, dateTime, DOC_mgL, TP_mgL, TN_mgL)%>%
  pivot_longer(-(1:2), names_to = "nutrient") %>%
  ggplot(aes(x=dateTime, y=value, color=nutrient)) + 
  geom_point()+geom_line()+
  scale_x_datetime(date_breaks = "2 months", date_labels = "%m-%d") +
  ggtitle("PRPO Lake 2018-2019") +
  facet_wrap(year~nutrient, scales="free", nrow=2)
#Both years look good. And holy shit these ponds are stanky eutrophic. 


# FINAL PRPO dataset ------------------------------------------------------
PRPO_full_2019<-PRPO_full %>%
  mutate(year=year(dateTime)) %>%
  filter(year=="2019")


####export for modelling###########
PRPODOobs <- PRPO_full_2019 %>%
  dplyr::select(dateTime, DO) 
write.csv(PRPODOobs, "data/metab_data_clean/prairiepothole/prairiepothole_2019_DO.txt", row.names=FALSE)

#DO sensor depth = 0.5m
PRPOsensorTemp <- PRPO_full_2019 %>%
  dplyr::select(dateTime, wtr_0.55) %>%
  rename(sensorTemp=wtr_0.55) 
write.csv(PRPOsensorTemp, "data/metab_data_clean/prairiepothole/prairiepothole_2019_sensorTemp.txt", row.names=FALSE)

#PAR
PRPOPAR <- PRPO_full_2019 %>%
  dplyr::select(dateTime, PAR) 
write.csv(PRPOPAR, "data/metab_data_clean/prairiepothole/prairiepothole_2019_PAR.txt", row.names=FALSE)

#wind height = 2.85 m above lake surface
PRPOwind <- PRPO_full_2019 %>%
  dplyr::select(dateTime, wind) %>%
  rename(windSpeed=wind) 
write.csv(PRPOwind, "data/metab_data_clean/prairiepothole/prairiepothole_2019_windSpeed.txt", row.names=FALSE)

#temp profile
PRPOtempprofile<- PRPO_full_2019 %>%
  dplyr::select(dateTime, wtr_0.05:wtr_1.05)
write.csv(PRPOtempprofile, "data/metab_data_clean/prairiepothole/prairiepothole_2019_tempProfile.txt", row.names=FALSE)





# > Sugg lake (SUGG, NEON) -------------------------------------------------------------


SUGGDOobs <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/NEON sites/SUGG/SUGG_DO_2018-2019.csv") %>%
  dplyr::select(-X)
# SUGGwnd <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/NEON sites/SUGG/SUGG_wind_2018-2019.csv")%>% dplyr::select(-X)
SUGGtemp <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/NEON sites/SUGG/SUGG_wtr_2018-2019.csv")
# SUGGPAR <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/NEON sites/SUGG/SUGG_PAR_2018-2019.csv")%>%
#   dplyr::select(-X)
SUGGnuts <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/NEON sites/SUGG/SUGG_surfChem_2018-2019.csv")%>%
  dplyr::select(-X)
#SUGGS wind data sucks so pulling this from Ordway-Swisher Biological Station (OSBS)
#29.689282
#-81.993431
#2.21 km away from Suggs lake in a straight line. 
SUGGwnd <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/NEON sites/SUGG/SUGG_wind_2019.csv")%>% 
  dplyr::select(-X)

#SUGGS PAR data are also bad so pulling from  Ordway-Swisher Biological Station (OSBS)
#29.689282
#-81.993431
#2.21 km away from Suggs lake in a straight line. 
SUGGPAR <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/NEON sites/BARC/OSBS_PAR_2019.csv")%>% 
  dplyr::select(-X)


# Data collation/cleaning -------------------------------------------------
#Recgonize dateTimes as datese 
####DATE CONVERSION###

SUGGtemp <- SUGGtemp %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="UTC"),
         dateTime = with_tz(dateTime, "GMT")) %>%
  dplyr::select(-lakeID, -tz)
SUGGDOobs <- SUGGDOobs %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="UTC"),
         dateTime = with_tz(dateTime, "GMT"))%>%
  dplyr::select(-lakeID, -tz)
SUGGwnd <- SUGGwnd %>%
  rename(dateTime = startDateTime)%>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="UTC"),
         dateTime = with_tz(dateTime, "GMT")) %>%
  dplyr::select(-siteID)
SUGGPAR <- SUGGPAR %>%
  rename(dateTime = startDateTime)%>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="UTC"),
         dateTime = with_tz(dateTime, "GMT")) %>%
  dplyr::select(-siteID)
SUGGnuts <- SUGGnuts %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="UTC"),
         dateTime = with_tz(dateTime, "GMT")) 

#Print first five time readings for each variable
firstFiveTimes <- data.frame(DO=SUGGDOobs$dateTime[1:5],
                             PAR=SUGGPAR$dateTime[1:5],
                             wind=SUGGwnd$dateTime[1:5],
                             temp=SUGGtemp$dateTime[1:5])
print('First five time readings for each variable'); print(firstFiveTimes)
#Ok this is going to be annoying because the times don't line up...

#Calculate first differences of time readings, display unique values
cat('\n','First differences of datetime','\n')
difTimesDO <- diff(SUGGDOobs$dateTime); print(table(difTimesDO))
difTimesPAR <- diff(SUGGPAR$dateTime); print(table(difTimesPAR))
difTimesWindSpeed <- diff(SUGGwnd$dateTime); print(table(difTimesWindSpeed))
difTimesSensorTemp <- diff(SUGGtemp$dateTime); print(table(difTimesSensorTemp))
# difTimesSensorTemp_alt <- diff(SUGGtemp_alt$dateTime); print(table(difTimesSensorTemp))

timeStep=15	# in minutes
#DOs kind of all over the place, but I think if we truncate to 15 minute timeSteps we'll be ok



notDupRows <- findNotDupRows("SUGGDOobs")
SUGGDOobs <- SUGGDOobs[notDupRows,]
notDupRows <- findNotDupRows("SUGGPAR")
SUGGPAR <- SUGGPAR[notDupRows,]
notDupRows <- findNotDupRows("SUGGwnd")
SUGGwnd <- SUGGwnd[notDupRows,]
notDupRows <- findNotDupRows("SUGGtemp")
SUGGtemp <- SUGGtemp[notDupRows,]
# notDupRows <- findNotDupRows("SUGGtemp_alt")
# SUGGtemp_alt <- SUGGtemp_alt[notDupRows,]

#Make all data sets extend from startTime to endTime by timeStep
#Note that for some lakes it may be necessary to aggregate some variables to coarser time scale to get match up

#Round all time down to nearest timeStep (e.g. if timeStep is 5, round 00:07 to 00:05)
SUGGDOobs$dateTime <- floorMins(SUGGDOobs)
SUGGPAR$dateTime <- floorMins(SUGGPAR)
SUGGwnd$dateTime <- floorMins(SUGGwnd)
SUGGtemp$dateTime <- floorMins(SUGGtemp)
# SUGGtemp_alt$dateTime <- floorMins(SUGGtemp_alt)

#Repeat check for dup rows in case any introduced during floorMins
notDupRows <- findNotDupRows("SUGGDOobs")
SUGGDOobs <- SUGGDOobs[notDupRows,]
notDupRows <- findNotDupRows("SUGGPAR")
SUGGPAR <- SUGGPAR[notDupRows,]
notDupRows <- findNotDupRows("SUGGwnd")
SUGGwnd <- SUGGwnd[notDupRows,]
notDupRows <- findNotDupRows("SUGGtemp")
SUGGtemp <- SUGGtemp[notDupRows,]
# notDupRows <- findNotDupRows("SUGGtemp_alt")
# SUGGtemp_alt <- SUGGtemp_alt[notDupRows,]

#Find the latest first time point and the earliest last time point of all the data
startTime <- max(min(SUGGDOobs$dateTime),min(SUGGPAR$dateTime),min(SUGGwnd$dateTime),min(SUGGtemp$dateTime))
endTime <- min(max(SUGGDOobs$dateTime),max(SUGGPAR$dateTime),max(SUGGwnd$dateTime),max(SUGGtemp$dateTime))

#Data.frame with one column "dateTime" which is sequence of times at time interval of timeStep, from startTime to endTime
completeTimes <- data.frame(dateTime=seq(startTime,endTime,paste(timeStep,"mins")))

#Merge all of input data.frames with completeTimes, so that they now all extend from startTime to endTime by timeStep
SUGGDOobs <- merge(completeTimes,SUGGDOobs,by="dateTime",all.x=T)
SUGGPAR <- merge(completeTimes,SUGGPAR,by="dateTime",all.x=T)
SUGGwnd <- merge(completeTimes,SUGGwnd,by="dateTime",all.x=T)
SUGGtemp <- merge(completeTimes,SUGGtemp,by="dateTime",all.x=T)
# SUGGtemp_alt <- merge(completeTimes,SUGGtemp_alt,by="dateTime",all.x=T)


SUGG_full <- left_join(SUGGDOobs, SUGGwnd, by = c("dateTime"))
SUGG_full <- left_join(SUGG_full, SUGGtemp, by = c("dateTime"))
SUGG_full <- left_join(SUGG_full, SUGGPAR, by = c("dateTime"))

#For some reason (maybe can de-bug later) there are lots of duplicate rows.
#Use distinct function to pull out only distinct dateTimes.
SUGG_full<-SUGG_full %>%
  distinct(dateTime, .keep_all = TRUE)

#Plot data to see which year looks better. 2018 or 2019?


####plot DO###########
SUGG_full %>%
  mutate(year=year(dateTime)) %>%
  filter(year=="2019")%>%
  ggplot(aes(x=dateTime, y=DO)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%m-%d") +
  ylab("DO obs")+
  facet_wrap(.~year, scales="free")+
  ggtitle("SUGG Lake 2019")

####plot WIND###########
SUGG_full %>%
  mutate(year=year(dateTime)) %>%
  filter(year=="2019")%>%
  ggplot(aes(x=dateTime, y=windSpeedMean)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "2 months", date_labels = "%m-%d") +
  ylab("Wind (m/s)")+
  facet_wrap(.~year, scales="free")+
  ggtitle("SUGG Lake 2019")

####plot TEMPS###########
SUGG_full %>%
  dplyr::select(dateTime, wtr_0.05:wtr_1.05)%>%
  pivot_longer(-(dateTime), names_sep = "_", names_to = c("variable","depth")) %>%
  dplyr::select(-variable)%>%
  mutate(year=year(dateTime)) %>%
  filter(year=="2019")%>%
  mutate(depth=as.numeric(as.character(depth))) %>%
  rename(temp=value)%>%
  ggplot(aes(x=dateTime, y=temp, color=depth)) + 
  geom_point(size=2)+
  scale_x_datetime(date_breaks = "2 months", date_labels = "%m-%d") +
  scale_color_continuous_sequential(palette = "Dark Mint", rev=TRUE)+
  ylab("water temperature")+
  facet_wrap(.~year, scales="free")+
  ggtitle("SUGG Lake 2019")


####plot PAR###########
SUGG_full %>%
  mutate(year=year(dateTime)) %>%
  filter(year=="2019")%>%
  ggplot(aes(x=dateTime, y=PAR)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "2 months", date_labels = "%m-%d") +
  ylab("PAR")+
  facet_wrap(.~year, scales="free")+
  ggtitle("SUGG Lake 2019")

####plot nutrients###########
SUGGnuts %>%
  mutate(dateTime = as_datetime(dateTime), year=year(dateTime)) %>%
  dplyr::select(year, dateTime, DOC_mgL, TP_mgL, TN_mgL)%>%
  pivot_longer(-(1:2), names_to = "nutrient") %>%
  ggplot(aes(x=dateTime, y=value, color=nutrient)) + 
  geom_point()+geom_line()+
  scale_x_datetime(date_breaks = "2 months", date_labels = "%m-%d") +
  ggtitle("SUGG Lake 2018-2019") +
  facet_wrap(year~nutrient, scales="free", nrow=2)


# FINAL SUGG dataset ------------------------------------------------------
SUGG_full_2019<-SUGG_full %>%
  mutate(year=year(dateTime)) %>%
  filter(year=="2019")

head(SUGG_full_2019)
sugg_summary<-SUGG_full_2019 %>%
  mutate(date=yday(dateTime))%>%
  group_by(date) %>%
  summarize(count_PAR=n_unique(PARMean),
            count_wind=n_unique(windSpeedMean),
            count_DO=n_unique(DO))


####export for modelling###########
SUGGDOobs <- SUGG_full_2019 %>%
  dplyr::select(dateTime, DO) 
write.csv(SUGGDOobs, "data/metab_data_clean/sugg/sugg_2019_DO.txt", row.names=FALSE)

#DO sensor depth = 0.5m
SUGGsensorTemp <- SUGG_full_2019 %>%
  dplyr::select(dateTime, wtr_0.55) %>%
  rename(sensorTemp=wtr_0.55) 
write.csv(SUGGsensorTemp, "data/metab_data_clean/sugg/sugg_2019_sensorTemp.txt", row.names=FALSE)

#PAR
SUGGPAR <- SUGG_full_2019 %>%
  dplyr::select(dateTime, PARMean) %>%
  rename(PAR = PARMean)
write.csv(SUGGPAR, "data/metab_data_clean/sugg/sugg_2019_PAR.txt", row.names=FALSE)

#wind height = 3 m above ground
#from Ordway-Swisher Biological Station (OSBS), 2km east
SUGGwind <- SUGG_full_2019 %>%
  dplyr::select(dateTime, windSpeedMean) %>%
  rename(windSpeed=windSpeedMean) 
write.csv(SUGGwind, "data/metab_data_clean/sugg/sugg_2019_windSpeed.txt", row.names=FALSE)

#temp profile
SUGGtempprofile<- SUGG_full_2019 %>%
  dplyr::select(dateTime, wtr_0.05:wtr_1.05)
write.csv(SUGGtempprofile, "data/metab_data_clean/sugg/sugg_2019_tempProfile.txt", row.names=FALSE)












# > Toolik lake (TOOK, NEON) -------------------------------------------------------------


TOOKDOobs <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/NEON sites/TOOK/TOOK_DO_2019.csv") 
TOOKwnd <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/NEON sites/TOOK/TOOK_wind_2018-2019.csv")%>% dplyr::select(-X)
TOOKtemp <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/NEON sites/TOOK/TOOK_wtr_2018-2019.csv")
TOOKPAR <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/NEON sites/TOOK/TOOK_PAR_2018-2019.csv")%>% dplyr::select(-X)
# TOOKnuts <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/NEON sites/TOOK/TOOK_surfChem_2018-2019.csv")%>% dplyr::select(-X)


# Data collation/cleaning -------------------------------------------------
#Recgonize dateTimes as datese 
####DATE CONVERSION###

TOOKtemp <- TOOKtemp %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="UTC"),
         dateTime = with_tz(dateTime, "GMT")) 
TOOKDOobs <- TOOKDOobs %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="UTC"),
         dateTime = with_tz(dateTime, "GMT"))
TOOKwnd <- TOOKwnd %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="UTC"),
         dateTime = with_tz(dateTime, "GMT")) 
TOOKPAR <- TOOKPAR %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="UTC"),
         dateTime = with_tz(dateTime, "GMT")) 
# TOOKnuts <- TOOKnuts %>%
#   mutate(dateTime = ymd_hms(as.factor(dateTime)),
#          dateTime = force_tz(dateTime, tz="UTC"),
#          dateTime = with_tz(dateTime, "GMT")) 
# 

#Print first five time readings for each variable
firstFiveTimes <- data.frame(DO=TOOKDOobs$dateTime[1:5],
                             PAR=TOOKPAR$dateTime[1:5],
                             wind=TOOKwnd$dateTime[1:5],
                             temp=TOOKtemp$dateTime[1:5])
print('First five time readings for each variable'); print(firstFiveTimes)
#Ok this is going to be annoying because the times don't line up...

#Calculate first differences of time readings, display unique values
cat('\n','First differences of datetime','\n')
difTimesDO <- diff(TOOKDOobs$dateTime); print(table(difTimesDO))
difTimesPAR <- diff(TOOKPAR$dateTime); print(table(difTimesPAR))
difTimesWindSpeed <- diff(TOOKwnd$dateTime); print(table(difTimesWindSpeed))
difTimesSensorTemp <- diff(TOOKtemp$dateTime); print(table(difTimesSensorTemp))
# difTimesSensorTemp_alt <- diff(TOOKtemp_alt$dateTime); print(table(difTimesSensorTemp))

timeStep=15	# in minutes
#DOs kind of all over the place, but I think if we truncate to 15 minute timeSteps we'll be ok



notDupRows <- findNotDupRows("TOOKDOobs")
TOOKDOobs <- TOOKDOobs[notDupRows,]
notDupRows <- findNotDupRows("TOOKPAR")
TOOKPAR <- TOOKPAR[notDupRows,]
notDupRows <- findNotDupRows("TOOKwnd")
TOOKwnd <- TOOKwnd[notDupRows,]
notDupRows <- findNotDupRows("TOOKtemp")
TOOKtemp <- TOOKtemp[notDupRows,]
# notDupRows <- findNotDupRows("TOOKtemp_alt")
# TOOKtemp_alt <- TOOKtemp_alt[notDupRows,]

#Make all data sets extend from startTime to endTime by timeStep
#Note that for some lakes it may be necessary to aggregate some variables to coarser time scale to get match up

#Round all time down to nearest timeStep (e.g. if timeStep is 5, round 00:07 to 00:05)
TOOKDOobs$dateTime <- floorMins(TOOKDOobs)
TOOKPAR$dateTime <- floorMins(TOOKPAR)
TOOKwnd$dateTime <- floorMins(TOOKwnd)
TOOKtemp$dateTime <- floorMins(TOOKtemp)
# TOOKtemp_alt$dateTime <- floorMins(TOOKtemp_alt)

#Repeat check for dup rows in case any introduced during floorMins
notDupRows <- findNotDupRows("TOOKDOobs")
TOOKDOobs <- TOOKDOobs[notDupRows,]
notDupRows <- findNotDupRows("TOOKPAR")
TOOKPAR <- TOOKPAR[notDupRows,]
notDupRows <- findNotDupRows("TOOKwnd")
TOOKwnd <- TOOKwnd[notDupRows,]
notDupRows <- findNotDupRows("TOOKtemp")
TOOKtemp <- TOOKtemp[notDupRows,]
# notDupRows <- findNotDupRows("TOOKtemp_alt")
# TOOKtemp_alt <- TOOKtemp_alt[notDupRows,]

#Find the latest first time point and the earliest last time point of all the data
startTime <- max(min(TOOKDOobs$dateTime),min(TOOKPAR$dateTime),min(TOOKwnd$dateTime),min(TOOKtemp$dateTime))
endTime <- min(max(TOOKDOobs$dateTime),max(TOOKPAR$dateTime),max(TOOKwnd$dateTime),max(TOOKtemp$dateTime))

#Data.frame with one column "dateTime" which is sequence of times at time interval of timeStep, from startTime to endTime
completeTimes <- data.frame(dateTime=seq(startTime,endTime,paste(timeStep,"mins")))

#Merge all of input data.frames with completeTimes, so that they now all extend from startTime to endTime by timeStep
TOOKDOobs <- merge(completeTimes,TOOKDOobs,by="dateTime",all.x=T)
TOOKPAR <- merge(completeTimes,TOOKPAR,by="dateTime",all.x=T)
TOOKwnd <- merge(completeTimes,TOOKwnd,by="dateTime",all.x=T)
TOOKtemp <- merge(completeTimes,TOOKtemp,by="dateTime",all.x=T)
# TOOKtemp_alt <- merge(completeTimes,TOOKtemp_alt,by="dateTime",all.x=T)


TOOK_full <- left_join(TOOKDOobs, TOOKwnd, by = c("dateTime", "tz","lakeID"))
TOOK_full <- left_join(TOOK_full, TOOKtemp, by = c("dateTime", "tz","lakeID"))
TOOK_full <- left_join(TOOK_full, TOOKPAR, by = c("dateTime", "tz","lakeID"))

#For some reason (maybe can de-bug later) there are lots of duplicate rows.
#Use distinct function to pull out only distinct dateTimes.
TOOK_full<-TOOK_full %>%
  distinct(dateTime, .keep_all = TRUE)



####plot DO###########
TOOK_full %>%
  mutate(year=year(dateTime)) %>%
  ggplot(aes(x=dateTime, y=DO)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%m-%d") +
  ylab("DO obs")+
  facet_wrap(.~year, scales="free")+
  ggtitle("TOOK Lake 2019")


####plot WIND###########
TOOK_full %>%
  mutate(year=year(dateTime)) %>%
  ggplot(aes(x=dateTime, y=wind)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "2 months", date_labels = "%m-%d") +
  ylab("Wind (m/s)")+
  facet_wrap(.~year, scales="free")+
  ggtitle("TOOK Lake 2019")
#Wind is kind of sparse... ugh
####plot TEMPS###########
TOOK_full %>%
  dplyr::select(dateTime, wtr_0.05:wtr_15.75)%>%
  pivot_longer(-(dateTime), names_sep = "_", names_to = c("variable","depth")) %>%
  dplyr::select(-variable)%>%
  mutate(year=year(dateTime)) %>%
  mutate(depth=as.numeric(as.character(depth))) %>%
  rename(temp=value)%>%
  ggplot(aes(x=dateTime, y=temp, color=depth)) + 
  geom_point(size=2)+
  scale_x_datetime(date_breaks = "2 months", date_labels = "%m-%d") +
  scale_color_continuous_sequential(palette = "Dark Mint", rev=TRUE)+
  ylab("water temperature")+
  facet_wrap(.~year, scales="free")+
  ggtitle("TOOK Lake 2019")

####plot PAR###########
TOOK_full %>%
  mutate(year=year(dateTime)) %>%
  ggplot(aes(x=dateTime, y=PAR)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "2 months", date_labels = "%m-%d") +
  ylab("PAR")+
  facet_wrap(.~year, scales="free")+
  ggtitle("TOOK Lake 2019")
#2019

####plot nutrients###########
# TOOKnuts %>%
#   mutate(dateTime = as_datetime(dateTime), year=year(dateTime)) %>%
#   dplyr::select(year, dateTime, DOC_mgL, TP_mgL, TN_mgL)%>%
#   pivot_longer(-(1:2), names_to = "nutrient") %>%
#   ggplot(aes(x=dateTime, y=value, color=nutrient)) + 
#   geom_point()+geom_line()+
#   scale_x_datetime(date_breaks = "2 months", date_labels = "%m-%d") +
#   ggtitle("TOOK Lake 2018-2019") +
#   facet_wrap(year~nutrient, scales="free", nrow=2)


# FINAL TOOK dataset ------------------------------------------------------
TOOK_full_2019<-TOOK_full %>%
  mutate(year=year(dateTime)) %>%
  filter(year=="2019")

####export for modelling###########
TOOKDOobs <- TOOK_full_2019 %>%
  dplyr::select(dateTime, DO) 
write.csv(TOOKDOobs, "data/metab_data_clean/toolik/took_2019_DO.txt", row.names=FALSE)

#DO sensor depth = 0.86m but there is no sensor there. Using the next closest. 
#I'm afraid the 0.05m will have too high of temperature fluctuations. 
TOOKsensorTemp <- TOOK_full_2019 %>%
  dplyr::select(dateTime, wtr_1.75) %>%
  rename(sensorTemp=wtr_1.75) 
write.csv(TOOKsensorTemp, "data/metab_data_clean/toolik/took_2019_sensorTemp.txt", row.names=FALSE)

#PAR
TOOKPAR <- TOOK_full_2019 %>%
  dplyr::select(dateTime, PAR) 
write.csv(TOOKPAR, "data/metab_data_clean/toolik/took_2019_PAR.txt", row.names=FALSE)

#wind height = 2.85 m above lake surface
TOOKwind <- TOOK_full_2019 %>%
  dplyr::select(dateTime, wind) %>%
  rename(windSpeed=wind) 
write.csv(TOOKwind, "data/metab_data_clean/toolik/took_2019_windSpeed.txt", row.names=FALSE)

#temp profile
TOOKtempprofile<- TOOK_full_2019 %>%
  dplyr::select(dateTime, wtr_0.05:wtr_15.75)
write.csv(TOOKtempprofile, "data/metab_data_clean/toolik/took_2019_tempProfile.txt", row.names=FALSE)












# > The Loch -------------------------------------------------------------


# ---2016--- --------------------------------------------------------------

# LochDOobs <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/The Loch/Loch2016_doobs.txt",stringsAsFactors = FALSE) 
Lochwnd <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/The Loch/Loch2016_wnd.txt", stringsAsFactors = FALSE) 
Lochtemp <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/The Loch/Loch2016_wtr.txt", stringsAsFactors = FALSE)  %>%
  rename(wtr_0.5 = wtr0.5,
         wtr_4.5 = wtr4.5)
LochPAR <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/The Loch/Loch2016_par.txt", stringsAsFactors = FALSE) 
#Adding these DO at multiple depths
LochDOobs <- read.csv("~/Google Drive/Research (common)/Research/Data/Master - buoys/Loch/Loch_buoy_HOBO_MiniDot_master_181029.csv", stringsAsFactors = FALSE) %>%
  select(datetime, DOconc_0.5, DOconc_4.5)


####DATE CONVERSION###
glimpse(Lochtemp)
Lochtemp <- Lochtemp %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Denver"),
         dateTime = with_tz(dateTime, "GMT")) %>%
  filter(dateTime > "2016-05-31" & dateTime <= "2016-10-30") #ice off and on dates
tz(Lochtemp$dateTime)


# glimpse(LochDOobs)
# LochDOobs <- LochDOobs %>%
#   mutate(dateTime = ymd_hms(as.factor(dateTime)),
#          dateTime = force_tz(dateTime, tz="America/Denver"),
#          dateTime = with_tz(dateTime, "GMT")) %>%
#   filter(dateTime > "2016-05-31" & dateTime <= "2016-10-30") #ice off and on dates
glimpse(LochDOobs)
LochDOobs <- LochDOobs %>%
  rename(dateTime = datetime,
         doObs_0.5 = DOconc_0.5,
         doObs_4.5 = DOconc_4.5)%>%
  mutate(dateTime = mdy_hm(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Denver"),
         dateTime = with_tz(dateTime, "GMT")) %>%
  filter(dateTime > "2016-05-31" & dateTime <= "2016-10-30") #ice off and on dates
glimpse(LochDOobs)


glimpse(Lochwnd)
Lochwnd <- Lochwnd %>%
  rename(dateTime = timestamp) %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Denver"),
         dateTime = with_tz(dateTime, "GMT"))%>%
  # convert(num(wind))%>% #convert from character to numeric -- doesn't come up with NAs or warnings
  # wind=as.numeric(wind)) %>%
  filter(dateTime > "2016-05-31" & dateTime <= "2016-10-30") #ice off and on dates


glimpse(LochPAR)
LochPAR <- LochPAR %>%
  rename(dateTime = timestamp) %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Denver"),
         dateTime = with_tz(dateTime, "GMT"),
         PAR=PAR*2.15) %>% #See "Estimating PAR from solar flux data" Word Doc in Troubleshooting folder
  filter(dateTime > "2016-05-31" & dateTime <= "2016-10-30") #ice off and on dates

#Print first five time readings for each variable
firstFiveTimes <- data.frame(DO=LochDOobs$dateTime[1:5],
                             PAR=LochPAR$dateTime[1:5],
                             wind=Lochwnd$dateTime[1:5],
                             temp=Lochtemp$dateTime[1:5])
print('First five time readings for each variable'); print(firstFiveTimes)

#Calculate first differences of time readings, display unique values
cat('\n','First differences of datetime','\n')
difTimesDO <- diff(LochDOobs$dateTime); print(table(difTimesDO))
difTimesPAR <- diff(LochPAR$dateTime); print(table(difTimesPAR))
difTimesWindSpeed <- diff(Lochwnd$dateTime); print(table(difTimesWindSpeed))
difTimesSensorTemp <- diff(Lochtemp$dateTime); print(table(difTimesSensorTemp))
timeStep=30	# in minutes

notDupRows <- findNotDupRows("LochDOobs")
LochDOobs <- LochDOobs[notDupRows,]

notDupRows <- findNotDupRows("LochPAR")
LochPAR <- LochPAR[notDupRows,]

notDupRows <- findNotDupRows("Lochwnd")
Lochwnd <- Lochwnd[notDupRows,]

notDupRows <- findNotDupRows("Lochtemp")
Lochtemp <- Lochtemp[notDupRows,]

#Make all data sets extend from startTime to endTime by timeStep
#Note that for some lakes it may be necessary to aggregate some variables to coarser time scale to get match up
LochDOobs$dateTime <- floorMins(LochDOobs)
LochPAR$dateTime <- floorMins(LochPAR)
Lochwnd$dateTime <- floorMins(Lochwnd)
Lochtemp$dateTime <- floorMins(Lochtemp)

#Repeat check for dup rows in case any introduced during floorMins
notDupRows <- findNotDupRows("LochDOobs")
LochDOobs <- LochDOobs[notDupRows,]
notDupRows <- findNotDupRows("LochPAR")
LochPAR <- LochPAR[notDupRows,]
notDupRows <- findNotDupRows("Lochwnd")
Lochwnd <- Lochwnd[notDupRows,]
notDupRows <- findNotDupRows("Lochtemp")
Lochtemp <- Lochtemp[notDupRows,]

#Find the latest first time point and the earliest last time point of all the data
startTime <- max(min(LochDOobs$dateTime),
                 min(LochPAR$dateTime),
                 min(Lochwnd$dateTime),
                 min(Lochtemp$dateTime))
endTime <- min(max(LochDOobs$dateTime),
               max(LochPAR$dateTime),
               max(Lochwnd$dateTime),
               max(Lochtemp$dateTime))

#Data.frame with one column "dateTime" which is sequence of times at time interval of timeStep, from startTime to endTime
completeTimes <- data.frame(dateTime=seq(startTime,endTime,paste(timeStep,"mins")))

#Merge all of input data.frames with completeTimes, so that they now all extend from startTime to endTime by timeStep
LochDOobs <- merge(completeTimes,LochDOobs,by="dateTime",all.x=T)
LochPAR <- merge(completeTimes,LochPAR,by="dateTime",all.x=T)%>%
  select(-localTZ, -lakeID, -daylightSavings)
Lochwnd <- merge(completeTimes,Lochwnd,by="dateTime",all.x=T)%>%
  select(-localTZ, -lakeID, -daylightSavings)
Lochtemp <- merge(completeTimes,Lochtemp,by="dateTime",all.x=T)%>%
  select(-localTZ, -lakeID, -daylightSavings)

#Convert wind from character to number
Lochwnd$wind <- as.numeric(Lochwnd$wind)

#Full dataset
Loch_full_2016 <- left_join(LochDOobs, Lochwnd, by = c("dateTime"))
Loch_full_2016 <- left_join(Loch_full_2016, Lochtemp, by = c("dateTime"))
Loch_full_2016 <- left_join(Loch_full_2016, LochPAR, by = c("dateTime"))

glimpse(Loch_full_2016)
tz(Loch_full_2016$dateTime)
check_tzones(Loch_full_2016$dateTime)


# ---2017--- --------------------------------------------------------------

# LochDOobs <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/The Loch/Loch2017_doobs.txt",stringsAsFactors = FALSE) 
Lochwnd <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/The Loch/Loch2017_wnd.txt", stringsAsFactors = FALSE) 
Lochtemp <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/The Loch/Loch2017_wtr.txt", stringsAsFactors = FALSE)  %>%
  rename(wtr_0.5 = wtr0.5,
         wtr_4.5 = wtr4.5)
LochPAR <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/The Loch/Loch2017_par.txt", stringsAsFactors = FALSE) 
#Adding these DO at multiple depths
LochDOobs <- read.csv("~/Google Drive/Research (common)/Research/Data/Master - buoys/Loch/Loch_buoy_HOBO_MiniDot_master_181029.csv", stringsAsFactors = FALSE) %>%
  select(datetime, DOconc_0.5, DOconc_4.5)


####DATE CONVERSION###
glimpse(Lochtemp)
Lochtemp <- Lochtemp %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Denver"),
         dateTime = with_tz(dateTime, "GMT")) %>%
  filter(dateTime > "2017-05-16" & dateTime <= "2017-10-17") #ice off and on dates


# glimpse(LochDOobs)
# LochDOobs <- LochDOobs %>%
#   mutate(dateTime = ymd_hms(as.factor(dateTime)),
#          dateTime = force_tz(dateTime, tz="America/Denver"),
#          dateTime = with_tz(dateTime, "GMT")) %>%
#   filter(dateTime > "2017-05-16" & dateTime <= "2017-10-17") #ice off and on dates
glimpse(LochDOobs)
LochDOobs <- LochDOobs %>%
  rename(dateTime = datetime,
         doObs_0.5 = DOconc_0.5,
         doObs_4.5 = DOconc_4.5)%>%
  mutate(dateTime = mdy_hm(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Denver"),
         dateTime = with_tz(dateTime, "GMT")) %>%
  filter(dateTime > "2017-05-16" & dateTime <= "2017-10-17") #ice off and on dates
glimpse(LochDOobs)

glimpse(Lochwnd)
Lochwnd <- Lochwnd %>%
  rename(dateTime = timestamp) %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Denver"),
         dateTime = with_tz(dateTime, "GMT"))%>%
  # convert(num(wind))%>% #convert from character to numeric -- doesn't come up with NAs or warnings
  # wind=as.numeric(wind)) %>%
  filter(dateTime > "2017-05-16" & dateTime <= "2017-10-17") #ice off and on dates


glimpse(LochPAR)
LochPAR <- LochPAR %>%
  rename(dateTime = timestamp) %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Denver"),
         dateTime = with_tz(dateTime, "GMT"),
         PAR=PAR*2.15) %>% #See "Estimating PAR from solar flux data" Word Doc in Troubleshooting folder
  filter(dateTime > "2017-05-16" & dateTime <= "2017-10-17") #ice off and on dates

#Print first five time readings for each variable
firstFiveTimes <- data.frame(DO=LochDOobs$dateTime[1:5],
                             PAR=LochPAR$dateTime[1:5],
                             wind=Lochwnd$dateTime[1:5],
                             temp=Lochtemp$dateTime[1:5])
print('First five time readings for each variable'); print(firstFiveTimes)

#Calculate first differences of time readings, display unique values
cat('\n','First differences of datetime','\n')
difTimesDO <- diff(LochDOobs$dateTime); print(table(difTimesDO))
difTimesPAR <- diff(LochPAR$dateTime); print(table(difTimesPAR))
difTimesWindSpeed <- diff(Lochwnd$dateTime); print(table(difTimesWindSpeed))
difTimesSensorTemp <- diff(Lochtemp$dateTime); print(table(difTimesSensorTemp))
timeStep=30	# in minutes

notDupRows <- findNotDupRows("LochDOobs")
LochDOobs <- LochDOobs[notDupRows,]

notDupRows <- findNotDupRows("LochPAR")
LochPAR <- LochPAR[notDupRows,]

notDupRows <- findNotDupRows("Lochwnd")
Lochwnd <- Lochwnd[notDupRows,]

notDupRows <- findNotDupRows("Lochtemp")
Lochtemp <- Lochtemp[notDupRows,]

#Make all data sets extend from startTime to endTime by timeStep
#Note that for some lakes it may be necessary to aggregate some variables to coarser time scale to get match up
LochDOobs$dateTime <- floorMins(LochDOobs)
LochPAR$dateTime <- floorMins(LochPAR)
Lochwnd$dateTime <- floorMins(Lochwnd)
Lochtemp$dateTime <- floorMins(Lochtemp)

#Repeat check for dup rows in case any introduced during floorMins
notDupRows <- findNotDupRows("LochDOobs")
LochDOobs <- LochDOobs[notDupRows,]
notDupRows <- findNotDupRows("LochPAR")
LochPAR <- LochPAR[notDupRows,]
notDupRows <- findNotDupRows("Lochwnd")
Lochwnd <- Lochwnd[notDupRows,]
notDupRows <- findNotDupRows("Lochtemp")
Lochtemp <- Lochtemp[notDupRows,]

#Find the latest first time point and the earliest last time point of all the data
startTime <- max(min(LochDOobs$dateTime),min(LochPAR$dateTime),min(Lochwnd$dateTime),min(Lochtemp$dateTime))
endTime <- min(max(LochDOobs$dateTime),max(LochPAR$dateTime),max(Lochwnd$dateTime),max(Lochtemp$dateTime))

#Data.frame with one column "dateTime" which is sequence of times at time interval of timeStep, from startTime to endTime
completeTimes <- data.frame(dateTime=seq(startTime,endTime,paste(timeStep,"mins")))

#Merge all of input data.frames with completeTimes, so that they now all extend from startTime to endTime by timeStep
LochDOobs <- merge(completeTimes,LochDOobs,by="dateTime",all.x=T)
LochPAR <- merge(completeTimes,LochPAR,by="dateTime",all.x=T)%>%
  select(-localTZ, -lakeID, -daylightSavings)
Lochwnd <- merge(completeTimes,Lochwnd,by="dateTime",all.x=T)%>%
  select(-localTZ, -lakeID, -daylightSavings)
Lochtemp <- merge(completeTimes,Lochtemp,by="dateTime",all.x=T)%>%
  select(-localTZ, -lakeID, -daylightSavings)

#Convert wind from character to number
Lochwnd$wind <- as.numeric(Lochwnd$wind)

#Is it a problem that there are NAs in the PAR data? I guess we will find out
##IAO - 2021-02-12 --  I just checked and it shouldn't be because of the fillHoles helper function. 

Loch_full_2017 <- left_join(LochDOobs, Lochwnd, by = c("dateTime"))
Loch_full_2017 <- left_join(Loch_full_2017, Lochtemp, by = c("dateTime"))
Loch_full_2017 <- left_join(Loch_full_2017, LochPAR, by = c("dateTime"))

glimpse(Loch_full_2017)
tz(Loch_full_2017$dateTime)


# ---2018--- --------------------------------------------------------------

#
LochDOobs_lateseason <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/The Loch/Loch2018_doobs.txt",stringsAsFactors = FALSE)
Lochtemp_lateseason <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/The Loch/Loch2018_wtr.txt", stringsAsFactors = FALSE) %>%
  rename(wtr_0.5 = wtr0.5,
         wtr_4.5 = wtr4.5)

Lochwnd_lateseason <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/The Loch/Loch2018_wnd.txt", stringsAsFactors = FALSE) %>% dplyr::select(-X)
LochPAR_lateseason <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/The Loch/Loch2018_par.txt", stringsAsFactors = FALSE) %>%
  dplyr::select(-X)

### USING SKY PAR and wind because it's incomplete for The Loch. Same weather station anyway. 
LochPAR_earlyseason <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Sky Pond/Sky2018_par.txt", stringsAsFactors = FALSE) 
Lochwnd_earlyseason <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Sky Pond/Sky2018_wnd.txt", stringsAsFactors = FALSE) 


#Adding these -- 2018 data for The Loch was incomplete, so I'm pulling from one of my files rather than what Tim sent me
LochtempDO_earlyseason <- read.csv("~/Google Drive/Research (common)/Research/Data/Master - buoys/Loch/Loch_buoy_HOBO_MiniDot_master_181029.csv", stringsAsFactors = FALSE) %>%
  select(datetime, DOconc_0.5, DOconc_4.5, temp_0.5, temp_4.5)



####DATE CONVERSION###
glimpse(LochtempDO_earlyseason)
Lochtemp_earlyseason <- LochtempDO_earlyseason %>%
  select(datetime, temp_0.5, temp_4.5)%>%
  rename(dateTime = datetime,
         wtr_0.5 = temp_0.5,
         wtr_4.5 = temp_4.5)%>%
  mutate(dateTime = mdy_hm(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Denver"),
         dateTime = with_tz(dateTime, "GMT")) %>%
  filter(dateTime > "2018-05-15" & dateTime <= "2018-10-16") #ice off and on dates

glimpse(LochtempDO_earlyseason)
LochDOobs_earlyseason <- LochtempDO_earlyseason %>%
  select(datetime, DOconc_0.5, DOconc_4.5)%>%
  rename(dateTime = datetime,
         doObs_0.5 = DOconc_0.5,
         doObs_4.5 = DOconc_4.5)%>%
  mutate(dateTime = mdy_hm(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Denver"),
         dateTime = with_tz(dateTime, "GMT")) %>%
  filter(dateTime > "2018-05-15" & dateTime <= "2018-10-16") #ice off and on dates

glimpse(Lochtemp_lateseason)
Lochtemp_lateseason <- Lochtemp_lateseason %>%
  mutate(dateTime = mdy_hm(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Denver"),
         dateTime = with_tz(dateTime, "GMT")) %>%
  filter(dateTime > "2018-05-15" & dateTime <= "2018-10-16") #ice off and on dates

glimpse(LochDOobs_lateseason)
LochDOobs_lateseason <- LochDOobs_lateseason %>%
  rename(doObs_0.5 = doObs)%>%
  mutate(dateTime = mdy_hm(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Denver"),
         dateTime = with_tz(dateTime, "GMT")) %>%
  filter(dateTime > "2018-05-15" & dateTime <= "2018-10-16") #ice off and on dates

glimpse(Lochwnd_lateseason)
Lochwnd_lateseason <- Lochwnd_lateseason %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Denver"),
         dateTime = with_tz(dateTime, "GMT"))%>%
  # convert(num(wind))%>% #convert from character to numeric -- doesn't come up with NAs or warnings
  # wind=as.numeric(wind)) %>%
  filter(dateTime > "2018-05-15" & dateTime <= "2018-10-16") #ice off and on dates

glimpse(Lochwnd_earlyseason)
Lochwnd_earlyseason <- Lochwnd_earlyseason %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Denver"),
         dateTime = with_tz(dateTime, "GMT"))%>%
  # convert(num(wind))%>% #convert from character to numeric -- doesn't come up with NAs or warnings
  # wind=as.numeric(wind)) %>%
  filter(dateTime > "2018-05-15" & dateTime <= "2018-10-16") #ice off and on dates


glimpse(LochPAR_lateseason)
LochPAR_lateseason <- LochPAR_lateseason %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Denver"),
         dateTime = with_tz(dateTime, "GMT"),
         PAR=PAR*2.15) %>% #See "Estimating PAR from solar flux data" Word Doc in Troubleshooting folder
  filter(dateTime > "2018-05-15" & dateTime <= "2018-10-16") #ice off and on dates

glimpse(LochPAR_earlyseason)
LochPAR_earlyseason <- LochPAR_earlyseason %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Denver"),
         dateTime = with_tz(dateTime, "GMT"),
         PAR=PAR*2.15) %>% #See "Estimating PAR from solar flux data" Word Doc in Troubleshooting folder
  filter(dateTime > "2018-05-15" & dateTime <= "2018-10-16") #ice off and on dates


#Print first five time readings for each variable
firstFiveTimes <- data.frame(DO_earlyseason=LochDOobs_earlyseason$dateTime[1:5],
                             PAR_earlyseason=LochPAR_earlyseason$dateTime[1:5],
                             wind_earlyseason=Lochwnd_earlyseason$dateTime[1:5],
                             temp_earlyseason=Lochtemp_earlyseason$dateTime[1:5],
                             DO_lateseason=LochDOobs_lateseason$dateTime[1:5],
                             PAR_lateseason=LochPAR_lateseason$dateTime[1:5],
                             wind_lateseason=Lochwnd_lateseason$dateTime[1:5],
                             temp_lateseason=Lochtemp_lateseason$dateTime[1:5])
print('First five time readings for each variable'); print(firstFiveTimes)

#Calculate first differences of time readings, display unique values
cat('\n','First differences of datetime','\n')
difTimesDO_earlyseason <- diff(LochDOobs_earlyseason$dateTime); print(table(difTimesDO_earlyseason))
difTimesPAR_earlyseason <- diff(LochPAR_earlyseason$dateTime); print(table(difTimesPAR_earlyseason))
difTimesWindSpeed_earlyseason <- diff(Lochwnd_earlyseason$dateTime); print(table(difTimesWindSpeed_earlyseason))
difTimesSensorTemp_earlyseason <- diff(Lochtemp_earlyseason$dateTime); print(table(difTimesSensorTemp_earlyseason))
difTimesDO_lateseason <- diff(LochDOobs_lateseason$dateTime); print(table(difTimesDO_lateseason))
difTimesPAR_lateseason <- diff(LochPAR_lateseason$dateTime); print(table(difTimesPAR_lateseason))
difTimesWindSpeed_lateseason <- diff(Lochwnd_lateseason$dateTime); print(table(difTimesWindSpeed_lateseason))
difTimesSensorTemp_lateseason <- diff(Lochtemp_lateseason$dateTime); print(table(difTimesSensorTemp_lateseason))
timeStep=30	# in minutes

notDupRows <- findNotDupRows("LochDOobs_earlyseason")
LochDOobs_earlyseason <- LochDOobs_earlyseason[notDupRows,]

notDupRows <- findNotDupRows("LochPAR_earlyseason")
LochPAR_earlyseason <- LochPAR_earlyseason[notDupRows,]

notDupRows <- findNotDupRows("Lochwnd_earlyseason")
Lochwnd_earlyseason <- Lochwnd_earlyseason[notDupRows,]

notDupRows <- findNotDupRows("Lochtemp_earlyseason")
Lochtemp_earlyseason <- Lochtemp_earlyseason[notDupRows,]

notDupRows <- findNotDupRows("LochDOobs_lateseason")
LochDOobs_lateseason <- LochDOobs_lateseason[notDupRows,]

notDupRows <- findNotDupRows("LochPAR_lateseason")
LochPAR_lateseason <- LochPAR_lateseason[notDupRows,]

notDupRows <- findNotDupRows("Lochwnd_lateseason")
Lochwnd_lateseason <- Lochwnd_lateseason[notDupRows,]

notDupRows <- findNotDupRows("Lochtemp_lateseason")
Lochtemp_lateseason <- Lochtemp_lateseason[notDupRows,]

#Make all data sets extend from startTime to endTime by timeStep
#Note that for some lakes it may be necessary to aggregate some variables to coarser time scale to get match up
LochDOobs_earlyseason$dateTime <- floorMins(LochDOobs_earlyseason)
LochPAR_earlyseason$dateTime <- floorMins(LochPAR_earlyseason)
Lochwnd_earlyseason$dateTime <- floorMins(Lochwnd_earlyseason)
Lochtemp_earlyseason$dateTime <- floorMins(Lochtemp_earlyseason)
LochDOobs_lateseason$dateTime <- floorMins(LochDOobs_lateseason)
LochPAR_lateseason$dateTime <- floorMins(LochPAR_lateseason)
Lochwnd_lateseason$dateTime <- floorMins(Lochwnd_lateseason)
Lochtemp_lateseason$dateTime <- floorMins(Lochtemp_lateseason)

#Repeat check for dup rows in case any introduced during floorMins
notDupRows <- findNotDupRows("LochDOobs_earlyseason")
LochDOobs_earlyseason <- LochDOobs_earlyseason[notDupRows,]
notDupRows <- findNotDupRows("LochPAR_earlyseason")
LochPAR_earlyseason <- LochPAR_earlyseason[notDupRows,]
notDupRows <- findNotDupRows("Lochwnd_earlyseason")
Lochwnd_earlyseason <- Lochwnd_earlyseason[notDupRows,]
notDupRows <- findNotDupRows("Lochtemp_earlyseason")
Lochtemp_earlyseason <- Lochtemp_earlyseason[notDupRows,]
notDupRows <- findNotDupRows("LochDOobs_lateseason")
LochDOobs_lateseason <- LochDOobs_lateseason[notDupRows,]
notDupRows <- findNotDupRows("LochPAR_lateseason")
LochPAR_lateseason <- LochPAR_lateseason[notDupRows,]
notDupRows <- findNotDupRows("Lochwnd_lateseason")
Lochwnd_lateseason <- Lochwnd_lateseason[notDupRows,]
notDupRows <- findNotDupRows("Lochtemp_lateseason")
Lochtemp_lateseason <- Lochtemp_lateseason[notDupRows,]

#Bind early and late season dataframes together...
LochDOobs<-bind_rows(LochDOobs_earlyseason,LochDOobs_lateseason)
LochPAR<-bind_rows(LochPAR_earlyseason,LochPAR_lateseason)
Lochwnd<-bind_rows(Lochwnd_earlyseason,Lochwnd_lateseason)
Lochtemp<-bind_rows(Lochtemp_earlyseason,Lochtemp_lateseason)

#Find the latest first time point and the earliest last time point of all the data
startTime <- max(min(LochDOobs$dateTime),min(LochPAR$dateTime),min(Lochwnd$dateTime),min(Lochtemp$dateTime))
endTime <- min(max(LochDOobs$dateTime),max(LochPAR$dateTime),max(Lochwnd$dateTime),max(Lochtemp$dateTime))

#Data.frame with one column "dateTime" which is sequence of times at time interval of timeStep, from startTime to endTime
completeTimes <- data.frame(dateTime=seq(startTime,endTime,paste(timeStep,"mins")))

#Merge all of input data.frames with completeTimes, so that they now all extend from startTime to endTime by timeStep
LochDOobs <- merge(completeTimes,LochDOobs,by="dateTime",all.x=T)%>%
  select(-localTZ, -lakeID, -daylightSavings)
LochPAR <- merge(completeTimes,LochPAR,by="dateTime",all.x=T)%>%
  select(-localTZ, -lakeID, -daylightSavings)
Lochwnd <- merge(completeTimes,Lochwnd,by="dateTime",all.x=T)%>%
  select(-localTZ, -lakeID, -daylightSavings)
Lochtemp <- merge(completeTimes,Lochtemp,by="dateTime",all.x=T)%>%
  select(-localTZ, -lakeID, -daylightSavings)

#Convert wind from character to number
Lochwnd$wind <- as.numeric(Lochwnd$wind)

#Is it a problem that there are NAs in the PAR data? I guess we will find out
##IAO - 2021-02-12 --  I just checked and it shouldn't be because of the fillHoles helper function. 

Loch_full_2018 <- left_join(LochDOobs, Lochwnd, by = c("dateTime"))
Loch_full_2018 <- left_join(Loch_full_2018, Lochtemp, by = c("dateTime"))
Loch_full_2018 <- left_join(Loch_full_2018, LochPAR, by = c("dateTime"))

glimpse(Loch_full_2018)



# ---2019--- --------------------------------------------------------------

LochDOobs <- read.delim("data/metab_data_raw/The Loch/Loch2019_doobs.txt",stringsAsFactors = FALSE) 
Lochwnd <- read.delim("data/metab_data_raw/The Loch/Loch2019_wnd.txt", stringsAsFactors = FALSE) %>% dplyr::select(-X, -X.1, -X.2)
Lochtemp <- read.delim("data/metab_data_raw/The Loch/Loch2019_wtr.txt", stringsAsFactors = FALSE)  %>%
  rename(wtr_0.5 = wtr0.5)
LochPAR <- read.delim("data/metab_data_raw/The Loch/Loch2019_par.txt", stringsAsFactors = FALSE) %>% dplyr::select(-X, -X.1, -X.2)


glimpse(LochDOobs)
glimpse(Lochwnd)
glimpse(Lochtemp)
glimpse(LochPAR)





####DATE CONVERSION###


Lochtemp <- Lochtemp %>%
  mutate(dateTime = mdy_hm(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Denver"),
         dateTime = with_tz(dateTime, "GMT")) %>%
  filter(dateTime > "2019-06-11" & dateTime <= "2019-10-15") #ice off and on dates

LochDOobs <- LochDOobs %>%
  mutate(dateTime = mdy_hm(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Denver"),
         dateTime = with_tz(dateTime, "GMT")) %>%
  rename(doObs_0.5=doobs) %>%
  filter(dateTime > "2019-06-11" & dateTime <= "2019-10-15") #ice off and on dates

glimpse(Lochwnd)
Lochwnd <- Lochwnd %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Denver"),
         dateTime = with_tz(dateTime, "GMT"))%>%
  # convert(num(wind))%>% #convert from character to numeric -- doesn't come up with NAs or warnings
           # wind=as.numeric(wind)) %>%
  filter(dateTime > "2019-06-11" & dateTime <= "2019-10-15") #ice off and on dates


LochPAR <- LochPAR %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Denver"),
         dateTime = with_tz(dateTime, "GMT"),
         PAR=PAR*2.15) %>% #See "Estimating PAR from solar flux data" Word Doc in Troubleshooting folder
  filter(dateTime > "2019-06-11" & dateTime <= "2019-10-15") %>%#ice off and on dates
  mutate(daylightSavings = case_when(daylightSavings == 'Yes' ~ 'yes')) # renaming for joining purposes

#Print first five time readings for each variable
firstFiveTimes <- data.frame(DO=LochDOobs$dateTime[1:5],
                             PAR=LochPAR$dateTime[1:5],
                             wind=Lochwnd$dateTime[1:5],
                             temp=Lochtemp$dateTime[1:5])
print('First five time readings for each variable'); print(firstFiveTimes)

#Calculate first differences of time readings, display unique values
cat('\n','First differences of datetime','\n')
difTimesDO <- diff(LochDOobs$dateTime); print(table(difTimesDO))
difTimesPAR <- diff(LochPAR$dateTime); print(table(difTimesPAR))
difTimesWindSpeed <- diff(Lochwnd$dateTime); print(table(difTimesWindSpeed))
difTimesSensorTemp <- diff(Lochtemp$dateTime); print(table(difTimesSensorTemp))
timeStep=10	# in minutes


notDupRows <- findNotDupRows("LochDOobs")
LochDOobs <- LochDOobs[notDupRows,]

notDupRows <- findNotDupRows("LochPAR")
LochPAR <- LochPAR[notDupRows,]

notDupRows <- findNotDupRows("Lochwnd")
Lochwnd <- Lochwnd[notDupRows,]

notDupRows <- findNotDupRows("Lochtemp")
Lochtemp <- Lochtemp[notDupRows,]

#Make all data sets extend from startTime to endTime by timeStep
#Note that for some lakes it may be necessary to aggregate some variables to coarser time scale to get match up
LochDOobs$dateTime <- floorMins(LochDOobs)
LochPAR$dateTime <- floorMins(LochPAR)
Lochwnd$dateTime <- floorMins(Lochwnd)
Lochtemp$dateTime <- floorMins(Lochtemp)

#Repeat check for dup rows in case any introduced during floorMins
notDupRows <- findNotDupRows("LochDOobs")
LochDOobs <- LochDOobs[notDupRows,]
notDupRows <- findNotDupRows("LochPAR")
LochPAR <- LochPAR[notDupRows,]
notDupRows <- findNotDupRows("Lochwnd")
Lochwnd <- Lochwnd[notDupRows,]
notDupRows <- findNotDupRows("Lochtemp")
Lochtemp <- Lochtemp[notDupRows,]

#Find the latest first time point and the earliest last time point of all the data
startTime <- max(min(LochDOobs$dateTime),min(LochPAR$dateTime),min(Lochwnd$dateTime),min(Lochtemp$dateTime))
endTime <- min(max(LochDOobs$dateTime),max(LochPAR$dateTime),max(Lochwnd$dateTime),max(Lochtemp$dateTime))

#Data.frame with one column "dateTime" which is sequence of times at time interval of timeStep, from startTime to endTime
completeTimes <- data.frame(dateTime=seq(startTime,endTime,paste(timeStep,"mins")))

#Merge all of input data.frames with completeTimes, so that they now all extend from startTime to endTime by timeStep
LochDOobs <- merge(completeTimes,LochDOobs,by="dateTime",all.x=T)%>%
  select(-localTZ, -lakeID, -daylightSavings)
LochPAR <- merge(completeTimes,LochPAR,by="dateTime",all.x=T)%>%
  select(-localTZ, -lakeID, -daylightSavings)
Lochwnd <- merge(completeTimes,Lochwnd,by="dateTime",all.x=T)%>%
  select(-localTZ, -lakeID, -daylightSavings)
Lochtemp <- merge(completeTimes,Lochtemp,by="dateTime",all.x=T)%>%
  select(-localTZ, -lakeID, -daylightSavings)

#Convert wind from character to number
Lochwnd$wind <- as.numeric(Lochwnd$wind)


#Is it a problem that there are NAs in the PAR data? I guess we will find out
##IAO - 2021-02-12 --  I just checked and it shouldn't be because of the fillHoles helper function. 

Loch_full_2019 <- left_join(LochDOobs, Lochwnd, by = c("dateTime"))
Loch_full_2019 <- left_join(Loch_full_2019, Lochtemp, by = c("dateTime"))
Loch_full_2019 <- left_join(Loch_full_2019, LochPAR, by = c("dateTime"))

glimpse(Loch_full_2019)


#  ---2020--- --------------------------------------------------------------

LochDOobs <- read.delim("data/metab_data_raw/The Loch/Loch2020_doobs.txt",stringsAsFactors = FALSE) 
Lochwnd <- read.delim("data/metab_data_raw/The Loch/Loch2020_wnd.txt", stringsAsFactors = FALSE) 
Lochtemp <- read.delim("data/metab_data_raw/The Loch/Loch2020_wtr.txt", stringsAsFactors = FALSE)  
LochPAR <- read.delim("data/metab_data_raw/The Loch/Loch2020_par.txt", stringsAsFactors = FALSE)



####DATE CONVERSION###

glimpse(Lochtemp)
Lochtemp <- Lochtemp %>%
  mutate(dateTime = mdy_hm(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Denver"),
         dateTime = with_tz(dateTime, "GMT")) %>%
  filter(dateTime <= "2020-10-01") #conservative estimate

glimpse(LochDOobs)
LochDOobs <- LochDOobs %>%
  rename(dateTime="Mountain.Standard.Time")%>%
  mutate(dateTime = mdy_hm(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Denver"),
         dateTime = with_tz(dateTime, "GMT")) %>%
  rename(doObs_0.5=doobs) %>%
  filter(dateTime <= "2020-10-01") #conservative estimate

glimpse(Lochwnd)
Lochwnd <- Lochwnd %>%
  mutate(dateTime = mdy_hm(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Denver"),
         dateTime = with_tz(dateTime, "GMT"))%>%
  filter(dateTime <= "2020-10-01") #conservative estimate

glimpse(LochPAR)
LochPAR <- LochPAR %>%
  mutate(dateTime = mdy_hm(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Denver"),
         dateTime = with_tz(dateTime, "GMT"),
         PAR=PAR*2.15) %>% #See "Estimating PAR from solar flux data" Word Doc in Troubleshooting folder
  filter(dateTime <= "2020-10-01") 

#Print first five time readings for each variable
firstFiveTimes <- data.frame(DO=LochDOobs$dateTime[1:5],
                             PAR=LochPAR$dateTime[1:5],
                             wind=Lochwnd$dateTime[1:5],
                             temp=Lochtemp$dateTime[1:5])
print('First five time readings for each variable'); print(firstFiveTimes)

#Calculate first differences of time readings, display unique values
cat('\n','First differences of datetime','\n')
difTimesDO <- diff(LochDOobs$dateTime); print(table(difTimesDO))
difTimesPAR <- diff(LochPAR$dateTime); print(table(difTimesPAR))
difTimesWindSpeed <- diff(Lochwnd$dateTime); print(table(difTimesWindSpeed))
difTimesSensorTemp <- diff(Lochtemp$dateTime); print(table(difTimesSensorTemp))
timeStep=30	# in minutes


notDupRows <- findNotDupRows("LochDOobs")
LochDOobs <- LochDOobs[notDupRows,]

notDupRows <- findNotDupRows("LochPAR")
LochPAR <- LochPAR[notDupRows,]

notDupRows <- findNotDupRows("Lochwnd")
Lochwnd <- Lochwnd[notDupRows,]

notDupRows <- findNotDupRows("Lochtemp")
Lochtemp <- Lochtemp[notDupRows,]

#Make all data sets extend from startTime to endTime by timeStep
#Note that for some lakes it may be necessary to aggregate some variables to coarser time scale to get match up
LochDOobs$dateTime <- floorMins(LochDOobs)
LochPAR$dateTime <- floorMins(LochPAR)
Lochwnd$dateTime <- floorMins(Lochwnd)
Lochtemp$dateTime <- floorMins(Lochtemp)

#Repeat check for dup rows in case any introduced during floorMins
notDupRows <- findNotDupRows("LochDOobs")
LochDOobs <- LochDOobs[notDupRows,]
notDupRows <- findNotDupRows("LochPAR")
LochPAR <- LochPAR[notDupRows,]
notDupRows <- findNotDupRows("Lochwnd")
Lochwnd <- Lochwnd[notDupRows,]
notDupRows <- findNotDupRows("Lochtemp")
Lochtemp <- Lochtemp[notDupRows,]

#Find the latest first time point and the earliest last time point of all the data
startTime <- max(min(LochDOobs$dateTime),min(LochPAR$dateTime),min(Lochwnd$dateTime),min(Lochtemp$dateTime))
endTime <- min(max(LochDOobs$dateTime),max(LochPAR$dateTime),max(Lochwnd$dateTime),max(Lochtemp$dateTime))

#Data.frame with one column "dateTime" which is sequence of times at time interval of timeStep, from startTime to endTime
completeTimes <- data.frame(dateTime=seq(startTime,endTime,paste(timeStep,"mins")))

#Merge all of input data.frames with completeTimes, so that they now all extend from startTime to endTime by timeStep
LochDOobs <- merge(completeTimes,LochDOobs,by="dateTime",all.x=T)%>%
  select(-localTZ, -lakeID, -daylightSavings)
LochPAR <- merge(completeTimes,LochPAR,by="dateTime",all.x=T)%>%
  select(-localTZ, -lakeID, -daylightSavings)
Lochwnd <- merge(completeTimes,Lochwnd,by="dateTime",all.x=T)%>%
  select(-localTZ, -lakeID, -daylightSavings)
Lochtemp <- merge(completeTimes,Lochtemp,by="dateTime",all.x=T)%>%
  select(-localTZ, -lakeID, -daylightSavings)

#Convert wind from character to number
Lochwnd$wind <- as.numeric(Lochwnd$wind)



Loch_full_2020 <- left_join(LochDOobs, Lochwnd, by = c("dateTime"))
Loch_full_2020 <- left_join(Loch_full_2020, Lochtemp, by = c("dateTime"))
Loch_full_2020 <- left_join(Loch_full_2020, LochPAR, by = c("dateTime"))

glimpse(Loch_full_2020)

#Preview data
#temp
Loch_full_2020 %>%
  dplyr::select(dateTime, wtr0.5:wtr4.5)%>%
  pivot_longer(-(dateTime), names_sep = "r", names_to = c("variable","depth")) %>%
  dplyr::select(-variable)%>%
  rename(temp=value)%>%
  ggplot(aes(x=dateTime, y=temp, color=depth)) + 
  geom_point(size=2)+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%y-%m-%d") +
  ylab("Temp C")+
  ggtitle(" Loch 2020")

#DO

####plot DO###########

Loch_full_2020 %>%
  ggplot(aes(x=dateTime, y=doObs_0.5)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%y-%m") +
  ylab("DO obs")+
  ggtitle(" Loch 2020") 

Loch_full_2020 %>%
  ggplot(aes(x=dateTime, y=wind)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%y-%m") +
  ylab("Wind (m/s)")+
  ggtitle(" Loch 2020") 

Loch_full_2020 %>%
  ggplot(aes(x=dateTime, y=PAR)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%y-%m") +
  ylab("PAR")+
  ggtitle("Loch 2020") 



Loch_full_master <- bind_rows(Loch_full_2016,
                              Loch_full_2017,
                              Loch_full_2018,
                              Loch_full_2019)
glimpse(Loch_full_master)

####plot TEMPS###########
head(Loch_full_master)
Loch_full_master %>%
  dplyr::select(dateTime, wtr0.5, wtr4.5)%>%
  pivot_longer(-(dateTime), names_sep = "r", names_to = c("variable","depth")) %>%
  dplyr::select(-variable)%>%
  mutate(depth=as.numeric(as.character(depth)),
         year=year(dateTime)) %>%
  rename(temp=value)%>%
  ggplot(aes(x=dateTime, y=temp, color=depth)) + 
  geom_point(size=2)+
  scale_x_datetime(date_breaks = "2 weeks", date_labels = "%m-%d") +
  ylab("Temp C")+
  ggtitle("Lake Loch 2016-2019")+
  scale_color_continuous_sequential(palette = "Dark Mint", rev=TRUE)+
  facet_wrap(.~year, scales="free_x")



####plot DO###########
Loch_full_master %>%
  mutate(year=year(dateTime))%>%
  ggplot(aes(x=dateTime, y=doObs)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%y-%m") +
  ylab("DO obs")+
  ggtitle("The Loch 2016-2019")+
  facet_wrap(.~year, scales="free_x")


####plot WIND###########
Loch_full_master %>%
  mutate(year=year(dateTime))%>%
  ggplot(aes(x=dateTime, y=wind)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "2 weeks", date_labels = "%m-%d") +
  ylab("Wind")+
  ggtitle("The Loch 2016-2019")+
  facet_wrap(.~year, scales="free_x")

####plot PAR###########
Loch_full_master %>%
  mutate(year=year(dateTime))%>%
  ggplot(aes(x=dateTime, y=PAR)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "2 weeks", date_labels = "%m-%d") +
  ylab("PAR")+
  ggtitle("The Loch 2016-2019")+
  facet_wrap(.~year, scales="free_x")


####export for modelling###########
LochDOobs <- Loch_full_2016 %>%
  dplyr::select(dateTime, doObs_0.5) %>%
  rename(DO=doObs_0.5) 
write.csv(LochDOobs, "data/metab_data_clean/loch/loch_2016_DO_0.5m.txt", row.names=FALSE)

LochDOobs <- Loch_full_2017 %>%
  dplyr::select(dateTime, doObs_0.5) %>%
  rename(DO=doObs_0.5) 
write.csv(LochDOobs, "data/metab_data_clean/loch/loch_2017_DO_0.5m.txt", row.names=FALSE)

LochDOobs <- Loch_full_2018 %>%
  dplyr::select(dateTime, doObs_0.5) %>%
  rename(DO=doObs_0.5) 
write.csv(LochDOobs, "data/metab_data_clean/loch/loch_2018_DO_0.5m.txt", row.names=FALSE)

LochDOobs <- Loch_full_2019 %>%
  dplyr::select(dateTime, doObs_0.5) %>%
  rename(DO=doObs_0.5) 
write.csv(LochDOobs, "data/metab_data_clean/loch/loch_2019_DO_0.5m.txt", row.names=FALSE)

LochDOobs <- Loch_full_2020 %>%
  dplyr::select(dateTime, doObs_0.5) %>%
  rename(DO=doObs_0.5) 
write.csv(LochDOobs, "data/metab_data_clean/loch/loch_2020_DO_0.5m.txt", row.names=FALSE)



LochDOobs <- Loch_full_2016 %>%
  dplyr::select(dateTime, doObs_4.5) %>%
  rename(DO=doObs_4.5) 
write.csv(LochDOobs, "data/metab_data_clean/loch/loch_2016_DO_4.5m.txt", row.names=FALSE)

LochDOobs <- Loch_full_2017 %>%
  dplyr::select(dateTime, doObs_4.5) %>%
  rename(DO=doObs_4.5) 
write.csv(LochDOobs, "data/metab_data_clean/loch/loch_2017_DO_4.5m.txt", row.names=FALSE)

LochDOobs <- Loch_full_2018 %>%
  dplyr::select(dateTime, doObs_4.5) %>%
  rename(DO=doObs_4.5)
write.csv(LochDOobs, "data/metab_data_clean/loch/loch_2018_DO_4.5m.txt", row.names=FALSE)

LochDOobs <- Loch_full_2019 %>%
  dplyr::select(dateTime, doObs_4.5) %>%
  rename(DO=doObs_4.5) 
write.csv(LochDOobs, "data/metab_data_clean/loch/loch_2019_DO_4.5m.txt", row.names=FALSE)



#DO sensor depth = 0.5
LochsensorTemp <- Loch_full_2016 %>%
  dplyr::select(dateTime, wtr_0.5) %>%
  rename(sensorTemp=wtr_0.5) 
write.csv(LochsensorTemp, "data/metab_data_clean/loch/loch_2016_sensorTemp_0.5m.txt", row.names=FALSE)

LochsensorTemp <- Loch_full_2017 %>%
  dplyr::select(dateTime, wtr_0.5) %>%
  rename(sensorTemp=wtr_0.5) 
write.csv(LochsensorTemp, "data/metab_data_clean/loch/loch_2017_sensorTemp_0.5m.txt", row.names=FALSE)

LochsensorTemp <- Loch_full_2018 %>%
  dplyr::select(dateTime, wtr_0.5) %>%
  rename(sensorTemp=wtr_0.5) 
write.csv(LochsensorTemp, "data/metab_data_clean/loch/loch_2018_sensorTemp_0.5m.txt", row.names=FALSE)

LochsensorTemp <- Loch_full_2019 %>%
  dplyr::select(dateTime, wtr_0.5) %>%
  rename(sensorTemp=wtr_0.5) 
write.csv(LochsensorTemp, "data/metab_data_clean/loch/loch_2019_sensorTemp_0.5m.txt", row.names=FALSE)

LochsensorTemp <- Loch_full_2020 %>%
  dplyr::select(dateTime, wtr0.5) %>%
  rename(sensorTemp=wtr0.5) 
write.csv(LochsensorTemp, "data/metab_data_clean/loch/loch_2020_sensorTemp_0.5m.txt", row.names=FALSE)


#DO sensor depth = 4.5
LochsensorTemp <- Loch_full_2016 %>%
  dplyr::select(dateTime, wtr_4.5) %>%
  rename(sensorTemp=wtr_4.5) 
write.csv(LochsensorTemp, "data/metab_data_clean/loch/loch_2016_sensorTemp_4.5m.txt", row.names=FALSE)

LochsensorTemp <- Loch_full_2017 %>%
  dplyr::select(dateTime, wtr_4.5) %>%
  rename(sensorTemp=wtr_4.5) 
write.csv(LochsensorTemp, "data/metab_data_clean/loch/loch_2017_sensorTemp_4.5m.txt", row.names=FALSE)

LochsensorTemp <- Loch_full_2018 %>%
  dplyr::select(dateTime, wtr_4.5) %>%
  rename(sensorTemp=wtr_4.5) 
write.csv(LochsensorTemp, "data/metab_data_clean/loch/loch_2018_sensorTemp_4.5m.txt", row.names=FALSE)

# LochsensorTemp <- Loch_full_2019 %>%
#   dplyr::select(dateTime, wtr_4.5) %>%
#   rename(sensorTemp=wtr_4.5) 
# write.csv(LochsensorTemp, "data/metab_data_clean/loch/loch_2019_sensorTemp_4.5m.txt", row.names=FALSE)

LochsensorTemp <- Loch_full_2020 %>%
  dplyr::select(dateTime, wtr4.5) %>%
  rename(sensorTemp=wtr4.5) 
write.csv(LochsensorTemp, "data/metab_data_clean/loch/loch_2020_sensorTemp_4.5m.txt", row.names=FALSE)



#PAR
LochPAR <- Loch_full_2016 %>%
  dplyr::select(dateTime, PAR) 
write.csv(LochPAR, "data/metab_data_clean/loch/loch_2016_PAR.txt", row.names=FALSE)

LochPAR <- Loch_full_2017 %>%
  dplyr::select(dateTime, PAR) 
write.csv(LochPAR, "data/metab_data_clean/loch/loch_2017_PAR.txt", row.names=FALSE)

LochPAR <- Loch_full_2018 %>%
  dplyr::select(dateTime, PAR) 
write.csv(LochPAR, "data/metab_data_clean/loch/loch_2018_PAR.txt", row.names=FALSE)

LochPAR <- Loch_full_2019 %>%
  dplyr::select(dateTime, PAR) 
write.csv(LochPAR, "data/metab_data_clean/loch/loch_2019_PAR.txt", row.names=FALSE)

LochPAR <- Loch_full_2020 %>%
  dplyr::select(dateTime, PAR) 
write.csv(LochPAR, "data/metab_data_clean/loch/loch_2020_PAR.txt", row.names=FALSE)


#wind height = 2 above lake surface
Lochwind <- Loch_full_2016 %>%
  dplyr::select(dateTime, wind) %>%
  rename(windSpeed=wind) 
write.csv(Lochwind, "data/metab_data_clean/loch/loch_2016_windSpeed.txt", row.names=FALSE)

Lochwind <- Loch_full_2017 %>%
  dplyr::select(dateTime, wind) %>%
  rename(windSpeed=wind) 
write.csv(Lochwind, "data/metab_data_clean/loch/loch_2017_windSpeed.txt", row.names=FALSE)

Lochwind <- Loch_full_2018 %>%
  dplyr::select(dateTime, wind) %>%
  rename(windSpeed=wind) 
write.csv(Lochwind, "data/metab_data_clean/loch/loch_2018_windSpeed.txt", row.names=FALSE)

Lochwind <- Loch_full_2019 %>%
  dplyr::select(dateTime, wind) %>%
  rename(windSpeed=wind) 
write.csv(Lochwind, "data/metab_data_clean/loch/loch_2019_windSpeed.txt", row.names=FALSE)

Lochwind <- Loch_full_2020 %>%
  dplyr::select(dateTime, wind) %>%
  rename(windSpeed=wind) 
write.csv(Lochwind, "data/metab_data_clean/loch/loch_2020_windSpeed.txt", row.names=FALSE)





#temp profile
Lochtempprofile<- Loch_full_2016 %>%
  dplyr::select(dateTime, wtr_0.5, wtr_4.5) 
write.csv(Lochtempprofile, "data/metab_data_clean/loch/loch_2016_tempProfile.txt", row.names=FALSE)


Lochtempprofile<- Loch_full_2017 %>%
  dplyr::select(dateTime, wtr_0.5, wtr_4.5) 
write.csv(Lochtempprofile, "data/metab_data_clean/loch/loch_2017_tempProfile.txt", row.names=FALSE)



Lochtempprofile<- Loch_full_2018 %>%
  dplyr::select(dateTime, wtr_0.5, wtr_4.5) 
write.csv(Lochtempprofile, "data/metab_data_clean/loch/loch_2018_tempProfile.txt", row.names=FALSE)


Lochtempprofile<- Loch_full_2019 %>%
  dplyr::select(dateTime, wtr_0.5) 
write.csv(Lochtempprofile, "data/metab_data_clean/loch/loch_2019_tempProfile.txt", row.names=FALSE)


Lochtempprofile<- Loch_full_2020 %>%
  dplyr::select(dateTime, wtr0.5, wtr4.5) %>%
  rename_all(function(x) gsub("wtr", "wtr_", x)) #Add an underscore to all heads to read wtr_#.#
write.csv(Lochtempprofile, "data/metab_data_clean/loch/loch_2020_tempProfile.txt", row.names=FALSE)





# > Sky Pond -------------------------------------------------------------


# ---2016--- --------------------------------------------------------------


SkyDOobs <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Sky Pond/Sky2016_doobs.txt",stringsAsFactors = FALSE) 
Skywnd <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Sky Pond/Sky2016_wnd.txt", stringsAsFactors = FALSE) 
Skytemp <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Sky Pond/Sky2016_wtr.txt", stringsAsFactors = FALSE) 
SkyPAR <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Sky Pond/Sky2016_par.txt", stringsAsFactors = FALSE) 

#Doing an experiment to see how our estimates change if we use the 6.5m sensor from 2016
SkytempDO_6.5m <- read.csv("~/Google Drive/Research (common)/Research/Data/Master - buoys/Sky/sky_buoy_HOBO_MiniDot_master_181029.csv", stringsAsFactors = FALSE) %>%
  select(datetime, DOconc_6.5, temp_6.5)






####DATE CONVERSION###

glimpse(Skytemp)
Skytemp <- Skytemp %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Denver"),
         dateTime = with_tz(dateTime, "GMT"),
         year = year(dateTime)) %>%
  filter(case_when(year=="2016" ~ dateTime >= "2016-06-13" & dateTime <= "2016-11-02", 
                   T ~ dateTime >= "2017-05-30" & dateTime <= "2017-10-03"))
#Filter dates on a conditional basis:
# based on the assumption that ice-off is 2 weeks later and ice-on is 2 weeks earlier in Sky Pond
# Also have some data from 2017 in here that we don't want to miss. 
glimpse(Skytemp)

glimpse(SkyDOobs)
SkyDOobs <- SkyDOobs %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Denver"),
         dateTime = with_tz(dateTime, "GMT"),
         year = year(dateTime)) %>%
  filter(case_when(year=="2016" ~ dateTime >= "2016-06-13" & dateTime <= "2016-11-02", 
                   T ~ dateTime >= "2017-05-30" & dateTime <= "2017-10-03"))
#Filter dates on a conditional basis:
# based on the assumption that ice-off is 2 weeks later and ice-on is 2 weeks earlier in Sky Pond
# Also have some data from 2017 in here that we don't want to miss. 
glimpse(SkyDOobs)

glimpse(Skywnd)
Skywnd <- Skywnd %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Denver"),
         dateTime = with_tz(dateTime, "GMT"),
         year = year(dateTime)) %>%
  filter(case_when(year=="2016" ~ dateTime >= "2016-06-13" & dateTime <= "2016-11-02", 
                   T ~ dateTime >= "2017-05-30" & dateTime <= "2017-10-03"))
#Filter dates on a conditional basis:
# based on the assumption that ice-off is 2 weeks later and ice-on is 2 weeks earlier in Sky Pond
# Also have some data from 2017 in here that we don't want to miss. 
glimpse(Skywnd)

glimpse(SkyPAR)
SkyPAR <- SkyPAR %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Denver"),
         dateTime = with_tz(dateTime, "GMT"),
         year = year(dateTime),
         PAR=PAR*2.15) %>% #See "Estimating PAR from solar flux data" Word Doc in Troubleshooting folder
  filter(case_when(year=="2016" ~ dateTime >= "2016-06-13" & dateTime <= "2016-11-02", 
                   T ~ dateTime >= "2017-05-30" & dateTime <= "2017-10-03"))
#Filter dates on a conditional basis:
# based on the assumption that ice-off is 2 weeks later and ice-on is 2 weeks earlier in Sky Pond
# Also have some data from 2017 in here that we don't want to miss. 
glimpse(SkyPAR)

glimpse(SkytempDO_6.5m)
SkytempDO_6.5m <- SkytempDO_6.5m %>%
  rename(dateTime = datetime,
         doObs = DOconc_6.5,
         wtr_6.5 = temp_6.5) %>%
  mutate(dateTime = mdy_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Denver"),
         dateTime = with_tz(dateTime, "GMT"),
         year = year(dateTime)) %>% #See "Estimating PAR from solar flux data" Word Doc in Troubleshooting folder
  filter(case_when(year=="2016" ~ dateTime >= "2016-06-13" & dateTime <= "2016-11-02", 
                   T ~ dateTime >= "2017-05-30" & dateTime <= "2017-10-03"))
#basing this time period of when it looks like sensor was removed
glimpse(SkytempDO_6.5m)
SkyDOobs<-SkytempDO_6.5m %>%
  select(dateTime, year, doObs)
Skytemp<-SkytempDO_6.5m %>%
  select(dateTime, year, wtr_6.5)


#Print first five time readings for each variable
firstFiveTimes <- data.frame(DO=SkyDOobs$dateTime[1:5],
                             PAR=SkyPAR$dateTime[1:5],
                             wind=Skywnd$dateTime[1:5],
                             temp=Skytemp$dateTime[1:5])
print('First five time readings for each variable'); print(firstFiveTimes)

#Calculate first differences of time readings, display unique values
cat('\n','First differences of datetime','\n')
difTimesDO <- diff(SkyDOobs$dateTime); print(table(difTimesDO))
difTimesPAR <- diff(SkyPAR$dateTime); print(table(difTimesPAR))
difTimesWindSpeed <- diff(Skywnd$dateTime); print(table(difTimesWindSpeed))
difTimesSensorTemp <- diff(Skytemp$dateTime); print(table(difTimesSensorTemp))
timeStep=30	# in minutes


notDupRows <- findNotDupRows("SkyDOobs")
SkyDOobs <- SkyDOobs[notDupRows,]

notDupRows <- findNotDupRows("SkyPAR")
SkyPAR <- SkyPAR[notDupRows,]

notDupRows <- findNotDupRows("Skywnd")
Skywnd <- Skywnd[notDupRows,]

notDupRows <- findNotDupRows("Skytemp")
Skytemp <- Skytemp[notDupRows,]

#Make all data sets extend from startTime to endTime by timeStep
#Note that for some lakes it may be necessary to aggregate some variables to coarser time scale to get match up
SkyDOobs$dateTime <- floorMins(SkyDOobs)
SkyPAR$dateTime <- floorMins(SkyPAR)
Skywnd$dateTime <- floorMins(Skywnd)
Skytemp$dateTime <- floorMins(Skytemp)

#Repeat check for dup rows in case any introduced during floorMins
notDupRows <- findNotDupRows("SkyDOobs")
SkyDOobs <- SkyDOobs[notDupRows,]
notDupRows <- findNotDupRows("SkyPAR")
SkyPAR <- SkyPAR[notDupRows,]
notDupRows <- findNotDupRows("Skywnd")
Skywnd <- Skywnd[notDupRows,]
notDupRows <- findNotDupRows("Skytemp")
Skytemp <- Skytemp[notDupRows,]

#Find the latest first time point and the earliest last time point of all the data
startTime <- max(min(SkyDOobs$dateTime),min(SkyPAR$dateTime),min(Skywnd$dateTime),min(Skytemp$dateTime))
endTime <- min(max(SkyDOobs$dateTime),max(SkyPAR$dateTime),max(Skywnd$dateTime),max(Skytemp$dateTime))

#Data.frame with one column "dateTime" which is sequence of times at time interval of timeStep, from startTime to endTime
completeTimes <- data.frame(dateTime=seq(startTime,endTime,paste(timeStep,"mins"))) %>%
  mutate(year = year(dateTime)) %>%
  filter(case_when(year=="2016" ~ dateTime >= "2016-06-13" & dateTime <= "2016-11-02",
                   T ~ dateTime >= "2017-05-30" & dateTime <= "2017-10-03"))

#Merge all of input data.frames with completeTimes, so that they now all extend from startTime to endTime by timeStep
SkyDOobs <- merge(completeTimes,SkyDOobs,by=c("dateTime","year"),all.x=T)
SkyPAR <- merge(completeTimes,SkyPAR,by=c("dateTime","year"),all.x=T)
Skywnd <- merge(completeTimes,Skywnd,by=c("dateTime","year"),all.x=T)%>%
  select(-localTZ, -lakeID, -daylightSavings)
Skytemp <- merge(completeTimes,Skytemp,by=c("dateTime","year"),all.x=T)

#Is it a problem that there are NAs in the PAR data? I guess we will find out
##IAO - 2021-02-12 --  I just checked and it shouldn't be because of the fillHoles helper function. 

Sky_full_2016 <- left_join(SkyDOobs, Skywnd, by = c("dateTime","year"))
Sky_full_2016 <- left_join(Sky_full_2016, Skytemp, by = c("dateTime","year"))
Sky_full_2016 <- left_join(Sky_full_2016, SkyPAR, by = c("dateTime","year"))

glimpse(Sky_full_2016)
tz(Sky_full_2016$dateTime)



# ---2017--- --------------------------------------------------------------


SkyDOobs <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Sky Pond/Sky2017_doobs.txt",stringsAsFactors = FALSE) 
Skywnd <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Sky Pond/Sky2017_wnd.txt", stringsAsFactors = FALSE) 
Skytemp <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Sky Pond/Sky2017_wtr.txt", stringsAsFactors = FALSE) 
SkyPAR <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Sky Pond/Sky2017_par.txt", stringsAsFactors = FALSE) 


#Doing an experiment to see how our estimates change if we use the 6.5m sensor from 2016
SkytempDO_6.5m <- read.csv("~/Google Drive/Research (common)/Research/Data/Master - buoys/Sky/sky_buoy_HOBO_MiniDot_master_181029.csv", stringsAsFactors = FALSE) %>%
  select(datetime, DOconc_6.5, temp_6.5)






####DATE CONVERSION###

glimpse(Skytemp)
Skytemp <- Skytemp %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Denver"),
         dateTime = with_tz(dateTime, "GMT"),
         year = year(dateTime)) %>%
  filter(case_when(year=="2017" ~ dateTime >= "2017-05-30" & dateTime <= "2017-10-03", 
                   T ~ dateTime >= "2018-05-30" & dateTime <= "2018-10-03"))
#Filter dates on a conditional basis:
# based on the assumption that ice-off is 2 weeks later and ice-on is 2 weeks earlier in Sky Pond
# Also have some data from 2018 in here that we don't want to miss. 
glimpse(Skytemp)

glimpse(SkyDOobs)
SkyDOobs <- SkyDOobs %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Denver"),
         dateTime = with_tz(dateTime, "GMT"),
         year = year(dateTime)) %>%
  filter(case_when(year=="2017" ~ dateTime >= "2017-05-30" & dateTime <= "2017-10-03", 
                   T ~ dateTime >= "2018-05-30" & dateTime <= "2018-10-03"))
#Filter dates on a conditional basis:
# based on the assumption that ice-off is 2 weeks later and ice-on is 2 weeks earlier in Sky Pond
# Also have some data from 2018 in here that we don't want to miss. 
glimpse(SkyDOobs)

glimpse(Skywnd)
Skywnd <- Skywnd %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Denver"),
         dateTime = with_tz(dateTime, "GMT"),
         year = year(dateTime)) %>%
  filter(case_when(year=="2017" ~ dateTime >= "2017-05-30" & dateTime <= "2017-10-03", 
                   T ~ dateTime >= "2018-05-30" & dateTime <= "2018-10-03"))
#Filter dates on a conditional basis:
# based on the assumption that ice-off is 2 weeks later and ice-on is 2 weeks earlier in Sky Pond
# Also have some data from 2018 in here that we don't want to miss. 
glimpse(Skywnd)

glimpse(SkyPAR)
SkyPAR <- SkyPAR %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Denver"),
         dateTime = with_tz(dateTime, "GMT"),
         year = year(dateTime),
         PAR=PAR*2.15) %>% #See "Estimating PAR from solar flux data" Word Doc in Troubleshooting folder
  filter(case_when(year=="2017" ~ dateTime >= "2017-05-30" & dateTime <= "2017-10-03", 
                   T ~ dateTime >= "2018-05-30" & dateTime <= "2018-10-03"))
#Filter dates on a conditional basis:
# based on the assumption that ice-off is 2 weeks later and ice-on is 2 weeks earlier in Sky Pond
# Also have some data from 2018 in here that we don't want to miss. 
glimpse(SkyPAR)


glimpse(SkytempDO_6.5m)
SkytempDO_6.5m <- SkytempDO_6.5m %>%
  rename(dateTime = datetime,
         doObs = DOconc_6.5,
         wtr_6.5 = temp_6.5) %>%
  mutate(dateTime = mdy_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Denver"),
         dateTime = with_tz(dateTime, "GMT"),
         year = year(dateTime)) %>% #See "Estimating PAR from solar flux data" Word Doc in Troubleshooting folder
  filter(case_when(year=="2017" ~ dateTime >= "2017-05-30" & dateTime <= "2017-10-03", 
                   T ~ dateTime >= "2018-05-30" & dateTime <= "2018-10-03"))
#basing this time period of when it looks like sensor was removed
glimpse(SkytempDO_6.5m)
SkyDOobs<-SkytempDO_6.5m %>%
  select(dateTime, year, doObs)
Skytemp<-SkytempDO_6.5m %>%
  select(dateTime, year, wtr_6.5)




#Print first five time readings for each variable
firstFiveTimes <- data.frame(DO=SkyDOobs$dateTime[1:5],
                             PAR=SkyPAR$dateTime[1:5],
                             wind=Skywnd$dateTime[1:5],
                             temp=Skytemp$dateTime[1:5])
print('First five time readings for each variable'); print(firstFiveTimes)

#Calculate first differences of time readings, display unique values
cat('\n','First differences of datetime','\n')
difTimesDO <- diff(SkyDOobs$dateTime); print(table(difTimesDO))
difTimesPAR <- diff(SkyPAR$dateTime); print(table(difTimesPAR))
difTimesWindSpeed <- diff(Skywnd$dateTime); print(table(difTimesWindSpeed))
difTimesSensorTemp <- diff(Skytemp$dateTime); print(table(difTimesSensorTemp))
timeStep=30	# in minutes


notDupRows <- findNotDupRows("SkyDOobs")
SkyDOobs <- SkyDOobs[notDupRows,]

notDupRows <- findNotDupRows("SkyPAR")
SkyPAR <- SkyPAR[notDupRows,]

notDupRows <- findNotDupRows("Skywnd")
Skywnd <- Skywnd[notDupRows,]

notDupRows <- findNotDupRows("Skytemp")
Skytemp <- Skytemp[notDupRows,]

#Make all data sets extend from startTime to endTime by timeStep
#Note that for some lakes it may be necessary to aggregate some variables to coarser time scale to get match up
SkyDOobs$dateTime <- floorMins(SkyDOobs)
SkyPAR$dateTime <- floorMins(SkyPAR)
Skywnd$dateTime <- floorMins(Skywnd)
Skytemp$dateTime <- floorMins(Skytemp)

#Repeat check for dup rows in case any introduced during floorMins
notDupRows <- findNotDupRows("SkyDOobs")
SkyDOobs <- SkyDOobs[notDupRows,]
notDupRows <- findNotDupRows("SkyPAR")
SkyPAR <- SkyPAR[notDupRows,]
notDupRows <- findNotDupRows("Skywnd")
Skywnd <- Skywnd[notDupRows,]
notDupRows <- findNotDupRows("Skytemp")
Skytemp <- Skytemp[notDupRows,]

#Find the latest first time point and the earliest last time point of all the data
startTime <- max(min(SkyDOobs$dateTime),min(SkyPAR$dateTime),min(Skywnd$dateTime),min(Skytemp$dateTime))
endTime <- min(max(SkyDOobs$dateTime),max(SkyPAR$dateTime),max(Skywnd$dateTime),max(Skytemp$dateTime))

#Data.frame with one column "dateTime" which is sequence of times at time interval of timeStep, from startTime to endTime
completeTimes <- data.frame(dateTime=seq(startTime,endTime,paste(timeStep,"mins"))) %>%
  mutate(year = year(dateTime)) %>%
  filter(case_when(year=="2017" ~ dateTime >= "2017-05-30" & dateTime <= "2017-10-03", 
                   T ~ dateTime >= "2018-05-30" & dateTime <= "2018-10-03"))

#Merge all of input data.frames with completeTimes, so that they now all extend from startTime to endTime by timeStep
SkyDOobs <- merge(completeTimes,SkyDOobs,by=c("dateTime","year"),all.x=T)
SkyPAR <- merge(completeTimes,SkyPAR,by=c("dateTime","year"),all.x=T)%>%
  select(-localTZ, -lakeID, -daylightSavings)
Skywnd <- merge(completeTimes,Skywnd,by=c("dateTime","year"),all.x=T)%>%
  select(-localTZ, -lakeID, -daylightSavings)
Skytemp <- merge(completeTimes,Skytemp,by=c("dateTime","year"),all.x=T)

#Is it a problem that there are NAs in the PAR data? I guess we will find out
##IAO - 2021-02-12 --  I just checked and it shouldn't be because of the fillHoles helper function. 

Sky_full_2017 <- left_join(SkyDOobs, Skywnd, by = c("dateTime","year"))
Sky_full_2017 <- left_join(Sky_full_2017, Skytemp, by = c("dateTime","year"))
Sky_full_2017 <- left_join(Sky_full_2017, SkyPAR, by = c("dateTime","year"))

glimpse(Sky_full_2017)
tz(Sky_full_2017$dateTime)




# ---2018--- --------------------------------------------------------------


SkyDOobs <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Sky Pond/Sky2018_doobs.txt",stringsAsFactors = FALSE) 
Skytemp <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Sky Pond/Sky2018_wtr.txt", stringsAsFactors = FALSE) 
SkyPAR <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Sky Pond/Sky2018_par.txt", stringsAsFactors = FALSE) 
Skywnd <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Sky Pond/Sky2018_wnd.txt", stringsAsFactors = FALSE) 

#Doing an experiment to see how our estimates change if we use the 6.5m sensor from 2018
SkytempDO_6.5m <- read.csv("~/Google Drive/Research (common)/Research/Data/Master - buoys/Sky/sky_buoy_HOBO_MiniDot_master_181029.csv", stringsAsFactors = FALSE) %>%
  select(datetime, DOconc_6.5, temp_6.5)




####DATE CONVERSION###

glimpse(Skytemp)
Skytemp <- Skytemp %>%
  mutate(dateTime = mdy_hm(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Denver"),
         dateTime = with_tz(dateTime, "GMT"),
         year = year(dateTime)) %>%
  filter(dateTime >= "2018-06-26 18:00:00" & dateTime <= "2018-08-25 16:00:00") 
#basing this time period of when it looks like sensor was removed

glimpse(SkyDOobs)
SkyDOobs <- SkyDOobs %>%
  mutate(dateTime = mdy_hm(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Denver"),
         dateTime = with_tz(dateTime, "GMT"),
         year = year(dateTime)) %>%
  filter(dateTime >= "2018-06-26 18:00:00" & dateTime <= "2018-08-25 16:00:00")
#basing this time period of when it looks like sensor was removed

glimpse(Skywnd)
Skywnd <- Skywnd %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Denver"),
         dateTime = with_tz(dateTime, "GMT"),
         year = year(dateTime))%>%
  # convert(num(wind))%>% #convert from character to numeric -- doesn't come up with NAs or warnings
  # wind=as.numeric(wind)) %>%
  filter(dateTime >= "2018-06-26 18:00:00" & dateTime <= "2018-08-25 16:00:00")
#basing this time period of when it looks like sensor was removed

glimpse(SkytempDO_6.5m)
SkytempDO_6.5m <- SkytempDO_6.5m %>%
  rename(dateTime = datetime,
         doObs = DOconc_6.5,
         wtr_6.5 = temp_6.5) %>%
  mutate(dateTime = mdy_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Denver"),
         dateTime = with_tz(dateTime, "GMT"),
         year = year(dateTime)) %>% #See "Estimating PAR from solar flux data" Word Doc in Troubleshooting folder
  filter(dateTime >= "2018-06-26 18:00:00" & dateTime <= "2018-08-25 16:00:00")
#basing this time period of when it looks like sensor was removed
glimpse(SkytempDO_6.5m)
SkyDOobs<-SkytempDO_6.5m %>%
  select(dateTime, year, doObs)
Skytemp<-SkytempDO_6.5m %>%
  select(dateTime, year, wtr_6.5)

glimpse(SkyPAR)
SkyPAR <- SkyPAR %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Denver"),
         dateTime = with_tz(dateTime, "GMT"),
         PAR = PAR*2.15,
         year = year(dateTime)) %>% #See "Estimating PAR from solar flux data" Word Doc in Troubleshooting folder
  filter(dateTime >= "2018-06-26 18:00:00" & dateTime <= "2018-08-25 16:00:00")
#basing this time period of when it looks like sensor was removed


#Print first five time readings for each variable
firstFiveTimes <- data.frame(DO=SkyDOobs$dateTime[1:5],
                             PAR=SkyPAR$dateTime[1:5],
                             wind=Skywnd$dateTime[1:5],
                             temp=Skytemp$dateTime[1:5])
print('First five time readings for each variable'); print(firstFiveTimes)

#Calculate first differences of time readings, display unique values
cat('\n','First differences of datetime','\n')
difTimesDO <- diff(SkyDOobs$dateTime); print(table(difTimesDO))
difTimesPAR <- diff(SkyPAR$dateTime); print(table(difTimesPAR))
difTimesWindSpeed <- diff(Skywnd$dateTime); print(table(difTimesWindSpeed))
difTimesSensorTemp <- diff(Skytemp$dateTime); print(table(difTimesSensorTemp))
timeStep=30	# in minutes


notDupRows <- findNotDupRows("SkyDOobs")
SkyDOobs <- SkyDOobs[notDupRows,]

notDupRows <- findNotDupRows("SkyPAR")
SkyPAR <- SkyPAR[notDupRows,]

notDupRows <- findNotDupRows("Skywnd")
Skywnd <- Skywnd[notDupRows,]

notDupRows <- findNotDupRows("Skytemp")
Skytemp <- Skytemp[notDupRows,]

#Make all data sets extend from startTime to endTime by timeStep
#Note that for some lakes it may be necessary to aggregate some variables to coarser time scale to get match up
SkyDOobs$dateTime <- floorMins(SkyDOobs)
SkyPAR$dateTime <- floorMins(SkyPAR)
Skywnd$dateTime <- floorMins(Skywnd)
Skytemp$dateTime <- floorMins(Skytemp)

#Repeat check for dup rows in case any introduced during floorMins
notDupRows <- findNotDupRows("SkyDOobs")
SkyDOobs <- SkyDOobs[notDupRows,]
notDupRows <- findNotDupRows("SkyPAR")
SkyPAR <- SkyPAR[notDupRows,]
notDupRows <- findNotDupRows("Skywnd")
Skywnd <- Skywnd[notDupRows,]
notDupRows <- findNotDupRows("Skytemp")
Skytemp <- Skytemp[notDupRows,]

#Find the latest first time point and the earliest last time point of all the data
startTime <- max(min(SkyDOobs$dateTime),min(SkyPAR$dateTime),min(Skywnd$dateTime),min(Skytemp$dateTime))
endTime <- min(max(SkyDOobs$dateTime),max(SkyPAR$dateTime),max(Skywnd$dateTime),max(Skytemp$dateTime))

#Data.frame with one column "dateTime" which is sequence of times at time interval of timeStep, from startTime to endTime
completeTimes <- data.frame(dateTime=seq(startTime,endTime,paste(timeStep,"mins"))) %>%
  mutate(year=year(dateTime))

#Merge all of input data.frames with completeTimes, so that they now all extend from startTime to endTime by timeStep
SkyDOobs <- merge(completeTimes,SkyDOobs,by=c("dateTime","year"),all.x=T)
SkyPAR <- merge(completeTimes,SkyPAR,by=c("dateTime","year"),all.x=T)
Skywnd <- merge(completeTimes,Skywnd,by=c("dateTime","year"),all.x=T)%>%
  select(-localTZ, -lakeID, -daylightSavings)
#Convert wind from character to number
Skywnd$wind <- as.numeric(Skywnd$wind)

Skytemp <- merge(completeTimes,Skytemp,by=c("dateTime","year"),all.x=T)

#Is it a problem that there are NAs in the PAR data? I guess we will find out
##IAO - 2021-02-12 --  I just checked and it shouldn't be because of the fillHoles helper function. 

Sky_full_2018 <- left_join(SkyDOobs, Skywnd, by = c("dateTime","year"))
Sky_full_2018 <- left_join(Sky_full_2018, Skytemp, by = c("dateTime","year"))
Sky_full_2018 <- left_join(Sky_full_2018, SkyPAR, by = c("dateTime","year"))


glimpse(Sky_full_2018)
tz(Sky_full_2018$dateTime)


# ---2019--- --------------------------------------------------------------

SkyDOobs <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Sky Pond/Sky2019_doobs.txt",stringsAsFactors = FALSE) 
Skywnd <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Sky Pond/Sky2019_wnd.txt", stringsAsFactors = FALSE) %>% select(-(X:X.3))
Skytemp <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Sky Pond/Sky2019_wtr.txt", stringsAsFactors = FALSE) 
SkyPAR <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Sky Pond/Sky2019_par.txt", stringsAsFactors = FALSE) %>% select(-X, -X.1)





####DATE CONVERSION###

glimpse(Skytemp)
Skytemp <- Skytemp %>%
  mutate(dateTime = mdy_hm(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Denver"),
         dateTime = with_tz(dateTime, "GMT")) %>%
  filter(dateTime >= "2019-06-25" & dateTime <= "2019-10-02") #Basing this cut-off 
 # on the assumption that ice-off and ice-on are about two weeks later or earlier (respectively)
 # in Sky Pond than The Loch 

glimpse(SkyDOobs)
SkyDOobs <- SkyDOobs %>%
  mutate(dateTime = mdy_hm(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Denver"),
         dateTime = with_tz(dateTime, "GMT")) %>%
  filter(dateTime >= "2019-06-25" & dateTime <= "2019-10-02") #Basing this cut-off 
# on the assumption that ice-off and ice-on are about two weeks later or earlier (respectively)
# in Sky Pond than The Loch 

glimpse(Skywnd)
Skywnd <- Skywnd %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Denver"),
         dateTime = with_tz(dateTime, "GMT"))%>%
  filter(dateTime >= "2019-06-25" & dateTime <= "2019-10-02") #Basing this cut-off 
# on the assumption that ice-off and ice-on are about two weeks later or earlier (respectively)
# in Sky Pond than The Loch 



glimpse(SkyPAR)
SkyPAR <- SkyPAR %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Denver"),
         dateTime = with_tz(dateTime, "GMT"),
         PAR=PAR*2.15) %>% #See "Estimating PAR from solar flux data" Word Doc in Troubleshooting folder
  filter(dateTime >= "2019-06-25" & dateTime <= "2019-10-02") #Basing this cut-off 
# on the assumption that ice-off and ice-on are about two weeks later or earlier (respectively)
# in Sky Pond than The Loch 

#Print first five time readings for each variable
firstFiveTimes <- data.frame(DO=SkyDOobs$dateTime[1:5],
                             PAR=SkyPAR$dateTime[1:5],
                             wind=Skywnd$dateTime[1:5],
                             temp=Skytemp$dateTime[1:5])
print('First five time readings for each variable'); print(firstFiveTimes)

#Calculate first differences of time readings, display unique values
cat('\n','First differences of datetime','\n')
difTimesDO <- diff(SkyDOobs$dateTime); print(table(difTimesDO))
difTimesPAR <- diff(SkyPAR$dateTime); print(table(difTimesPAR))
difTimesWindSpeed <- diff(Skywnd$dateTime); print(table(difTimesWindSpeed))
difTimesSensorTemp <- diff(Skytemp$dateTime); print(table(difTimesSensorTemp))
timeStep=30	# in minutes


notDupRows <- findNotDupRows("SkyDOobs")
SkyDOobs <- SkyDOobs[notDupRows,]

notDupRows <- findNotDupRows("SkyPAR")
SkyPAR <- SkyPAR[notDupRows,]

notDupRows <- findNotDupRows("Skywnd")
Skywnd <- Skywnd[notDupRows,]

notDupRows <- findNotDupRows("Skytemp")
Skytemp <- Skytemp[notDupRows,]

#Make all data sets extend from startTime to endTime by timeStep
#Note that for some lakes it may be necessary to aggregate some variables to coarser time scale to get match up
SkyDOobs$dateTime <- floorMins(SkyDOobs)
SkyPAR$dateTime <- floorMins(SkyPAR)
Skywnd$dateTime <- floorMins(Skywnd)
Skytemp$dateTime <- floorMins(Skytemp)

#Repeat check for dup rows in case any introduced during floorMins
notDupRows <- findNotDupRows("SkyDOobs")
SkyDOobs <- SkyDOobs[notDupRows,]
notDupRows <- findNotDupRows("SkyPAR")
SkyPAR <- SkyPAR[notDupRows,]
notDupRows <- findNotDupRows("Skywnd")
Skywnd <- Skywnd[notDupRows,]
notDupRows <- findNotDupRows("Skytemp")
Skytemp <- Skytemp[notDupRows,]

#Find the latest first time point and the earliest last time point of all the data
startTime <- max(min(SkyDOobs$dateTime),min(SkyPAR$dateTime),min(Skywnd$dateTime),min(Skytemp$dateTime))
endTime <- min(max(SkyDOobs$dateTime),max(SkyPAR$dateTime),max(Skywnd$dateTime),max(Skytemp$dateTime))

#Data.frame with one column "dateTime" which is sequence of times at time interval of timeStep, from startTime to endTime
completeTimes <- data.frame(dateTime=seq(startTime,endTime,paste(timeStep,"mins")))

#Merge all of input data.frames with completeTimes, so that they now all extend from startTime to endTime by timeStep
SkyDOobs <- merge(completeTimes,SkyDOobs,by="dateTime",all.x=T)%>%
  select(-localTZ, -lakeID, -daylightSavings)
SkyPAR <- merge(completeTimes,SkyPAR,by="dateTime",all.x=T)%>%
  select(-localTZ, -lakeID, -daylightSavings)
Skywnd <- merge(completeTimes,Skywnd,by="dateTime",all.x=T)%>%
  select(-localTZ, -lakeID, -daylightSavings)
Skytemp <- merge(completeTimes,Skytemp,by="dateTime",all.x=T)%>%
  select(-localTZ, -lakeID, -daylightSavings)

#Is it a problem that there are NAs in the PAR data? I guess we will find out
##IAO - 2021-02-12 --  I just checked and it shouldn't be because of the fillHoles helper function. 

Sky_full_2019 <- left_join(SkyDOobs, Skywnd, by = c("dateTime"))
Sky_full_2019 <- left_join(Sky_full_2019, Skytemp, by = c("dateTime"))
Sky_full_2019 <- left_join(Sky_full_2019, SkyPAR, by = c("dateTime")) %>%
  mutate(year = year(dateTime))

glimpse(Sky_full_2019)
tz(Sky_full_2019$dateTime)



#   ---2020--- --------------------------------------------------------------

SkyDOobs <- read.delim("data/metab_data_raw/Sky Pond/Sky2020_doobs.txt",stringsAsFactors = FALSE) 
Skywnd <- read.delim("data/metab_data_raw/Sky Pond/Sky2020_wnd.txt", stringsAsFactors = FALSE) 
Skytemp <- read.delim("data/metab_data_raw/Sky Pond/Sky2020_wtr.txt", stringsAsFactors = FALSE) 
SkyPAR <- read.delim("data/metab_data_raw/Sky Pond/Sky2020_par.txt", stringsAsFactors = FALSE) 

####DATE CONVERSION###

glimpse(Skytemp)
Skytemp <- Skytemp %>%
  mutate(dateTime = mdy_hm(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Denver"),
         dateTime = with_tz(dateTime, "GMT")) %>%
  filter(dateTime <= "2020-10-02") %>% #Let's just use this as a cutoff for now
  mutate(wtr0.5=as.numeric((wtr0.5)),
         wtr7.0=as.numeric((wtr7.0)))

glimpse(SkyDOobs)
SkyDOobs <- SkyDOobs %>%
  mutate(dateTime = mdy_hm(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Denver"),
         dateTime = with_tz(dateTime, "GMT")) %>%
  filter(dateTime <= "2020-10-02") %>%#Let's just use this as a cutoff for now
  mutate(doObs=as.numeric((doObs)))

glimpse(Skywnd)
Skywnd <- Skywnd %>%
  mutate(dateTime = mdy_hm(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Denver"),
         dateTime = with_tz(dateTime, "GMT"))%>%
  filter(dateTime <= "2020-10-02") #Let's just use this as a cutoff for now

glimpse(SkyPAR)
SkyPAR <- SkyPAR %>%
  mutate(dateTime = mdy_hm(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Denver"),
         dateTime = with_tz(dateTime, "GMT"),
         PAR=PAR*2.15) %>% #See "Estimating PAR from solar flux data" Word Doc in Troubleshooting folder
  filter(dateTime <= "2020-10-02") #Let's just use this as a cutoff for now


#Print first five time readings for each variable
firstFiveTimes <- data.frame(DO=SkyDOobs$dateTime[1:5],
                             PAR=SkyPAR$dateTime[1:5],
                             wind=Skywnd$dateTime[1:5],
                             temp=Skytemp$dateTime[1:5])
print('First five time readings for each variable'); print(firstFiveTimes)

#Calculate first differences of time readings, display unique values
cat('\n','First differences of datetime','\n')
difTimesDO <- diff(SkyDOobs$dateTime); print(table(difTimesDO))
difTimesPAR <- diff(SkyPAR$dateTime); print(table(difTimesPAR))
difTimesWindSpeed <- diff(Skywnd$dateTime); print(table(difTimesWindSpeed))
difTimesSensorTemp <- diff(Skytemp$dateTime); print(table(difTimesSensorTemp))
timeStep=30	# in minutes


notDupRows <- findNotDupRows("SkyDOobs")
SkyDOobs <- SkyDOobs[notDupRows,]

notDupRows <- findNotDupRows("SkyPAR")
SkyPAR <- SkyPAR[notDupRows,]

notDupRows <- findNotDupRows("Skywnd")
Skywnd <- Skywnd[notDupRows,]

notDupRows <- findNotDupRows("Skytemp")
Skytemp <- Skytemp[notDupRows,]

#Make all data sets extend from startTime to endTime by timeStep
#Note that for some lakes it may be necessary to aggregate some variables to coarser time scale to get match up
SkyDOobs$dateTime <- floorMins(SkyDOobs)
SkyPAR$dateTime <- floorMins(SkyPAR)
Skywnd$dateTime <- floorMins(Skywnd)
Skytemp$dateTime <- floorMins(Skytemp)

#Repeat check for dup rows in case any introduced during floorMins
notDupRows <- findNotDupRows("SkyDOobs")
SkyDOobs <- SkyDOobs[notDupRows,]
notDupRows <- findNotDupRows("SkyPAR")
SkyPAR <- SkyPAR[notDupRows,]
notDupRows <- findNotDupRows("Skywnd")
Skywnd <- Skywnd[notDupRows,]
notDupRows <- findNotDupRows("Skytemp")
Skytemp <- Skytemp[notDupRows,]

#Find the latest first time point and the earliest last time point of all the data
startTime <- max(min(SkyDOobs$dateTime),min(SkyPAR$dateTime),min(Skywnd$dateTime),min(Skytemp$dateTime))
endTime <- min(max(SkyDOobs$dateTime),max(SkyPAR$dateTime),max(Skywnd$dateTime),max(Skytemp$dateTime))

#Data.frame with one column "dateTime" which is sequence of times at time interval of timeStep, from startTime to endTime
completeTimes <- data.frame(dateTime=seq(startTime,endTime,paste(timeStep,"mins")))

#Merge all of input data.frames with completeTimes, so that they now all extend from startTime to endTime by timeStep
SkyDOobs <- merge(completeTimes,SkyDOobs,by="dateTime",all.x=T)%>%
  select(-localTZ, -lakeID, -daylightSavings)
SkyPAR <- merge(completeTimes,SkyPAR,by="dateTime",all.x=T)%>%
  select(-localTZ, -lakeID, -daylightSavings)
Skywnd <- merge(completeTimes,Skywnd,by="dateTime",all.x=T)%>%
  select(-localTZ, -lakeID, -daylightSavings)
Skytemp <- merge(completeTimes,Skytemp,by="dateTime",all.x=T)%>%
  select(-localTZ, -lakeID, -daylightSavings)

#Join separate dataframes
Sky_full_2020 <- left_join(SkyDOobs, Skywnd, by = c("dateTime"))
Sky_full_2020 <- left_join(Sky_full_2020, Skytemp, by = c("dateTime"))
Sky_full_2020 <- left_join(Sky_full_2020, SkyPAR, by = c("dateTime")) 

glimpse(Sky_full_2020)
tz(Sky_full_2020$dateTime)



#Preview data
#temp
Sky_full_2020 %>%
  dplyr::select(dateTime, wtr0.5:wtr7.0)%>%
  pivot_longer(-(dateTime), names_sep = "r", names_to = c("variable","depth")) %>%
  dplyr::select(-variable)%>%
  rename(temp=value)%>%
  ggplot(aes(x=dateTime, y=temp, color=depth)) + 
  geom_point(size=2)+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%y-%m-%d") +
  ylab("Temp C")+
  ggtitle(" Sky 2020")

#DO

####plot DO###########

Sky_full_2020 %>%
  ggplot(aes(x=dateTime, y=doObs)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%y-%m") +
  ylab("DO obs")+
  geom_hline(yintercept = 7.6)+ #This must be when they pulled the sensor out so we will want to remove those values
  ggtitle(" Sky 2020") 

Sky_full_2020<-Sky_full_2020 %>%
  filter(doObs>7.6)

Sky_full_2020 %>%
  ggplot(aes(x=dateTime, y=wind)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%y-%m") +
  ylab("Wind (m/s)")+
  ggtitle(" Sky 2020") 

Sky_full_2020 %>%
  ggplot(aes(x=dateTime, y=PAR)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%y-%m") +
  ylab("PAR")+
  ggtitle("Sky 2020") 


####plot TEMPS###########
#Master Sky dataframe
Sky_full_master <- bind_rows(Sky_full_2016,
                             Sky_full_2017,
                             Sky_full_2018,
                             Sky_full_2019,
                             Sky_full_2020)

glimpse(Sky_full_master)
tz(Sky_full_master$dateTime)


#Based on the readme, it seems like there are two sensor arrays, and some of the depths overlap
#Specifically, sensor 9 and sensor 13 both measure 0.9m below surface (or 4.4m above sediment)
#Do they match? 
head(Sky_full_master)
Sky_full_master %>%
  filter(dateTime < "2016-08-01")%>%
  dplyr::select(dateTime, wtr0.5:wtr7.0)%>%
  pivot_longer(-(dateTime), names_sep = "r", names_to = c("variable","depth")) %>%
  dplyr::select(-variable)%>%
  mutate(depth=as.numeric(as.character(depth)),
         year=year(dateTime)) %>%
  rename(temp=value)%>%
  ggplot(aes(x=dateTime, y=temp, color=depth)) + 
  geom_point(size=2)+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%y-%m-%d") +
  ylab("Temp C")+
  ggtitle(" Sky 2016-2019")+
  scale_color_continuous_sequential(palette = "Dark Mint", rev=TRUE)+
  facet_wrap(.~year, scales="free_x")

#Since we only have one year (2016) with a temperature profile, what
#can we learn about zMix during that time?
Sky_profile<-Sky_full_2016 %>%
  filter(dateTime < "2016-08-01")%>%
  rename_all(function(x) gsub("wtr", "wtr_", x)) #Add an underscore to all heads to read wtr_#.#
Sky_zmix = ts.meta.depths(get.vars(Sky_profile, 'wtr'))
names(Sky_zmix) = c('datetime','z.mix', 'bottom')
Sky_zmix_daily <- Sky_zmix %>%
  mutate(yday=yday(datetime))%>%
  group_by(yday) %>%
  summarize(z.mix=mean(z.mix,na.rm=TRUE))
ggplot(Sky_zmix_daily, aes(x=yday, y=z.mix)) + 
  geom_point()+
  ylab("DO obs")+
  ggtitle(" Sky 2016")
#Somewhere between 1.25m and and 2.0m


ggplot(Skytemp, aes(x=dateTime, y=wtr_6.5)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%y-%m") +
  ylab("DO obs")+
  ggtitle(" Sky 2018") +
  facet_wrap(.~year, scales="free_x")


####plot DO###########
ggplot(Sky_full_master, aes(x=dateTime, y=doObs)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%y-%m") +
  ylab("DO obs")+
  ggtitle(" Sky 2018") +
  facet_wrap(.~year, scales="free_x")

#Just 2018
ggplot(SkyDOobs, aes(x=dateTime, y=doObs)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%y-%m") +
  ylab("DO obs")+
  ggtitle(" Sky 2018") +
  facet_wrap(.~year, scales="free_x")



####plot WIND###########
ggplot(Sky_full_master, aes(x=dateTime, y=wind)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%y-%m") +
  ylab("Wind (m/s)")+
  ggtitle(" Sky 2018") +
  facet_wrap(.~year, scales="free_x")

####plot PAR###########
ggplot(Sky_full_master, aes(x=dateTime, y=PAR)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%y-%m") +
  ylab("PAR")+
  ggtitle("Sky 2018") +
  facet_wrap(.~year, scales="free_x")


####export for modelling###########
skyDOobs <- Sky_full_master %>%
  filter(year=="2016")%>%
  dplyr::select(dateTime, doObs) %>%
  rename(DO=doObs) 
write.csv(skyDOobs, "data/metab_data_clean/sky/sky_2016_DO.txt", row.names=FALSE)

#Just 2016 DO @ 6.5m
# SkyDOobs <- SkyDOobs %>%
#   dplyr::select(dateTime, doObs) %>%
#   rename(DO=doObs)
# write.csv(SkyDOobs, "data/metab_data_clean/sky/sky_2016_DO_6.5m.txt", row.names=FALSE)



skyDOobs <- Sky_full_master %>%
  filter(year=="2017")%>%
  dplyr::select(dateTime, doObs) %>%
  rename(DO=doObs) 
write.csv(skyDOobs, "data/metab_data_clean/sky/sky_2017_DO.txt", row.names=FALSE)

#Just 2017 DO @ 6.5m
# SkyDOobs <- SkyDOobs %>%
#   dplyr::select(dateTime, doObs) %>%
#   rename(DO=doObs)
# write.csv(SkyDOobs, "data/metab_data_clean/sky/sky_2017_DO_6.5m.txt", row.names=FALSE)



# skyDOobs <- Sky_full_master %>%
#   filter(year=="2018")%>%
#   dplyr::select(dateTime, doObs) %>%
#   rename(DO=doObs)
# write.csv(skyDOobs, "data/metab_data_clean/sky/sky_2018_DO.txt", row.names=FALSE)

#Just 2018 DO @ 6.5m
# SkyDOobs <- SkyDOobs %>%
#   dplyr::select(dateTime, doObs) %>%
#   rename(DO=doObs)
# write.csv(SkyDOobs, "data/metab_data_clean/sky/sky_2018_DO_6.5m.txt", row.names=FALSE)


skyDOobs <-  Sky_full_master %>%
  filter(year=="2019")%>%
  dplyr::select(dateTime, doObs) %>%
  rename(DO=doObs) 
write.csv(skyDOobs, "data/metab_data_clean/sky/sky_2019_DO.txt", row.names=FALSE)

skyDOobs <- Sky_full_2020 %>%
  dplyr::select(dateTime, doObs) %>%
  rename(DO=doObs) 
write.csv(skyDOobs, "data/metab_data_clean/sky/sky_2020_DO.txt", row.names=FALSE)


#DO sensor depth = 0.5
skysensorTemp <- Sky_full_master %>%
  filter(year=="2016")%>%
  dplyr::select(dateTime, wtr0.5) %>%
  rename(sensorTemp=wtr0.5) 
write.csv(skysensorTemp, "data/metab_data_clean/sky/sky_2016_sensorTemp.txt", row.names=FALSE)

#2016 sensor at 6.5m
# SkysensorTemp <- Skytemp %>%
#   dplyr::select(dateTime, wtr_6.5) %>%
#   rename(sensorTemp=wtr_6.5)
# write.csv(SkysensorTemp, "data/metab_data_clean/sky/sky_2016_sensorTemp_6.5m.txt", row.names=FALSE)


skysensorTemp <- Sky_full_master %>%
  filter(year=="2017")%>%
  dplyr::select(dateTime, wtr0.5) %>%
  rename(sensorTemp=wtr0.5) 
write.csv(skysensorTemp, "data/metab_data_clean/sky/sky_2017_sensorTemp.txt", row.names=FALSE)

# 2017 sensor at 6.5m
SkysensorTemp <- Skytemp %>%
  dplyr::select(dateTime, wtr_6.5) %>%
  rename(sensorTemp=wtr_6.5)
write.csv(SkysensorTemp, "data/metab_data_clean/sky/sky_2017_sensorTemp_6.5m.txt", row.names=FALSE)



skysensorTemp <- Sky_full_master %>%
  filter(year=="2018")%>%
  dplyr::select(dateTime, wtr0.5) %>%
  rename(sensorTemp=wtr0.5)
write.csv(skysensorTemp, "data/metab_data_clean/sky/sky_2018_sensorTemp.txt", row.names=FALSE)

#2018 sensor at 6.5m
# SkysensorTemp <- Skytemp %>%
#   dplyr::select(dateTime, wtr_6.5) %>%
#   rename(sensorTemp=wtr_6.5) 
# write.csv(SkysensorTemp, "data/metab_data_clean/sky/sky_2018_sensorTemp_6.5m.txt", row.names=FALSE)


skysensorTemp <- Sky_full_master %>%
  filter(year=="2019")%>%
  dplyr::select(dateTime, wtr0.5) %>%
  rename(sensorTemp=wtr0.5) 
write.csv(skysensorTemp, "data/metab_data_clean/sky/sky_2019_sensorTemp.txt", row.names=FALSE)

skysensorTemp <- Sky_full_2020 %>%
  dplyr::select(dateTime, wtr0.5) %>%
  rename(sensorTemp=wtr0.5) 
write.csv(skysensorTemp, "data/metab_data_clean/sky/sky_2020_sensorTemp.txt", row.names=FALSE)

#PAR
skyPAR <- Sky_full_master %>%
  filter(year=="2016")%>%
  dplyr::select(dateTime, PAR) 
write.csv(skyPAR, "data/metab_data_clean/sky/sky_2016_PAR.txt", row.names=FALSE)

skyPAR <- Sky_full_master %>%
  filter(year=="2017")%>%
  dplyr::select(dateTime, PAR) 
write.csv(skyPAR, "data/metab_data_clean/sky/sky_2017_PAR.txt", row.names=FALSE)

skyPAR <- Sky_full_master %>%
  filter(year=="2018")%>%
  dplyr::select(dateTime, PAR) 
write.csv(skyPAR, "data/metab_data_clean/sky/sky_2018_PAR.txt", row.names=FALSE)

skyPAR <- Sky_full_master %>%
  filter(year=="2019")%>%
  dplyr::select(dateTime, PAR) 
write.csv(skyPAR, "data/metab_data_clean/sky/sky_2019_PAR.txt", row.names=FALSE)

skyPAR <- Sky_full_2020 %>%
  dplyr::select(dateTime, PAR) 
write.csv(skyPAR, "data/metab_data_clean/sky/sky_2020_PAR.txt", row.names=FALSE)




#wind height = 2 above lake surface
skywind <- Sky_full_master %>%
  filter(year=="2016")%>%
  dplyr::select(dateTime, wind) %>%
  rename(windSpeed=wind) 
write.csv(skywind, "data/metab_data_clean/sky/sky_2016_windSpeed.txt", row.names=FALSE)

skywind <- Sky_full_master %>%
  filter(year=="2017")%>%
  dplyr::select(dateTime, wind) %>%
  rename(windSpeed=wind) 
write.csv(skywind, "data/metab_data_clean/sky/sky_2017_windSpeed.txt", row.names=FALSE)

skywind <- Sky_full_master %>%
  filter(year=="2018")%>%
  dplyr::select(dateTime, wind) %>%
  rename(windSpeed=wind) 
write.csv(skywind, "data/metab_data_clean/sky/sky_2018_windSpeed.txt", row.names=FALSE)

skywind <- Sky_full_master %>%
  filter(year=="2019")%>%
  dplyr::select(dateTime, wind) %>%
  rename(windSpeed=wind) 
write.csv(skywind, "data/metab_data_clean/sky/sky_2019_windSpeed.txt", row.names=FALSE)

skywind <- Sky_full_2020 %>%
  dplyr::select(dateTime, wind) %>%
  rename(windSpeed=wind) 
write.csv(skywind, "data/metab_data_clean/sky/sky_2020_windSpeed.txt", row.names=FALSE)




#temp profile
skytempprofile<- Sky_full_master %>%
  filter(year=="2016")%>%
  dplyr::select(dateTime, wtr0.5: wtr7.0) %>%
  rename_all(function(x) gsub("wtr", "wtr_", x)) #Add an underscore to all heads to read wtr_#.#
write.csv(skytempprofile, "data/metab_data_clean/sky/sky_2016_tempProfile.txt", row.names=FALSE)


skytempprofile<- Sky_full_master %>%
  filter(year=="2017")%>%
  dplyr::select(dateTime, wtr0.5, wtr7.0) %>%
  rename_all(function(x) gsub("wtr", "wtr_", x)) #Add an underscore to all heads to read wtr_#.#
write.csv(skytempprofile, "data/metab_data_clean/sky/sky_2017_tempProfile.txt", row.names=FALSE)



skytempprofile<- Sky_full_master %>%
  filter(year=="2018")%>%
  dplyr::select(dateTime, wtr0.5, wtr7.0) %>%
  rename_all(function(x) gsub("wtr", "wtr_", x)) #Add an underscore to all heads to read wtr_#.#
write.csv(skytempprofile, "data/metab_data_clean/sky/sky_2018_tempProfile.txt", row.names=FALSE)


skytempprofile<- Sky_full_master %>%
  filter(year=="2019")%>%
  dplyr::select(dateTime, wtr0.5) %>%
  rename_all(function(x) gsub("wtr", "wtr_", x)) #Add an underscore to all heads to read wtr_#.#
write.csv(skytempprofile, "data/metab_data_clean/sky/sky_2019_tempProfile.txt", row.names=FALSE)

skytempprofile<- Sky_full_2020 %>%
  dplyr::select(dateTime, wtr0.5, wtr7.0) %>%
  rename_all(function(x) gsub("wtr", "wtr_", x)) #Add an underscore to all heads to read wtr_#.#
write.csv(skytempprofile, "data/metab_data_clean/sky/sky_2020_tempProfile.txt", row.names=FALSE)


# > P1  -------------------------------------------------------------


P1DOobs <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Prairie1/P1_doobs.txt",stringsAsFactors = FALSE) 
P1wnd <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Prairie1/P1_wnd.txt", stringsAsFactors = FALSE) 
P1temp <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Prairie1/P1_wtr.txt", stringsAsFactors = FALSE) 
P1PAR <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Prairie1/P1_par.txt", stringsAsFactors = FALSE) 
glimpse(P1DOobs)
glimpse(P1wnd)
glimpse(P1temp)
glimpse(P1PAR)


####DATE CONVERSION###
# P1_full$dateTime <- ymd_hm(as.factor(P1_full$dateTime))
# str(P1_full$dateTime)
# P1temp$dateTime <- ymd_hm(as.factor(P1temp$dateTime))
# force_tz(P1temp$dateTime, tz="Etc/GMT-1") #This is confusing, because if you specify -1 it prints as GMT+1... 
# P1temp$dateTimeGMT<-with_tz(dateTime, "GMT")
# P1DOobs$dateTime <- ymd_hm(as.factor(P1DOobs$dateTime))
# force_tz(P1DOobs$dateTime, tz="Etc/GMT-1")
# P1wnd$dateTime <- ymd_hm(as.factor(P1wnd$dateTime))
# force_tz(P1wnd$dateTime, tz="Etc/GMT-1")
# P1PAR$dateTime <- ymd_hm(as.factor(P1PAR$dateTime))
# force_tz(P1PAR$dateTime, tz="Etc/GMT-1")

P1temp <- P1temp %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Chicago"),
         dateTime = with_tz(dateTime, "GMT"))
P1DOobs <- P1DOobs %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Chicago"),
         dateTime = with_tz(dateTime, "GMT")) 
P1wnd <- P1wnd %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Chicago"),
         dateTime = with_tz(dateTime, "GMT"))
P1PAR <- P1PAR %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Chicago"),
         dateTime = with_tz(dateTime, "GMT"))%>%
  dplyr::select(-LUX_1.0, -LUX_1.8) %>%
  rename(LUX=LUX_0.1) %>%
  mutate(PAR=LUX*0.0185) #https://www.apogeeinstruments.com/conversion-ppfd-to-lux/

#Print first five time readings for each variable
firstFiveTimes <- data.frame(DO=P1DOobs$dateTime[1:5],
                             PAR=P1PAR$dateTime[1:5],
                             wind=P1wnd$dateTime[1:5],
                             temp=P1temp$dateTime[1:5])
print('First five time readings for each variable'); print(firstFiveTimes)

#Calculate first differences of time readings, display unique values
cat('\n','First differences of datetime','\n')
difTimesDO <- diff(P1DOobs$dateTime); print(table(difTimesDO))
difTimesPAR <- diff(P1PAR$dateTime); print(table(difTimesPAR))
difTimesWindSpeed <- diff(P1wnd$dateTime); print(table(difTimesWindSpeed))
difTimesSensorTemp <- diff(P1temp$dateTime); print(table(difTimesSensorTemp))
timeStep=10	# in minutes


notDupRows <- findNotDupRows("P1DOobs")
P1DOobs <- P1DOobs[notDupRows,]

notDupRows <- findNotDupRows("P1PAR")
P1PAR <- P1PAR[notDupRows,]

notDupRows <- findNotDupRows("P1wnd")
P1wnd <- P1wnd[notDupRows,]

notDupRows <- findNotDupRows("P1temp")
P1temp <- P1temp[notDupRows,]

#Make all data sets extend from startTime to endTime by timeStep
#Note that for some lakes it may be necessary to aggregate some variables to coarser time scale to get match up
P1DOobs$dateTime <- floorMins(P1DOobs)
P1PAR$dateTime <- floorMins(P1PAR)
P1wnd$dateTime <- floorMins(P1wnd)
P1temp$dateTime <- floorMins(P1temp)

#Repeat check for dup rows in case any introduced during floorMins
notDupRows <- findNotDupRows("P1DOobs")
P1DOobs <- P1DOobs[notDupRows,]
notDupRows <- findNotDupRows("P1PAR")
P1PAR <- P1PAR[notDupRows,]
notDupRows <- findNotDupRows("P1wnd")
P1wnd <- P1wnd[notDupRows,]
notDupRows <- findNotDupRows("P1temp")
P1temp <- P1temp[notDupRows,]

#Find the latest first time point and the earliest last time point of all the data
startTime <- max(min(P1DOobs$dateTime),min(P1PAR$dateTime),min(P1wnd$dateTime),min(P1temp$dateTime))
endTime <- min(max(P1DOobs$dateTime),max(P1PAR$dateTime),max(P1wnd$dateTime),max(P1temp$dateTime))

#Data.frame with one column "dateTime" which is sequence of times at time interval of timeStep, from startTime to endTime
completeTimes <- data.frame(dateTime=seq(startTime,endTime,paste(timeStep,"mins")))

#Merge all of input data.frames with completeTimes, so that they now all extend from startTime to endTime by timeStep
P1DOobs <- merge(completeTimes,P1DOobs,by="dateTime",all.x=T)
P1PAR <- merge(completeTimes,P1PAR,by="dateTime",all.x=T)
P1wnd <- merge(completeTimes,P1wnd,by="dateTime",all.x=T)
P1temp <- merge(completeTimes,P1temp,by="dateTime",all.x=T)

#Is it a problem that there are NAs in the PAR data? I guess we will find out
##IAO - 2021-02-12 --  I just checked and it shouldn't be because of the fillHoles helper function. 

P1_full <- left_join(P1DOobs, P1wnd, by = c("dateTime", "localTZ", "daylightSavings","lakeID"))
P1_full <- left_join(P1_full, P1temp, by = c("dateTime", "localTZ", "daylightSavings","lakeID"))
P1_full <- left_join(P1_full, P1PAR, by = c("dateTime", "localTZ", "daylightSavings","lakeID"))



####plot TEMPS###########
#Based on the readme, it seems like there are two sensor arrays, and some of the depths overlap
#Specifically, sensor 9 and sensor 13 both measure 0.9m below surface (or 4.4m above sediment)
#Do they match? 
head(P1_full)
P1_full %>%
  dplyr::select(dateTime, wtr0.1:wtr1.8)%>%
  pivot_longer(-(dateTime), names_sep = "r", names_to = c("variable","depth")) %>%
  dplyr::select(-variable)%>%
  mutate(depth=as.numeric(as.character(depth)),
         year=year(dateTime)) %>%
  rename(temp=value)%>%
  ggplot(aes(x=dateTime, y=temp, color=depth)) + 
  geom_point(size=2)+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%y-%m-%d") +
  ylab("Temp C")+
  ggtitle(" P1 2019")+
  scale_color_continuous_sequential(palette = "Dark Mint", rev=TRUE)+
  facet_wrap(.~year, scales="free_x")



####plot DO###########
ggplot(P1_full, aes(x=dateTime, y=doObs)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%y-%m") +
  ylab("DO obs")+
  ggtitle(" P1 2018")


####plot WIND###########
ggplot(P1_full, aes(x=dateTime, y=wind)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%y-%m") +
  ylab("Wind (m/s)")+
  ggtitle(" P1 2018")

####plot PAR###########
ggplot(P1_full, aes(x=dateTime, y=PAR)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%y-%m") +
  ylab("LUX")+
  ggtitle("P1 2018")

#Some of these values seem crazy high. 
P1_full %>%
  filter(dateTime > "2019-06-01" & dateTime < "2019-06-08")%>%
  ggplot(aes(x=dateTime, y=PAR)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%y-%m") +
  ylab("LUX")+
  ggtitle("P1 2018")


####export for modelling###########
P1DOobs <- P1_full %>%
  dplyr::select(dateTime, doObs) %>%
  rename(DO=doObs) 
write.csv(P1DOobs, "data/metab_data_clean/p1/P1_2019_DO.txt", row.names=FALSE)

#DO sensor depth = 0.2
P1sensorTemp <- P1_full %>%
  dplyr::select(dateTime, wtr0.1) %>%
  rename(sensorTemp=wtr0.1) 
write.csv(P1sensorTemp, "data/metab_data_clean/p1/P1_2019_sensorTemp.txt", row.names=FALSE)

#PAR
P1PAR <- P1_full %>%
  dplyr::select(dateTime, PAR) 
write.csv(P1PAR, "data/metab_data_clean/p1/P1_2019_PAR.txt", row.names=FALSE)

#wind height = unknown. measured from nearby weather station.
P1wind <- P1_full %>%
  dplyr::select(dateTime, wind) %>%
  rename(windSpeed=wind) 
write.csv(P1wind, "data/metab_data_clean/p1/P1_2019_windSpeed.txt", row.names=FALSE)

#temp profile
P1tempprofile<- P1_full %>%
  dplyr::select(dateTime, wtr0.1:wtr1.8) %>%
  rename_all(function(x) gsub("wtr", "wtr_", x)) #Add an underscore to all heads to read wtr_#.#
write.csv(P1tempprofile, "data/metab_data_clean/p1/P1_2019_tempProfile.txt", row.names=FALSE)






# > P8  -------------------------------------------------------------


P8DOobs <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Prairie8/P8_doobs.txt",stringsAsFactors = FALSE) 
P8wnd <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Prairie8/P8_wnd.txt", stringsAsFactors = FALSE) 
P8temp <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Prairie8/P8_wtr.txt", stringsAsFactors = FALSE) 
P8PAR <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Prairie8/P8_par.txt", stringsAsFactors = FALSE) 
glimpse(P8DOobs)
glimpse(P8wnd)
glimpse(P8temp)
glimpse(P8PAR)


####DATE CONVERSION###
# P8_full$dateTime <- ymd_hm(as.factor(P8_full$dateTime))
# str(P8_full$dateTime)
# P8temp$dateTime <- ymd_hm(as.factor(P8temp$dateTime))
# force_tz(P8temp$dateTime, tz="Etc/GMT-1") #This is confusing, because if you specify -1 it prints as GMT+1... 
# P8temp$dateTimeGMT<-with_tz(dateTime, "GMT")
# P8DOobs$dateTime <- ymd_hm(as.factor(P8DOobs$dateTime))
# force_tz(P8DOobs$dateTime, tz="Etc/GMT-1")
# P8wnd$dateTime <- ymd_hm(as.factor(P8wnd$dateTime))
# force_tz(P8wnd$dateTime, tz="Etc/GMT-1")
# P8PAR$dateTime <- ymd_hm(as.factor(P8PAR$dateTime))
# force_tz(P8PAR$dateTime, tz="Etc/GMT-1")

P8temp <- P8temp %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Chicago"),
         dateTime = with_tz(dateTime, "GMT"))
P8DOobs <- P8DOobs %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Chicago"),
         dateTime = with_tz(dateTime, "GMT")) 
P8wnd <- P8wnd %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Chicago"),
         dateTime = with_tz(dateTime, "GMT"))
P8PAR <- P8PAR %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Chicago"),
         dateTime = with_tz(dateTime, "GMT"))%>%
  dplyr::select(-LUX_0.1, -LUX_1.2) %>%
  rename(LUX=LUX_0.6) %>%
  mutate(PAR=LUX*0.0185) #https://www.apogeeinstruments.com/conversion-ppfd-to-lux/

#Print first five time readings for each variable
firstFiveTimes <- data.frame(DO=P8DOobs$dateTime[1:5],
                             PAR=P8PAR$dateTime[1:5],
                             wind=P8wnd$dateTime[1:5],
                             temp=P8temp$dateTime[1:5])
print('First five time readings for each variable'); print(firstFiveTimes)

#Calculate first differences of time readings, display unique values
cat('\n','First differences of datetime','\n')
difTimesDO <- diff(P8DOobs$dateTime); print(table(difTimesDO))
difTimesPAR <- diff(P8PAR$dateTime); print(table(difTimesPAR))
difTimesWindSpeed <- diff(P8wnd$dateTime); print(table(difTimesWindSpeed))
difTimesSensorTemp <- diff(P8temp$dateTime); print(table(difTimesSensorTemp))
timeStep=30	# in minutes


notDupRows <- findNotDupRows("P8DOobs")
P8DOobs <- P8DOobs[notDupRows,]

notDupRows <- findNotDupRows("P8PAR")
P8PAR <- P8PAR[notDupRows,]

notDupRows <- findNotDupRows("P8wnd")
P8wnd <- P8wnd[notDupRows,]

notDupRows <- findNotDupRows("P8temp")
P8temp <- P8temp[notDupRows,]

#Make all data sets extend from startTime to endTime by timeStep
#Note that for some lakes it may be necessary to aggregate some variables to coarser time scale to get match up
P8DOobs$dateTime <- floorMins(P8DOobs)
P8PAR$dateTime <- floorMins(P8PAR)
P8wnd$dateTime <- floorMins(P8wnd)
P8temp$dateTime <- floorMins(P8temp)

#Repeat check for dup rows in case any introduced during floorMins
notDupRows <- findNotDupRows("P8DOobs")
P8DOobs <- P8DOobs[notDupRows,]
notDupRows <- findNotDupRows("P8PAR")
P8PAR <- P8PAR[notDupRows,]
notDupRows <- findNotDupRows("P8wnd")
P8wnd <- P8wnd[notDupRows,]
notDupRows <- findNotDupRows("P8temp")
P8temp <- P8temp[notDupRows,]

#Find the latest first time point and the earliest last time point of all the data
startTime <- max(min(P8DOobs$dateTime),min(P8PAR$dateTime),min(P8wnd$dateTime),min(P8temp$dateTime))
endTime <- min(max(P8DOobs$dateTime),max(P8PAR$dateTime),max(P8wnd$dateTime),max(P8temp$dateTime))

#Data.frame with one column "dateTime" which is sequence of times at time interval of timeStep, from startTime to endTime
completeTimes <- data.frame(dateTime=seq(startTime,endTime,paste(timeStep,"mins")))

#Merge all of input data.frames with completeTimes, so that they now all extend from startTime to endTime by timeStep
P8DOobs <- merge(completeTimes,P8DOobs,by="dateTime",all.x=T)
P8PAR <- merge(completeTimes,P8PAR,by="dateTime",all.x=T)
P8wnd <- merge(completeTimes,P8wnd,by="dateTime",all.x=T)
P8temp <- merge(completeTimes,P8temp,by="dateTime",all.x=T)

#Is it a problem that there are NAs in the PAR data? I guess we will find out
##IAO - 2021-02-12 --  I just checked and it shouldn't be because of the fillHoles helper function. 

P8_full <- left_join(P8DOobs, P8wnd, by = c("dateTime", "localTZ", "daylightSavings","lakeID"))
P8_full <- left_join(P8_full, P8temp, by = c("dateTime", "localTZ", "daylightSavings","lakeID"))
P8_full <- left_join(P8_full, P8PAR, by = c("dateTime", "localTZ", "daylightSavings","lakeID"))



####plot TEMPS###########
#Based on the readme, it seems like there are two sensor arrays, and some of the depths overlap
#Specifically, sensor 9 and sensor 13 both measure 0.9m below surface (or 4.4m above sediment)
#Do they match? 
head(P8_full)
P8_full %>%
  dplyr::select(dateTime, wtr0.1:wtr1.2)%>%
  pivot_longer(-(dateTime), names_sep = "r", names_to = c("variable","depth")) %>%
  dplyr::select(-variable)%>%
  mutate(depth=as.numeric(as.character(depth)),
         year=year(dateTime)) %>%
  rename(temp=value)%>%
  ggplot(aes(x=dateTime, y=temp, color=depth)) + 
  geom_point(size=2)+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%y-%m-%d") +
  ylab("Temp C")+
  ggtitle(" P8 2019")+
  scale_color_continuous_sequential(palette = "Dark Mint", rev=TRUE)+
  facet_wrap(.~year, scales="free_x")



####plot DO###########
ggplot(P8_full, aes(x=dateTime, y=doObs)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%y-%m") +
  ylab("DO obs")+
  ggtitle(" P8 2018")


####plot WIND###########
ggplot(P8_full, aes(x=dateTime, y=wind)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%y-%m") +
  ylab("Wind (m/s)")+
  ggtitle(" P8 2018")

####plot PAR###########
ggplot(P8_full, aes(x=dateTime, y=PAR)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%y-%m") +
  ylab("LUX")+
  ggtitle("P8 2018")


####export for modelling###########
P8DOobs <- P8_full %>%
  dplyr::select(dateTime, doObs) %>%
  rename(DO=doObs) 
write.csv(P8DOobs, "data/metab_data_clean/p8/P8_2019_DO.txt", row.names=FALSE)

#DO sensor depth = 0.2
P8sensorTemp <- P8_full %>%
  dplyr::select(dateTime, wtr0.1) %>%
  rename(sensorTemp=wtr0.1) 
write.csv(P8sensorTemp, "data/metab_data_clean/p8/P8_2019_sensorTemp.txt", row.names=FALSE)

#PAR
P8PAR <- P8_full %>%
  dplyr::select(dateTime, PAR) 
write.csv(P8PAR, "data/metab_data_clean/p8/P8_2019_PAR.txt", row.names=FALSE)

#wind height = unknown. measured from nearby weather station.
P8wind <- P8_full %>%
  dplyr::select(dateTime, wind) %>%
  rename(windSpeed=wind) 
write.csv(P8wind, "data/metab_data_clean/p8/P8_2019_windSpeed.txt", row.names=FALSE)

#temp profile
P8tempprofile<- P8_full %>%
  dplyr::select(dateTime, wtr0.1:wtr1.2)%>%
  rename_all(function(x) gsub("wtr", "wtr_", x)) #Add an underscore to all heads to read wtr_#.#
write.csv(P8tempprofile, "data/metab_data_clean/P8/P8_2019_tempProfile.txt", row.names=FALSE)








# > Oneida lake (ONEI) -------------------------------------------------------------


ONEIDOobs <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Oneida/Oneida_doobs.txt", sep="\t") 
ONEIwnd <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Oneida/Oneida_wind.txt", sep="\t")
ONEItemp <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Oneida/Oneida_wtr.txt", sep="\t")
ONEIPAR <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Oneida/Oneida_PAR.txt", sep="\t")






# Data collation/cleaning -------------------------------------------------
#Recgonize dateTimes as datese 
####DATE CONVERSION###
glimpse(ONEItemp)
ONEItemp <- ONEItemp %>%
  mutate(dateTime=as.POSIXct(paste(date, time),
                             format="%m/%d/%y %H:%M:%S"),
         dateTime = force_tz(dateTime, tz="America/New_York"),
         dateTime = with_tz(dateTime, "GMT")) %>%
  dplyr::select(-date, -time) %>%
  filter(site=="Shackelton Point temperature Buoy") %>%
  #Ignore other two sites, which only measure at 10m and not even the whole time period
  select(-site) %>%
  pivot_wider(values_from = "temp", names_from="depth", names_prefix="wtr_")
glimpse(ONEItemp)


glimpse(ONEIDOobs)
ONEIDOobs <- ONEIDOobs %>%
  mutate(dateTime = mdy_hm(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/New_York"),
         dateTime = with_tz(dateTime, "GMT")) %>%
  select(-localTZ)
glimpse(ONEIDOobs)

glimpse(ONEIwnd)
ONEIwnd <- ONEIwnd %>%
  mutate(dateTime = mdy_hm(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/New_York"),
         dateTime = with_tz(dateTime, "GMT"),
         wind = wind/2.237) %>% #Convert from mph to m/s?
  select(-localTZ)
glimpse(ONEIwnd)
#Convert wind from character to number
# ONEIwnd$wind <- as.numeric(ONEIwnd$wind)

glimpse(ONEIPAR)
ONEIPAR$solarRad_Wm2 <- as.numeric(ONEIPAR$solarRad_Wm2)
ONEIPAR <- ONEIPAR %>%
  mutate(dateTime = mdy_hm(as.factor(dateTime)),
         # solarRad_Wm2=as.numeric(solarRad_Wm2),
         dateTime = force_tz(dateTime, tz="America/New_York"),
         dateTime = with_tz(dateTime, "GMT"),
         PAR= solarRad_Wm2*2.15) %>% #See "Estimating PAR from solar flux data" Word Doc in Troubleshooting folder) 
  select(-solarRad_Wm2, -localTZ)
glimpse(ONEIPAR)
# ONEInuts <- ONEInuts %>%
#   mutate(dateTime = ymd_hms(as.factor(dateTime)),
#          dateTime = force_tz(dateTime, tz="UTC"),
#          dateTime = with_tz(dateTime, "GMT")) 
# 

#Print first five time readings for each variable
firstFiveTimes <- data.frame(DO=ONEIDOobs$dateTime[1:5],
                             PAR=ONEIPAR$dateTime[1:5],
                             wind=ONEIwnd$dateTime[1:5],
                             temp=ONEItemp$dateTime[1:5])
print('First five time readings for each variable'); print(firstFiveTimes)
#Ok this is going to be annoying because the times don't line up...

#Calculate first differences of time readings, display unique values
cat('\n','First differences of datetime','\n')
difTimesDO <- diff(ONEIDOobs$dateTime); print(table(difTimesDO))
difTimesPAR <- diff(ONEIPAR$dateTime); print(table(difTimesPAR))
difTimesWindSpeed <- diff(ONEIwnd$dateTime); print(table(difTimesWindSpeed))
difTimesSensorTemp <- diff(ONEItemp$dateTime); print(table(difTimesSensorTemp))
# difTimesSensorTemp_alt <- diff(ONEItemp_alt$dateTime); print(table(difTimesSensorTemp))

timeStep=30	# in minutes
#DO is at 5 minutes but most everything else is at 30min. 


notDupRows <- findNotDupRows("ONEIDOobs")
ONEIDOobs <- ONEIDOobs[notDupRows,]
notDupRows <- findNotDupRows("ONEIPAR")
ONEIPAR <- ONEIPAR[notDupRows,]
notDupRows <- findNotDupRows("ONEIwnd")
ONEIwnd <- ONEIwnd[notDupRows,]
notDupRows <- findNotDupRows("ONEItemp")
ONEItemp <- ONEItemp[notDupRows,]
# notDupRows <- findNotDupRows("ONEItemp_alt")
# ONEItemp_alt <- ONEItemp_alt[notDupRows,]

#Make all data sets extend from startTime to endTime by timeStep
#Note that for some lakes it may be necessary to aggregate some variables to coarser time scale to get match up

#Round all time down to nearest timeStep (e.g. if timeStep is 5, round 00:07 to 00:05)
ONEIDOobs$dateTime <- floorMins(ONEIDOobs)
ONEIPAR$dateTime <- floorMins(ONEIPAR)
ONEIwnd$dateTime <- floorMins(ONEIwnd)
ONEItemp$dateTime <- floorMins(ONEItemp)
# ONEItemp_alt$dateTime <- floorMins(ONEItemp_alt)

#Repeat check for dup rows in case any introduced during floorMins
notDupRows <- findNotDupRows("ONEIDOobs")
ONEIDOobs <- ONEIDOobs[notDupRows,]
notDupRows <- findNotDupRows("ONEIPAR")
ONEIPAR <- ONEIPAR[notDupRows,]
notDupRows <- findNotDupRows("ONEIwnd")
ONEIwnd <- ONEIwnd[notDupRows,]
notDupRows <- findNotDupRows("ONEItemp")
ONEItemp <- ONEItemp[notDupRows,]
# notDupRows <- findNotDupRows("ONEItemp_alt")
# ONEItemp_alt <- ONEItemp_alt[notDupRows,]

#Find the latest first time point and the earliest last time point of all the data
startTime <- max(min(ONEIDOobs$dateTime),min(ONEIPAR$dateTime),min(ONEIwnd$dateTime),min(ONEItemp$dateTime))
endTime <- min(max(ONEIDOobs$dateTime),max(ONEIPAR$dateTime),max(ONEIwnd$dateTime),max(ONEItemp$dateTime))

#Data.frame with one column "dateTime" which is sequence of times at time interval of timeStep, from startTime to endTime
completeTimes <- data.frame(dateTime=seq(startTime,endTime,paste(timeStep,"mins")))

#Merge all of input data.frames with completeTimes, so that they now all extend from startTime to endTime by timeStep
ONEIDOobs <- merge(completeTimes,ONEIDOobs,by="dateTime",all.x=T)
ONEIPAR <- merge(completeTimes,ONEIPAR,by="dateTime",all.x=T)
ONEIwnd <- merge(completeTimes,ONEIwnd,by="dateTime",all.x=T)
ONEItemp <- merge(completeTimes,ONEItemp,by="dateTime",all.x=T)
# ONEItemp_alt <- merge(completeTimes,ONEItemp_alt,by="dateTime",all.x=T)


ONEI_full <- left_join(ONEIDOobs, ONEIwnd, by = c("dateTime"))
ONEI_full <- left_join(ONEI_full, ONEItemp, by = c("dateTime"))
ONEI_full <- left_join(ONEI_full, ONEIPAR, by = c("dateTime"))

#For some reason (maybe can de-bug later) there are lots of duplicate rows.
#Use distinct function to pull out only distinct dateTimes.
ONEI_full<-ONEI_full %>%
  distinct(dateTime, .keep_all = TRUE) %>%
  # select(-localTZ) %>%
  relocate(wtr_2.0, .before = wtr_2.2) %>% #rearranging in case this presents a problem in modeling
  rename(wtr_8.0=wtr_8) #renaming so all water depths are in 'wtr_X.X' form

glimpse(ONEI_full)

####plot DO###########
ONEI_full %>%
  mutate(year=year(dateTime)) %>%
  ggplot(aes(x=dateTime, y=doObs)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%m-%d") +
  ylab("DO obs")+
  facet_wrap(.~year, scales="free")+
  ggtitle("ONEI Lake 2019")


####plot WIND###########
ONEI_full %>%
  mutate(year=year(dateTime)) %>%
  ggplot(aes(x=dateTime, y=wind)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "2 months", date_labels = "%m-%d") +
  ylab("Wind (m/s)")+
  facet_wrap(.~year, scales="free")+
  ggtitle("ONEI Lake 2019")


####plot TEMPS###########
ONEI_full %>%
  dplyr::select(dateTime, wtr_1.1:wtr_11.1)%>%
  pivot_longer(-(dateTime), names_sep = "_", names_to = c("variable","depth")) %>%
  dplyr::select(-variable)%>%
  mutate(year=year(dateTime)) %>%
  mutate(depth=as.numeric(as.character(depth))) %>%
  rename(temp=value)%>%
  ggplot(aes(x=dateTime, y=temp, color=depth)) + 
  geom_point(size=2)+
  scale_x_datetime(date_breaks = "2 months", date_labels = "%m-%d") +
  scale_color_continuous_sequential(palette = "Dark Mint", rev=TRUE)+
  ylab("water temperature")+
  facet_wrap(.~year, scales="free")+
  ggtitle("ONEI Lake 2019")





####plot PAR###########
ONEI_full %>%
  mutate(year=year(dateTime)) %>%
  mutate(PAR = replace(PAR, PAR<=12.90, 0))%>%
  mutate(PAR = replace(PAR, PAR==485.90, NA))%>% #I THINK THIS MUST BE AN ERROR?
  mutate(PAR = replace(PAR, PAR==653.60, NA))%>% #I THINK THIS MUST BE AN ERROR?
  ggplot(aes(x=dateTime, y=PAR)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "2 months", date_labels = "%m-%d") +
  ylab("PAR")+
  facet_wrap(.~year, scales="free")+
  ggtitle("ONEI Lake 2019")
#2019

ONEI_full %>%
  filter(dateTime >="2019-08-05" & dateTime <="2019-08-12") %>%
  mutate(PAR = replace(PAR, PAR<=12.90, 0))%>%
  mutate(PAR = replace(PAR, PAR==485.90, NA))%>% #I THINK THIS MUST BE AN ERROR?
  mutate(PAR = replace(PAR, PAR==653.60, NA))%>% #I THINK THIS MUST BE AN ERROR?
  mutate(date=date(dateTime))%>%
  ggplot(aes(x=dateTime, y=PAR, color=factor(date)))+
  geom_point()+
  ggtitle("Oneida")+
  scale_x_datetime(date_breaks = "4 hours",
                   date_labels = "%m-%d\n%H:%M")+
  theme(panel.grid.minor = element_line(color="lightgrey"),
        axis.text.x= element_text(size=8))



####plot nutrients###########
# ONEInuts %>%
#   mutate(dateTime = as_datetime(dateTime), year=year(dateTime)) %>%
#   dplyr::select(year, dateTime, DOC_mgL, TP_mgL, TN_mgL)%>%
#   pivot_longer(-(1:2), names_to = "nutrient") %>%
#   ggplot(aes(x=dateTime, y=value, color=nutrient)) + 
#   geom_point()+geom_line()+
#   scale_x_datetime(date_breaks = "2 months", date_labels = "%m-%d") +
#   ggtitle("ONEI Lake 2018-2019") +
#   facet_wrap(year~nutrient, scales="free", nrow=2)


# FINAL ONEI dataset ------------------------------------------------------
ONEI_full_2019<-ONEI_full %>%
  mutate(year=year(dateTime)) %>%
  filter(year=="2019")

####export for modelling###########
ONEIDOobs <- ONEI_full_2019 %>%
  dplyr::select(dateTime, doObs)%>%
  rename(DO=doObs)
write.csv(ONEIDOobs, "data/metab_data_clean/oneida/oneida_2019_DO.txt", row.names=FALSE)

#DO sensor depth = 0.86m but there is no sensor there. Using the next closest. 
#I'm afraid the 0.05m will have too high of temperature fluctuations. 
ONEIsensorTemp <- ONEI_full_2019 %>%
  dplyr::select(dateTime, wtr_2.0) %>%
  rename(sensorTemp=wtr_2.0) 
write.csv(ONEIsensorTemp, "data/metab_data_clean/oneida/oneida_2019_sensorTemp.txt", row.names=FALSE)

#PAR
ONEIPAR <- ONEI_full_2019 %>%
  dplyr::select(dateTime, PAR) %>%
  mutate(PAR = replace(PAR, PAR<=12.90, 0))%>%
  mutate(PAR = replace(PAR, PAR==485.90, NA))%>% #I THINK THIS MUST BE AN ERROR?
  mutate(PAR = replace(PAR, PAR==653.60, NA)) #I THINK THIS MUST BE AN ERROR?
write.csv(ONEIPAR, "data/metab_data_clean/oneida/oneida_2019_PAR.txt", row.names=FALSE)

#wind height = 2.85 m above lake surface
ONEIwind <- ONEI_full_2019 %>%
  dplyr::select(dateTime, wind) %>%
  rename(windSpeed=wind) 
write.csv(ONEIwind, "data/metab_data_clean/oneida/oneida_2019_windSpeed.txt", row.names=FALSE)

#temp profile
ONEItempprofile<- ONEI_full_2019 %>%
  dplyr::select(dateTime, wtr_1.1:wtr_11.1)
write.csv(ONEItempprofile, "data/metab_data_clean/oneida/oneida_2019_tempProfile.txt", row.names=FALSE)



### 2021-04-30 Checking on Oneida wtr profiles because the estimated zMix from the model
### is 30m which doesn't seem right. What does lakeAnalyzer/Metabolizer say? 
library(LakeMetabolizer)
Oneida_wtr<-ONEItempprofile %>%
  # mutate(datetime=ymd_hm(dateTime)) %>%
  filter(dateTime > "2019-05-01" & dateTime < "2019-10-01") %>%
  rename_all(function(x) gsub("temp", "wtr_", x)) %>%
  drop_na()#Add an underscore to all heads to read wtr_#.#
  # select(-dateTime, -wtr_0.0, -wtr_0.5, -wtr_13.0)

wtr.heat.map(Oneida_wtr, plot.title="Oneida Water Temp (C)")
View(Oneida_wtr)

#calculate and plot the metalimnion depths
m.d = ts.meta.depths(Oneida_wtr)

plot(m.d$datetime, m.d$top, type='l', ylab='Meta Depths (m)', xlab='Date', col='blue')
lines(m.d$datetime, m.d$bottom, col='red')

#calculate and plot the thermocline depth
t.d = ts.thermo.depth(Oneida_wtr)
plot(t.d$datetime, t.d$thermo.depth, type='l', ylab='Thermocline Depth (m)', xlab='Date')
str(t.d)
mean(t.d$thermo.depth, na.rm=TRUE)
median(t.d$thermo.depth, na.rm=TRUE)
t.d$thermo.depth[is.nan(t.d$thermo.depth)]<-NA
t.d

OneidazMix<- t.d %>%
  mutate(solarDay=date(datetime))%>%
  rename(zMix=thermo.depth) %>%
  group_by(solarDay) %>%
  summarize(zMix=mean(zMix, na.rm=TRUE))

OneidazMix$zMix[is.nan(OneidazMix$zMix)]<-NA
OneidazMix

OneidazMix %>%
  # filter(solarDay > "2018-05-01" & solarDay < "2018-10-01") %>%
  # mutate(if_nan(NA))%>%
  summarize(meanzMix=mean(zMix, na.rm=TRUE),
            medianzMix=median(zMix, na.rm=TRUE))
write.table(OneidazMix, "results/model_output_raw/Oneida/OneidazMix.txt", row.names=FALSE)





# > Croche -------------------------------------------------------------


croche_data <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Croche/croche_parWindWtrDO.csv", sep=",") 
glimpse(croche_data)
croche_full <- croche_data %>%
  rename(dateTime = Lac_Croche_2016,
         wtr_0.44 = TChain_Temp_1_Deg_C_Smp_0.44m,
         wtr_0.62 = TChain_Temp_2_Deg_C_Smp_0.62m,
         wtr_0.8 = TChain_Temp_3_Deg_C_Smp_0.8m,
         wtr_1.8 = TChain_Temp_4_Deg_C_Smp_1.8m,
         wtr_2.8 = TChain_Temp_5_Deg_C_Smp_2.8m,
         wtr_3.8 = TChain_Temp_6_Deg_C_Smp_3.8m,
         wtr_4.65 = TChain_Temp_7_Deg_C_Smp_4.65m,
         wtr_5.65 = TChain_Temp_8_Deg_C_Smp_5.65m,
         wtr_6.6 = TChain_Temp_9_Deg_C_Smp_6.6m,
         wtr_7.6 = TChain_Temp_10_Deg_C_Smp_7.6m,
         wtr_8.6 = TChain_Temp_11_Deg_C_Smp_8.6m,
         wtr_10.6 = TChain_Temp_12_Deg_C_Smp_10.6m,
         DO_umolL = OA_0.5corr) %>%
  mutate(DO = DO_umolL*31.998*(1/1000), #Convert umol L^-1 to mg L^-1
         dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/New_York"), #Timezone is GMT -5, or EST
         dateTime = with_tz(dateTime, "GMT")) %>%
  select(-DO_umolL)        


glimpse(croche_data)
Crochetemp <- croche_data %>%
  select(dateTime, wtr_0.44:wtr_10.6)%>%
  mutate(dateTime = mdy_hm(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/New_York"),
         dateTime = with_tz(dateTime, "GMT")) 
glimpse(Crochetemp)

glimpse(croche_data)
CrocheDOobs <- croche_data %>%
  select(dateTime,DO_0.5_umolL )%>%
  rename(DO=DO_0.5_umolL)%>% 
  mutate(DO = DO*31.998*(1/1000))%>% #Convert umol L^-1 to mg L^-1
  mutate(dateTime = mdy_hm(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/New_York"),
         dateTime = with_tz(dateTime, "GMT")) 
glimpse(CrocheDOobs)

glimpse(croche_data)
Crochewnd <- croche_data %>%
  select(dateTime, windSpeed)%>%
  mutate(dateTime = mdy_hm(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/New_York"),
         dateTime = with_tz(dateTime, "GMT")) 
glimpse(Crochewnd)

glimpse(croche_data)
CrochePAR <- croche_data %>%
  select(dateTime, PAR_0.87m_umol_per_sm2)%>%
  rename(PAR=PAR_0.87m_umol_per_sm2)%>%
  mutate(dateTime = mdy_hm(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/New_York"),
         dateTime = with_tz(dateTime, "GMT")) 
glimpse(CrochePAR)



#Print first five time readings for each variable
firstFiveTimes <- data.frame(DO=CrocheDOobs$dateTime[1:5],
                             PAR=CrochePAR$dateTime[1:5],
                             wind=Crochewnd$dateTime[1:5],
                             temp=Crochetemp$dateTime[1:5])
print('First five time readings for each variable'); print(firstFiveTimes)
#Ok this is going to be annoying because the times don't line up...

#Calculate first differences of time readings, display unique values
cat('\n','First differences of datetime','\n')
difTimesDO <- diff(CrocheDOobs$dateTime); print(table(difTimesDO))
difTimesPAR <- diff(CrochePAR$dateTime); print(table(difTimesPAR))
difTimesWindSpeed <- diff(Crochewnd$dateTime); print(table(difTimesWindSpeed))
difTimesSensorTemp <- diff(Crochetemp$dateTime); print(table(difTimesSensorTemp))
timeStep=10	# in minutes


notDupRows <- findNotDupRows("CrocheDOobs")
CrocheDOobs <- CrocheDOobs[notDupRows,]

notDupRows <- findNotDupRows("CrochePAR")
CrochePAR <- CrochePAR[notDupRows,]

notDupRows <- findNotDupRows("Crochewnd")
Crochewnd <- Crochewnd[notDupRows,]

notDupRows <- findNotDupRows("Crochetemp")
Crochetemp <- Crochetemp[notDupRows,]

#Make all data sets extend from startTime to endTime by timeStep
#Note that for some lakes it may be necessary to aggregate some variables to coarser time scale to get match up


CrocheDOobs$dateTime <- floorMins(CrocheDOobs)
CrochePAR$dateTime <- floorMins(CrochePAR)
Crochewnd$dateTime <- floorMins(Crochewnd)
Crochetemp$dateTime <- floorMins(Crochetemp)


# #Repeat check for dup rows in case any introduced during floorMins
notDupRows <- findNotDupRows("CrocheDOobs")
CrocheDOobs <- CrocheDOobs[notDupRows,]
notDupRows <- findNotDupRows("CrochePAR")
CrochePAR <- CrochePAR[notDupRows,]
notDupRows <- findNotDupRows("Crochewnd")
Crochewnd <- Crochewnd[notDupRows,]
notDupRows <- findNotDupRows("Crochetemp")
Crochetemp <- Crochetemp[notDupRows,]

#Find the latest first time point and the earliest last time point of all the data
startTime <- max(min(CrocheDOobs$dateTime),min(CrochePAR$dateTime),min(Crochewnd$dateTime),min(Crochetemp$dateTime))
endTime <- min(max(CrocheDOobs$dateTime),max(CrochePAR$dateTime),max(Crochewnd$dateTime),max(Crochetemp$dateTime))

#Data.frame with one column "dateTime" which is sequence of times at time interval of timeStep, from startTime to endTime
completeTimes <- data.frame(dateTime=seq(startTime,endTime,paste(timeStep,"mins")))

#Merge all of input data.frames with completeTimes, so that they now all extend from startTime to endTime by timeStep
CrocheDOobs <- merge(completeTimes,CrocheDOobs,by="dateTime",all.x=T)
CrochePAR <- merge(completeTimes,CrochePAR,by="dateTime",all.x=T)
Crochewnd <- merge(completeTimes,Crochewnd,by="dateTime",all.x=T)
Crochetemp <- merge(completeTimes,Crochetemp,by="dateTime",all.x=T)

#Is it a problem that there are NAs in the PAR data? I guess we will find out
##IAO - 2021-02-12 --  I just checked and it shouldn't be because of the fillHoles helper function. 

Croche_full <- left_join(CrocheDOobs, Crochewnd, by = c("dateTime"))
Croche_full <- left_join(Croche_full, Crochetemp, by = c("dateTime"))
Croche_full <- left_join(Croche_full, CrochePAR, by = c("dateTime"))

glimpse(Croche_full)



####plot DO###########
Croche_full %>%
  mutate(year=year(dateTime)) %>%
  ggplot(aes(x=dateTime, y=DO)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%m-%d") +
  ylab("DO obs")+
  facet_wrap(.~year, scales="free")+
  ggtitle("Croche Lake 2016")


####plot WIND###########
Croche_full %>%
  mutate(year=year(dateTime)) %>%
  ggplot(aes(x=dateTime, y=windSpeed)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "2 months", date_labels = "%m-%d") +
  ylab("Wind (m/s)")+
  facet_wrap(.~year, scales="free")+
  ggtitle("Croche Lake 2016")


####plot TEMPS###########
Croche_full %>%
  dplyr::select(dateTime, wtr_0.44:wtr_10.6)%>%
  pivot_longer(-(dateTime), names_sep = "_", names_to = c("variable","depth")) %>%
  dplyr::select(-variable)%>%
  mutate(year=year(dateTime)) %>%
  mutate(depth=as.numeric(as.character(depth))) %>%
  rename(temp=value)%>%
  ggplot(aes(x=dateTime, y=temp, color=depth)) + 
  geom_point(size=2)+
  scale_x_datetime(date_breaks = "2 months", date_labels = "%m-%d") +
  scale_color_continuous_sequential(palette = "Dark Mint", rev=TRUE)+
  ylab("water temperature")+
  facet_wrap(.~year, scales="free")+
  ggtitle("Croche Lake 2016")

####plot PAR###########
Croche_full %>%
  ggplot(aes(x=dateTime, y=PAR)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "2 months", date_labels = "%m-%d") +
  ylab("PAR")+
  ggtitle("Croche Lake 2016")
#2019


# export for modeling -----------------------------------------------------


#DO
CrocheDOobs <- Croche_full %>%
  dplyr::select(dateTime, DO)
write.csv(CrocheDOobs, "data/metab_data_clean/croche/Croche_DO.txt", row.names=FALSE)

#Sensor temp
glimpse(Croche_full)
CrochesensorTemp <- Croche_full %>%
  dplyr::select(dateTime, wtr_0.44) %>%
  rename(sensorTemp=wtr_0.44) 
write.csv(CrochesensorTemp, "data/metab_data_clean/croche/Croche_sensorTemp.txt", row.names=FALSE)

#PAR
CrochePAR <- Croche_full %>%
  dplyr::select(dateTime, PAR) 
write.csv(CrochePAR, "data/metab_data_clean/croche/Croche_PAR.txt", row.names=FALSE)

#wind height =  ????
Crochewind <- Croche_full %>%
  dplyr::select(dateTime, windSpeed)
write.csv(Crochewind, "data/metab_data_clean/croche/Croche_windSpeed.txt", row.names=FALSE)

#temp profile
Crochetempprofile<- Croche_full %>%
  dplyr::select(dateTime, wtr_0.44:wtr_10.6) 
write.csv(Crochetempprofile, "data/metab_data_clean/croche/Croche_tempProfile.txt", row.names=FALSE)




# > Simoncouche -------------------------------------------------------------


Simoncouche_data <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Simoncouche/simoncoucheWindPARwtrDO.csv", sep=",") 


glimpse(Simoncouche_data)
Simoncouchetemp <- Simoncouche_data %>%
  select(dateTime, wtr_0.4:wtr_9.5)%>%
  mutate(dateTime = mdy_hm(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/New_York"),
         dateTime = with_tz(dateTime, "GMT")) 
glimpse(Simoncouchetemp)

glimpse(Simoncouche_data)
SimoncoucheDOobs <- Simoncouche_data %>%
  select(dateTime,DO_umolL_0.5 )%>%
  rename(DO=DO_umolL_0.5)%>% 
  mutate(DO = DO*31.998*(1/1000))%>% #Convert umol L^-1 to mg L^-1
  mutate(dateTime = mdy_hm(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/New_York"),
         dateTime = with_tz(dateTime, "GMT")) 
glimpse(SimoncoucheDOobs)

glimpse(Simoncouche_data)
Simoncouchewnd <- Simoncouche_data %>%
  select(dateTime, windSpeed)%>%
  mutate(dateTime = mdy_hm(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/New_York"),
         dateTime = with_tz(dateTime, "GMT")) 
glimpse(Simoncouchewnd)

glimpse(Simoncouche_data)
SimoncouchePAR <- Simoncouche_data %>%
  select(dateTime, PARwater_1m)%>%
  rename(PAR=PARwater_1m)%>%
  mutate(dateTime = mdy_hm(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/New_York"),
         dateTime = with_tz(dateTime, "GMT")) 
glimpse(SimoncouchePAR)



#Print first five time readings for each variable
firstFiveTimes <- data.frame(DO=SimoncoucheDOobs$dateTime[1:5],
                             PAR=SimoncouchePAR$dateTime[1:5],
                             wind=Simoncouchewnd$dateTime[1:5],
                             temp=Simoncouchetemp$dateTime[1:5])
print('First five time readings for each variable'); print(firstFiveTimes)
#Ok this is going to be annoying because the times don't line up...

#Calculate first differences of time readings, display unique values
cat('\n','First differences of datetime','\n')
difTimesDO <- diff(SimoncoucheDOobs$dateTime); print(table(difTimesDO))
difTimesPAR <- diff(SimoncouchePAR$dateTime); print(table(difTimesPAR))
difTimesWindSpeed <- diff(Simoncouchewnd$dateTime); print(table(difTimesWindSpeed))
difTimesSensorTemp <- diff(Simoncouchetemp$dateTime); print(table(difTimesSensorTemp))
timeStep=12	# in minutes


notDupRows <- findNotDupRows("SimoncoucheDOobs")
SimoncoucheDOobs <- SimoncoucheDOobs[notDupRows,]

notDupRows <- findNotDupRows("SimoncouchePAR")
SimoncouchePAR <- SimoncouchePAR[notDupRows,]

notDupRows <- findNotDupRows("Simoncouchewnd")
Simoncouchewnd <- Simoncouchewnd[notDupRows,]

notDupRows <- findNotDupRows("Simoncouchetemp")
Simoncouchetemp <- Simoncouchetemp[notDupRows,]

#Make all data sets extend from startTime to endTime by timeStep
#Note that for some lakes it may be necessary to aggregate some variables to coarser time scale to get match up


SimoncoucheDOobs$dateTime <- floorMins(SimoncoucheDOobs)
SimoncouchePAR$dateTime <- floorMins(SimoncouchePAR)
Simoncouchewnd$dateTime <- floorMins(Simoncouchewnd)
Simoncouchetemp$dateTime <- floorMins(Simoncouchetemp)


# #Repeat check for dup rows in case any introduced during floorMins
notDupRows <- findNotDupRows("SimoncoucheDOobs")
SimoncoucheDOobs <- SimoncoucheDOobs[notDupRows,]
notDupRows <- findNotDupRows("SimoncouchePAR")
SimoncouchePAR <- SimoncouchePAR[notDupRows,]
notDupRows <- findNotDupRows("Simoncouchewnd")
Simoncouchewnd <- Simoncouchewnd[notDupRows,]
notDupRows <- findNotDupRows("Simoncouchetemp")
Simoncouchetemp <- Simoncouchetemp[notDupRows,]

#Find the latest first time point and the earliest last time point of all the data
startTime <- max(min(SimoncoucheDOobs$dateTime),min(SimoncouchePAR$dateTime),min(Simoncouchewnd$dateTime),min(Simoncouchetemp$dateTime))
endTime <- min(max(SimoncoucheDOobs$dateTime),max(SimoncouchePAR$dateTime),max(Simoncouchewnd$dateTime),max(Simoncouchetemp$dateTime))

#Data.frame with one column "dateTime" which is sequence of times at time interval of timeStep, from startTime to endTime
completeTimes <- data.frame(dateTime=seq(startTime,endTime,paste(timeStep,"mins")))

#Merge all of input data.frames with completeTimes, so that they now all extend from startTime to endTime by timeStep
SimoncoucheDOobs <- merge(completeTimes,SimoncoucheDOobs,by="dateTime",all.x=T)
SimoncouchePAR <- merge(completeTimes,SimoncouchePAR,by="dateTime",all.x=T)
Simoncouchewnd <- merge(completeTimes,Simoncouchewnd,by="dateTime",all.x=T)
Simoncouchetemp <- merge(completeTimes,Simoncouchetemp,by="dateTime",all.x=T)

#Is it a problem that there are NAs in the PAR data? I guess we will find out
##IAO - 2021-02-12 --  I just checked and it shouldn't be because of the fillHoles helper function. 

Simoncouche_full <- left_join(SimoncoucheDOobs, Simoncouchewnd, by = c("dateTime"))
Simoncouche_full <- left_join(Simoncouche_full, Simoncouchetemp, by = c("dateTime"))
Simoncouche_full <- left_join(Simoncouche_full, SimoncouchePAR, by = c("dateTime"))

glimpse(Simoncouche_full)



####plot DO###########
Simoncouche_full %>%
  mutate(year=year(dateTime)) %>%
  ggplot(aes(x=dateTime, y=DO)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%m-%d") +
  ylab("DO obs")+
  facet_wrap(.~year, scales="free")+
  ggtitle("Simoncouche Lake 2016")


####plot WIND###########
Simoncouche_full %>%
  mutate(year=year(dateTime)) %>%
  ggplot(aes(x=dateTime, y=windSpeed)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "2 months", date_labels = "%m-%d") +
  ylab("Wind (m/s)")+
  facet_wrap(.~year, scales="free")+
  ggtitle("Simoncouche Lake 2016")


####plot TEMPS###########
Simoncouche_full %>%
  dplyr::select(dateTime, wtr_0.4:wtr_9.5)%>%
  pivot_longer(-(dateTime), names_sep = "_", names_to = c("variable","depth")) %>%
  dplyr::select(-variable)%>%
  mutate(year=year(dateTime)) %>%
  mutate(depth=as.numeric(as.character(depth))) %>%
  rename(temp=value)%>%
  ggplot(aes(x=dateTime, y=temp, color=depth)) + 
  geom_point(size=2)+
  scale_x_datetime(date_breaks = "2 months", date_labels = "%m-%d") +
  scale_color_continuous_sequential(palette = "Dark Mint", rev=TRUE)+
  ylab("water temperature")+
  facet_wrap(.~year, scales="free")+
  ggtitle("Simoncouche Lake 2016")

####plot PAR###########
Simoncouche_full %>%
  ggplot(aes(x=dateTime, y=PAR)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "2 months", date_labels = "%m-%d") +
  ylab("PAR")+
  ggtitle("Simoncouche Lake 2016")
#2019


# export for modeling -----------------------------------------------------


#DO
SimoncoucheDOobs <- Simoncouche_full %>%
  dplyr::select(dateTime, DO)
write.csv(SimoncoucheDOobs, "data/metab_data_clean/Simoncouche/Simoncouche_DO.txt", row.names=FALSE)

#Sensor temp
glimpse(Simoncouche_full)
SimoncouchesensorTemp <- Simoncouche_full %>%
  dplyr::select(dateTime, wtr_0.4) %>%
  rename(sensorTemp=wtr_0.4) 
write.csv(SimoncouchesensorTemp, "data/metab_data_clean/Simoncouche/Simoncouche_sensorTemp.txt", row.names=FALSE)

#PAR
SimoncouchePAR <- Simoncouche_full %>%
  dplyr::select(dateTime, PAR) 
write.csv(SimoncouchePAR, "data/metab_data_clean/Simoncouche/Simoncouche_PAR.txt", row.names=FALSE)

#wind height =  ????
Simoncouchewind <- Simoncouche_full %>%
  dplyr::select(dateTime, windSpeed)
write.csv(Simoncouchewind, "data/metab_data_clean/Simoncouche/Simoncouche_windSpeed.txt", row.names=FALSE)

#temp profile
Simoncouchetempprofile<- Simoncouche_full %>%
  dplyr::select(dateTime, wtr_0.4:wtr_9.5) 
write.csv(Simoncouchetempprofile, "data/metab_data_clean/Simoncouche/Simoncouche_tempProfile.txt", row.names=FALSE)




### 2021-04-30 Checking on Simoncouche wtr profiles because the estimated zMix from the model
### is 30m which doesn't seem right. What does lakeAnalyzer/Metabolizer say? 
library(LakeMetabolizer)
Simoncouche_wtr<-Simoncouchetempprofile %>%
  # mutate(datetime=ymd_hm(dateTime)) %>%
  filter(dateTime >= "2016-05-01" & dateTime < "2016-10-01") %>%
  rename_all(function(x) gsub("temp", "wtr_", x)) %>%
  drop_na()#Add an underscore to all heads to read wtr_#.#
# select(-dateTime, -wtr_0.0, -wtr_0.5, -wtr_13.0)

wtr.heat.map(Simoncouche_wtr, plot.title="Simoncouche Water Temp (C)")
View(Simoncouche_wtr)

#calculate and plot the metalimnion depths
m.d = ts.meta.depths(Simoncouche_wtr)

plot(m.d$datetime, m.d$top, type='l', ylab='Meta Depths (m)', xlab='Date', col='blue')
lines(m.d$datetime, m.d$bottom, col='red')

#calculate and plot the thermocline depth
t.d = ts.thermo.depth(Simoncouche_wtr)
plot(t.d$datetime, t.d$thermo.depth, type='l', ylab='Thermocline Depth (m)', xlab='Date')
str(t.d)
mean(t.d$thermo.depth, na.rm=TRUE)
median(t.d$thermo.depth, na.rm=TRUE)
t.d$thermo.depth[is.nan(t.d$thermo.depth)]<-NA
t.d

SimoncouchezMix<- t.d %>%
  mutate(solarDay=date(datetime))%>%
  rename(zMix=thermo.depth) %>%
  group_by(solarDay) %>%
  summarize(zMix=mean(zMix, na.rm=TRUE))

# SimoncouchezMix$zMix[is.nan(SimoncouchezMix$zMix)]<-NA
# SimoncouchezMix

SimoncouchezMix %>%
  # filter(solarDay > "2018-05-01" & solarDay < "2018-10-01") %>%
  # mutate(if_nan(NA))%>%
  summarize(meanzMix=mean(zMix, na.rm=TRUE),
            medianzMix=median(zMix, na.rm=TRUE))
write.table(SimoncouchezMix, "results/model_output_raw/Simoncouche/SimoncouchezMix.txt", row.names=FALSE)




# > Lake Erken -------------------------------------------------------------

ErkenDOobs <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Erken/Erken_ODO mg l_2018.txt", stringsAsFactors = FALSE) %>%
  rename(dateTime=Date.Time,
         localTZ=TimeZone) %>%
  select(-DayLightSavings)
Erkenwnd <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Erken/Erken_Wind_2018.txt", stringsAsFactors = FALSE, skip=3) %>%
  rename(dateTime=TMSTAMP,
         localTZ=Time.Zone,
         windSpeed=X.m.s.) %>%
  select(-Daylight.savings)
Erkentemp <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Erken/Erken_2018_WTemp_60min.txt", stringsAsFactors = FALSE) %>%
  rename(dateTime=TMSTAMP,
         localTZ=Time.Zone) %>%
  select(-Daylight.Savings)
ErkenPAR <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Erken/Erken_Rad_2018.txt", stringsAsFactors = FALSE)%>%
  rename(dateTime=TMSTAMP,
         localTZ=TimeZone) %>%
  select(-Daylight.Savings)

tz(ErkenPAR$dateTime)

####DATE CONVERSION###
glimpse(Erkentemp)
Erkentemp <- Erkentemp %>%
  mutate(dateTime = mdy_hm(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="Europe/Berlin"),
         dateTime = with_tz(dateTime, "GMT")) %>%
  select(-localTZ)
glimpse(Erkentemp)

glimpse(ErkenDOobs)
ErkenDOobs <- ErkenDOobs %>%
  mutate(dateTime = mdy_hm(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="Europe/Berlin"),
         dateTime = with_tz(dateTime, "GMT")) %>%
  select(-localTZ)
glimpse(ErkenDOobs)

glimpse(Erkenwnd)
Erkenwnd <- Erkenwnd %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="Europe/Berlin"),
         dateTime = with_tz(dateTime, "GMT")) 
glimpse(Erkenwnd)

glimpse(ErkenPAR)
ErkenPAR <- ErkenPAR %>%
  mutate(dateTime = mdy_hm(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="Europe/Berlin"),
         dateTime = with_tz(dateTime, "GMT")) %>%
  select(-localTZ)
glimpse(ErkenPAR)

#Print first five time readings for each variable
firstFiveTimes <- data.frame(DO=ErkenDOobs$dateTime[1:5],
                             PAR=ErkenPAR$dateTime[1:5],
                             wind=Erkenwnd$dateTime[1:5],
                             temp=Erkentemp$dateTime[1:5])
print('First five time readings for each variable'); print(firstFiveTimes)
#Ok this is going to be annoying because the times don't line up...

#Calculate first differences of time readings, display unique values
cat('\n','First differences of datetime','\n')
difTimesDO <- diff(ErkenDOobs$dateTime); print(table(difTimesDO))
difTimesPAR <- diff(ErkenPAR$dateTime); print(table(difTimesPAR))
difTimesWindSpeed <- diff(Erkenwnd$dateTime); print(table(difTimesWindSpeed))
difTimesSensorTemp <- diff(Erkentemp$dateTime); print(table(difTimesSensorTemp))
timeStep=60	# in minutes


notDupRows <- findNotDupRows("ErkenDOobs")
ErkenDOobs <- ErkenDOobs[notDupRows,]

notDupRows <- findNotDupRows("ErkenPAR")
ErkenPAR <- ErkenPAR[notDupRows,]

notDupRows <- findNotDupRows("Erkenwnd")
Erkenwnd <- Erkenwnd[notDupRows,]

notDupRows <- findNotDupRows("Erkentemp")
Erkentemp <- Erkentemp[notDupRows,]

###Remove any rows where dateTime is NA for some reason,
###otherwise, floorMins won't work 
ErkenDOobs<-ErkenDOobs %>%
  filter_at(vars(dateTime), any_vars(!is.na(.)))

ErkenPAR<-ErkenPAR %>%
  filter_at(vars(dateTime), any_vars(!is.na(.)))

Erkenwnd<-Erkenwnd %>%
  filter_at(vars(dateTime), any_vars(!is.na(.)))

Erkentemp<-Erkentemp %>%
  filter_at(vars(dateTime), any_vars(!is.na(.)))


#Make all data sets extend from startTime to endTime by timeStep
#Note that for some lakes it may be necessary to aggregate some variables to coarser time scale to get match up


ErkenDOobs$dateTime <- floorMins(ErkenDOobs)
ErkenPAR$dateTime <- floorMins(ErkenPAR)
Erkenwnd$dateTime <- floorMins(Erkenwnd)
Erkentemp$dateTime <- floorMins(Erkentemp)


# #Repeat check for dup rows in case any introduced during floorMins
notDupRows <- findNotDupRows("ErkenDOobs")
ErkenDOobs <- ErkenDOobs[notDupRows,]
notDupRows <- findNotDupRows("ErkenPAR")
ErkenPAR <- ErkenPAR[notDupRows,]
notDupRows <- findNotDupRows("Erkenwnd")
Erkenwnd <- Erkenwnd[notDupRows,]
notDupRows <- findNotDupRows("Erkentemp")
Erkentemp <- Erkentemp[notDupRows,]

#Find the latest first time point and the earliest last time point of all the data
startTime <- max(min(ErkenDOobs$dateTime),min(ErkenPAR$dateTime),min(Erkenwnd$dateTime),min(Erkentemp$dateTime))
endTime <- min(max(ErkenDOobs$dateTime),max(ErkenPAR$dateTime),max(Erkenwnd$dateTime),max(Erkentemp$dateTime))

#Data.frame with one column "dateTime" which is sequence of times at time interval of timeStep, from startTime to endTime
completeTimes <- data.frame(dateTime=seq(startTime,endTime,paste(timeStep,"mins")))

#Merge all of input data.frames with completeTimes, so that they now all extend from startTime to endTime by timeStep
ErkenDOobs <- merge(completeTimes,ErkenDOobs,by="dateTime",all.x=T)
ErkenPAR <- merge(completeTimes,ErkenPAR,by="dateTime",all.x=T)
Erkenwnd <- merge(completeTimes,Erkenwnd,by="dateTime",all.x=T)
Erkentemp <- merge(completeTimes,Erkentemp,by="dateTime",all.x=T)

#Is it a problem that there are NAs in the PAR data? I guess we will find out
##IAO - 2021-02-12 --  I just checked and it shouldn't be because of the fillHoles helper function. 

Erken_full <- left_join(ErkenDOobs, Erkenwnd, by = c("dateTime"))
Erken_full <- left_join(Erken_full, Erkentemp, by = c("dateTime"))
Erken_full <- left_join(Erken_full, ErkenPAR, by = c("dateTime"))

glimpse(Erken_full)

tz(Erken_full$dateTime)

# plot temps ----------------------------------------------------------
Erken_full %>%
  dplyr::select(dateTime, wtr_1:wtr_15.5)%>%
  pivot_longer(-(dateTime), names_sep = "_", names_to = c("variable","depth")) %>%
  dplyr::select(-variable)%>%
  mutate(depth=as.numeric(as.character(depth)),
         year=year(dateTime)) %>%
  rename(temp=value)%>%
  ggplot(aes(x=dateTime, y=temp, color=depth)) + 
  geom_point(size=2)+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%y-%m-%d") +
  ylab("Temp C")+
  ggtitle(" Erken 2018")+
  scale_color_continuous_sequential(palette = "Dark Mint", rev=TRUE)+
  facet_wrap(.~year, scales="free_x")

library(LakeMetabolizer)
Erken.wtr<- Erken_full %>%
  select(dateTime, wtr_1:wtr_15.5)
wtr.heat.map(Erken.wtr, plot.title="Erken Water Temp (C)")

#calculate and plot the metalimnion depths
m.d = ts.meta.depths(Erken.wtr)
plot(m.d$datetime, m.d$top, type='l', ylab='Meta Depths (m)', xlab='Date', col='blue')
lines(m.d$datetime, m.d$bottom, col='red')

#calculate and plot the thermocline depth
t.d = ts.thermo.depth(Erken.wtr)
plot(t.d$datetime, t.d$thermo.depth, type='l', ylab='Thermocline Depth (m)', xlab='Date')
mean(t.d$thermo.depth, na.rm=TRUE)
median(t.d$thermo.depth, na.rm=TRUE)

ErkenzMix<-t.d %>%
  mutate(solarDay=date(datetime))%>%
  rename(zMix=thermo.depth) %>%
  group_by(solarDay) %>%
  summarize(zMix=mean(zMix, na.rm=TRUE))
# write.table(KentuckyzMix, "results/model_output_raw/Kentucky/KentuckyzMix.txt", row.names=FALSE)





####plot DO###########

Erken_full %>%
  dplyr::select(dateTime, DO_1:DO_17.5)%>%
  pivot_longer(-(dateTime), names_sep = "_", names_to = c("variable","depth")) %>%
  dplyr::select(-variable)%>%
  mutate(depth=as.numeric(as.character(depth)),
         year=year(dateTime)) %>%
  rename(temp=value)%>%
  ggplot(aes(x=dateTime, y=temp, color=depth)) + 
  geom_point(size=2)+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%y-%m-%d") +
  ylab("DO (mg/L)")+
  ggtitle(" Erken 2018")+
  scale_color_continuous_sequential(palette = "Dark Mint", rev=TRUE)+
  facet_wrap(.~year, scales="free_x")

#Just one DO depth... 
ggplot(Erken_full, aes(x=dateTime, y=wtr_1)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%y-%m") +
  ylab("DO obs @ 1m")+
  ggtitle(" Erken")

ggplot(Erken_full, aes(x=dateTime, y=wtr_2)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%y-%m") +
  ylab("DO obs @ 2m")+
  ggtitle(" Erken")

#Plot diel DO by depth
Erken_full %>%
  dplyr::select(dateTime, DO_1:DO_17.5)%>%
  pivot_longer(-(dateTime), names_sep = "_", names_to = c("variable","depth")) %>%
  mutate(date=date(dateTime)) %>%
  group_by(date,depth)%>%
  summarize(diel_DO=max(value)-min(value)) %>%
  ungroup()%>%
  mutate(depth= factor(depth, levels=c("1","1.5","2","2.5","3","3.5","4","4.5",
                                       "5","5.5","6","6.5","7","7.5","8","8.5",
                                       "9","9.5","10","10.5","11","11.5","12",
                                       "12.5","13","13.5","14","14.5","15",
                                       "15.5","16","16.5","17","17.5"))) %>%
  ggplot(aes(x=date, y=diel_DO, color=depth)) + 
  geom_point(size=2)+
  scale_x_date(date_breaks = "3 weeks", date_labels = "%y-%m-%d") +
  ylab("DIEL DO (mg/L)")+
  ggtitle(" Erken 2018")+
  scale_color_discrete_sequential(palette = "Dark Mint", rev=TRUE)

#Some depth have HUGE diel swings. Which are they?
#Plot histograms of diel swings by depth. 
Erken_full %>%
  dplyr::select(dateTime, DO_1:DO_17.5)%>%
  pivot_longer(-(dateTime), names_sep = "_", names_to = c("variable","depth")) %>%
  mutate(date=date(dateTime)) %>%
  group_by(date,depth)%>%
  summarize(diel_DO=max(value)-min(value)) %>%
  ungroup()%>%
  mutate(depth= factor(depth, levels=c("1","1.5","2","2.5","3","3.5","4","4.5",
                                       "5","5.5","6","6.5","7","7.5","8","8.5",
                                       "9","9.5","10","10.5","11","11.5","12",
                                       "12.5","13","13.5","14","14.5","15",
                                       "15.5","16","16.5","17","17.5"))) %>%
  ggplot( aes(x=diel_DO, color=depth, fill=depth)) +
  geom_histogram(alpha=0.6) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("") +
  ylab("Counts") +
  facet_wrap(~depth)

#Seems like decent DO swings. Will try 5m? 
ggplot(Erken_full, aes(x=dateTime, y=wtr_5)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%y-%m") +
  ylab("DO obs @ 5m")+
  ggtitle(" Erken")

Erken_full %>%
  dplyr::select(dateTime, DO_5)%>%
  pivot_longer(-(dateTime), names_sep = "_", names_to = c("variable","depth")) %>%
  mutate(date=date(dateTime)) %>%
  group_by(date,depth)%>%
  summarize(diel_DO=max(value)-min(value)) %>%
  ungroup()%>%
  ggplot(aes(x=date, y=diel_DO, color=depth)) + 
  geom_point(size=2)+
  scale_x_date(date_breaks = "3 weeks", date_labels = "%y-%m-%d") +
  ylab("DIEL DO (mg/L) at 5m")+
  ggtitle(" Erken 2018")+
  scale_color_discrete_sequential(palette = "Dark Mint", rev=TRUE)

####plot WIND###########
ggplot(Erken_full, aes(x=dateTime, y=windSpeed)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%y-%m") +
  ylab("Wind (m/s)")+
  ggtitle(" Erken 2018")

####plot PAR###########
ggplot(Erken_full, aes(x=dateTime, y=PAR_umol_m2_S)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%y-%m") +
  ylab("PAR")+
  ggtitle("Erken 2018")

Erken_full %>%
  filter(dateTime >="2018-08-05" & dateTime <="2018-08-08") %>%
  mutate(date=date(dateTime))%>%
  ggplot(aes(x=dateTime, y=PAR_umol_m2_S, color=factor(date)))+
  geom_point()+
  ggtitle("Erken")+
  scale_x_datetime(date_breaks = "4 hours",
                   date_labels = "%m-%d\n%H:%M")+
  theme(panel.grid.minor = element_line(color="lightgrey"),
        axis.text.x= element_text(size=8))

head(Erken_full)
####export for modelling###########
ErkenDOobs <- Erken_full %>%
  dplyr::select(dateTime, DO_1) %>%
  rename(DO=DO_1) 
write.csv(ErkenDOobs, "data/metab_data_clean/erken/Erken_2018_DO_1.0m.txt", row.names=FALSE)

#DO sensor depth = 0.2
ErkensensorTemp <- Erken_full %>%
  dplyr::select(dateTime, wtr_1) %>%
  rename(sensorTemp=wtr_1) 
write.csv(ErkensensorTemp, "data/metab_data_clean/erken/Erken_2018_sensorTemp_1.0m.txt", row.names=FALSE)

#PAR
ErkenPAR <- Erken_full %>%
  dplyr::select(dateTime, PAR) 
write.csv(ErkenPAR, "data/metab_data_clean/erken/Erken_2018_PAR.txt", row.names=FALSE)

#wind height = unknown. measured from nearby weather station.
Erkenwind <- Erken_full %>%
  dplyr::select(dateTime, windSpeed)
write.csv(Erkenwind, "data/metab_data_clean/erken/Erken_2018_windSpeed.txt", row.names=FALSE)

#temp profile
glimpse(Erken_full)
Erkentempprofile<- Erken_full %>%
  dplyr::select(dateTime, wtr_1:wtr_15.5)
glimpse(Erkentempprofile)
write.csv(Erkentempprofile, "data/metab_data_clean/erken/Erken_2018_tempProfile.txt", row.names=FALSE)



# > Utah Lake -------------------------------------------------------------

UtahDOobs <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Utah/UtahLake_doobs.txt", stringsAsFactors = FALSE) %>%
  select(-daylightSavings, -lakeID)
Utahwnd <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Utah/UtahLake_wnd.txt", stringsAsFactors = FALSE) %>%
  select(-daylightSavings, -lakeID)
Utahtemp <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Utah/UtahLake_wtr.txt", stringsAsFactors = FALSE) %>%
  select(-daylightSavings, -lakeID)
UtahPAR <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Utah/UtahLake_par.txt", stringsAsFactors = FALSE)%>%
  select(-daylightSavings, -lakeID)


####DATE CONVERSION###
glimpse(Utahtemp)
Utahtemp <- Utahtemp %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Denver"),
         dateTime = with_tz(dateTime, "GMT")) %>%
  select(-localTZ) %>%
  rename(wtr_1.0=wtr1.0)
glimpse(Utahtemp)

glimpse(UtahDOobs)
UtahDOobs <- UtahDOobs %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Denver"),
         dateTime = with_tz(dateTime, "GMT")) %>%
  select(-localTZ)
glimpse(UtahDOobs)

glimpse(Utahwnd)
Utahwnd <- Utahwnd %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Denver"),
         dateTime = with_tz(dateTime, "GMT")) %>%
  select(-localTZ)
glimpse(Utahwnd)

glimpse(UtahPAR)
UtahPAR <- UtahPAR %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Denver"),
         dateTime = with_tz(dateTime, "GMT")) %>%
  select(-localTZ)
glimpse(UtahPAR)

#Print first five time readings for each variable
firstFiveTimes <- data.frame(DO=UtahDOobs$dateTime[1:5],
                             PAR=UtahPAR$dateTime[1:5],
                             wind=Utahwnd$dateTime[1:5],
                             temp=Utahtemp$dateTime[1:5])
print('First five time readings for each variable'); print(firstFiveTimes)
#Ok this is going to be annoying because the times don't line up...

#Calculate first differences of time readings, display unique values
cat('\n','First differences of datetime','\n')
difTimesDO <- diff(UtahDOobs$dateTime); print(table(difTimesDO))
difTimesPAR <- diff(UtahPAR$dateTime); print(table(difTimesPAR))
difTimesWindSpeed <- diff(Utahwnd$dateTime); print(table(difTimesWindSpeed))
difTimesSensorTemp <- diff(Utahtemp$dateTime); print(table(difTimesSensorTemp))
timeStep=15	# in minutes


notDupRows <- findNotDupRows("UtahDOobs")
UtahDOobs <- UtahDOobs[notDupRows,]

notDupRows <- findNotDupRows("UtahPAR")
UtahPAR <- UtahPAR[notDupRows,]

notDupRows <- findNotDupRows("Utahwnd")
Utahwnd <- Utahwnd[notDupRows,]

notDupRows <- findNotDupRows("Utahtemp")
Utahtemp <- Utahtemp[notDupRows,]

###Remove any rows where dateTime is NA for some reason,
###otherwise, floorMins won't work 
UtahDOobs<-UtahDOobs %>%
  filter_at(vars(dateTime), any_vars(!is.na(.)))

UtahPAR<-UtahPAR %>%
  filter_at(vars(dateTime), any_vars(!is.na(.)))

Utahwnd<-Utahwnd %>%
  filter_at(vars(dateTime), any_vars(!is.na(.)))

Utahtemp<-Utahtemp %>%
  filter_at(vars(dateTime), any_vars(!is.na(.)))


#Make all data sets extend from startTime to endTime by timeStep
#Note that for some lakes it may be necessary to aggregate some variables to coarser time scale to get match up


UtahDOobs$dateTime <- floorMins(UtahDOobs)
UtahPAR$dateTime <- floorMins(UtahPAR)
Utahwnd$dateTime <- floorMins(Utahwnd)
Utahtemp$dateTime <- floorMins(Utahtemp)


# #Repeat check for dup rows in case any introduced during floorMins
notDupRows <- findNotDupRows("UtahDOobs")
UtahDOobs <- UtahDOobs[notDupRows,]
notDupRows <- findNotDupRows("UtahPAR")
UtahPAR <- UtahPAR[notDupRows,]
notDupRows <- findNotDupRows("Utahwnd")
Utahwnd <- Utahwnd[notDupRows,]
notDupRows <- findNotDupRows("Utahtemp")
Utahtemp <- Utahtemp[notDupRows,]

#Find the latest first time point and the earliest last time point of all the data
startTime <- max(min(UtahDOobs$dateTime),min(UtahPAR$dateTime),min(Utahwnd$dateTime),min(Utahtemp$dateTime))
endTime <- min(max(UtahDOobs$dateTime),max(UtahPAR$dateTime),max(Utahwnd$dateTime),max(Utahtemp$dateTime))

#Data.frame with one column "dateTime" which is sequence of times at time interval of timeStep, from startTime to endTime
completeTimes <- data.frame(dateTime=seq(startTime,endTime,paste(timeStep,"mins")))

#Merge all of input data.frames with completeTimes, so that they now all extend from startTime to endTime by timeStep
UtahDOobs <- merge(completeTimes,UtahDOobs,by="dateTime",all.x=T)
UtahPAR <- merge(completeTimes,UtahPAR,by="dateTime",all.x=T)
Utahwnd <- merge(completeTimes,Utahwnd,by="dateTime",all.x=T)
Utahtemp <- merge(completeTimes,Utahtemp,by="dateTime",all.x=T)

#Is it a problem that there are NAs in the PAR data? I guess we will find out
##IAO - 2021-02-12 --  I just checked and it shouldn't be because of the fillHoles helper function. 

Utah_full <- left_join(UtahDOobs, Utahwnd, by = c("dateTime"))
Utah_full <- left_join(Utah_full, Utahtemp, by = c("dateTime"))
Utah_full <- left_join(Utah_full, UtahPAR, by = c("dateTime"))

glimpse(Utah_full)
tz(Utah_full$dateTime)


# plot temps ----------------------------------------------------------
Utah_full %>%
  dplyr::select(dateTime, wtr_1.0)%>%
  pivot_longer(-(dateTime), names_sep = "_", names_to = c("variable","depth")) %>%
  dplyr::select(-variable)%>%
  mutate(depth=as.numeric(as.character(depth)),
         year=year(dateTime)) %>%
  rename(temp=value)%>%
  ggplot(aes(x=dateTime, y=temp, color=depth)) + 
  geom_point(size=2)+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%y-%m-%d") +
  ylab("Temp C")+
  ggtitle(" Utah 2018")+
  scale_color_continuous_sequential(palette = "Dark Mint", rev=TRUE)+
  facet_wrap(.~year, scales="free_x")


####plot DO###########


#Just one DO depth... 
ggplot(Utah_full, aes(x=dateTime, y=doObs)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%y-%m") +
  ylab("DO obs @ 0.75m")+
  ggtitle(" Utah")

####plot WIND###########
ggplot(Utah_full, aes(x=dateTime, y=wind)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%y-%m") +
  ylab("Wind (m/s)")+
  ggtitle(" Utah 2018")

####plot PAR###########
ggplot(Utah_full, aes(x=dateTime, y=PAR)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%y-%m") +
  ylab("PAR")+
  ggtitle("Utah 2018")


####export for modelling###########
UtahDOobs <- Utah_full %>%
  dplyr::select(dateTime, doObs) %>%
  rename(DO=doObs) 
write.csv(UtahDOobs, "data/metab_data_clean/utah/utah_2017_DO.txt", row.names=FALSE)

#DO sensor depth = 0.2
UtahsensorTemp <- Utah_full %>%
  dplyr::select(dateTime, wtr_1.0) %>%
  rename(sensorTemp=wtr_1.0) 
write.csv(UtahsensorTemp, "data/metab_data_clean/utah/utah_2017_sensorTemp.txt", row.names=FALSE)

#PAR
UtahPAR <- Utah_full %>%
  dplyr::select(dateTime, PAR) 
write.csv(UtahPAR, "data/metab_data_clean/utah/utah_2017_PAR.txt", row.names=FALSE)

#wind height = 10m from nearby airport
Utahwind <- Utah_full %>%
  dplyr::select(dateTime, wind) %>%
  rename(windSpeed=wind)
write.csv(Utahwind, "data/metab_data_clean/utah/utah_2017_windSpeed.txt", row.names=FALSE)

#temp profile
glimpse(Utah_full)
Utahtempprofile<- Utah_full %>%
  dplyr::select(dateTime, wtr_1.0) %>%
  rename(sensorTemp=wtr_1.0) 
write.csv(Utahtempprofile, "data/metab_data_clean/utah/utah_2017_tempProfile.txt", row.names=FALSE)





# > Zwart's lakes -----------------------------------------------------------

##Nabbed this code from his LakeMetabolismEstimation.R file. Need to reformat the data and export them to my "clean"
##folder, then will rerun the modified Solomon model for consistency.

#Manually change i
i=4
toRm=ls()
toRm=toRm[-which(toRm=='i')]
rm(list=toRm)
library(LakeMetabolizer)
source('~/Google Drive/Research (common)/Research/Data/R/catchment_metab_wg/R_code/metabolism_code/fillHoles.R')
source('~/Google Drive/Research (common)/Research/Data/R/catchment_metab_wg/R_code/metabolism_code/floorMins.R')
# library(xts) # useful for timeseries visulization
Sys.setenv(tz='GMT')

dir<-'~/Google Drive/Research (common)/Research/Data/R/catchment_metab_wg/data/metab_data/' # directory where data is stored
lakes<-list.files(dir)
lake<-lakes[i] # current lake for metabolism estimates
metaData<-read.csv('~/Google Drive/Research (common)/Research/Data/R/catchment_metab_wg/data/metadataLookUp.csv',stringsAsFactor=F)
alt<-metaData$Altitude..m.[metaData$Lake.Name==lake] #altitude in m
wnd.z<-metaData$Wind.Height..m.[metaData$Lake.Name==lake] # wind sensor height in m
lake.area<-metaData$Surface.Area..m2.[metaData$Lake.Name==lake] # lake area in m2
do.z<-metaData$DO.Sensor.Depth..m.[metaData$Lake.Name==lake] # DO sensor depth in m
lat<-metaData$Latitude..decimal.degrees.[metaData$Lake.Name==lake] # latitude in decimal degrees

# load time series metabolism data
doobs<-load.ts(file.path(dir,lake,paste(lake,'_doobs.txt',sep='')))
wtr<-load.ts(file.path(dir,lake,paste(lake,'_wtr.txt',sep='')))
wnd<-load.ts(file.path(dir,lake,paste(lake,'_wnd.txt',sep='')))
par<-load.ts(file.path(dir,lake,paste(lake,'_par.txt',sep='')))
if(ncol(doobs)>2){
  doobs<-doobs[,which(colnames(doobs)%in%tolower(c('datetime','doobs')))]
}
if(ncol(wnd)>2){
  wnd<-wnd[,which(colnames(wnd)%in%tolower(c('datetime','wnd')))]
}
timeStep<-as.numeric(diff(doobs$datetime)[1]) # time step difference for doobs

if(length(grep('_',colnames(wtr[,2:ncol(wtr)])))<(ncol(wtr)-1)){
  colnames(wtr)[2:ncol(wtr)]<-gsub('wtr','wtr_',colnames(wtr)[2:ncol(wtr)])
}

# can't have 0 meter temp depth; change to 0.1m if so
if(0%in%get.offsets(wtr)){
  colnames(wtr)[which(0==get.offsets(wtr))+1]<-'wtr_0.1'
}

doobs <- doobs %>%
  arrange(datetime)
wtr <- wtr %>%
  arrange(datetime)
wnd <- wnd %>%
  arrange(datetime)
par <- par %>%
  arrange(datetime)

# making time step all the same
#Make all data sets extend from startTime to endTime by timeStep
#Note that for some lakes it may be necessary to aggregate some variables to coarser time scale to get match up
# #Round all time down to nearest timeStep (e.g. if timeStep is 5, round 00:07 to 00:05)
if(lake=='Trout'|lake=='Feeagh'){
  timeStep=10 # forcing Trout and Feeagh data to 10 min timestep
  if(lake=='Trout'){
    wtr<-wtr[,!colnames(wtr)=='wtr_0.1'] #Trout shallow temp pendent is consistently lower than 1 meter pendant so it was removed
  }
}
if(lake=='lillsjoliden'){
  timeStep=10 # lillsjoliden par and wtr are 60 mins; linearly interpolating below
}

doobs$datetime <- floorMins(doobs)
par$datetime <- floorMins(par)
wnd$datetime <- floorMins(wnd)
wtr$datetime <- floorMins(wtr)

#Remove rows with duplicate datetime stamps (and warn)
notDupRows <- findNotDupRows("doobs")
doobs <- doobs[notDupRows,]
notDupRows <- findNotDupRows("par")
par <- par[notDupRows,]
notDupRows <- findNotDupRows("wnd")
wnd <- wnd[notDupRows,]
notDupRows <- findNotDupRows("wtr")
wtr <- wtr[notDupRows,]

doTemp<-data.frame(datetime=wtr$datetime,wtr=rep(NA,length(wtr$datetime))) # temperature at do sensor
colnames(doTemp)[2]<-paste('wtr_',do.z,sep='')

# linearly interpolate wind data to fill holes; max 60 min gap
wnd <- fillHoles(wnd,maxLength=60,timeStep=timeStep)




#Export data
####export for modelling###########
doobs <- doobs %>%
  rename(dateTime=datetime)%>%
  dplyr::select(dateTime, doobs) %>%
  rename(DO=doobs) %>%
  mutate(dateTime = force_tz(dateTime, tz="Europe/Tallinn"),
         dateTime= with_tz(dateTime, "GMT"))
write.csv(doobs, "data/metab_data_clean/zwart_metab_data/vortsjarv/vortsjarv_DO.txt", row.names=FALSE)

#DO sensor depth 

doTemp <- wtr %>%
  rename(dateTime=datetime) %>%
  select(dateTime, wtr_0.5) %>%
  mutate(dateTime = force_tz(dateTime, tz="Europe/Tallinn"),
         dateTime= with_tz(dateTime, "GMT"))
colnames(doTemp)[2]<-paste('sensorTemp')
# doTemp <- doTemp %>%
#   rename(dateTime=datetime)
write.csv(doTemp, "data/metab_data_clean/zwart_metab_data/vortsjarv/vortsjarv_sensorTemp.txt", row.names=FALSE)

#PAR
par <- par %>%
  rename(dateTime=datetime)%>%
  rename(PAR=par)%>%
  dplyr::select(dateTime, PAR) %>%
  mutate(dateTime = force_tz(dateTime, tz="Europe/Tallinn"),
         dateTime= with_tz(dateTime, "GMT"))
write.csv(par, "data/metab_data_clean/zwart_metab_data/vortsjarv/vortsjarv_PAR.txt", row.names=FALSE)

#wind height = 2 
wnd <- wnd %>%
  rename(dateTime=datetime,
         windSpeed=wnd)%>%
  dplyr::select(dateTime, windSpeed) %>%
  mutate(dateTime = force_tz(dateTime, tz="Europe/Tallinn"),
         dateTime= with_tz(dateTime, "GMT"))
write.csv(wnd, "data/metab_data_clean/zwart_metab_data/vortsjarv/vortsjarv_windSpeed.txt", row.names=FALSE)


#temp profile
wtr<- wtr %>%
  rename(dateTime=datetime)%>%
  mutate(dateTime = force_tz(dateTime, tz="America/Chicago"),
         dateTime= with_tz(dateTime, "GMT"))
write.csv(wtr, "data/metab_data_clean/zwart_metab_data/vortsjarv/vortsjarv_tempProfile.txt", row.names=FALSE)

head(doobs)
head(doTemp)
head(par)
head(wnd)
head(wtr)


# Jordan.wtr = load.ts("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Jordan/JordanPond_temp.txt")
# Jordan.wtr = load.ts("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Jordan/dataTempProfile.txt")
# Jordan.wtr<- Jordan.wtr %>%
#   # dplyr::select(-localtz,-daylightsavings) %>%
#   rename_all(function(x) gsub("wtr", "wtr_", x)) #Add an underscore to all heads to read wtr_#.#

### 2021-04-16 Checking on Feeagh wtr profiles because the estimated zMix from the model
### is 30m which doesn't seem right. What does lakeAnalyzer/Metabolizer say? 
library(LakeMetabolizer)
str(wtr)
feeagh_wtr<-wtr %>%
  filter(datetime > "2013-05-01" & datetime < "2013-10-01")
wtr.heat.map(feeagh_wtr, plot.title="Feeagh Water Temp (C)")

#calculate and plot the metalimnion depths
m.d = ts.meta.depths(feeagh_wtr)

plot(m.d$datetime, m.d$top, type='l', ylab='Meta Depths (m)', xlab='Date', col='blue')
lines(m.d$datetime, m.d$bottom, col='red')

#calculate and plot the thermocline depth
t.d = ts.thermo.depth(feeagh_wtr)
plot(t.d$datetime, t.d$thermo.depth, type='l', ylab='Thermocline Depth (m)', xlab='Date')
str(t.d)
mean(t.d$thermo.depth, na.rm=TRUE)
median(t.d$thermo.depth, na.rm=TRUE)
FeeaghzMix<-t.d %>%
  mutate(solarDay=date(datetime))%>%
  rename(zMix=thermo.depth) %>%
  group_by(solarDay) %>%
  summarize(zMix=mean(zMix, na.rm=TRUE))
write.table(FeeaghzMix, "results/model_output_raw/Feeagh/FeeaghzMix.txt", row.names=FALSE)


# > Rotorua - mixed layer depth only --------------------------------------

### 2021-04-30 Checking on Feeagh wtr profiles because the estimated zMix from the model
### is 30m which doesn't seem right. What does lakeAnalyzer/Metabolizer say? 
library(LakeMetabolizer)
Rotorua_temp <- read.delim(here("data/metab_data_raw/Rotorua_Solomon/Rotorua_2007_tempProfile.txt"))
Rotorua_wtr<-Rotorua_temp %>%
  mutate(dateTime=ymd_hm(dateTime)) %>%
  filter(dateTime > "2007-11-01" & dateTime < "2008-04-01") %>%
  rename_all(function(x) gsub("temp", "wtr_", x)) #Add an underscore to all heads to read wtr_#.#
wtr.heat.map(Rotorua_wtr, plot.title="Rotorua Water Temp (C)")

#calculate and plot the metalimnion depths
m.d = ts.meta.depths(Rotorua_wtr)

plot(m.d$datetime, m.d$top, type='l', ylab='Meta Depths (m)', xlab='Date', col='blue')
lines(m.d$datetime, m.d$bottom, col='red')

#calculate and plot the thermocline depth
t.d = ts.thermo.depth(Rotorua_wtr)
plot(t.d$datetime, t.d$thermo.depth, type='l', ylab='Thermocline Depth (m)', xlab='Date')
str(t.d)
mean(t.d$thermo.depth, na.rm=TRUE)
median(t.d$thermo.depth, na.rm=TRUE)
RotoruazMix<-t.d %>%
  mutate(solarDay=date(datetime))%>%
  rename(zMix=thermo.depth) %>%
  group_by(solarDay) %>%
  summarize(zMix=mean(zMix, na.rm=TRUE))
write.table(RotoruazMix, "results/model_output_raw/Rotorua/RotoruazMix.txt", row.names=FALSE)



# > Kentucky - mixed layer depth only --------------------------------------

### 2021-04-30 Checking on Feeagh wtr profiles because the estimated zMix from the model
### is 30m which doesn't seem right. What does lakeAnalyzer/Metabolizer say? 
library(LakeMetabolizer)
Kentucky_temp <- read.delim(here("data/metab_data_raw/Kentucky_Solomon/Kentucky_2008_tempProfile.txt"))
Kentucky_wtr<-Kentucky_temp %>%
  mutate(dateTime=ymd_hm(dateTime)) %>%
  filter(dateTime > "2008-05-01" & dateTime < "2008-09-30") %>%
  rename_all(function(x) gsub("temp", "wtr_", x)) #Add an underscore to all heads to read wtr_#.#
wtr.heat.map(Kentucky_wtr, plot.title="Kentucky Water Temp (C)")
View(Kentucky_wtr)
#calculate and plot the metalimnion depths
m.d = ts.meta.depths(Kentucky_wtr)

plot(m.d$datetime, m.d$top, type='l', ylab='Meta Depths (m)', xlab='Date', col='blue')
lines(m.d$datetime, m.d$bottom, col='red')

#calculate and plot the thermocline depth
t.d = ts.thermo.depth(Kentucky_wtr)
plot(t.d$datetime, t.d$thermo.depth, type='l', ylab='Thermocline Depth (m)', xlab='Date')
str(t.d)
mean(t.d$thermo.depth, na.rm=TRUE)
median(t.d$thermo.depth, na.rm=TRUE)
KentuckyzMix<-t.d %>%
  mutate(solarDay=date(datetime))%>%
  rename(zMix=thermo.depth) %>%
  group_by(solarDay) %>%
  summarize(zMix=mean(zMix, na.rm=TRUE))
write.table(KentuckyzMix, "results/model_output_raw/Kentucky/KentuckyzMix.txt", row.names=FALSE)



# > Hampenso - mixed layer depth only --------------------------------------

### 2021-04-30 Checking on Feeagh wtr profiles because the estimated zMix from the model
### is 30m which doesn't seem right. What does lakeAnalyzer/Metabolizer say? 
library(LakeMetabolizer)
Hampenso_temp <- read.delim(here("data/metab_data_raw/Hampenso_Solomon/Hampenso_2007_tempProfile.txt"))
Hampenso_wtr<-Hampenso_temp %>%
  mutate(dateTime=ymd_hm(dateTime)) %>%
  filter(dateTime > "2007-05-01" & dateTime < "2007-09-30") %>%
  rename_all(function(x) gsub("temp", "wtr_", x)) #Add an underscore to all heads to read wtr_#.#
wtr.heat.map(Hampenso_wtr, plot.title="Hampenso Water Temp (C)")
View(Hampenso_wtr)
#calculate and plot the metalimnion depths
m.d = ts.meta.depths(Hampenso_wtr)

plot(m.d$datetime, m.d$top, type='l', ylab='Meta Depths (m)', xlab='Date', col='blue')
lines(m.d$datetime, m.d$bottom, col='red')

#calculate and plot the thermocline depth
t.d = ts.thermo.depth(Hampenso_wtr)
plot(t.d$datetime, t.d$thermo.depth, type='l', ylab='Thermocline Depth (m)', xlab='Date')
str(t.d)
mean(t.d$thermo.depth, na.rm=TRUE)
median(t.d$thermo.depth, na.rm=TRUE)
HampensozMix<-t.d %>%
  mutate(solarDay=date(datetime))%>%
  rename(zMix=thermo.depth) %>%
  group_by(solarDay) %>%
  summarize(zMix=mean(zMix, na.rm=TRUE))
# write.table(HampensozMix, "results/model_output_raw/Hampenso/HampensozMix.txt", row.names=FALSE)



# > Sunapeee - mixed layer depth only --------------------------------------

### 2021-04-30 Checking on Sunapee wtr profiles because the estimated zMix from the model
### is 30m which doesn't seem right. What does lakeAnalyzer/Metabolizer say? 
library(LakeMetabolizer)
Sunapee_temp <- read.delim(here("data/metab_data_raw/Solomon 2013 study/Core data - QAd FINAL/Sunapee/Sunapee_2008_tempProfile.txt"))
Sunapee_wtr<-Sunapee_temp %>%
  mutate(datetime=ymd_hm(dateTime)) %>%
  filter(datetime > "2008-05-01" & datetime < "2008-10-01") %>%
  rename_all(function(x) gsub("temp", "wtr_", x)) %>% #Add an underscore to all heads to read wtr_#.#
  select(-dateTime, -wtr_0.0, -wtr_0.5, -wtr_13.0)

wtr.heat.map(Sunapee_wtr, plot.title="Sunapee Water Temp (C)")
View(Sunapee_wtr)

#calculate and plot the metalimnion depths
m.d = ts.meta.depths(Sunapee_wtr)

plot(m.d$datetime, m.d$top, type='l', ylab='Meta Depths (m)', xlab='Date', col='blue')
lines(m.d$datetime, m.d$bottom, col='red')

#calculate and plot the thermocline depth
t.d = ts.thermo.depth(Sunapee_wtr)
plot(t.d$datetime, t.d$thermo.depth, type='l', ylab='Thermocline Depth (m)', xlab='Date')
str(t.d)
mean(t.d$thermo.depth, na.rm=TRUE)
median(t.d$thermo.depth, na.rm=TRUE)
t.d$thermo.depth[is.nan(t.d$thermo.depth)]<-NA
t.d

SunapeezMix<- t.d %>%
  mutate(solarDay=date(datetime))%>%
  rename(zMix=thermo.depth) %>%
  group_by(solarDay) %>%
  summarize(zMix=mean(zMix, na.rm=TRUE))

SunapeezMix$zMix[is.nan(SunapeezMix$zMix)]<-NA
SunapeezMix

SunapeezMix %>%
  # filter(solarDay > "2018-05-01" & solarDay < "2018-10-01") %>%
  # mutate(if_nan(NA))%>%
  summarize(meanzMix=mean(zMix, na.rm=TRUE),
            medianzMix=median(zMix, na.rm=TRUE))
write.table(SunapeezMix, "results/model_output_raw/Sunapee/SunapeezMix.txt", row.names=FALSE)





# >  UNDERC lakes ------------------------------------------------------------

#exclude EL, MO, CR which are already included in Zwart's dataset

#Run for UNDERC sites
#Source database script
dbdir=file.path("/Users/solomonlab/Google Drive/Research (common)/Research/Data/R/MFE")
db="MFEdb_20210112.db"  
sensordb="MFEsensordb.db" 
source("/Users/solomonlab/Google Drive/Research (common)/Research/Data/R/MFE/000_dbUtil.R")
source("/Users/solomonlab/Google Drive/Research (common)/Research/Data/R/MFE/000_sensordbTable.R")

lakeID<-c( "WL","HB", "BO", "BR", "CB", "WA")
# lakeID <-"NG"
minDate<-"2017-05-30"
maxDate<-"2017-09-15"

#Pull DO data
# rawDOysi<-sensordbTable("YSI_CORR",lakeID="WL",minDate=minDate,maxDate=maxDate)
# dataDO_WL<-rawDOysi[,c("dateTime","lakeID","cleanedDO_mg_L")]
# colnames(dataDO_WL)<-c("dateTime","lakeID","DO")

rawDO<-sensordbTable("DO_CORR",lakeID=lakeID,minDate=minDate,maxDate=maxDate)
dataDO<-rawDO[,c("dateTime","lakeID","cleanedDO_mg_L")]
colnames(dataDO)<-c("dateTime","lakeID","DO")
# dataDO<-bind_rows(dataDO, dataDO_WL)


#Pull sensorTemp data
# dataSensorTemp_WL<-rawDOysi[,c("dateTime","lakeID","cleanedTemp_C")]
# colnames(dataSensorTemp_WL)<-c("dateTime","lakeID","sensorTemp")

dataSensorTemp<-rawDO[,c("dateTime","lakeID","cleanedTemp_C")]
colnames(dataSensorTemp)<-c("dateTime","lakeID","sensorTemp")
# dataSensorTemp<-bind_rows(dataSensorTemp, dataSensorTemp_WL)

#Temp profile
tempChain<-sensordbTable("HOBO_TCHAIN_CORR",lakeID=lakeID,minDate=minDate,maxDate=maxDate)
tempChain2<-tempChain[,c("dateTime","lakeID","depth_m","cleanedTemp_C")]
colnames(tempChain2)=c('dateTime','lakeID','depth_m','temp')
dataTempProfile<- tempChain2 %>%
  pivot_wider(values_from = "temp", values_fn=mean,names_from="depth_m", names_prefix="wtr_")
head(dataTempProfile)
dataTempProfile<-dataTempProfile %>%
  relocate(wtr_1, .before = wtr_1.5) #rearranging/reordering in case this presents a problem in modeling
  # relocate(wtr_0.75:wtr_1.5, .before = wtr_2) %>% #rearranging in case this presents a problem in modeling
  # relocate(wtr_2.5, .before = wtr_3) %>% #rearranging in case this presents a problem in modeling
  # relocate(wtr_3.5, .before = wtr_4) %>% #rearranging in case this presents a problem in modeling
  # relocate(wtr_4.5, .before = wtr_5) %>% #rearranging in case this presents a problem in modeling
  # relocate(wtr_6, .before = wtr_7) #rearranging in case this presents a problem in modeling
# rm(tempChain)
# rm(tempChain2)

#Met data - pull from both weather stations and aggregate
metDataEL<-sensordbTable("HOBO_METSTATION_CORR",lakeID="EL",minDate=minDate,maxDate=maxDate)
metDataWL<-sensordbTable("HOBO_METSTATION_CORR",lakeID="WL",minDate=minDate,maxDate=maxDate)
metData=rbind(metDataEL,metDataWL)
rm(metDataEL)
rm(metDataWL)


#PAR
#made choice to average PAR readings from WL & EL/FE
dataPAR<-aggregate(x=as.numeric(metData$cleanedPAR_uE_m2_s),by=list(metData$dateTime),FUN=mean,na.rm=TRUE)
colnames(dataPAR)=c("dateTime","PAR")
#Wind speed
dataWind<-aggregate(x=as.numeric(metData$cleanedWindSpeed_m_s),by=list(metData$dateTime),FUN=mean,na.rm=TRUE)
colnames(dataWind)=c("dateTime","windSpeed")


dataDO <- dataDO %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Chicago"),
         dateTime = with_tz(dateTime, "GMT")) 
dataPAR <- dataPAR %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
       dateTime = force_tz(dateTime, tz="America/Chicago"),
       dateTime = with_tz(dateTime, "GMT")) 
dataWind <- dataWind %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Chicago"),
         dateTime = with_tz(dateTime, "GMT")) 
dataTempProfile <- dataTempProfile %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Chicago"),
         dateTime = with_tz(dateTime, "GMT")) 
dataSensorTemp <- dataSensorTemp %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Chicago"),
         dateTime = with_tz(dateTime, "GMT")) 

# export for modeling -----------------------------------------------------

# .... WL -----------------------------------------------------------------

#DO
doobs <- dataDO %>%
  filter(lakeID=="WL")%>%
  dplyr::select(dateTime, DO) 
write.csv(doobs, "data/metab_data_clean/mfe_data/westlong/westlong_DO.txt", row.names=FALSE)


#DO sensor depth 
#always 0.5m
doTemp <- dataSensorTemp %>%
  filter(lakeID=="WL")%>%
  dplyr::select(dateTime, sensorTemp) 
write.csv(doTemp, "data/metab_data_clean/mfe_data/westlong/westlong_sensorTemp.txt", row.names=FALSE)

#PAR- all sites use the aggregate of EL and WL
par <- dataPAR 
write.csv(par, "data/metab_data_clean/mfe_data/westlong/westlong_PAR.txt", row.names=FALSE)

#wind height = all sites use the aggregate of EL and WL
#wind height = 2m
wnd <- dataWind
write.csv(wnd, "data/metab_data_clean/mfe_data/westlong/westlong_windSpeed.txt", row.names=FALSE)

#temp profile
wtr<- dataTempProfile %>%
  filter(lakeID=="WL") %>%
  select(-lakeID) %>%
  select_if(all_na) #removes any columns where every row in an NA
write.csv(wtr, "data/metab_data_clean/mfe_data/westlong/westlong_tempProfile.txt", row.names=FALSE)


# .... HB -----------------------------------------------------------------

#DO
doobs <- dataDO %>%
  filter(lakeID=="HB")%>%
  dplyr::select(dateTime, DO) 
write.csv(doobs, "data/metab_data_clean/mfe_data/hummingbird/hummingbird_DO.txt", row.names=FALSE)


#DO sensor depth 
#always 0.5m
doTemp <- dataSensorTemp %>%
  filter(lakeID=="HB")%>%
  dplyr::select(dateTime, sensorTemp) 
write.csv(doTemp, "data/metab_data_clean/mfe_data/hummingbird/hummingbird_sensorTemp.txt", row.names=FALSE)

#PAR- all sites use the aggregate of EL and HB
par <- dataPAR 
write.csv(par, "data/metab_data_clean/mfe_data/hummingbird/hummingbird_PAR.txt", row.names=FALSE)

#wind height = all sites use the aggregate of EL and HB
#wind height = 2m
wnd <- dataWind
write.csv(wnd, "data/metab_data_clean/mfe_data/hummingbird/hummingbird_windSpeed.txt", row.names=FALSE)

#temp profile
wtr<- dataTempProfile %>%
  filter(lakeID=="HB") %>%
  select(-lakeID) %>%
  select_if(all_na) #removes any columns where every row in an NA
write.csv(wtr, "data/metab_data_clean/mfe_data/hummingbird/hummingbird_tempProfile.txt", row.names=FALSE)

# .... BO -----------------------------------------------------------------

#DO
doobs <- dataDO %>%
  filter(lakeID=="BO")%>%
  dplyr::select(dateTime, DO) 
write.csv(doobs, "data/metab_data_clean/mfe_data/bolger/bolger_DO.txt", row.names=FALSE)


#DO sensor depth 
#always 0.5m
doTemp <- dataSensorTemp %>%
  filter(lakeID=="BO")%>%
  dplyr::select(dateTime, sensorTemp) 
write.csv(doTemp, "data/metab_data_clean/mfe_data/bolger/bolger_sensorTemp.txt", row.names=FALSE)

#PAR- all sites use the aggregate of EL and BA
par <- dataPAR 
write.csv(par, "data/metab_data_clean/mfe_data/bolger/bolger_PAR.txt", row.names=FALSE)

#wind height = all sites use the aggregate of EL and BA
#wind height = 2m
wnd <- dataWind
write.csv(wnd, "data/metab_data_clean/mfe_data/bolger/bolger_windSpeed.txt", row.names=FALSE)

#temp profile
wtr<- dataTempProfile %>%
  filter(lakeID=="BO") %>%
  select(-lakeID) %>%
  select_if(all_na) #removes any columns where every row in an NA
write.csv(wtr, "data/metab_data_clean/mfe_data/bolger/bolger_tempProfile.txt", row.names=FALSE)

# .... BR -----------------------------------------------------------------

#DO
doobs <- dataDO %>%
  filter(lakeID=="BR")%>%
  dplyr::select(dateTime, DO) 
write.csv(doobs, "data/metab_data_clean/mfe_data/brown/brown_DO.txt", row.names=FALSE)


#DO sensor depth 
#always 0.5m
doTemp <- dataSensorTemp %>%
  filter(lakeID=="BR")%>%
  dplyr::select(dateTime, sensorTemp) 
write.csv(doTemp, "data/metab_data_clean/mfe_data/brown/brown_sensorTemp.txt", row.names=FALSE)

#PAR- all sites use the aggregate of EL and BA
par <- dataPAR 
write.csv(par, "data/metab_data_clean/mfe_data/brown/brown_PAR.txt", row.names=FALSE)

#wind height = all sites use the aggregate of EL and BA
#wind height = 2m
wnd <- dataWind
write.csv(wnd, "data/metab_data_clean/mfe_data/brown/brown_windSpeed.txt", row.names=FALSE)

#temp profile
wtr<- dataTempProfile %>%
  filter(lakeID=="BR") %>%
  select(-lakeID) %>%
  select_if(all_na) #removes any columns where every row in an NA
write.csv(wtr, "data/metab_data_clean/mfe_data/brown/brown_tempProfile.txt", row.names=FALSE)

# .... CB -----------------------------------------------------------------

#DO
doobs <- dataDO %>%
  filter(lakeID=="CB")%>%
  dplyr::select(dateTime, DO) 
write.csv(doobs, "data/metab_data_clean/mfe_data/cranberry/cranberry_DO.txt", row.names=FALSE)


#DO sensor depth 
#always 0.5m
doTemp <- dataSensorTemp %>%
  filter(lakeID=="CB")%>%
  dplyr::select(dateTime, sensorTemp) 
write.csv(doTemp, "data/metab_data_clean/mfe_data/cranberry/cranberry_sensorTemp.txt", row.names=FALSE)

#PAR- all sites use the aggregate of EL and BA
par <- dataPAR 
write.csv(par, "data/metab_data_clean/mfe_data/cranberry/cranberry_PAR.txt", row.names=FALSE)

#wind height = all sites use the aggregate of EL and BA
#wind height = 2m
wnd <- dataWind
write.csv(wnd, "data/metab_data_clean/mfe_data/cranberry/cranberry_windSpeed.txt", row.names=FALSE)

#temp profile
wtr<- dataTempProfile %>%
  filter(lakeID=="CB") %>%
  select(-lakeID) %>%
  select_if(all_na) #removes any columns where every row in an NA
write.csv(wtr, "data/metab_data_clean/mfe_data/cranberry/cranberry_tempProfile.txt", row.names=FALSE)


# A few lakes that don't have good 2013 data ------------------------------

dbdir=file.path("/Users/solomonlab/Google Drive/Research (common)/Research/Data/R/MFE")
db="MFEdb_20210112.db"  
sensordb="MFEsensordb.db" 
source("/Users/solomonlab/Google Drive/Research (common)/Research/Data/R/MFE/000_dbUtil.R")
source("/Users/solomonlab/Google Drive/Research (common)/Research/Data/R/MFE/000_sensordbTable.R")

lakeID<-c("BA","WA")
minDate<-"2016-05-30"
maxDate<-"2016-09-15"

#Pull DO data
rawDO<-sensordbTable("DO_CORR",lakeID=lakeID,minDate=minDate,maxDate=maxDate)
dataDO<-rawDO[,c("dateTime","lakeID","cleanedDO_mg_L")]
colnames(dataDO)<-c("dateTime","lakeID","DO")


#Pull sensorTemp data
dataSensorTemp<-rawDO[,c("dateTime","lakeID","cleanedTemp_C")]
colnames(dataSensorTemp)<-c("dateTime","lakeID","sensorTemp")

#Temp profile
tempChain<-sensordbTable("HOBO_TCHAIN_CORR",lakeID=lakeID,minDate=minDate,maxDate=maxDate)
tempChain2<-tempChain[,c("dateTime","lakeID","depth_m","cleanedTemp_C")]
colnames(tempChain2)=c('dateTime','lakeID','depth_m','temp')
dataTempProfile<- tempChain2 %>%
  pivot_wider(values_from = "temp", values_fn=mean,names_from="depth_m", names_prefix="wtr_")
head(dataTempProfile)
dataTempProfile<-dataTempProfile %>%
  relocate(wtr_0.5, wtr_1, .before = wtr_1.5)  #rearranging/reordering in case this presents a problem in modeling
  # relocate(wtr_2, .before = wtr_2.5)  #rearranging in case this presents a problem in modeling

# rm(tempChain)
# rm(tempChain2)

#Met data - pull from both weather stations and aggregate
metDataEL<-sensordbTable("HOBO_METSTATION_CORR",lakeID="EL",minDate=minDate,maxDate=maxDate)
metDataWL<-sensordbTable("HOBO_METSTATION_CORR",lakeID="WL",minDate=minDate,maxDate=maxDate)
metData=rbind(metDataEL,metDataWL)
rm(metDataEL)
rm(metDataWL)


#PAR
#made choice to average PAR readings from WL & EL/FE
dataPAR<-aggregate(x=as.numeric(metData$cleanedPAR_uE_m2_s),by=list(metData$dateTime),FUN=mean,na.rm=TRUE)
colnames(dataPAR)=c("dateTime","PAR")
#Wind speed
dataWind<-aggregate(x=as.numeric(metData$cleanedWindSpeed_m_s),by=list(metData$dateTime),FUN=mean,na.rm=TRUE)
colnames(dataWind)=c("dateTime","windSpeed")


dataDO <- dataDO %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Chicago"),
         dateTime = with_tz(dateTime, "GMT")) 
dataPAR <- dataPAR %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Chicago"),
         dateTime = with_tz(dateTime, "GMT")) 
dataWind <- dataWind %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Chicago"),
         dateTime = with_tz(dateTime, "GMT")) 
dataTempProfile <- dataTempProfile %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Chicago"),
         dateTime = with_tz(dateTime, "GMT")) 
dataSensorTemp <- dataSensorTemp %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Chicago"),
         dateTime = with_tz(dateTime, "GMT")) 




# .... BA -----------------------------------------------------------------

#DO
doobs <- dataDO %>%
  filter(lakeID=="BA")%>%
  dplyr::select(dateTime, DO) 
write.csv(doobs, "data/metab_data_clean/mfe_data/bay/bay_DO.txt", row.names=FALSE)


#DO sensor depth 
#always 0.5m
doTemp <- dataSensorTemp %>%
  filter(lakeID=="BA")%>%
  dplyr::select(dateTime, sensorTemp) 
write.csv(doTemp, "data/metab_data_clean/mfe_data/bay/bay_sensorTemp.txt", row.names=FALSE)

#PAR- all sites use the aggregate of EL and BA
par <- dataPAR 
write.csv(par, "data/metab_data_clean/mfe_data/bay/bay_PAR.txt", row.names=FALSE)

#wind height = all sites use the aggregate of EL and BA
#wind height = 2m
wnd <- dataWind
write.csv(wnd, "data/metab_data_clean/mfe_data/bay/bay_windSpeed.txt", row.names=FALSE)

#temp profile
wtr<- dataTempProfile %>%
  filter(lakeID=="BA") %>%
  select(-lakeID) %>%
  select_if(all_na) #removes any columns where every row in an NA
write.csv(wtr, "data/metab_data_clean/mfe_data/bay/bay_tempProfile.txt", row.names=FALSE)


# .... NG -----------------------------------------------------------------
# 
# #DO
# doobs <- dataDO %>%
#   filter(lakeID=="NG")%>%
#   dplyr::select(dateTime, DO) 
# write.csv(doobs, "data/metab_data_clean/mfe_data/northgate/northgate_DO.txt", row.names=FALSE)
# 
# 
# #DO sensor depth 
# #always 0.5m
# doTemp <- dataSensorTemp %>%
#   filter(lakeID=="NG")%>%
#   dplyr::select(dateTime, sensorTemp) 
# write.csv(doTemp, "data/metab_data_clean/mfe_data/northgate/northgate_sensorTemp.txt", row.names=FALSE)
# 
# #PAR- all sites use the aggregate of EL and BA
# par <- dataPAR 
# write.csv(par, "data/metab_data_clean/mfe_data/northgate/northgate_PAR.txt", row.names=FALSE)
# 
# #wind height = all sites use the aggregate of EL and BA
# #wind height = 2m
# wnd <- dataWind
# write.csv(wnd, "data/metab_data_clean/mfe_data/northgate/northgate_windSpeed.txt", row.names=FALSE)
# 
# #temp profile
# wtr<- dataTempProfile %>%
#   filter(lakeID=="NG") %>%
#   select(-lakeID) %>%
#   select_if(all_na) #removes any columns where every row in an NA
# write.csv(wtr, "data/metab_data_clean/mfe_data/northgate/northgate_tempProfile.txt", row.names=FALSE)
# 

# .... WA -----------------------------------------------------------------

#DO
doobs <- dataDO %>%
  filter(lakeID=="WA")%>%
  dplyr::select(dateTime, DO) 
write.csv(doobs, "data/metab_data_clean/mfe_data/ward/ward_DO.txt", row.names=FALSE)


#DO sensor depth 
#always 0.5m
doTemp <- dataSensorTemp %>%
  filter(lakeID=="WA")%>%
  dplyr::select(dateTime, sensorTemp) 
write.csv(doTemp, "data/metab_data_clean/mfe_data/ward/ward_sensorTemp.txt", row.names=FALSE)

#PAR- all sites use the aggregate of EL and BA
par <- dataPAR 
write.csv(par, "data/metab_data_clean/mfe_data/ward/ward_PAR.txt", row.names=FALSE)

#wind height = all sites use the aggregate of EL and BA
#wind height = 2m
wnd <- dataWind
write.csv(wnd, "data/metab_data_clean/mfe_data/ward/ward_windSpeed.txt", row.names=FALSE)

#temp profile
wtr<- dataTempProfile %>%
  filter(lakeID=="WA") %>%
  select(-lakeID) %>%
  select_if(all_na) #removes any columns where every row in an NA
write.csv(wtr, "data/metab_data_clean/mfe_data/ward/ward_tempProfile.txt", row.names=FALSE)



# > Gollinsee -------------------------------------------------------------
closeAllConnections()

GollinseeDOobs <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Gollinsee/Gollinsee_doobs.csv", stringsAsFactors = FALSE) %>%
  rename(daylightSavings=Daylight.Savings,
         localTZ=local.TZ) %>%
  select(-daylightSavings)
Gollinseewnd <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Gollinsee/Gollinsee_wnd.csv", stringsAsFactors = FALSE) %>%
  rename(daylightSavings=daylight.savings,
         localTZ=local.TZ,
         windSpeed=wind.speed) %>%
  select(-daylightSavings)
Gollinseetemp <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Gollinsee/Gollinsee_wtr.csv", stringsAsFactors = FALSE) %>%
  rename(localTZ=local.TZ,
         wtr_1.4=Wtr1.4) %>%
  select(-daylightSavings, -X, -X.1)
GollinseePAR <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Gollinsee/gollin_globalRadiation.csv", stringsAsFactors = FALSE)%>%
  rename(localTZ=local.TZ) %>%
  select(-daylightSavings) %>%
  mutate(PAR=energy_Wm2*2.15) #See "Estimating PAR from solar flux data" Word Doc in Troubleshooting folder
GollinseeH2OPAR <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Gollinsee/Gollinsee_par.csv", stringsAsFactors = FALSE)%>%
  rename(localTZ=local.TZ) %>%
  select(-Daylight.Savings, -(PAR_0.2:PAR_3.0))
##UNITS = E/m2h

tz(GollinseePAR$dateTime)

####DATE CONVERSION###
glimpse(Gollinseetemp)
Gollinseetemp <- Gollinseetemp %>%
  mutate(dateTime = mdy_hm(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="Europe/Berlin"),
         dateTime = with_tz(dateTime, "GMT")) %>%
  select(-localTZ, -Lake)
glimpse(Gollinseetemp)

glimpse(GollinseeDOobs)
GollinseeDOobs <- GollinseeDOobs %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="Europe/Berlin"),
         dateTime = with_tz(dateTime, "GMT")) %>%
  select(-localTZ, -Lake) %>%
  rename(DO=doObs)
glimpse(GollinseeDOobs)

glimpse(Gollinseewnd)
Gollinseewnd <- Gollinseewnd %>%
  mutate(dateTime = mdy_hm(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="Europe/Berlin"),
         dateTime = with_tz(dateTime, "GMT")) %>%
  select(-localTZ, -Lake)
glimpse(Gollinseewnd)

glimpse(GollinseePAR)
GollinseePAR <- GollinseePAR %>%
  mutate(dateTime = mdy_hm(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="Europe/Berlin"),
         dateTime = with_tz(dateTime, "GMT")) %>%
  select(-localTZ, -Lake, -energy_Wm2)
glimpse(GollinseePAR)

glimpse(GollinseeH2OPAR)
GollinseeH2OPAR <- GollinseeH2OPAR %>%
  mutate(dateTime = mdy_hm(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="Europe/Berlin"),
         dateTime = with_tz(dateTime, "GMT")) %>%
  select(-localTZ, -Lake)
glimpse(GollinseeH2OPAR)

#Print first five time readings for each variable
firstFiveTimes <- data.frame(DO=GollinseeDOobs$dateTime[1:5],
                             PAR=GollinseePAR$dateTime[1:5],
                             wind=Gollinseewnd$dateTime[1:5],
                             temp=Gollinseetemp$dateTime[1:5])
print('First five time readings for each variable'); print(firstFiveTimes)
#Ok this is going to be annoying because the times don't line up...

#Calculate first differences of time readings, display unique values
cat('\n','First differences of datetime','\n')
difTimesDO <- diff(GollinseeDOobs$dateTime); print(table(difTimesDO))
difTimesPAR <- diff(GollinseePAR$dateTime); print(table(difTimesPAR))
difTimesWindSpeed <- diff(Gollinseewnd$dateTime); print(table(difTimesWindSpeed))
difTimesSensorTemp <- diff(Gollinseetemp$dateTime); print(table(difTimesSensorTemp))
timeStep=10	# in minutes


notDupRows <- findNotDupRows("GollinseeDOobs")
GollinseeDOobs <- GollinseeDOobs[notDupRows,]

notDupRows <- findNotDupRows("GollinseePAR")
GollinseePAR <- GollinseePAR[notDupRows,]

notDupRows <- findNotDupRows("Gollinseewnd")
Gollinseewnd <- Gollinseewnd[notDupRows,]

notDupRows <- findNotDupRows("Gollinseetemp")
Gollinseetemp <- Gollinseetemp[notDupRows,]

###Remove any rows where dateTime is NA for some reason,
###otherwise, floorMins won't work 
GollinseeDOobs<-GollinseeDOobs %>%
  filter_at(vars(dateTime), any_vars(!is.na(.)))

GollinseePAR<-GollinseePAR %>%
  filter_at(vars(dateTime), any_vars(!is.na(.)))

Gollinseewnd<-Gollinseewnd %>%
  filter_at(vars(dateTime), any_vars(!is.na(.)))

Gollinseetemp<-Gollinseetemp %>%
  filter_at(vars(dateTime), any_vars(!is.na(.)))


#Make all data sets extend from startTime to endTime by timeStep
#Note that for some lakes it may be necessary to aggregate some variables to coarser time scale to get match up


GollinseeDOobs$dateTime <- floorMins(GollinseeDOobs)
GollinseePAR$dateTime <- floorMins(GollinseePAR)
Gollinseewnd$dateTime <- floorMins(Gollinseewnd)
Gollinseetemp$dateTime <- floorMins(Gollinseetemp)


# #Repeat check for dup rows in case any introduced during floorMins
notDupRows <- findNotDupRows("GollinseeDOobs")
GollinseeDOobs <- GollinseeDOobs[notDupRows,]
notDupRows <- findNotDupRows("GollinseePAR")
GollinseePAR <- GollinseePAR[notDupRows,]
notDupRows <- findNotDupRows("Gollinseewnd")
Gollinseewnd <- Gollinseewnd[notDupRows,]
notDupRows <- findNotDupRows("Gollinseetemp")
Gollinseetemp <- Gollinseetemp[notDupRows,]

#Find the latest first time point and the earliest last time point of all the data
startTime <- max(min(GollinseeDOobs$dateTime),min(GollinseePAR$dateTime),min(Gollinseewnd$dateTime),min(Gollinseetemp$dateTime))
endTime <- min(max(GollinseeDOobs$dateTime),max(GollinseePAR$dateTime),max(Gollinseewnd$dateTime),max(Gollinseetemp$dateTime))

#Data.frame with one column "dateTime" which is sequence of times at time interval of timeStep, from startTime to endTime
completeTimes <- data.frame(dateTime=seq(startTime,endTime,paste(timeStep,"mins")))

#Merge all of input data.frames with completeTimes, so that they now all extend from startTime to endTime by timeStep
GollinseeDOobs <- merge(completeTimes,GollinseeDOobs,by="dateTime",all.x=T)
GollinseePAR <- merge(completeTimes,GollinseePAR,by="dateTime",all.x=T)
Gollinseewnd <- merge(completeTimes,Gollinseewnd,by="dateTime",all.x=T)
Gollinseetemp <- merge(completeTimes,Gollinseetemp,by="dateTime",all.x=T)

#Is it a problem that there are NAs in the PAR data? I guess we will find out
##IAO - 2021-02-12 --  I just checked and it shouldn't be because of the fillHoles helper function. 

Gollinsee_full <- left_join(GollinseeDOobs, Gollinseewnd, by = c("dateTime"))
Gollinsee_full <- left_join(Gollinsee_full, Gollinseetemp, by = c("dateTime"))
Gollinsee_full <- left_join(Gollinsee_full, GollinseePAR, by = c("dateTime"))

glimpse(Gollinsee_full)

#For some reason (maybe can de-bug later) there are lots of duplicate rows.
#Use distinct function to pull out only distinct dateTimes.
Gollinsee_full<-Gollinsee_full %>%
  distinct(dateTime, .keep_all = TRUE)


tz(Gollinsee_full$dateTime)

####plot DO###########
Gollinsee_full %>%
  mutate(year=year(dateTime)) %>%
  ggplot(aes(x=dateTime, y=DO)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%m-%d") +
  ylab("DO obs")+
  facet_wrap(.~year, scales="free")+
  ggtitle("Gollinsee Lake 2010")


####plot WIND###########
Gollinsee_full %>%
  mutate(year=year(dateTime)) %>%
  ggplot(aes(x=dateTime, y=windSpeed)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "2 months", date_labels = "%m-%d") +
  ylab("Wind (m/s)")+
  facet_wrap(.~year, scales="free")+
  ggtitle("Gollinsee Lake 10")


####plot TEMPS###########
Gollinsee_full %>%
  dplyr::select(dateTime, wtr_1.4)%>%
  pivot_longer(-(dateTime), names_sep = "_", names_to = c("variable","depth")) %>%
  dplyr::select(-variable)%>%
  mutate(year=year(dateTime)) %>%
  mutate(depth=as.numeric(as.character(depth))) %>%
  rename(temp=value)%>%
  ggplot(aes(x=dateTime, y=temp, color=depth)) + 
  geom_point(size=2)+
  scale_x_datetime(date_breaks = "2 months", date_labels = "%m-%d") +
  scale_color_continuous_sequential(palette = "Dark Mint", rev=TRUE)+
  ylab("water temperature")+
  facet_wrap(.~year, scales="free")+
  ggtitle("Gollinsee Lake 2010")

####plot PAR###########
Gollinsee_full %>%
  mutate(year=year(dateTime)) %>%
  ggplot(aes(x=dateTime, y=PAR)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "2 months", date_labels = "%m-%d") +
  ylab("PAR")+
  facet_wrap(.~year, scales="free")+
  ggtitle("Gollinsee Lake 2010")


Gollinsee_full %>%
  filter(dateTime >="2010-05-08" & dateTime <="2010-05-10") %>%
  mutate(date=date(dateTime))%>%
  ggplot(aes(x=dateTime, y=PAR, color=factor(date)))+
  geom_point()+
  ggtitle("Gollinsee")+
  scale_x_datetime(date_breaks = "4 hours",
                   date_labels = "%m-%d\n%H:%M")+
  theme(panel.grid.minor = element_line(color="lightgrey"),
        axis.text.x= element_text(size=8))

#2019

#Compare surface PAR with underwater PAR...

Gollinsee_allPAR<-right_join(GollinseePAR, GollinseeH2OPAR, by="dateTime")
Gollinsee_allPAR %>%
  rename(PAR_surface=PAR)%>%
  mutate(PAR_surface_z=scale(PAR_surface),
         PAR_0.1_z=scale(PAR_0.1)) %>%
  pivot_longer(-(dateTime)) %>%
  filter(name %in% c("PAR_surface_z","PAR_0.1_z"))%>%
  filter(dateTime > "2010-07-01" & dateTime < "2010-07-08")%>%
  ggplot(aes(x=dateTime, y=value, fill=name))+
  geom_point(size=2, shape=21)+
  scale_x_datetime(date_breaks = "2 months", date_labels = "%m-%d") 

#Are they correlated? 
Gollinsee_allPAR %>%
  rename(PAR_surface=PAR)%>%
  # filter(dateTime > "2010-07-01" & dateTime < "2010-07-08")%>%
  ggplot(aes(x=PAR_surface, y=PAR_0.1))+
  geom_point(size=2, shape=21)+
  geom_smooth(method="lm")+
  ggtitle("07-01 to 07-08")

Gollinsee_allPAR %>%
  rename(PAR_surface=PAR)%>%
  filter(dateTime > "2010-07-01" & dateTime < "2010-07-08")%>%
  ggplot(aes(x=PAR_surface, y=PAR_0.1))+
  geom_point(size=2, shape=21, fill="#666666")+
  geom_smooth(method="lm")+
  ggtitle("07-01 to 07-08")

#Filter out low surface PAR measurements? 
Gollinsee_allPAR %>%
  rename(PAR_surface=PAR)%>%
  filter(PAR_surface < 500)%>%
  ggplot(aes(x=PAR_surface, y=PAR_0.1))+
  geom_point(size=2, shape=21)+
  geom_smooth(method="lm")
#No


# export for modeling -----------------------------------------------------


GollinseeDOobs <- Gollinsee_full %>%
  dplyr::select(dateTime, DO)
write.csv(GollinseeDOobs, "data/metab_data_clean/gollinsee/gollinsee_DO.txt", row.names=FALSE)

#DO sensor depth = 0.86m but there is no sensor there. Using the next closest. 
#I'm afraid the 0.05m will have too high of temperature fluctuations. 
GollinseesensorTemp <- Gollinsee_full %>%
  dplyr::select(dateTime, wtr_1.4) %>%
  rename(sensorTemp=wtr_1.4) 
write.csv(GollinseesensorTemp, "data/metab_data_clean/gollinsee/gollinsee_sensorTemp.txt", row.names=FALSE)

#PAR
GollinseePAR <- Gollinsee_full %>%
  dplyr::select(dateTime, PAR) 
write.csv(GollinseePAR, "data/metab_data_clean/gollinsee/gollinsee_PAR.txt", row.names=FALSE)

#wind height = 2.85 m above lake surface
Gollinseewind <- Gollinsee_full %>%
  dplyr::select(dateTime, windSpeed)
write.csv(Gollinseewind, "data/metab_data_clean/gollinsee/gollinsee_windSpeed.txt", row.names=FALSE)

#temp profile
Gollinseetempprofile<- Gollinsee_full %>%
  dplyr::select(dateTime, wtr_1.4)
write.csv(Gollinseetempprofile, "data/metab_data_clean/gollinsee/gollinsee_tempProfile.txt", row.names=FALSE)




# > Schulzensee -------------------------------------------------------------

SchulzenseeDOobs <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Schulzensee/Schulzensee_doobs.csv", stringsAsFactors = FALSE) %>%
  rename(daylightSavings=Daylight.Savings,
         localTZ=local.TZ) %>%
  select(-daylightSavings)
Schulzenseewnd <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Schulzensee/Schulzensee_wnd.csv", stringsAsFactors = FALSE) %>%
  rename(daylightSavings=daylight.savings,
         localTZ=local.TZ) %>%
  select(-daylightSavings)
Schulzenseetemp <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Schulzensee/Schulzensee_wtr.csv", stringsAsFactors = FALSE) %>%
  rename(localTZ=local.TZ,
         wtr_1.4=Wtr1.4) %>%
  select(-daylightSavings)
SchulzenseePAR <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Schulzensee/Schulzensee_global_radiation.csv", stringsAsFactors = FALSE)%>%
  rename(localTZ=local.TZ) %>%
  select(-daylightSavings) 

####DATE CONVERSION###
glimpse(Schulzenseetemp)
Schulzenseetemp <- Schulzenseetemp %>%
  mutate(dateTime = mdy_hm(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="Europe/Berlin"),
         dateTime = with_tz(dateTime, "GMT")) %>%
  select(-localTZ, -Lake)
glimpse(Schulzenseetemp)

glimpse(SchulzenseeDOobs)
SchulzenseeDOobs <- SchulzenseeDOobs %>%
  rename(dateTime=datetime)%>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="Europe/Berlin"),
         dateTime = with_tz(dateTime, "GMT")) %>%
  select(-localTZ, -Lake) %>%
  rename(DO=doObs)
glimpse(SchulzenseeDOobs)

glimpse(Schulzenseewnd)
Schulzenseewnd <- Schulzenseewnd %>%
  rename(dateTime=X.TIME.)%>%
  mutate(dateTime = mdy_hm(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="Europe/Berlin"),
         dateTime = with_tz(dateTime, "GMT")) %>%
  select(-localTZ, -Lake)
glimpse(Schulzenseewnd)

glimpse(SchulzenseePAR)
#Convert wind from character to number
SchulzenseePAR$energy_Wm2 <- as.numeric(SchulzenseePAR$energy_Wm2)
SchulzenseePAR <- SchulzenseePAR %>%
  mutate(dateTime = mdy_hm(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="Europe/Berlin"),
         dateTime = with_tz(dateTime, "GMT"),
         PAR=energy_Wm2*2.15) %>%  #See "Estimating PAR from solar flux data" Word Doc in Troubleshooting folder
  select(-localTZ, -Lake, -energy_Wm2)
glimpse(SchulzenseePAR)



#Print first five time readings for each variable
firstFiveTimes <- data.frame(DO=SchulzenseeDOobs$dateTime[1:5],
                             PAR=SchulzenseePAR$dateTime[1:5],
                             wind=Schulzenseewnd$dateTime[1:5],
                             temp=Schulzenseetemp$dateTime[1:5])
print('First five time readings for each variable'); print(firstFiveTimes)
#Ok this is going to be annoying because the times don't line up...

#Calculate first differences of time readings, display unique values
cat('\n','First differences of datetime','\n')
difTimesDO <- diff(SchulzenseeDOobs$dateTime); print(table(difTimesDO))
difTimesPAR <- diff(SchulzenseePAR$dateTime); print(table(difTimesPAR))
difTimesWindSpeed <- diff(Schulzenseewnd$dateTime); print(table(difTimesWindSpeed))
difTimesSensorTemp <- diff(Schulzenseetemp$dateTime); print(table(difTimesSensorTemp))
timeStep=10	# in minutes


notDupRows <- findNotDupRows("SchulzenseeDOobs")
SchulzenseeDOobs <- SchulzenseeDOobs[notDupRows,]

notDupRows <- findNotDupRows("SchulzenseePAR")
SchulzenseePAR <- SchulzenseePAR[notDupRows,]

notDupRows <- findNotDupRows("Schulzenseewnd")
Schulzenseewnd <- Schulzenseewnd[notDupRows,]

notDupRows <- findNotDupRows("Schulzenseetemp")
Schulzenseetemp <- Schulzenseetemp[notDupRows,]

###Remove any rows where dateTime is NA for some reason,
###otherwise, floorMins won't work 
SchulzenseeDOobs<-SchulzenseeDOobs %>%
  filter_at(vars(dateTime), any_vars(!is.na(.)))

SchulzenseePAR<-SchulzenseePAR %>%
  filter_at(vars(dateTime), any_vars(!is.na(.)))

Schulzenseewnd<-Schulzenseewnd %>%
  filter_at(vars(dateTime), any_vars(!is.na(.)))

Schulzenseetemp<-Schulzenseetemp %>%
  filter_at(vars(dateTime), any_vars(!is.na(.)))


#Make all data sets extend from startTime to endTime by timeStep
#Note that for some lakes it may be necessary to aggregate some variables to coarser time scale to get match up


SchulzenseeDOobs$dateTime <- floorMins(SchulzenseeDOobs)
SchulzenseePAR$dateTime <- floorMins(SchulzenseePAR)
Schulzenseewnd$dateTime <- floorMins(Schulzenseewnd)
Schulzenseetemp$dateTime <- floorMins(Schulzenseetemp)


# #Repeat check for dup rows in case any introduced during floorMins
notDupRows <- findNotDupRows("SchulzenseeDOobs")
SchulzenseeDOobs <- SchulzenseeDOobs[notDupRows,]
notDupRows <- findNotDupRows("SchulzenseePAR")
SchulzenseePAR <- SchulzenseePAR[notDupRows,]
notDupRows <- findNotDupRows("Schulzenseewnd")
Schulzenseewnd <- Schulzenseewnd[notDupRows,]
notDupRows <- findNotDupRows("Schulzenseetemp")
Schulzenseetemp <- Schulzenseetemp[notDupRows,]

#Find the latest first time point and the earliest last time point of all the data
startTime <- max(min(SchulzenseeDOobs$dateTime),min(SchulzenseePAR$dateTime),min(Schulzenseewnd$dateTime),min(Schulzenseetemp$dateTime))
endTime <- min(max(SchulzenseeDOobs$dateTime),max(SchulzenseePAR$dateTime),max(Schulzenseewnd$dateTime),max(Schulzenseetemp$dateTime))

#Data.frame with one column "dateTime" which is sequence of times at time interval of timeStep, from startTime to endTime
completeTimes <- data.frame(dateTime=seq(startTime,endTime,paste(timeStep,"mins")))

#Merge all of input data.frames with completeTimes, so that they now all extend from startTime to endTime by timeStep
SchulzenseeDOobs <- merge(completeTimes,SchulzenseeDOobs,by="dateTime",all.x=T)
SchulzenseePAR <- merge(completeTimes,SchulzenseePAR,by="dateTime",all.x=T)
Schulzenseewnd <- merge(completeTimes,Schulzenseewnd,by="dateTime",all.x=T)
Schulzenseetemp <- merge(completeTimes,Schulzenseetemp,by="dateTime",all.x=T)

#Is it a problem that there are NAs in the PAR data? I guess we will find out
##IAO - 2021-02-12 --  I just checked and it shouldn't be because of the fillHoles helper function. 

Schulzensee_full <- left_join(SchulzenseeDOobs, Schulzenseewnd, by = c("dateTime"))
Schulzensee_full <- left_join(Schulzensee_full, Schulzenseetemp, by = c("dateTime"))
Schulzensee_full <- left_join(Schulzensee_full, SchulzenseePAR, by = c("dateTime"))

glimpse(Schulzensee_full)




####plot DO###########
Schulzensee_full %>%
  mutate(year=year(dateTime)) %>%
  ggplot(aes(x=dateTime, y=DO)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%m-%d") +
  ylab("DO obs")+
  facet_wrap(.~year, scales="free")+
  ggtitle("Schulzensee Lake 2010")


####plot WIND###########
Schulzensee_full %>%
  mutate(year=year(dateTime)) %>%
  ggplot(aes(x=dateTime, y=windSpeed)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "2 months", date_labels = "%m-%d") +
  ylab("Wind (m/s)")+
  facet_wrap(.~year, scales="free")+
  ggtitle("Schulzensee Lake 10")


####plot TEMPS###########
Schulzensee_full %>%
  dplyr::select(dateTime, wtr_1.4)%>%
  pivot_longer(-(dateTime), names_sep = "_", names_to = c("variable","depth")) %>%
  dplyr::select(-variable)%>%
  mutate(year=year(dateTime)) %>%
  mutate(depth=as.numeric(as.character(depth))) %>%
  rename(temp=value)%>%
  ggplot(aes(x=dateTime, y=temp, color=depth)) + 
  geom_point(size=2)+
  scale_x_datetime(date_breaks = "2 months", date_labels = "%m-%d") +
  scale_color_continuous_sequential(palette = "Dark Mint", rev=TRUE)+
  ylab("water temperature")+
  facet_wrap(.~year, scales="free")+
  ggtitle("Schulzensee Lake 2010")

####plot PAR###########
Schulzensee_full %>%
  mutate(year=year(dateTime)) %>%
  ggplot(aes(x=dateTime, y=PAR)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "2 months", date_labels = "%m-%d") +
  ylab("PAR")+
  facet_wrap(.~year, scales="free")+
  ggtitle("Schulzensee Lake 2010")



# export for modeling -----------------------------------------------------


#DO
SchulzenseeDOobs <- Schulzensee_full %>%
  dplyr::select(dateTime, DO)
write.csv(SchulzenseeDOobs, "data/metab_data_clean/schulzensee/Schulzensee_DO.txt", row.names=FALSE)

#Sensor temp
SchulzenseesensorTemp <- Schulzensee_full %>%
  dplyr::select(dateTime, wtr_1.4) %>%
  rename(sensorTemp=wtr_1.4) 
write.csv(SchulzenseesensorTemp, "data/metab_data_clean/schulzensee/Schulzensee_sensorTemp.txt", row.names=FALSE)

#PAR
SchulzenseePAR <- Schulzensee_full %>%
  dplyr::select(dateTime, PAR) 
write.csv(SchulzenseePAR, "data/metab_data_clean/schulzensee/Schulzensee_PAR.txt", row.names=FALSE)

#wind height = 2.85 m above lake surface
Schulzenseewind <- Schulzensee_full %>%
  dplyr::select(dateTime, windSpeed)
write.csv(Schulzenseewind, "data/metab_data_clean/schulzensee/Schulzensee_windSpeed.txt", row.names=FALSE)

#temp profile
Schulzenseetempprofile<- Schulzensee_full %>%
  dplyr::select(dateTime, wtr_1.4)
write.csv(Schulzenseetempprofile, "data/metab_data_clean/schulzensee/Schulzensee_tempProfile.txt", row.names=FALSE)





# > Mueggelsee -------------------------------------------------------------


MueggelseeDOobs <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Mueggelsee/Mueggelsee_doobs.txt", stringsAsFactors = FALSE, sep="\t") %>%
  select(-daylightSavings, -localTZ, -lakeID)
Mueggelseewnd <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Mueggelsee/Mueggelsee_wnd.txt", stringsAsFactors = FALSE, sep="\t") %>%
  select(-daylightSavings, -localTZ, -lakeID)
Mueggelseetemp <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Mueggelsee/Mueggelsee_wtr.txt", stringsAsFactors = FALSE, sep="\t") %>%
  select(-daylightSavings, -localTZ, -lakeID)
MueggelseePAR <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Mueggelsee/Mueggelsee_par.txt", stringsAsFactors = FALSE, sep="\t")%>%
  select(-daylightSavings, -localTZ, -lakeID)

####DATE CONVERSION###
glimpse(Mueggelseetemp)
Mueggelseetemp <- Mueggelseetemp %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="Europe/Berlin"),
         dateTime = with_tz(dateTime, "GMT")) %>%
  rename(wtr_0.5=wtr0.5,
         wtr_1.0=wtr1.0m,
         wtr_1.5=wtr1.5m,
         wtr_2.0=wtr2.0m,
         wtr_2.5=wtr2.5m,
         wtr_3.0=wtr3.0m,
         wtr_3.5=wtr3.5m,
         wtr_4.0=wtr4.0m,
         wtr_4.5=wtr4.5m,
         wtr_5.0=wtr5.0m)
glimpse(Mueggelseetemp)

glimpse(MueggelseeDOobs)
MueggelseeDOobs <- MueggelseeDOobs %>%
  select(dateTime, doObs_0.5m)%>%
  rename(doObs=doObs_0.5m)%>% #Just pick one DO depth
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="Europe/Berlin"),
         dateTime = with_tz(dateTime, "GMT")) 
glimpse(MueggelseeDOobs)

glimpse(Mueggelseewnd)
Mueggelseewnd <- Mueggelseewnd %>%
  rename(dateTime=datetTime)%>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="Europe/Berlin"),
         dateTime = with_tz(dateTime, "GMT")) %>%
  rename(windSpeed=wind)
glimpse(Mueggelseewnd)

glimpse(MueggelseePAR)
#Convert wind from character to number

MueggelseePAR <- MueggelseePAR %>%
  rename(dateTime=datetTime)%>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="Europe/Berlin"),
         dateTime = with_tz(dateTime, "GMT")) %>%  #I belive this is underwater PAR... choosing the depth closest to surface
  select(dateTime, PAR_0.75m)%>%
  rename(PAR=PAR_0.75m)
glimpse(MueggelseePAR)



#Print first five time readings for each variable
firstFiveTimes <- data.frame(DO=MueggelseeDOobs$dateTime[1:5],
                             PAR=MueggelseePAR$dateTime[1:5],
                             wind=Mueggelseewnd$dateTime[1:5],
                             temp=Mueggelseetemp$dateTime[1:5])
print('First five time readings for each variable'); print(firstFiveTimes)
#Ok this is going to be annoying because the times don't line up...

#Calculate first differences of time readings, display unique values
cat('\n','First differences of datetime','\n')
difTimesDO <- diff(MueggelseeDOobs$dateTime); print(table(difTimesDO))
difTimesPAR <- diff(MueggelseePAR$dateTime); print(table(difTimesPAR))
difTimesWindSpeed <- diff(Mueggelseewnd$dateTime); print(table(difTimesWindSpeed))
difTimesSensorTemp <- diff(Mueggelseetemp$dateTime); print(table(difTimesSensorTemp))
timeStep=30	# in minutes


notDupRows <- findNotDupRows("MueggelseeDOobs")
MueggelseeDOobs <- MueggelseeDOobs[notDupRows,]

notDupRows <- findNotDupRows("MueggelseePAR")
MueggelseePAR <- MueggelseePAR[notDupRows,]

notDupRows <- findNotDupRows("Mueggelseewnd")
Mueggelseewnd <- Mueggelseewnd[notDupRows,]

notDupRows <- findNotDupRows("Mueggelseetemp")
Mueggelseetemp <- Mueggelseetemp[notDupRows,]

###Remove any rows where dateTime is NA for some reason,
###otherwise, floorMins won't work 
MueggelseeDOobs<-MueggelseeDOobs %>%
  filter_at(vars(dateTime), any_vars(!is.na(.)))

MueggelseePAR<-MueggelseePAR %>%
  filter_at(vars(dateTime), any_vars(!is.na(.)))

Mueggelseewnd<-Mueggelseewnd %>%
  filter_at(vars(dateTime), any_vars(!is.na(.)))

Mueggelseetemp<-Mueggelseetemp %>%
  filter_at(vars(dateTime), any_vars(!is.na(.)))


#Make all data sets extend from startTime to endTime by timeStep
#Note that for some lakes it may be necessary to aggregate some variables to coarser time scale to get match up


MueggelseeDOobs$dateTime <- floorMins(MueggelseeDOobs)
MueggelseePAR$dateTime <- floorMins(MueggelseePAR)
Mueggelseewnd$dateTime <- floorMins(Mueggelseewnd)
Mueggelseetemp$dateTime <- floorMins(Mueggelseetemp)


# #Repeat check for dup rows in case any introduced during floorMins
notDupRows <- findNotDupRows("MueggelseeDOobs")
MueggelseeDOobs <- MueggelseeDOobs[notDupRows,]
notDupRows <- findNotDupRows("MueggelseePAR")
MueggelseePAR <- MueggelseePAR[notDupRows,]
notDupRows <- findNotDupRows("Mueggelseewnd")
Mueggelseewnd <- Mueggelseewnd[notDupRows,]
notDupRows <- findNotDupRows("Mueggelseetemp")
Mueggelseetemp <- Mueggelseetemp[notDupRows,]

#Find the latest first time point and the earliest last time point of all the data
startTime <- max(min(MueggelseeDOobs$dateTime),min(MueggelseePAR$dateTime),min(Mueggelseewnd$dateTime),min(Mueggelseetemp$dateTime))
endTime <- min(max(MueggelseeDOobs$dateTime),max(MueggelseePAR$dateTime),max(Mueggelseewnd$dateTime),max(Mueggelseetemp$dateTime))

#Data.frame with one column "dateTime" which is sequence of times at time interval of timeStep, from startTime to endTime
completeTimes <- data.frame(dateTime=seq(startTime,endTime,paste(timeStep,"mins")))

#Merge all of input data.frames with completeTimes, so that they now all extend from startTime to endTime by timeStep
MueggelseeDOobs <- merge(completeTimes,MueggelseeDOobs,by="dateTime",all.x=T)
MueggelseePAR <- merge(completeTimes,MueggelseePAR,by="dateTime",all.x=T)
Mueggelseewnd <- merge(completeTimes,Mueggelseewnd,by="dateTime",all.x=T)
Mueggelseetemp <- merge(completeTimes,Mueggelseetemp,by="dateTime",all.x=T)

#Is it a problem that there are NAs in the PAR data? I guess we will find out
##IAO - 2021-02-12 --  I just checked and it shouldn't be because of the fillHoles helper function. 

Mueggelsee_full <- left_join(MueggelseeDOobs, Mueggelseewnd, by = c("dateTime"))
Mueggelsee_full <- left_join(Mueggelsee_full, Mueggelseetemp, by = c("dateTime"))
Mueggelsee_full <- left_join(Mueggelsee_full, MueggelseePAR, by = c("dateTime"))

glimpse(Mueggelsee_full)

#For some reason (maybe can de-bug later) there are lots of duplicate rows.
#Use distinct function to pull out only distinct dateTimes.
Mueggelsee_full<-Mueggelsee_full %>%
  distinct(dateTime, .keep_all = TRUE)

####plot DO###########
Mueggelsee_full %>%
  mutate(year=year(dateTime)) %>%
  ggplot(aes(x=dateTime, y=doObs)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%m-%d") +
  ylab("DO obs")+
  facet_wrap(.~year, scales="free")+
  ggtitle("Mueggelsee Lake 2013")


####plot WIND###########
Mueggelsee_full %>%
  mutate(year=year(dateTime)) %>%
  ggplot(aes(x=dateTime, y=windSpeed)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "2 months", date_labels = "%m-%d") +
  ylab("Wind (m/s)")+
  facet_wrap(.~year, scales="free")+
  ggtitle("Mueggelsee Lake 10")


####plot TEMPS###########

Mueggelsee_full %>%
  dplyr::select(dateTime, wtr_0.5:wtr_5.0)%>%
  pivot_longer(-(dateTime), names_sep = "_", names_to = c("variable","depth")) %>%
  dplyr::select(-variable)%>%
  mutate(year=year(dateTime)) %>%
  mutate(depth=as.numeric(as.character(depth))) %>%
  rename(temp=value)%>%
  ggplot(aes(x=dateTime, y=temp, color=depth)) + 
  geom_point(size=2)+
  scale_x_datetime(date_breaks = "2 months", date_labels = "%m-%d") +
  scale_color_continuous_sequential(palette = "Dark Mint", rev=TRUE)+
  ylab("water temperature")+
  facet_wrap(.~year, scales="free")+
  ggtitle("Mueggelsee Lake")

####plot PAR###########
Mueggelsee_full %>%
  mutate(year=year(dateTime)) %>%
  ggplot(aes(x=dateTime, y=PAR)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "2 months", date_labels = "%m-%d") +
  ylab("PAR")+
  facet_wrap(.~year, scales="free")+
  ggtitle("Mueggelsee Lake ")




# export for modeling -----------------------------------------------------


#DO
MueggelseeDOobs <- Mueggelsee_full %>%
  mutate(year=year(dateTime))%>%
  filter(year=="2015")%>%
  dplyr::select(dateTime, doObs) %>%
  rename(DO=doObs)
write.csv(MueggelseeDOobs, "data/metab_data_clean/mueggelsee/Mueggelsee_DO.txt", row.names=FALSE)

#Sensor temp
MueggelseesensorTemp <- Mueggelsee_full %>%
  mutate(year=year(dateTime))%>%
  filter(year=="2015")%>%
  dplyr::select(dateTime, wtr_0.5) %>%
  rename(sensorTemp=wtr_0.5) 
write.csv(MueggelseesensorTemp, "data/metab_data_clean/mueggelsee/Mueggelsee_sensorTemp.txt", row.names=FALSE)

#PAR
MueggelseePAR <- Mueggelsee_full %>%
  mutate(year=year(dateTime))%>%
  filter(year=="2015")%>%
  dplyr::select(dateTime, PAR) 
write.csv(MueggelseePAR, "data/metab_data_clean/mueggelsee/Mueggelsee_PAR.txt", row.names=FALSE)

#wind height =  ????
Mueggelseewind <- Mueggelsee_full %>%
  mutate(year=year(dateTime))%>%
  filter(year=="2015")%>%
  dplyr::select(dateTime, windSpeed)
write.csv(Mueggelseewind, "data/metab_data_clean/mueggelsee/Mueggelsee_windSpeed.txt", row.names=FALSE)

#temp profile
Mueggelseetempprofile<- Mueggelsee_full %>%
  mutate(year=year(dateTime))%>%
  filter(year=="2015")%>%
  dplyr::select(dateTime, wtr_0.5:wtr_5.0)
write.csv(Mueggelseetempprofile, "data/metab_data_clean/mueggelsee/Mueggelsee_tempProfile.txt", row.names=FALSE)


# > Acton -----------------------------------------------------------------



ActonDOobs <- read_excel("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Acton/ActonLake_doobs.xlsx") %>%
  select(-daylightSavings, -localTZ, -lakeID)
Actonwnd <- read_excel("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Acton/ActonLake_wnd.xlsx")  %>%
  select(-daylightSavings, -localTZ, -lakeID)
Actontemp <- read_excel("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Acton/ActonLake_wtr.xlsx") %>%
  select(-daylightSavings, -localTZ, -lakeID)
ActonPAR <- read_excel("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Acton/ActonLake_par.xlsx") %>%
  select(-daylightSavings, -localTZ, -lakeID)

####DATE CONVERSION###
glimpse(Actontemp)
Actontemp <- Actontemp %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/New_York"),
         dateTime = with_tz(dateTime, "GMT")) 
glimpse(Actontemp)

glimpse(ActonDOobs)
ActonDOobs <- ActonDOobs %>%
  rename(DO=doObs)%>% 
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/New_York"),
         dateTime = with_tz(dateTime, "GMT")) 
glimpse(ActonDOobs)

glimpse(Actonwnd)
Actonwnd <- Actonwnd %>%
  mutate(wind=wind_km_hr/3.6) %>% #convert from km/hr to m/s
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/New_York"),
         dateTime = with_tz(dateTime, "GMT")) %>%
  rename(windSpeed=wind)
glimpse(Actonwnd)

glimpse(ActonPAR)
ActonPAR <- ActonPAR %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/New_York"),
         dateTime = with_tz(dateTime, "GMT")) 
glimpse(ActonPAR)



#Print first five time readings for each variable
firstFiveTimes <- data.frame(DO=ActonDOobs$dateTime[1:5],
                             PAR=ActonPAR$dateTime[1:5],
                             wind=Actonwnd$dateTime[1:5],
                             temp=Actontemp$dateTime[1:5])
print('First five time readings for each variable'); print(firstFiveTimes)
#Ok this is going to be annoying because the times don't line up...

#Calculate first differences of time readings, display unique values
cat('\n','First differences of datetime','\n')
difTimesDO <- diff(ActonDOobs$dateTime); print(table(difTimesDO))
difTimesPAR <- diff(ActonPAR$dateTime); print(table(difTimesPAR))
difTimesWindSpeed <- diff(Actonwnd$dateTime); print(table(difTimesWindSpeed))
difTimesSensorTemp <- diff(Actontemp$dateTime); print(table(difTimesSensorTemp))
timeStep=15	# in minutes


notDupRows <- findNotDupRows("ActonDOobs")
ActonDOobs <- ActonDOobs[notDupRows,]

notDupRows <- findNotDupRows("ActonPAR")
ActonPAR <- ActonPAR[notDupRows,]

notDupRows <- findNotDupRows("Actonwnd")
Actonwnd <- Actonwnd[notDupRows,]

notDupRows <- findNotDupRows("Actontemp")
Actontemp <- Actontemp[notDupRows,]

###Remove any rows where dateTime is NA for some reason,
###otherwise, floorMins won't work 
ActonDOobs<-ActonDOobs %>%
  filter_at(vars(dateTime), any_vars(!is.na(.)))

ActonPAR<-ActonPAR %>%
  filter_at(vars(dateTime), any_vars(!is.na(.)))

Actonwnd<-Actonwnd %>%
  filter_at(vars(dateTime), any_vars(!is.na(.)))

Actontemp<-Actontemp %>%
  filter_at(vars(dateTime), any_vars(!is.na(.)))


#Make all data sets extend from startTime to endTime by timeStep
#Note that for some lakes it may be necessary to aggregate some variables to coarser time scale to get match up


ActonDOobs$dateTime <- floorMins(ActonDOobs)
ActonPAR$dateTime <- floorMins(ActonPAR)
Actonwnd$dateTime <- floorMins(Actonwnd)
Actontemp$dateTime <- floorMins(Actontemp)


# #Repeat check for dup rows in case any introduced during floorMins
notDupRows <- findNotDupRows("ActonDOobs")
ActonDOobs <- ActonDOobs[notDupRows,]
notDupRows <- findNotDupRows("ActonPAR")
ActonPAR <- ActonPAR[notDupRows,]
notDupRows <- findNotDupRows("Actonwnd")
Actonwnd <- Actonwnd[notDupRows,]
notDupRows <- findNotDupRows("Actontemp")
Actontemp <- Actontemp[notDupRows,]

#Find the latest first time point and the earliest last time point of all the data
startTime <- max(min(ActonDOobs$dateTime),min(ActonPAR$dateTime),min(Actonwnd$dateTime),min(Actontemp$dateTime))
endTime <- min(max(ActonDOobs$dateTime),max(ActonPAR$dateTime),max(Actonwnd$dateTime),max(Actontemp$dateTime))

#Data.frame with one column "dateTime" which is sequence of times at time interval of timeStep, from startTime to endTime
completeTimes <- data.frame(dateTime=seq(startTime,endTime,paste(timeStep,"mins")))

#Merge all of input data.frames with completeTimes, so that they now all extend from startTime to endTime by timeStep
ActonDOobs <- merge(completeTimes,ActonDOobs,by="dateTime",all.x=T)
ActonPAR <- merge(completeTimes,ActonPAR,by="dateTime",all.x=T)
Actonwnd <- merge(completeTimes,Actonwnd,by="dateTime",all.x=T)
Actontemp <- merge(completeTimes,Actontemp,by="dateTime",all.x=T)

#Is it a problem that there are NAs in the PAR data? I guess we will find out
##IAO - 2021-02-12 --  I just checked and it shouldn't be because of the fillHoles helper function. 

Acton_full <- left_join(ActonDOobs, Actonwnd, by = c("dateTime"))
Acton_full <- left_join(Acton_full, Actontemp, by = c("dateTime"))
Acton_full <- left_join(Acton_full, ActonPAR, by = c("dateTime"))

glimpse(Acton_full)

Acton_full <- Acton_full %>%
  mutate(DO=as.numeric(DO)) %>% #convert from character to numeric 
  mutate(wtr0.5=as.numeric(wtr0.5)) %>%
  mutate(wtr1.0=as.numeric(wtr1.0)) %>%
  mutate(wtr2.0=as.numeric(wtr2.0)) %>%
  mutate(wtr3.0=as.numeric(wtr3.0)) %>%
  mutate(wtr4.0=as.numeric(wtr4.0)) %>%
  mutate(wtr5.0=as.numeric(wtr5.0)) %>%
  mutate(wtr6.0=as.numeric(wtr6.0)) %>%
  mutate(wtr7.0=as.numeric(wtr7.0))

####plot DO###########
Acton_full %>%
  mutate(year=year(dateTime)) %>%
  ggplot(aes(x=dateTime, y=DO)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%m-%d") +
  ylab("DO obs")+
  facet_wrap(.~year, scales="free")+
  ggtitle("Acton Lake")


####plot WIND###########
Acton_full %>%
  mutate(year=year(dateTime)) %>%
  ggplot(aes(x=dateTime, y=windSpeed)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "2 months", date_labels = "%m-%d") +
  ylab("Wind (m/s)")+
  facet_wrap(.~year, scales="free")+
  ggtitle("Acton Lake 10")


####plot TEMPS###########

Acton_full %>%
  dplyr::select(dateTime, wtr0.5:wtr7.0)%>%
  pivot_longer(-(dateTime), names_sep = "r", names_to = c("variable","depth")) %>%
  dplyr::select(-variable)%>%
  mutate(year=year(dateTime)) %>%
  mutate(depth=as.numeric(as.character(depth))) %>%
  rename(temp=value)%>%
  ggplot(aes(x=dateTime, y=temp, color=depth)) + 
  geom_point(size=2)+
  scale_x_datetime(date_breaks = "2 months", date_labels = "%m-%d") +
  scale_color_continuous_sequential(palette = "Dark Mint", rev=TRUE)+
  ylab("water temperature")+
  facet_wrap(.~year, scales="free")+
  ggtitle("Acton Lake")

####plot PAR###########
Acton_full %>%
  mutate(year=year(dateTime)) %>%
  ggplot(aes(x=dateTime, y=PAR)) + 
  geom_point()+
  scale_x_datetime(date_breaks = "2 months", date_labels = "%m-%d") +
  ylab("PAR")+
  facet_wrap(.~year, scales="free")+
  ggtitle("Acton Lake ")


## Check zMix for 2010

### 2021-04-30 Checking on Acton wtr profiles because the estimated zMix from the model
### is off for some other lakes. Is it here too? Checking... 
library(LakeMetabolizer)
Acton_temp <- read.delim("~/Google Drive/Research (common)/Research/Data/R/catchment_metab_wg/data/metab_data/Acton/Acton_wtr.txt")
Acton_wtr<-Acton_temp %>%
  mutate(dateTime=ymd_hms(dateTime)) %>%
  filter(dateTime > "2010-05-01" & dateTime < "2010-09-01") %>%
  rename_all(function(x) gsub("wtr", "wtr_", x)) %>% #Add an underscore to all heads to read wtr_#.#
  select(-wtr_2.5,-X)
wtr.heat.map(Acton_wtr, plot.title="Acton Water Temp (C)")

#calculate and plot the metalimnion depths
m.d = ts.meta.depths(Acton_wtr)

plot(m.d$datetime, m.d$top, type='l', ylab='Meta Depths (m)', xlab='Date', col='blue')
lines(m.d$datetime, m.d$bottom, col='red')

#calculate and plot the thermocline depth
t.d = ts.thermo.depth(Acton_wtr)
plot(t.d$datetime, t.d$thermo.depth, type='l', ylab='Thermocline Depth (m)', xlab='Date')
str(t.d)
mean(t.d$thermo.depth, na.rm=TRUE)
median(t.d$thermo.depth, na.rm=TRUE)
ActonzMix<-t.d %>%
  mutate(solarDay=date(datetime))%>%
  rename(zMix=thermo.depth) %>%
  group_by(solarDay) %>%
  summarize(zMix=mean(zMix, na.rm=TRUE))
write.table(ActonzMix, "results/model_output_raw/Acton/ActonzMix.txt", row.names=FALSE)






# export for modeling -----------------------------------------------------


#DO
ActonDOobs <- Acton_full %>%
  dplyr::select(dateTime, DO)
write.csv(ActonDOobs, "data/metab_data_clean/acton/Acton_DO.txt", row.names=FALSE)

#Sensor temp
ActonsensorTemp <- Acton_full %>%
  dplyr::select(dateTime, wtr0.5) %>%
  rename(sensorTemp=wtr0.5) 
write.csv(ActonsensorTemp, "data/metab_data_clean/Acton/Acton_sensorTemp.txt", row.names=FALSE)

#PAR
ActonPAR <- Acton_full %>%
  dplyr::select(dateTime, PAR) 
write.csv(ActonPAR, "data/metab_data_clean/Acton/Acton_PAR.txt", row.names=FALSE)

#wind height =  ????
Actonwind <- Acton_full %>%
  dplyr::select(dateTime, windSpeed)
write.csv(Actonwind, "data/metab_data_clean/Acton/Acton_windSpeed.txt", row.names=FALSE)

#temp profile
Actontempprofile<- Acton_full %>%
  dplyr::select(dateTime, wtr0.5:wtr7.0) %>%
  rename_all(function(x) gsub("wtr", "wtr_", x)) #Add an underscore to all heads to read wtr_#.#
write.csv(Actontempprofile, "data/metab_data_clean/Acton/Acton_tempProfile.txt", row.names=FALSE)

