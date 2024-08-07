#Prairie Lake (PRLA) script
#2021-02-26

#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/PRLA_test'

#Lake name and year, to be used in labeling outputs
outName <- 'PRLA2018'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/prairielake'


#Names of files to import
dataIn <- c('prairielake_2018_DO.txt','prairielake_2018_PAR.txt','prairielake_2018_windSpeed.txt',
            'prairielake_2018_sensorTemp.txt','prairielake_2018_tempProfile.txt')

#Set pars
lat <- 47.15909       #Latitude of lake, decimal degrees. N lats are positive, S lats are negative
elev <- 565         #Elevation above sea level at surface of lake, m
windHeight <- 2.85   #height above lake at which wind speed is meaured, m
timeStep <- 15       #number of minutes between DO measurements
sensorDepth <- 0.5 #depth of DO sensor, m 
tz <- "GMT"

#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)
source('metabFunc_v8_IAO.R')

# ...checking raw data ----------------------------------------------------
load('results/model_output_raw/PRLA_test/PRLA2018.RData')

data1 %>%
  # filter(dateTime >= "2018-08-09" & dateTime <= "2018-08-14") %>%
  pivot_longer(-dateTime) %>%
  filter(!name=="zMix")%>%
  ggplot(aes(x=dateTime, y=value, fill=name))+
  geom_point(size=2, shape=21)+
  facet_wrap(.~name, scales="free_y", nrow=4)+
  scale_x_datetime(breaks="1 month", date_labels="%m-%d")
#some breaks longer than an hour


data1 %>%
  filter(dateTime >= "2018-09-02" & dateTime <= "2018-09-07") %>%
  pivot_longer(-dateTime) %>%
  filter(!name=="zMix")%>%
  ggplot(aes(x=dateTime, y=value, fill=name))+
  geom_point(size=2, shape=21)+
  facet_wrap(.~name, scales="free_y", nrow=4)+
  scale_x_datetime(breaks="1 day", date_labels="%m-%d")
#some breaks longer than an hour



data1 %>%
  filter(dateTime >= "2018-09-06" & dateTime <= "2018-09-07") %>%
  pivot_longer(-dateTime) %>%
  # filter(!name=="zMix")%>%
  ggplot(aes(x=dateTime, y=value, fill=name))+
  geom_point(size=2, shape=21)+
  facet_wrap(.~name, scales="free_y", nrow=5)+
  scale_x_datetime(breaks="4 hours")
#some breaks longer than an hour

#Is the sunset/sunrise time off? PAR looks week. Convert back to central time to check. 

data1 %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="GMT"),
         dateTime = with_tz(dateTime, "CDT")) %>%
  filter(dateTime >= "2018-09-06" & dateTime <= "2018-09-07") %>%
  pivot_longer(-dateTime) %>%
  # filter(!name=="zMix")%>%
  ggplot(aes(x=dateTime, y=value, fill=name))+
  geom_point(size=2, shape=21)+
  facet_wrap(.~name, scales="free_y", nrow=5)+
  scale_x_datetime(breaks="2 hours")
#LOOKS FUCKY. 