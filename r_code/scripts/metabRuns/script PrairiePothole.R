#Prairie Pothole (PRPO) script
#2021-02-26

#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/PRPO_test'

#Lake name and year, to be used in labeling outputs
outName <- 'PRPO'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/prairiepothole'


#Names of files to import
dataIn <- c('prairiepothole_2019_DO.txt','prairiepothole_2019_PAR.txt','prairiepothole_2019_windSpeed.txt',
            'prairiepothole_2019_sensorTemp.txt','prairiepothole_2019_tempProfile.txt')

#Set pars
lat <- 47.12999       #Latitude of lake, decimal degrees. N lats are positive, S lats are negative
elev <- 579         #Elevation above sea level at surface of lake, m
windHeight <- 2.85   #height above lake at which wind speed is meaured, m
timeStep <- 15       #number of minutes between DO measurements
sensorDepth <- 0.5 #depth of DO sensor, m 


#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)
source('metabFunc_v8_IAO.R')




# ...checking raw data ----------------------------------------------------
load('results/model_output_raw/PRPO_test/PRPO.RData')

data1 %>%
  # filter(dateTime >= "2019-07-03" & dateTime <= "2019-07-08") %>%
  pivot_longer(-dateTime) %>%
  filter(!name=="zMix")%>%
  ggplot(aes(x=dateTime, y=value, fill=name))+
  geom_point(size=2, shape=21)+
  facet_wrap(.~name, scales="free_y", nrow=4)+
  scale_x_datetime(breaks="1 month", date_labels="%m-%d")
#Mising data on either side


data1 %>%
  filter(dateTime >= "2019-06-01" & dateTime <= "2019-06-08") %>%
  pivot_longer(-dateTime) %>%
  filter(!name=="zMix")%>%
  ggplot(aes(x=dateTime, y=value, fill=name))+
  geom_point(size=2, shape=21)+
  facet_wrap(.~name, scales="free_y", nrow=4)+
  scale_x_datetime(breaks="1 month", date_labels="%m-%d")
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