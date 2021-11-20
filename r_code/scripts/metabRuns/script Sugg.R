#Sugg script
#2021-02-23

#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Suggs'

#Lake name and year, to be used in labeling outputs
outName <- 'Suggs2019'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/sugg'


#Names of files to import
dataIn <- c('sugg_2019_DO.txt','sugg_2019_PAR.txt','sugg_2019_windSpeed.txt',
            'sugg_2019_sensorTemp.txt','sugg_2019_tempProfile.txt')

#Set pars
lat <- 29.68705       #Latitude of lake, decimal degrees. N lats are positive, S lats are negative
elev <- 29         #Elevation above sea level at surface of lake, m
windHeight <- 3   #height above lake at which wind speed is meaured, m
timeStep <- 15       #number of minutes between DO measurements
sensorDepth <- 0.5 #depth of DO sensor, m 
tz <- "GMT"

#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)
source('metabFunc_v8_IAO.R')




# ... checking raw data ---------------------------------------------------

#Random week of data
data1 %>%
  filter(dateTime >= "2019-05-30" & dateTime <= "2019-06-06") %>%
  pivot_longer(-dateTime) %>%
  filter(!name=="zMix")%>%
  ggplot(aes(x=dateTime, y=value, fill=name))+
  geom_point(size=2, shape=21)+
  facet_wrap(.~name, scales="free_y", nrow=4)+
  scale_x_datetime(breaks="1 day", date_labels="%m-%d")
#Missing DO

#Random week of data
data1 %>%
  filter(dateTime >= "2019-07-01" & dateTime <= "2019-07-04") %>%
  pivot_longer(-dateTime) %>%
  filter(!name=="zMix")%>%
  ggplot(aes(x=dateTime, y=value, fill=name))+
  geom_point(size=2, shape=21)+
  facet_wrap(.~name, scales="free_y", nrow=4)+
  scale_x_datetime(breaks="1 day", date_labels="%m-%d")

#How many NAs are there in the output if timeStep is 15 and we interpolate gaps up to 60 minutes long?
map(GPPFitOut, ~sum(is.na(.)))


GPPFitOut %>%
  mutate(doy=yday(solarDay)) %>%
  ggplot(aes(x=doy,y=GPPFit))+
  geom_point(size=2, shape=21)


optimOut %>%
  mutate(doy=yday(solarDay)) %>%
  ggplot(aes(x=doy,y=rhoEst))+
  geom_point(size=2, shape=21)
