#Zwart lakes



# ACTON -------------------------------------------------------------------


metaData<-read.csv('~/Google Drive/Research (common)/Research/Data/R/catchment_metab_wg/data/metadataLookUp.csv',stringsAsFactor=F)
#Lake name
lake<-'Acton'

#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Acton'

#Lake name and year, to be used in labeling outputs
outName <- 'acton'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/zwart_metab_data/acton'


#Names of files to import
dataIn <- c('acton_DO.txt','acton_PAR.txt','acton_windSpeed.txt',
            'acton_sensorTemp.txt','acton_tempProfile.txt')

#Set pars
lat<-metaData$Latitude..decimal.degrees.[metaData$Lake.Name==lake] # latitude in decimal degrees
elev <- metaData$Altitude..m.[metaData$Lake.Name==lake] #altitude in m
windHeight <- metaData$Wind.Height..m.[metaData$Lake.Name==lake] # wind sensor height in m
sensorDepth <-metaData$DO.Sensor.Depth..m.[metaData$Lake.Name==lake] # DO sensor depth in m
timeStep <- 15       #number of minutes between DO measurements


#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)
source('metabFunc_v8_IAO.R')


# ...checking raw data -------------------------------------------------------------


data1 %>%
  filter(dateTime >= "2010-06-18" & dateTime <= "2010-06-25") %>%
  pivot_longer(-dateTime) %>%
  filter(!name=="zMix")%>%
  ggplot(aes(x=dateTime, y=value, fill=name))+
  geom_point(size=2, shape=21)+
  facet_wrap(.~name, scales="free_y", nrow=4)+
  scale_x_datetime(breaks="1 day", date_labels="%m-%d")

rm(list=ls())




# CRAMPTON ----------------------------------------------------------------


###CRAMPTON
metaData<-read.csv('~/Google Drive/Research (common)/Research/Data/R/catchment_metab_wg/data/metadataLookUp.csv',stringsAsFactor=F)
#Lake name
lake<-'Crampton'

#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Crampton'

#Lake name and year, to be used in labeling outputs
outName <- 'crampton'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/zwart_metab_data/crampton'


#Names of files to import
dataIn <- c('crampton_DO.txt','crampton_PAR.txt','crampton_windSpeed.txt',
            'crampton_sensorTemp.txt','crampton_tempProfile.txt')

#Set pars
lat<-metaData$Latitude..decimal.degrees.[metaData$Lake.Name==lake] # latitude in decimal degrees
elev <- metaData$Altitude..m.[metaData$Lake.Name==lake] #altitude in m
windHeight <- metaData$Wind.Height..m.[metaData$Lake.Name==lake] # wind sensor height in m
sensorDepth <-metaData$DO.Sensor.Depth..m.[metaData$Lake.Name==lake] # DO sensor depth in m
timeStep <- 10       #number of minutes between DO measurements


#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)
source('metabFunc_v8_IAO.R')
rm(list=ls())




# FEEAGH ------------------------------------------------------------------


metaData<-read.csv('~/Google Drive/Research (common)/Research/Data/R/catchment_metab_wg/data/metadataLookUp.csv',stringsAsFactor=F)
#Lake name
lake<-'Feeagh'

#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/feeagh'

#Lake name and year, to be used in labeling outputs
outName <- 'feeagh'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/zwart_metab_data/feeagh'


#Names of files to import
dataIn <- c('feeagh_DO.txt','feeagh_PAR.txt','feeagh_windSpeed.txt',
            'feeagh_sensorTemp.txt','feeagh_tempProfile.txt')

#Set pars
lat<-metaData$Latitude..decimal.degrees.[metaData$Lake.Name==lake] # latitude in decimal degrees
elev <- metaData$Altitude..m.[metaData$Lake.Name==lake] #altitude in m
windHeight <- metaData$Wind.Height..m.[metaData$Lake.Name==lake] # wind sensor height in m
sensorDepth <-metaData$DO.Sensor.Depth..m.[metaData$Lake.Name==lake] # DO sensor depth in m
timeStep <- 10       #number of minutes between DO measurements
tz <- "GMT"

#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)
source('metabFunc_v8_IAO.R')
rm(list=ls())







# HARP --------------------------------------------------------------------


metaData<-read.csv('~/Google Drive/Research (common)/Research/Data/R/catchment_metab_wg/data/metadataLookUp.csv',stringsAsFactor=F)
#Lake name
lake<-'Harp'

#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Harp'

#Lake name and year, to be used in labeling outputs
outName <- 'harp'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/zwart_metab_data/harp'


#Names of files to import
dataIn <- c('harp_DO.txt','harp_PAR.txt','harp_windSpeed.txt',
            'harp_sensorTemp.txt','harp_tempProfile.txt')

#Set pars
lat<-metaData$Latitude..decimal.degrees.[metaData$Lake.Name==lake] # latitude in decimal degrees
elev <- metaData$Altitude..m.[metaData$Lake.Name==lake] #altitude in m
windHeight <- metaData$Wind.Height..m.[metaData$Lake.Name==lake] # wind sensor height in m
sensorDepth <-metaData$DO.Sensor.Depth..m.[metaData$Lake.Name==lake] # DO sensor depth in m
timeStep <- 10       #number of minutes between DO measurements
tz<-"America/New_York"


#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)
source('metabFunc_v8_IAO.R')
rm(list=ls())



# EASTLONG --------------------------------------------------------------------


metaData<-read.csv('~/Google Drive/Research (common)/Research/Data/R/catchment_metab_wg/data/metadataLookUp.csv',stringsAsFactor=F)
#Lake name
lake<-'EastLong'

#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/EastLong'

#Lake name and year, to be used in labeling outputs
outName <- 'eastlong'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/zwart_metab_data/eastlong'


#Names of files to import
dataIn <- c('eastlong_DO.txt','eastlong_PAR.txt','eastlong_windSpeed.txt',
            'eastlong_sensorTemp.txt','eastlong_tempProfile.txt')

#Set pars
lat<-metaData$Latitude..decimal.degrees.[metaData$Lake.Name==lake] # latitude in decimal degrees
elev <- metaData$Altitude..m.[metaData$Lake.Name==lake] #altitude in m
windHeight <- metaData$Wind.Height..m.[metaData$Lake.Name==lake] # wind sensor height in m
sensorDepth <-metaData$DO.Sensor.Depth..m.[metaData$Lake.Name==lake] # DO sensor depth in m
timeStep <- 10       #number of minutes between DO measurements
tz<-"America/Chicago"


#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)
source('metabFunc_v8_IAO.R')
rm(list=ls())





# LANGTJERN ---------------------------------------------------------------


metaData<-read.csv('~/Google Drive/Research (common)/Research/Data/R/catchment_metab_wg/data/metadataLookUp.csv',stringsAsFactor=F)
#Lake name
lake<-'Langtjern'

#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Langtjern'

#Lake name and year, to be used in labeling outputs
outName <- 'langtjern'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/zwart_metab_data/langtjern'


#Names of files to import
dataIn <- c('langtjern_DO.txt','langtjern_PAR.txt','langtjern_windSpeed.txt',
            'langtjern_sensorTemp.txt','langtjern_tempProfile.txt')

#Set pars
lat<-metaData$Latitude..decimal.degrees.[metaData$Lake.Name==lake] # latitude in decimal degrees
elev <- metaData$Altitude..m.[metaData$Lake.Name==lake] #altitude in m
windHeight <- metaData$Wind.Height..m.[metaData$Lake.Name==lake] # wind sensor height in m
sensorDepth <-metaData$DO.Sensor.Depth..m.[metaData$Lake.Name==lake] # DO sensor depth in m
timeStep <- 30       #number of minutes between DO measurements
tz <- "Europe/Berlin"

#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)
source('metabFunc_v8_IAO.R')
rm(list=ls())






# LILLINONAH --------------------------------------------------------------


metaData<-read.csv('~/Google Drive/Research (common)/Research/Data/R/catchment_metab_wg/data/metadataLookUp.csv',stringsAsFactor=F)
#Lake name
lake<-'Lillinonah'

#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Lillinonah'

#Lake name and year, to be used in labeling outputs
outName <- 'lillinonah'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/zwart_metab_data/lillinonah'


#Names of files to import
dataIn <- c('lillinonah_DO.txt','lillinonah_PAR.txt','lillinonah_windSpeed.txt',
            'lillinonah_sensorTemp.txt','lillinonah_tempProfile.txt')

#Set pars
lat<-metaData$Latitude..decimal.degrees.[metaData$Lake.Name==lake] # latitude in decimal degrees
elev <- metaData$Altitude..m.[metaData$Lake.Name==lake] #altitude in m
windHeight <- metaData$Wind.Height..m.[metaData$Lake.Name==lake] # wind sensor height in m
sensorDepth <-metaData$DO.Sensor.Depth..m.[metaData$Lake.Name==lake] # DO sensor depth in m
timeStep <- 15       #number of minutes between DO measurements
tz <- "America/New_York"

#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)
source('metabFunc_v8_IAO.R')
rm(list=ls())




# LILLSJOLIDEN ------------------------------------------------------------


metaData<-read.csv('~/Google Drive/Research (common)/Research/Data/R/catchment_metab_wg/data/metadataLookUp.csv',stringsAsFactor=F)
#Lake name
lake<-'Lillsjoliden'

#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Lillsjoliden'

#Lake name and year, to be used in labeling outputs
outName <- 'lillsjoliden'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/zwart_metab_data/lillsjoliden'


#Names of files to import
dataIn <- c('lillsjoliden_DO.txt','lillsjoliden_PAR.txt','lillsjoliden_windSpeed.txt',
            'lillsjoliden_sensorTemp.txt','lillsjoliden_tempProfile.txt')

#Set pars
lat<-metaData$Latitude..decimal.degrees.[metaData$Lake.Name==lake] # latitude in decimal degrees
elev <- metaData$Altitude..m.[metaData$Lake.Name==lake] #altitude in m
windHeight <- metaData$Wind.Height..m.[metaData$Lake.Name==lake] # wind sensor height in m
sensorDepth <-metaData$DO.Sensor.Depth..m.[metaData$Lake.Name==lake] # DO sensor depth in m
timeStep <- 20       #number of minutes between DO measurements
tz <- "Europe/Berlin"

#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)
source('metabFunc_v8_IAO.R')
rm(list=ls())






# MANGSTRETTJARN ----------------------------------------------------------


metaData<-read.csv('~/Google Drive/Research (common)/Research/Data/R/catchment_metab_wg/data/metadataLookUp.csv',stringsAsFactor=F)
#Lake name
lake<-'Mangstrettjarn'

#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Mangstrettjarn'

#Lake name and year, to be used in labeling outputs
outName <- 'mangstrettjarn'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/zwart_metab_data/mangstrettjarn'


#Names of files to import
dataIn <- c('mangstrettjarn_DO.txt','mangstrettjarn_PAR.txt','mangstrettjarn_windSpeed.txt',
            'mangstrettjarn_sensorTemp.txt','mangstrettjarn_tempProfile.txt')

#Set pars
lat<-metaData$Latitude..decimal.degrees.[metaData$Lake.Name==lake] # latitude in decimal degrees
elev <- metaData$Altitude..m.[metaData$Lake.Name==lake] #altitude in m
windHeight <- metaData$Wind.Height..m.[metaData$Lake.Name==lake] # wind sensor height in m
sensorDepth <-metaData$DO.Sensor.Depth..m.[metaData$Lake.Name==lake] # DO sensor depth in m
timeStep <- 10       #number of minutes between DO measurements
tz <- "Europe/Berlin"

#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)
source('metabFunc_v8_IAO.R')
rm(list=ls())





# MENDOTA -----------------------------------------------------------------


##
metaData<-read.csv('~/Google Drive/Research (common)/Research/Data/R/catchment_metab_wg/data/metadataLookUp.csv',stringsAsFactor=F)
#Lake name
lake<-'Mendota'

#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Mendota'

#Lake name and year, to be used in labeling outputs
outName <- 'mendota'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/zwart_metab_data/mendota'


#Names of files to import
dataIn <- c('mendota_DO.txt','mendota_PAR.txt','mendota_windSpeed.txt',
            'mendota_sensorTemp.txt','mendota_tempProfile.txt')

#Set pars
lat<-metaData$Latitude..decimal.degrees.[metaData$Lake.Name==lake] # latitude in decimal degrees
elev <- metaData$Altitude..m.[metaData$Lake.Name==lake] #altitude in m
windHeight <- metaData$Wind.Height..m.[metaData$Lake.Name==lake] # wind sensor height in m
sensorDepth <-metaData$DO.Sensor.Depth..m.[metaData$Lake.Name==lake] # DO sensor depth in m
timeStep <- 1       #number of minutes between DO measurements
tz <- "America/Chicago"

#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)
source('metabFunc_v8_IAO.R')
rm(list=ls())




# MORRIS ------------------------------------------------------------------


##
metaData<-read.csv('~/Google Drive/Research (common)/Research/Data/R/catchment_metab_wg/data/metadataLookUp.csv',stringsAsFactor=F)
#Lake name
lake<-'Morris'

#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Morris'

#Lake name and year, to be used in labeling outputs
outName <- 'morris'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/zwart_metab_data/morris'


#Names of files to import
dataIn <- c('morris_DO.txt','morris_PAR.txt','morris_windSpeed.txt',
            'morris_sensorTemp.txt','morris_tempProfile.txt')

#Set pars
lat<-metaData$Latitude..decimal.degrees.[metaData$Lake.Name==lake] # latitude in decimal degrees
elev <- metaData$Altitude..m.[metaData$Lake.Name==lake] #altitude in m
windHeight <- metaData$Wind.Height..m.[metaData$Lake.Name==lake] # wind sensor height in m
sensorDepth <-metaData$DO.Sensor.Depth..m.[metaData$Lake.Name==lake] # DO sensor depth in m
timeStep <- 10       #number of minutes between DO measurements
tz <- "America/Chicago"

#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)
source('metabFunc_v8_IAO.R')
rm(list=ls())


#
data1 %>%
  filter(dateTime >= "2013-07-30" & dateTime <= "2013-08-20") %>%
  pivot_longer(-dateTime) %>%
  filter(!name=="zMix")%>%
  ggplot(aes(x=dateTime, y=value, fill=name))+
  geom_point(size=2, shape=21)+
  facet_wrap(.~name, scales="free_y", nrow=4)+
  scale_x_datetime(breaks="1 day", date_labels="%m-%d")





# NASTJARN ----------------------------------------------------------------



metaData<-read.csv('~/Google Drive/Research (common)/Research/Data/R/catchment_metab_wg/data/metadataLookUp.csv',stringsAsFactor=F)
#Lake name
lake<-'Nastjarn'

#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Nastjarn'

#Lake name and year, to be used in labeling outputs
outName <- 'nastjarn'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/zwart_metab_data/nastjarn'


#Names of files to import
dataIn <- c('nastjarn_DO.txt','nastjarn_PAR.txt','nastjarn_windSpeed.txt',
            'nastjarn_sensorTemp.txt','nastjarn_tempProfile.txt')

#Set pars
lat<-metaData$Latitude..decimal.degrees.[metaData$Lake.Name==lake] # latitude in decimal degrees
elev <- metaData$Altitude..m.[metaData$Lake.Name==lake] #altitude in m
windHeight <- metaData$Wind.Height..m.[metaData$Lake.Name==lake] # wind sensor height in m
sensorDepth <-metaData$DO.Sensor.Depth..m.[metaData$Lake.Name==lake] # DO sensor depth in m
timeStep <- 10       #number of minutes between DO measurements
tz <- "Europe/Berlin"

#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)
source('metabFunc_v8_IAO.R')
rm(list=ls())

# ..checking raw data -----------------------------------------------------

data1 %>%
  filter(dateTime >= "2013-05-30" & dateTime <= "2013-06-06") %>%
  pivot_longer(-dateTime) %>%
  filter(!name=="zMix")%>%
  ggplot(aes(x=dateTime, y=value, fill=name))+
  geom_point(size=2, shape=21)+
  facet_wrap(.~name, scales="free_y", nrow=4)+
  scale_x_datetime(breaks="1 day", date_labels="%m-%d")


data1 %>%
  filter(dateTime >= "2013-06-09" & dateTime <= "2013-06-13") %>%
  pivot_longer(-dateTime) %>%
  filter(!name=="zMix")%>%
  ggplot(aes(x=dateTime, y=value, fill=name))+
  geom_point(size=2, shape=21)+
  facet_wrap(.~name, scales="free_y", nrow=4)+
  scale_x_datetime(breaks="1 day", date_labels="%m-%d")


data1 %>%
  filter(dateTime >= "2013-06-29" & dateTime <= "2013-07-13") %>%
  pivot_longer(-dateTime) %>%
  filter(!name=="zMix")%>%
  ggplot(aes(x=dateTime, y=value, fill=name))+
  geom_point(size=2, shape=21)+
  facet_wrap(.~name, scales="free_y", nrow=4)+
  scale_x_datetime(breaks="1 day", date_labels="%m-%d")

data1 %>%
  filter(dateTime >= "2013-07-30" & dateTime <= "2013-08-13") %>%
  pivot_longer(-dateTime) %>%
  filter(!name=="zMix")%>%
  ggplot(aes(x=dateTime, y=value, fill=name))+
  geom_point(size=2, shape=21)+
  facet_wrap(.~name, scales="free_y", nrow=4)+
  scale_x_datetime(breaks="1 day", date_labels="%m-%d")


data1 %>%
  filter(dateTime >= "2013-08-30" & dateTime <= "2013-09-13") %>%
  pivot_longer(-dateTime) %>%
  filter(!name=="zMix")%>%
  ggplot(aes(x=dateTime, y=value, fill=name))+
  geom_point(size=2, shape=21)+
  facet_wrap(.~name, scales="free_y", nrow=4)+
  scale_x_datetime(breaks="1 day", date_labels="%m-%d")




# OVRE --------------------------------------------------------------------


metaData<-read.csv('~/Google Drive/Research (common)/Research/Data/R/catchment_metab_wg/data/metadataLookUp.csv',stringsAsFactor=F)
#Lake name
lake<-'Ovre'

#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Ovre'

#Lake name and year, to be used in labeling outputs
outName <- 'ovre'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/zwart_metab_data/ovre'


#Names of files to import
dataIn <- c('ovre_DO.txt','ovre_PAR.txt','ovre_windSpeed.txt',
            'ovre_sensorTemp.txt','ovre_tempProfile.txt')

#Set pars
lat<-metaData$Latitude..decimal.degrees.[metaData$Lake.Name==lake] # latitude in decimal degrees
elev <- metaData$Altitude..m.[metaData$Lake.Name==lake] #altitude in m
windHeight <- metaData$Wind.Height..m.[metaData$Lake.Name==lake] # wind sensor height in m
sensorDepth <-metaData$DO.Sensor.Depth..m.[metaData$Lake.Name==lake] # DO sensor depth in m
timeStep <- 20       #number of minutes between DO measurements
tz <- "Europe/Berlin"

#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)
source('metabFunc_v8_IAO.R')
rm(list=ls())






# STRUPTJARN --------------------------------------------------------------


metaData<-read.csv('~/Google Drive/Research (common)/Research/Data/R/catchment_metab_wg/data/metadataLookUp.csv',stringsAsFactor=F)
#Lake name
lake<-'Struptjarn'

#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Struptjarn'

#Lake name and year, to be used in labeling outputs
outName <- 'struptjarn'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/zwart_metab_data/struptjarn'


#Names of files to import
dataIn <- c('struptjarn_DO.txt','struptjarn_PAR.txt','struptjarn_windSpeed.txt',
            'struptjarn_sensorTemp.txt','struptjarn_tempProfile.txt')

#Set pars
lat<-metaData$Latitude..decimal.degrees.[metaData$Lake.Name==lake] # latitude in decimal degrees
elev <- metaData$Altitude..m.[metaData$Lake.Name==lake] #altitude in m
windHeight <- metaData$Wind.Height..m.[metaData$Lake.Name==lake] # wind sensor height in m
sensorDepth <-metaData$DO.Sensor.Depth..m.[metaData$Lake.Name==lake] # DO sensor depth in m
timeStep <- 20       #number of minutes between DO measurements
tz <- "Europe/Berlin"

#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)
source('metabFunc_v8_IAO.R')
rm(list=ls())




# TROUT -------------------------------------------------------------------


##
metaData<-read.csv('~/Google Drive/Research (common)/Research/Data/R/catchment_metab_wg/data/metadataLookUp.csv',stringsAsFactor=F)
#Lake name
lake<-'Trout'

#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Trout'

#Lake name and year, to be used in labeling outputs
outName <- 'trout'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/zwart_metab_data/trout'


#Names of files to import
dataIn <- c('trout_DO.txt','trout_PAR.txt','trout_windSpeed.txt',
            'trout_sensorTemp.txt','trout_tempProfile.txt')

#Set pars
lat<-metaData$Latitude..decimal.degrees.[metaData$Lake.Name==lake] # latitude in decimal degrees
elev <- metaData$Altitude..m.[metaData$Lake.Name==lake] #altitude in m
windHeight <- metaData$Wind.Height..m.[metaData$Lake.Name==lake] # wind sensor height in m
sensorDepth <-metaData$DO.Sensor.Depth..m.[metaData$Lake.Name==lake] # DO sensor depth in m
timeStep <- 10       #number of minutes between DO measurements
tz <- "America/Chicago"

#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)
source('metabFunc_v8_IAO.R')
rm(list=ls())


# ..checking raw data -----------------------------------------------------

data1 %>%
  filter(dateTime >= "2013-06-11" & dateTime <= "2013-06-14") %>%
  pivot_longer(-dateTime) %>%
  filter(!name=="zMix")%>%
  ggplot(aes(x=dateTime, y=value, fill=name))+
  geom_point(size=2, shape=21)+
  facet_wrap(.~name, scales="free_y", nrow=4)+
  scale_x_datetime(breaks="1 day", date_labels="%m-%d")
#sensorTemp missing

data1 %>%
  filter(dateTime >= "2013-07-14" & dateTime <= "2013-07-26") %>%
  pivot_longer(-dateTime) %>%
  filter(!name=="zMix")%>%
  ggplot(aes(x=dateTime, y=value, fill=name))+
  geom_point(size=2, shape=21)+
  facet_wrap(.~name, scales="free_y", nrow=4)+
  scale_x_datetime(breaks="1 day", date_labels="%m-%d")
#sensorTemp and DO

data1 %>%
  filter(dateTime >= "2013-10-22" & dateTime <= "2013-11-05") %>%
  pivot_longer(-dateTime) %>%
  filter(!name=="zMix")%>%
  ggplot(aes(x=dateTime, y=value, fill=name))+
  geom_point(size=2, shape=21)+
  facet_wrap(.~name, scales="free_y", nrow=4)+
  scale_x_datetime(breaks="1 day", date_labels="%m-%d")
#sensorTemp, DO, wind




# VORTSJARV ---------------------------------------------------------------

######BEFORE YOU RUN######
#note that the floorMins function doesn't work for this dataset. Just comment it out before running. 

##
metaData<-read.csv('~/Google Drive/Research (common)/Research/Data/R/catchment_metab_wg/data/metadataLookUp.csv',stringsAsFactor=F)
#Lake name
lake<-'Vortsjarv'

#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Vortsjarv'

#Lake name and year, to be used in labeling outputs
outName <- 'vortsjarv'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/zwart_metab_data/vortsjarv'


#Names of files to import
dataIn <- c('vortsjarv_DO.txt','vortsjarv_PAR.txt','vortsjarv_windSpeed.txt',
            'vortsjarv_sensorTemp.txt','vortsjarv_tempProfile.txt')

#Set pars
lat<-metaData$Latitude..decimal.degrees.[metaData$Lake.Name==lake] # latitude in decimal degrees
elev <- metaData$Altitude..m.[metaData$Lake.Name==lake] #altitude in m
windHeight <- metaData$Wind.Height..m.[metaData$Lake.Name==lake] # wind sensor height in m
sensorDepth <-metaData$DO.Sensor.Depth..m.[metaData$Lake.Name==lake] # DO sensor depth in m
timeStep <- 10       #number of minutes between DO measurements
tz <- "Europe/Tallinn"

#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)
source('metabFunc_v8_IAO.R')
rm(list=ls())



# ..checking raw data -----------------------------------------------------

data1 %>%
  filter(dateTime >= "2013-07-24" & dateTime <= "2013-07-26") %>%
  pivot_longer(-dateTime) %>%
  filter(!name=="zMix")%>%
  ggplot(aes(x=dateTime, y=value, fill=name))+
  geom_point(size=2, shape=21)+
  facet_wrap(.~name, scales="free_y", nrow=4)+
  scale_x_datetime(breaks="1 day", date_labels="%m-%d")
#some breaks longer than an hour

data1 %>%
  filter(dateTime >= "2013-06-16" & dateTime <= "2013-06-19") %>%
  pivot_longer(-dateTime) %>%
  filter(!name=="zMix")%>%
  ggplot(aes(x=dateTime, y=value, fill=name))+
  geom_point(size=2, shape=21)+
  facet_wrap(.~name, scales="free_y", nrow=4)+
  scale_x_datetime(breaks="1 day", date_labels="%m-%d")
#DO and sensor temp gaps