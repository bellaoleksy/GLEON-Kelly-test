#Lac Croche 
#2021-03-22
library(googlesheets4)

#Read in metadata google sheet
metadata<-read_sheet("https://docs.google.com/spreadsheets/d/1is87WT3n_TU76pTyiis3Os08JJiSmWc2G6K4bthJhU8/edit#gid=952562522") 


#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Simoncouche'

#Lake name and year, to be used in labeling outputs
outName <- 'simoncouche'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/simoncouche'


#Names of files to import
dataIn <- c('Simoncouche_DO.txt','Simoncouche_PAR.txt','Simoncouche_windSpeed.txt',
            'Simoncouche_sensorTemp.txt','Simoncouche_tempProfile.txt')

#Set pars
lat<-metadata$`Latitude (decimal degrees)`[metadata$`Lake Name`=='Simoncouche'] # latitude in decimal degrees
elev <- metadata$`Altitude (m)`[metadata$`Lake Name`=='Simoncouche'] #altitude in m
windHeight <- 2   #height above lake at which wind speed is meaured, m
timeStep <- 12       #number of minutes between DO measurements
sensorDepth <- 0.5 #depth of DO sensor, m 
tz <- "America/New_York"

#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)
source('metabFunc_v8_IAO.R')

rm(list=ls())

closeAllConnections()
data1 %>%
  filter(dateTime >= "2010-06-08" & dateTime <= "2010-06-14") %>%
  pivot_longer(-dateTime)%>%
  filter(!name=="zMix")%>%
  ggplot(aes(x=dateTime,y=value, fill=name))+
  geom_point(size=2, shape=21)+
  facet_wrap(.~name, scale="free_y",
             nrow=4)
