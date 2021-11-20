#Gollinsee 
#2021-03-04
library(googlesheets4)

#Read in metadata google sheet
metadata<-read_sheet("https://docs.google.com/spreadsheets/d/1is87WT3n_TU76pTyiis3Os08JJiSmWc2G6K4bthJhU8/edit#gid=952562522") 


#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Mueggelsee'

#Lake name and year, to be used in labeling outputs
outName <- 'Mueggelsee'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/Mueggelsee'


#Names of files to import
dataIn <- c('Mueggelsee_DO.txt','Mueggelsee_PAR.txt','Mueggelsee_windSpeed.txt',
            'Mueggelsee_sensorTemp.txt','Mueggelsee_tempProfile.txt')

#Set pars
lat<-metadata$`Latitude (decimal degrees)`[metadata$`Lake Name`=='Mueggelsee'] # latitude in decimal degrees
elev <- metadata$`Altitude (m)`[metadata$`Lake Name`=='Mueggelsee'] #altitude in m
windHeight <- 2   #height above lake at which wind speed is meaured, m
timeStep <- 30       #number of minutes between DO measurements
sensorDepth <- 1.4 #depth of DO sensor, m 
# maxZMix <- metadata$`Mean lake depth (m)`[metadata$`Lake Name`=='Mueggelsee']
tz <- "Europe/Berlin"

#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)
source('metabFunc_v8_IAO.R')

rm(list=ls())
