#YunYang 
#2021-03-18
library(googlesheets4)

#Read in metadata google sheet
metadata<-read_sheet("https://docs.google.com/spreadsheets/d/1is87WT3n_TU76pTyiis3Os08JJiSmWc2G6K4bthJhU8/edit#gid=952562522") 


#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/YunYang'

#Lake name and year, to be used in labeling outputs
outName <- 'YunYang'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/yunyang'


#Names of files to import
dataIn <- c('YunYang_DO.txt','YunYang_PAR.txt','YunYang_windSpeed.txt',
            'YunYang_sensorTemp.txt','YunYang_tempProfile.txt')

#Set pars
lat<-metadata$`Latitude (decimal degrees)`[metadata$`Lake Name`=='YunYang'] # latitude in decimal degrees
elev <- metadata$`Altitude (m)`[metadata$`Lake Name`=='YunYang'] #altitude in m
windHeight <- 2   #height above lake at which wind speed is meaured, m
timeStep <- 10       #number of minutes between DO measurements
sensorDepth <- 0.5 #depth of DO sensor, m 
# maxZMix <- metadata$`Mean lake depth (m)`[metadata$`Lake Name`=='YunYang']
tz <- "Asia/Taipei"

#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)
source('metabFunc_v8_IAO.R')

rm(list=ls())
