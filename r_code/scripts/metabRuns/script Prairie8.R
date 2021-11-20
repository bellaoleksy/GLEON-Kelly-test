library(googlesheets4)

#Read in metadata google sheet
metadata<-read_sheet("https://docs.google.com/spreadsheets/d/1is87WT3n_TU76pTyiis3Os08JJiSmWc2G6K4bthJhU8/edit#gid=952562522") 

# P8 -------------------------------------------------------------------


#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/P8'

#Lake name and year, to be used in labeling outputs
outName <- 'p8'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/p8'


#Names of files to import
dataIn <- c('P8_2019_DO.txt','P8_2019_PAR.txt','P8_2019_windSpeed.txt',
            'P8_2019_sensorTemp.txt','P8_2019_tempProfile.txt')

#Set pars
lat<-metadata$`Latitude (decimal degrees)`[metadata$`Lake Name`=='P8'] # latitude in decimal degrees
elev <- metadata$`Altitude (m)`[metadata$`Lake Name`=='P8'] #altitude in m
windHeight <- 2 # wind sensor height in m
sensorDepth <- 0.2 # DO sensor depth in m
timeStep <- 30       #number of minutes between DO measurements
tz <- "America/Chicago"

#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)
source('metabFunc_v8_IAO.R')

