#Barco script
#2021-02-23

#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Barco'

#Lake name and year, to be used in labeling outputs
outName <- 'Barco2019'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/barco'


#Names of files to import
dataIn <- c('barco_2019_DO.txt','barco_2019_PAR.txt','barco_2019_windSpeed.txt',
            'barco_2019_sensorTemp.txt','barco_2019_tempProfile.txt')

#Set pars
lat <- 29.67647       #Latitude of lake, decimal degrees. N lats are positive, S lats are negative
elev <- 28         #Elevation above sea level at surface of lake, m
windHeight <- 2.85   #height above lake at which wind speed is meaured, m
timeStep <- 15       #number of minutes between DO measurements
sensorDepth <- 0.5 #depth of DO sensor, m 
tz <- "GMT"

#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)
source('metabFunc_v8_IAO.R')


#We get estimates for April 25th to May 8th with timeStep at 15 minutes and no interpolation of DO data
#Same results if we interpolate up to 60 minutes, except now we also get some stimates for October
