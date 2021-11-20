#Lake Taupo
#2021-02-26


# 2015-2020 --------------------------------------------------------------------


#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Taupo'

#Lake name and year, to be used in labeling outputs
outName <- 'taupo2015-2020'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/taupo'


#Names of files to import
dataIn <- c('taupo_2015-2020_DO.txt','taupo_2015-2020_PAR.txt','taupo_2015-2020_windSpeed.txt',
            'taupo_2015-2020_sensorTemp.txt','taupo_2015-2020_tempProfile.txt')

#Set pars
lat <- -38.798485       #Latitude of lake, decimal degrees. N lats are positive, S lats are negative
elev <- 356         #Elevation above sea level at surface of lake, m
windHeight <- 2   #height above lake at which wind speed is meaured, m
timeStep <- 60       #number of minutes between DO measurements
sensorDepth <- 4.9 #depth of DO sensor, m 
tz <- "Etc/GMT-5" #Note: this is NOT the correct timezone but is what I had to use to force correct sunrise/sunset times.

#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)

##NOTE: have to comment out the "floorMins" lines before running or it won't work
## also check for issues wiht reading in the dateTime column as date-only
source('metabFunc_v8_IAO.R')

