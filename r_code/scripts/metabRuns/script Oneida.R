#Oneida Lake
#2021-02-26


#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Oneida'

#Lake name and year, to be used in labeling outputs
outName <- 'oneida2019'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/oneida'


#Names of files to import
dataIn <- c('oneida_2019_DO.txt','oneida_2019_PAR.txt','oneida_2019_windSpeed.txt',
            'oneida_2019_sensorTemp.txt','oneida_2019_tempProfile.txt')

#Set pars
lat <- 43.200982       #Latitude of lake, decimal degrees. N lats are positive, S lats are negative
elev <- 112         #Elevation above sea level at surface of lake, m
windHeight <- 2   #height above lake at which wind speed is meaured, m
# ^^ Need to check with investigators on this. 
timeStep <- 30       #number of minutes between DO measurements
sensorDepth <- 2.0 #depth of DO sensor, m 
tz <- "America/New_York"

#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)

##NOTE: have to comment out the "floorMins" lines before running or it won't work
## also check for issues wiht reading in the dateTime column as date-only
source('metabFunc_v8_IAO.R')
