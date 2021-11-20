#Lake Erken
#2021-03-01


# 2018 DO @ 1m --- Defaulting to this --------------------------------------------------------------------


#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Erken'

#Lake name and year, to be used in labeling outputs
outName <- 'erken2018'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/erken'


#Names of files to import
dataIn <- c('Erken_2018_DO_1m.txt','Erken_2018_PAR.txt','Erken_2018_windSpeed.txt',
            'Erken_2018_sensorTemp_1m.txt','Erken_2018_tempProfile.txt')

#Set pars
lat <- 59.844674       #Latitude of lake, decimal degrees. N lats are positive, S lats are negative
elev <- 18         #Elevation above sea level at surface of lake, m
windHeight <- 2   #height above lake at which wind speed is meaured, m
# ^^ Need to check with Don Pierson on this. 
timeStep <- 60       #number of minutes between DO measurements
sensorDepth <- 1 #depth of DO sensor, m 
tz <- "Europe/Berlin"

#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)

##NOTE: have to comment out the "floorMins" lines before running or it won't work
## also check for issues wiht reading in the dateTime column as date-only
source('metabFunc_v8_IAO.R')


# 2018 DO @ 2.5m --------------------------------------------------------------------


#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Erken_test'

#Lake name and year, to be used in labeling outputs
outName <- 'erken2018_2.5m'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/erken'


#Names of files to import
dataIn <- c('Erken_2018_DO_2.5m.txt','Erken_2018_PAR.txt','Erken_2018_windSpeed.txt',
            'Erken_2018_sensorTemp_2.5m.txt','Erken_2018_tempProfile.txt')

#Set pars
lat <- 59.844674       #Latitude of lake, decimal degrees. N lats are positive, S lats are negative
elev <- 18         #Elevation above sea level at surface of lake, m
windHeight <- 2   #height above lake at which wind speed is meaured, m
# ^^ Need to check with Don Pierson on this. 
timeStep <- 60       #number of minutes between DO measurements
sensorDepth <- 2.5 #depth of DO sensor, m 


#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)

##NOTE: have to comment out the "floorMins" lines before running or it won't work
## also check for issues wiht reading in the dateTime column as date-only
source('metabFunc_v8_IAO.R')



# 2018 DO @ 5m --------------------------------------------------------------------


#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Erken_test'

#Lake name and year, to be used in labeling outputs
outName <- 'erken2018_5m'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/erken'


#Names of files to import
dataIn <- c('Erken_2018_DO_5m.txt','Erken_2018_PAR.txt','Erken_2018_windSpeed.txt',
            'Erken_2018_sensorTemp_5m.txt','Erken_2018_tempProfile.txt')

#Set pars
lat <- 59.844674       #Latitude of lake, decimal degrees. N lats are positive, S lats are negative
elev <- 18         #Elevation above sea level at surface of lake, m
windHeight <- 2   #height above lake at which wind speed is meaured, m
# ^^ Need to check with Don Pierson on this. 
timeStep <- 60       #number of minutes between DO measurements
sensorDepth <- 5 #depth of DO sensor, m 


#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)

##NOTE: have to comment out the "floorMins" lines before running or it won't work
## also check for issues wiht reading in the dateTime column as date-only
source('metabFunc_v8_IAO.R')

