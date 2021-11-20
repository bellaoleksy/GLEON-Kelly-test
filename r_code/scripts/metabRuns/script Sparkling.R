#Sparkling script
#2021-02-16

#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Sparkling'

#Lake name and year, to be used in labeling outputs
outName <- 'sparkling'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Sparkling_Solomon'


#Names of files to import
dataIn <- c('Sparkling_2008_DO.txt','Sparkling_2008_PAR.txt','Sparkling_2008_windSpeed.txt',
            'Sparkling_2008_sensorTemp.txt','Sparkling_2008_tempProfile.txt')

#Set pars
lat <- 46.008       #Latitude of lake, decimal degrees. N lats are positive, S lats are negative
elev <- 497         #Elevation above sea level at surface of lake, m
windHeight <- 2   #height above lake at which wind speed is meaured, m
timeStep <- 10       #number of minutes between DO measurements
sensorDepth <- 0.5 #depth of DO sensor, m 
tz <- "America/Chicago" 

#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)
source('metabFunc_v8_IAO.R')
