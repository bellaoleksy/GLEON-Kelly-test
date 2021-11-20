#Barco script
#2021-03-02

#maxZmix = max lake depth (4.3m)
#Then lake adjust the areal rate to the MEAN depth of the lake (3.4m)
#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Utah'

#Lake name and year, to be used in labeling outputs
outName <- 'Utah2017'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/utah'


#Names of files to import
dataIn <- c('utah_2017_DO.txt','utah_2017_PAR.txt','utah_2017_windSpeed.txt',
            'utah_2017_sensorTemp.txt','utah_2017_tempProfile.txt')

#Set pars
lat <- 40.169886       #Latitude of lake, decimal degrees. N lats are positive, S lats are negative
elev <- 1367.9         #Elevation above sea level at surface of lake, m
windHeight <- 10   #height above lake at which wind speed is meaured, m
timeStep <- 15       #number of minutes between DO measurements
sensorDepth <- 0.75 #depth of DO sensor, m 
maxZMix <- 4.3 # maxZMix       Maximum depth to use for zMix, if temp only measured at one depth
#               This input is not required unless temp is only measured at one depth
tz <- "America/Los_Angeles" #Actually Denver time but had to adjust for DST

#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)
source('metabFunc_v8_IAO.R')
# source('metabFunc_v7_IO.R')

