#Jordan script
#2021-02-16

#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Jordan'

#Lake name and year, to be used in labeling outputs
outName <- 'Jordan2018'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/jordan'


#Names of files to import
dataIn <- c('jordan_2018_DO.txt','jordan_2018_PAR.txt','jordan_2018_windSpeed.txt',
            'jordan_2018_sensorTemp.txt','jordan_2018_tempProfile.txt')

#Set pars
lat <- 44.3311101       #Latitude of lake, decimal degrees. N lats are positive, S lats are negative
elev <- 83.5         #Elevation above sea level at surface of lake, m
windHeight <- 9   #height above lake at which wind speed is meaured, m
timeStep <- 15       #number of minutes between DO measurements
sensorDepth <- 1 #depth of DO sensor, m 


#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)
source('metabFunc_v8_IAO.R')
# source('metabFunc_v7_IO.R')
