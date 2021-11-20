#Toolik (TOOK) script
#2021-02-26

#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Toolik_test'

#Lake name and year, to be used in labeling outputs
outName <- 'TOOK2019'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/toolik'


#Names of files to import
dataIn <- c('took_2019_DO.txt','took_2019_PAR.txt','took_2019_windSpeed.txt',
            'took_2019_sensorTemp.txt','took_2019_tempProfile.txt')

#Set pars
lat <- 68.630692       #Latitude of lake, decimal degrees. N lats are positive, S lats are negative
elev <- 745         #Elevation above sea level at surface of lake, m
windHeight <- 2.85   #height above lake at which wind speed is meaured, m
timeStep <- 15       #number of minutes between DO measurements
sensorDepth <- 0.5 #depth of DO sensor, m 


#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)
source('metabFunc_v8_IAO.R')
