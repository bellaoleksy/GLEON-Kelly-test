#Acton
#2021-04-22
#Rerunning Zwart's sites using Chris' modified metabolism model 

#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Acton'

#Lake name and year, to be used in labeling outputs
outName <- 'Acton'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/zwart_metab_data/acton'


#Names of files to import
dataIn <- c('acton_DO.txt','acton_PAR.txt','acton_windSpeed.txt',
            'acton_sensorTemp.txt','acton_tempProfile.txt')

#Set pars
lat <- 39.57132       #Latitude of lake, decimal degrees. N lats are positive, S lats are negative
elev <- 263         #Elevation above sea level at surface of lake, m
windHeight <- 5   #height above lake at which wind speed is meaured, m
timeStep <- 15       #number of minutes between DO measurements
sensorDepth <- 1.5 #depth of DO sensor, m 
tz <- "America/New_York"

#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)
source('metabFunc_v8_IAO.R')
# source('metabFunc_v7_IO.R')
