#Almberga script
#2021-03-04

#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Almberga'

#Lake name and year, to be used in labeling outputs
outName <- 'Almberga2019'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/almberga'


#Names of files to import
dataIn <- c('almberga_2019_DO.txt','almberga_2019_PAR.txt','almberga_2019_windSpeed.txt',
            'almberga_2019_sensorTemp.txt','almberga_2019_tempProfile.txt')

#Set pars
lat <- 68.33179       #Latitude of lake, decimal degrees. N lats are positive, S lats are negative
elev <- 380         #Elevation above sea level at surface of lake, m
windHeight <- 2.16   #height above lake at which wind speed is meaured, m
timeStep <- 10       #number of minutes between DO measurements
sensorDepth <- 0.5 #depth of DO sensor, m 
tz <- "Europe/Berlin"

#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)
# source('metabFunc_v7_IO.R')
source('metabFunc_v8_IAO.R')


##How to deal with no sunset?




