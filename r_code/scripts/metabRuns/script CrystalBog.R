#CrystalBog script
#2021-02-16

#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/CrystalBog_test'

#Lake name and year, to be used in labeling outputs
outName <- 'CrystalBog2008'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Crystal Bog_Solomon'


#Names of files to import
dataIn <- c('CrystalBog_2008_DO.txt','Trout_2008_PAR.txt','CrystalBog_2008_windSpeed.txt',
            'CrystalBog_2008_sensorTemp.txt','CrystalBog_2008_tempProfile.txt')

#Set pars
lat <- 46.008       #Latitude of lake, decimal degrees. N lats are positive, S lats are negative
elev <- 503         #Elevation above sea level at surface of lake, m
windHeight <- 2   #height above lake at which wind speed is meaured, m
timeStep <- 10       #number of minutes between DO measurements
sensorDepth <- 0.25 #depth of DO sensor, m 

#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)
source('metabFunc_v6_CS.R')
