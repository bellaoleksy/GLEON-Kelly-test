#Annie script
#9 Apr 2010

#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Annie_test'

#Lake name and year, to be used in labeling outputs
outName <- 'Annie2008'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/lakeMetabolism-master/Annie'

#Names of files to import
dataIn <- c('Annie_2008_DO.txt','Annie_2008_PAR.txt','Annie_2008_windSpeed.txt',
            'Annie_2008_sensorTemp.txt','Annie_2008_tempProfile.txt')

#Set pars
lat <- 27.207       #Latitude of lake, decimal degrees. N lats are positive, S lats are negative
elev <- 3.7         #Elevation above sea level at surface of lake, m
windHeight <- 10   #height above lake at which wind speed is meaured, m
timeStep <- 15       #number of minutes between DO measurements
sensorDepth <- 1.35  #depth of DO sensor, m 

#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/lakeMetabolism-master'

#Run main metab script
setwd(dirFxns)
source('metabFunc_v6.R')
