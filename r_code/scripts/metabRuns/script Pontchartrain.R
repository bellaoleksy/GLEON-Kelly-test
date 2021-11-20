#Barco script
#2021-03-02

#Rerunning this lake from Solomon's dataset because the sunrise/sunset times didn't line up with PAR
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Pontchartrain_test'

#Lake name and year, to be used in labeling outputs
outName <- 'Pontchartrain_test'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Pontchartrain_Solomon'


#Names of files to import
dataIn <- c('Pontchartrain_2008_DO.txt','Pontchartrain_2008_PAR.txt','Pontchartrain_2008_windSpeed.txt',
            'Pontchartrain_2008_sensorTemp.txt','Pontchartrain_2008_tempProfile.txt')

#Set pars
lat <- 30.316       #Latitude of lake, decimal degrees. N lats are positive, S lats are negative
elev <- 0         #Elevation above sea level at surface of lake, m
windHeight <- 13   #height above lake at which wind speed is meaured, m
timeStep <- 15       #number of minutes between DO measurements
sensorDepth <- 1 #depth of DO sensor, m 
tz <- "America/Chicago" #Not the right timezone but what I had to do to make the sunrise/sunset line up

#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)
source('metabFunc_v8_IAO.R')
# source('metabFunc_v7_IO.R')

