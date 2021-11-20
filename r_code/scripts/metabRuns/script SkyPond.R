#Sky Pond 
#2021-03-12

##2016 sensor at 0.5m

#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/SkyPond'

#Lake name and year, to be used in labeling outputs
outName <- 'sky2016'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/sky'


#Names of files to import
dataIn <- c('sky_2016_DO.txt','sky_2016_PAR.txt','sky_2016_windSpeed.txt',
            'sky_2016_sensorTemp.txt','sky_2016_tempProfile.txt')

#Set pars
lat <- 40.277865       #Latitude of lake, decimal degrees. N lats are positive, S lats are negative
elev <- 3300         #Elevation above sea level at surface of lake, m
windHeight <- 2   #height above lake at which wind speed is meaured, m
timeStep <- 30       #number of minutes between DO measurements
sensorDepth <- 0.5 #depth of DO sensor, m 
tz <- "America/Denver"

#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)
source('metabFunc_v8_IAO.R')

rm(list=ls())

#Sky Pond 
#2016 - sensor at 6.5m



#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/SkyPond'

#Lake name and year, to be used in labeling outputs
outName <- 'sky2016_6.5m'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/sky'


#Names of files to import
dataIn <- c('sky_2016_DO_6.5m.txt','sky_2016_PAR.txt','sky_2016_windSpeed.txt',
            'sky_2016_sensorTemp_6.5m.txt','sky_2016_tempProfile.txt')

#Set pars
lat <- 40.277865       #Latitude of lake, decimal degrees. N lats are positive, S lats are negative
elev <- 3300         #Elevation above sea level at surface of lake, m
windHeight <- 2   #height above lake at which wind speed is meaured, m
timeStep <- 30       #number of minutes between DO measurements
sensorDepth <- 6.5 #depth of DO sensor, m 
tz <- "America/Denver"
maxZMix<- 7

#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)
source('metabFunc_v8_IAO.R')

rm(list=ls())


#Sky Pond 
#2017 - DO sensor at 0.5m


#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/SkyPond'

#Lake name and year, to be used in labeling outputs
outName <- 'sky2017'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/sky'


#Names of files to import
dataIn <- c('sky_2017_DO.txt','sky_2017_PAR.txt','sky_2017_windSpeed.txt',
            'sky_2017_sensorTemp.txt','sky_2017_tempProfile.txt')

#Set pars
lat <- 40.277865       #Latitude of lake, decimal degrees. N lats are positive, S lats are negative
elev <- 3300         #Elevation above sea level at surface of lake, m
windHeight <- 2   #height above lake at which wind speed is meaured, m
timeStep <- 30       #number of minutes between DO measurements
sensorDepth <- 0.5 #depth of DO sensor, m 
tz <- "America/Denver"
maxZMix<- 7

#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)
source('metabFunc_v8_IAO.R')

rm(list=ls())

#Sky Pond 
#2017 - sensor at 6.5m



#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/SkyPond'

#Lake name and year, to be used in labeling outputs
outName <- 'sky2017_6.5m'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/sky'


#Names of files to import
dataIn <- c('sky_2017_DO_6.5m.txt','sky_2017_PAR.txt','sky_2017_windSpeed.txt',
            'sky_2017_sensorTemp_6.5m.txt','sky_2017_tempProfile.txt')

#Set pars
lat <- 40.277865       #Latitude of lake, decimal degrees. N lats are positive, S lats are negative
elev <- 3300         #Elevation above sea level at surface of lake, m
windHeight <- 2   #height above lake at which wind speed is meaured, m
timeStep <- 30       #number of minutes between DO measurements
sensorDepth <- 6.5 #depth of DO sensor, m 
tz <- "America/Denver"
maxZMix<- 7

#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)
source('metabFunc_v8_IAO.R')

rm(list=ls())



#Sky Pond 
#2018 - sensor at 0.5m



#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/SkyPond'

#Lake name and year, to be used in labeling outputs
outName <- 'sky2018'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/sky'


#Names of files to import
dataIn <- c('sky_2018_DO.txt','sky_2018_PAR.txt','sky_2018_windSpeed.txt',
            'sky_2018_sensorTemp.txt','sky_2018_tempProfile.txt')

#Set pars
lat <- 40.277865       #Latitude of lake, decimal degrees. N lats are positive, S lats are negative
elev <- 3300         #Elevation above sea level at surface of lake, m
windHeight <- 2   #height above lake at which wind speed is meaured, m
timeStep <- 30       #number of minutes between DO measurements
sensorDepth <- 0.5 #depth of DO sensor, m 
tz <- "America/Denver"
maxZMix<- 7

#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)
source('metabFunc_v8_IAO.R')

rm(list=ls())



#Sky Pond 
#2018 - sensor at 6.5m



#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/SkyPond'

#Lake name and year, to be used in labeling outputs
outName <- 'sky2018_6.5m'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/sky'


#Names of files to import
dataIn <- c('sky_2018_DO_6.5m.txt','sky_2018_PAR.txt','sky_2018_windSpeed.txt',
            'sky_2018_sensorTemp_6.5m.txt','sky_2018_tempProfile.txt')

#Set pars
lat <- 40.277865       #Latitude of lake, decimal degrees. N lats are positive, S lats are negative
elev <- 3300         #Elevation above sea level at surface of lake, m
windHeight <- 2   #height above lake at which wind speed is meaured, m
timeStep <- 30       #number of minutes between DO measurements
sensorDepth <- 6.5 #depth of DO sensor, m 
tz <- "America/Denver"
maxZMix<- 7

#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)
source('metabFunc_v8_IAO.R')

rm(list=ls())



#Sky Pond 
#2019



#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/SkyPond'

#Lake name and year, to be used in labeling outputs
outName <- 'sky2019'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/sky'


#Names of files to import
dataIn <- c('sky_2019_DO.txt','sky_2019_PAR.txt','sky_2019_windSpeed.txt',
            'sky_2019_sensorTemp.txt','sky_2019_tempProfile.txt')

#Set pars
lat <- 40.277865       #Latitude of lake, decimal degrees. N lats are positive, S lats are negative
elev <- 3300         #Elevation above sea level at surface of lake, m
windHeight <- 2   #height above lake at which wind speed is meaured, m
timeStep <- 30       #number of minutes between DO measurements
sensorDepth <- 0.5 #depth of DO sensor, m 
tz <- "America/Denver"
maxZMix<- 7

#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)
source('metabFunc_v8_IAO.R')

rm(list=ls())


#Sky Pond 
#2020



#Directory where results of model fitting should get dumped
dirDump <- '/Volumes/GoogleDrive/My Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/SkyPond/2020 at 0.5 m'

#Lake name and year, to be used in labeling outputs
outName <- 'sky2020'

#Directory where data files are located
dirData <- '/Volumes/GoogleDrive/My Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/sky'


#Names of files to import
dataIn <- c('sky_2020_DO.txt','sky_2020_PAR.txt','sky_2020_windSpeed.txt',
            'sky_2020_sensorTemp.txt','sky_2020_tempProfile.txt')

#Set pars
lat <- 40.277865       #Latitude of lake, decimal degrees. N lats are positive, S lats are negative
elev <- 3300         #Elevation above sea level at surface of lake, m
windHeight <- 2   #height above lake at which wind speed is meaured, m
timeStep <- 30       #number of minutes between DO measurements
sensorDepth <- 0.5 #depth of DO sensor, m 
tz <- "America/Denver"
maxZMix<- 7

#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '/Volumes/GoogleDrive/My Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)
source('metabFunc_v8_IAO.R')


#Try to diagnose why we have a lot of missing modeled days


data1 %>%
  pivot_longer(-dateTime)%>%
  filter(!name=="zMix")%>%
  filter(dateTime >= "2020-08-03" & dateTime <= "2020-08-11") %>%
  ggplot(aes(x=dateTime,y=value))+
  geom_point()+
  facet_wrap(~name, scale="free_y") 
