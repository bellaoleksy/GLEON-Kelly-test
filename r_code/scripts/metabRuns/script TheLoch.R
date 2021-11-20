#The Loch script
#2021-02-25


# 2016 @ 0.5m --------------------------------------------------------------------


#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/TheLoch'

#Lake name and year, to be used in labeling outputs
outName <- 'loch2016_0.5m'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/loch'


#Names of files to import
dataIn <- c('loch_2016_DO_0.5m.txt','loch_2016_PAR.txt','loch_2016_windSpeed.txt',
            'loch_2016_sensorTemp_0.5m.txt','loch_2016_tempProfile.txt')

#Set pars
lat <- 40.292462       #Latitude of lake, decimal degrees. N lats are positive, S lats are negative
elev <- 3048         #Elevation above sea level at surface of lake, m
windHeight <- 2   #height above lake at which wind speed is meaured, m
timeStep <- 30       #number of minutes between DO measurements
sensorDepth <- 0.5 #depth of DO sensor, m 
maxZMix <- 5
tz <- "America/Denver"

#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)
source('metabFunc_v8_IAO.R')



# 2016 @ 4.5m --------------------------------------------------------------------


#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/TheLoch'

#Lake name and year, to be used in labeling outputs
outName <- 'loch2016_4.5m'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/loch'


#Names of files to import
dataIn <- c('loch_2016_DO_4.5m.txt','loch_2016_PAR.txt','loch_2016_windSpeed.txt',
            'loch_2016_sensorTemp_4.5m.txt','loch_2016_tempProfile.txt')

#Set pars
lat <- 40.292462       #Latitude of lake, decimal degrees. N lats are positive, S lats are negative
elev <- 3048         #Elevation above sea level at surface of lake, m
windHeight <- 2   #height above lake at which wind speed is meaured, m
timeStep <- 30       #number of minutes between DO measurements
sensorDepth <- 4.5 #depth of DO sensor, m 
maxZMix <- 5
tz <- "America/Denver"

#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)
source('metabFunc_v8_IAO.R')

# 2017 @ 0.5m--------------------------------------------------------------------


#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/TheLoch'

#Lake name and year, to be used in labeling outputs
outName <- 'loch2017_0.5m'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/loch'


#Names of files to import
dataIn <- c('loch_2017_DO_0.5m.txt','loch_2017_PAR.txt','loch_2017_windSpeed.txt',
            'loch_2017_sensorTemp_0.5m.txt','loch_2017_tempProfile.txt')

#Set pars
lat <- 40.292462       #Latitude of lake, decimal degrees. N lats are positive, S lats are negative
elev <- 3048         #Elevation above sea level at surface of lake, m
windHeight <- 2   #height above lake at which wind speed is meaured, m
timeStep <- 30       #number of minutes between DO measurements
sensorDepth <- 0.5 #depth of DO sensor, m 
maxZMix <- 5 # maxZMix       Maximum depth to use for zMix, if temp only measured at one depth
#               This input is not required unless temp is only measured at one depth
tz <- "America/Denver"

#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)
source('metabFunc_v8_IAO.R')
rm(list=ls())

# 2017 @ 4.5m--------------------------------------------------------------------


#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/TheLoch'

#Lake name and year, to be used in labeling outputs
outName <- 'loch2017_4.5m'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/loch'


#Names of files to import
dataIn <- c('loch_2017_DO_4.5m.txt','loch_2017_PAR.txt','loch_2017_windSpeed.txt',
            'loch_2017_sensorTemp_4.5m.txt','loch_2017_tempProfile.txt')

#Set pars
lat <- 40.292462       #Latitude of lake, decimal degrees. N lats are positive, S lats are negative
elev <- 3048         #Elevation above sea level at surface of lake, m
windHeight <- 2   #height above lake at which wind speed is meaured, m
timeStep <- 30       #number of minutes between DO measurements
sensorDepth <- 4.5 #depth of DO sensor, m 
maxZMix <- 5 # maxZMix       Maximum depth to use for zMix, if temp only measured at one depth
#               This input is not required unless temp is only measured at one depth
tz <- "America/Denver"

#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)
source('metabFunc_v8_IAO.R')
rm(list=ls())




# 2018 @ 0.5m --------------------------------------------------------------------


#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/TheLoch'

#Lake name and year, to be used in labeling outputs
outName <- 'loch2018_0.5m'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/loch'


#Names of files to import
dataIn <- c('loch_2018_DO_0.5m.txt','loch_2018_PAR.txt','loch_2018_windSpeed.txt',
            'loch_2018_sensorTemp_0.5m.txt','loch_2018_tempProfile.txt')

#Set pars
lat <- 40.292462       #Latitude of lake, decimal degrees. N lats are positive, S lats are negative
elev <- 3048         #Elevation above sea level at surface of lake, m
windHeight <- 2   #height above lake at which wind speed is meaured, m
timeStep <- 30       #number of minutes between DO measurements
sensorDepth <- 0.5 #depth of DO sensor, m 
maxZMix <- 5 # maxZMix       Maximum depth to use for zMix, if temp only measured at one depth
#               This input is not required unless temp is only measured at one depth
tz <- "America/Denver"

#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)
source('metabFunc_v8_IAO.R')
rm(list=ls())



# 2018 @ 4.5m --------------------------------------------------------------------


#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/TheLoch'

#Lake name and year, to be used in labeling outputs
outName <- 'loch2018_4.5m'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/loch'


#Names of files to import
dataIn <- c('loch_2018_DO_4.5m.txt','loch_2018_PAR.txt','loch_2018_windSpeed.txt',
            'loch_2018_sensorTemp_4.5m.txt','loch_2018_tempProfile.txt')

#Set pars
lat <- 40.292462       #Latitude of lake, decimal degrees. N lats are positive, S lats are negative
elev <- 3048         #Elevation above sea level at surface of lake, m
windHeight <- 2   #height above lake at which wind speed is meaured, m
timeStep <- 30       #number of minutes between DO measurements
sensorDepth <- 4.5 #depth of DO sensor, m 
maxZMix <- 5 # maxZMix       Maximum depth to use for zMix, if temp only measured at one depth
#               This input is not required unless temp is only measured at one depth
tz <- "America/Denver"

#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)
source('metabFunc_v8_IAO.R')
rm(list=ls())



# 2019 @ 0.5m--------------------------------------------------------------------

#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/TheLoch'

#Lake name and year, to be used in labeling outputs
outName <- 'loch2019_0.5m'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/loch'


#Names of files to import
dataIn <- c('loch_2019_DO_0.5m.txt','loch_2019_PAR.txt','loch_2019_windSpeed.txt',
            'loch_2019_sensorTemp_0.5m.txt','loch_2019_tempProfile.txt')

#Set pars
lat <- 40.292462       #Latitude of lake, decimal degrees. N lats are positive, S lats are negative
elev <- 3048         #Elevation above sea level at surface of lake, m
windHeight <- 2   #height above lake at which wind speed is meaured, m
timeStep <- 10       #number of minutes between DO measurements
sensorDepth <- 0.5 #depth of DO sensor, m 
maxZMix <- 5 # maxZMix       Maximum depth to use for zMix, if temp only measured at one depth
#               This input is not required unless temp is only measured at one depth
tz <- "America/Denver"

#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)
source('metabFunc_v8_IAO.R')
rm(list=ls())



# 2020 @ 0.5m--------------------------------------------------------------------

#Directory where results of model fitting should get dumped
dirDump <- '/Volumes/GoogleDrive/My Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/TheLoch/2020 at 0.5m'

#Lake name and year, to be used in labeling outputs
outName <- 'loch2020_0.5m'

#Directory where data files are located
dirData <- '/Volumes/GoogleDrive/My Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/loch'


#Names of files to import
dataIn <- c('loch_2020_DO_0.5m.txt','loch_2020_PAR.txt','loch_2020_windSpeed.txt',
            'loch_2020_sensorTemp_0.5m.txt','loch_2020_tempProfile.txt')

#Set pars
lat <- 40.292462       #Latitude of lake, decimal degrees. N lats are positive, S lats are negative
elev <- 3048         #Elevation above sea level at surface of lake, m
windHeight <- 2   #height above lake at which wind speed is meaured, m
timeStep <- 30       #number of minutes between DO measurements
sensorDepth <- 0.5 #depth of DO sensor, m 
maxZMix <- 4.5 # maxZMix       Maximum depth to use for zMix, if temp only measured at one depth
#               This input is not required unless temp is only measured at one depth
tz <- "America/Denver"

#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '/Volumes/GoogleDrive/My Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)
source('metabFunc_v8_IAO.R')
# rm(list=ls())


data1 %>%
  pivot_longer(-dateTime)%>%
  filter(!name=="zMix")%>%
  # filter(dateTime >= "2020-07-20" & dateTime <= "2020-08-01") %>%
  ggplot(aes(x=dateTime,y=value))+
  geom_point()+
  facet_wrap(~name, scale="free_y")
