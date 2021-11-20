#MFE lakes
#"WL" "HB" "BA" "BO" "BR" "CB" "NG" "WA"
library(googlesheets4)

#Read in metadata google sheet
metadata<-read_sheet("https://docs.google.com/spreadsheets/d/1is87WT3n_TU76pTyiis3Os08JJiSmWc2G6K4bthJhU8/edit#gid=952562522") 

# west long -------------------------------------------------------------------


#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/MFE Lakes/westlong'

#Lake name and year, to be used in labeling outputs
outName <- 'westlong'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/mfe_data/westlong'


#Names of files to import
dataIn <- c('westlong_DO.txt','westlong_PAR.txt','westlong_windSpeed.txt',
            'westlong_sensorTemp.txt','westlong_tempProfile.txt')

#Set pars
lat<-metadata$`Latitude (decimal degrees)`[metadata$lakeID=='WL'] # latitude in decimal degrees
elev <- metadata$`Altitude (m)`[metadata$lakeID=='WL'] #altitude in m
windHeight <- 2 # wind sensor height in m
sensorDepth <- 0.5 # DO sensor depth in m
timeStep <- 10       #number of minutes between DO measurements
tz <- "America/Denver" #Not the actual timezone, but what I had to force it to in order to have sunrise line up with PAR


#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)
source('metabFunc_v8_IAO.R')

rm(list=ls())

# hummingbird -------------------------------------------------------------------

#Read in metadata google sheet
metadata<-read_sheet("https://docs.google.com/spreadsheets/d/1is87WT3n_TU76pTyiis3Os08JJiSmWc2G6K4bthJhU8/edit#gid=952562522") 

#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/MFE Lakes/hummingbird'

#Lake name and year, to be used in labeling outputs
outName <- 'hummingbird'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/mfe_data/hummingbird'


#Names of files to import
dataIn <- c('hummingbird_DO.txt','hummingbird_PAR.txt','hummingbird_windSpeed.txt',
            'hummingbird_sensorTemp.txt','hummingbird_tempProfile.txt')

#Set pars
lat<-metadata$`Latitude (decimal degrees)`[metadata$lakeID=='HB'] # latitude in decimal degrees
elev <- metadata$`Altitude (m)`[metadata$lakeID=='HB'] #altitude in m
windHeight <- 2 # wind sensor height in m
sensorDepth <- 0.5 # DO sensor depth in m
timeStep <- 10       #number of minutes between DO measurements
tz <- "America/Denver" #Not the actual timezone, but what I had to force it to in order to have sunrise line up with PAR


#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)
source('metabFunc_v8_IAO.R')

rm(list=ls())


# bolger -------------------------------------------------------------------

#Read in metadata google sheet
metadata<-read_sheet("https://docs.google.com/spreadsheets/d/1is87WT3n_TU76pTyiis3Os08JJiSmWc2G6K4bthJhU8/edit#gid=952562522") 

#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/MFE Lakes/bolger'

#Lake name and year, to be used in labeling outputs
outName <- 'bolger'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/mfe_data/bolger'


#Names of files to import
dataIn <- c('bolger_DO.txt','bolger_PAR.txt','bolger_windSpeed.txt',
            'bolger_sensorTemp.txt','bolger_tempProfile.txt')

#Set pars
lat<-metadata$`Latitude (decimal degrees)`[metadata$lakeID=='BO'] # latitude in decimal degrees
elev <- metadata$`Altitude (m)`[metadata$lakeID=='BO'] #altitude in m
windHeight <- 2 # wind sensor height in m
sensorDepth <- 0.5 # DO sensor depth in m
timeStep <- 10       #number of minutes between DO measurements
tz <- "America/Denver" #Not the actual timezone, but what I had to force it to in order to have sunrise line up with PAR


#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)
source('metabFunc_v8_IAO.R')

rm(list=ls())


# brown -------------------------------------------------------------------

#Read in metadata google sheet
metadata<-read_sheet("https://docs.google.com/spreadsheets/d/1is87WT3n_TU76pTyiis3Os08JJiSmWc2G6K4bthJhU8/edit#gid=952562522") 

#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/MFE Lakes/brown'

#Lake name and year, to be used in labeling outputs
outName <- 'brown'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/mfe_data/brown'


#Names of files to import
dataIn <- c('brown_DO.txt','brown_PAR.txt','brown_windSpeed.txt',
            'brown_sensorTemp.txt','brown_tempProfile.txt')

#Set pars
lat<-metadata$`Latitude (decimal degrees)`[metadata$lakeID=='BR'] # latitude in decimal degrees
elev <- metadata$`Altitude (m)`[metadata$lakeID=='BR'] #altitude in m
windHeight <- 2 # wind sensor height in m
sensorDepth <- 0.5 # DO sensor depth in m
timeStep <- 10       #number of minutes between DO measurements
tz <- "America/Denver" #Not the actual timezone, but what I had to force it to in order to have sunrise line up with PAR


#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)
source('metabFunc_v8_IAO.R')


rm(list=ls())


# cranberry -------------------------------------------------------------------
#Read in metadata google sheet
metadata<-read_sheet("https://docs.google.com/spreadsheets/d/1is87WT3n_TU76pTyiis3Os08JJiSmWc2G6K4bthJhU8/edit#gid=952562522") 


#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/MFE Lakes/cranberry'

#Lake name and year, to be used in labeling outputs
outName <- 'cranberry'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/mfe_data/cranberry'


#Names of files to import
dataIn <- c('cranberry_DO.txt','cranberry_PAR.txt','cranberry_windSpeed.txt',
            'cranberry_sensorTemp.txt','cranberry_tempProfile.txt')

#Set pars
lat<-metadata$`Latitude (decimal degrees)`[metadata$lakeID=='CB'] # latitude in decimal degrees
elev <- metadata$`Altitude (m)`[metadata$lakeID=='CB'] #altitude in m
windHeight <- 2 # wind sensor height in m
sensorDepth <- 0.5 # DO sensor depth in m
timeStep <- 10       #number of minutes between DO measurements
tz <- "America/Denver" #Not the actual timezone, but what I had to force it to in order to have sunrise line up with PAR


#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)
source('metabFunc_v8_IAO.R')


rm(list=ls())

# bay -------------------------------------------------------------------

#Read in metadata google sheet
metadata<-read_sheet("https://docs.google.com/spreadsheets/d/1is87WT3n_TU76pTyiis3Os08JJiSmWc2G6K4bthJhU8/edit#gid=952562522") 

#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/MFE Lakes/bay'

#Lake name and year, to be used in labeling outputs
outName <- 'bay'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/mfe_data/bay'


#Names of files to import
dataIn <- c('bay_DO.txt','bay_PAR.txt','bay_windSpeed.txt',
            'bay_sensorTemp.txt','bay_tempProfile.txt')

#Set pars
lat<-metadata$`Latitude (decimal degrees)`[metadata$lakeID=='BA'] # latitude in decimal degrees
elev <- metadata$`Altitude (m)`[metadata$lakeID=='BA'] #altitude in m
windHeight <- 2 # wind sensor height in m
sensorDepth <- 0.5 # DO sensor depth in m
timeStep <- 10       #number of minutes between DO measurements
tz <- "America/Denver" #Not the actual timezone, but what I had to force it to in order to have sunrise line up with PAR


#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)
source('metabFunc_v8_IAO.R')
rm(list=ls())



# # northgate -------------------------------------------------------------------

#Read in metadata google sheet
metadata<-read_sheet("https://docs.google.com/spreadsheets/d/1is87WT3n_TU76pTyiis3Os08JJiSmWc2G6K4bthJhU8/edit#gid=952562522") 

#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/MFE Lakes/northgate'

#Lake name and year, to be used in labeling outputs
outName <- 'northgate'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/mfe_data/northgate'


#Names of files to import
dataIn <- c('northgate_DO.txt','northgate_PAR.txt','northgate_windSpeed.txt',
            'northgate_sensorTemp.txt','northgate_tempProfile.txt')

#Set pars
lat<-metadata$`Latitude (decimal degrees)`[metadata$lakeID=='NG'] # latitude in decimal degrees
elev <- metadata$`Altitude (m)`[metadata$lakeID=='NG'] #altitude in m
windHeight <- 2 # wind sensor height in m
sensorDepth <- 0.5 # DO sensor depth in m
timeStep <- 10       #number of minutes between DO measurements
tz <- "America/Denver" #Not the actual timezone, but what I had to force it to in order to have sunrise line up with PAR

#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)
source('metabFunc_v8_IAO.R')

rm(list=ls())

# ... checking days -------------------------------------------------------

# #First break in data
# data1 %>%
#   filter(dateTime >= "2016-06-06" & dateTime <= "2016-06-10") %>%
#   pivot_longer(-dateTime) %>%
#   filter(!name=="zMix")%>%
#   ggplot(aes(x=dateTime, y=value, fill=name))+
#   geom_point(size=2, shape=21)+
#   facet_wrap(.~name, scales="free_y", nrow=4)+
#   scale_x_datetime(breaks="1 day", date_labels="%m-%d")
# #It looks fine. What's going on?
# 
# #How many NAs for each variable?
# data1_trim<-data1 %>%
#   filter(dateTime >= "2016-06-06" & dateTime <= "2016-06-10") 
# map(data1_trim, ~sum(is.na(.)))
# #zMIX is missing... 
# 
# #Look at temperature profile data.. 
# dataTempProfile %>%
#   pivot_longer(-(dateTime), names_sep = "_", names_to = c("variable","depth")) %>%
#   dplyr::select(-variable)%>%
#   rename(temp=value)%>%
#   ggplot(aes(x=dateTime, y=temp, color=as.numeric(depth))) + 
#   geom_point(size=2)+
#   scale_x_datetime(date_breaks = "3 weeks", date_labels = "%y-%m-%d") +
#   scale_color_continuous_sequential(palette = "Dark Mint", rev=TRUE)


# ward -------------------------------------------------------------------

#Read in metadata google sheet
metadata<-read_sheet("https://docs.google.com/spreadsheets/d/1is87WT3n_TU76pTyiis3Os08JJiSmWc2G6K4bthJhU8/edit#gid=952562522") 

#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/MFE Lakes/ward'

#Lake name and year, to be used in labeling outputs
outName <- 'ward'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/mfe_data/ward'


#Names of files to import
dataIn <- c('ward_DO.txt','ward_PAR.txt','ward_windSpeed.txt',
            'ward_sensorTemp.txt','ward_tempProfile.txt')

#Set pars
lat<-metadata$`Latitude (decimal degrees)`[metadata$lakeID=='WA'] # latitude in decimal degrees
elev <- metadata$`Altitude (m)`[metadata$lakeID=='WA'] #altitude in m
windHeight <- 2 # wind sensor height in m
sensorDepth <- 0.5 # DO sensor depth in m
timeStep <- 10       #number of minutes between DO measurements
tz <- "America/Denver" #Not the actual timezone, but what I had to force it to in order to have sunrise line up with PAR


#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)
source('metabFunc_v8_IAO.R')
rm(list=ls())
