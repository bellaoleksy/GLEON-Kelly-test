#Castle script
#2021-02-24

#Directory where results of model fitting should get dumped
dirDump <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Castle'

#Lake name and year, to be used in labeling outputs
outName <- 'Castle2019'

#Directory where data files are located
dirData <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_clean/castle'


#Names of files to import
dataIn <- c('castle_2019_DO.txt','castle_2019_PAR.txt','castle_2019_windSpeed.txt',
            'castle_2019_sensorTemp.txt','castle_2019_tempProfile.txt')

#Set pars
lat <- 41.226927       #Latitude of lake, decimal degrees. N lats are positive, S lats are negative
elev <- 1657         #Elevation above sea level at surface of lake, m
windHeight <- 19   #height above lake at which wind speed is meaured, m
timeStep <- 60       #number of minutes between DO measurements
sensorDepth <- 5 #depth of DO sensor, m 
tz <- "America/Los_Angeles"

#Directory where functions are located - this can be the same for any lake you run
dirFxns <- '~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts'

#Run main metab script
setwd(dirFxns)
source('metabFunc_v8_IAO.R')
# source('metabFunc_v7_IO.R')



# ... check data ----------------------------------------------------------

#06-18 and 06-19 are good but then there's a string of days later where do don't have any
#modeled estimates. Why?


data1 %>%
  filter(dateTime >= "2019-06-18" & dateTime <= "2019-06-24") %>%
  pivot_longer(-dateTime) %>%
  # filter(!name=="zMix")%>%
  ggplot(aes(x=dateTime, y=value, fill=name))+
  geom_point(size=2, shape=21)+
  facet_wrap(.~name, scales="free_y", nrow=5)+
  scale_x_datetime(breaks="1 day", date_labels="%m-%d")
#Missing PAR and wind from 2019-06-20 to 2019-07-07


#No estimate for 07-22
data1 %>%
  filter(dateTime >= "2019-07-21" & dateTime <= "2019-07-23") %>%
  pivot_longer(-dateTime) %>%
  # filter(!name=="zMix")%>%
  ggplot(aes(x=dateTime, y=value, fill=name))+
  geom_point(size=2, shape=21)+
  facet_wrap(.~name, scales="free_y", nrow=5)+
  scale_x_datetime(breaks="1 day", date_labels="%m-%d")


#No estimates from 08-05 to 09-20
data1 %>%
  filter(dateTime >= "2019-08-05" & dateTime <= "2019-09-20") %>%
  pivot_longer(-dateTime) %>%
  # filter(!name=="zMix")%>%
  ggplot(aes(x=dateTime, y=value, fill=name))+
  geom_point(size=2, shape=21)+
  facet_wrap(.~name, scales="free_y", nrow=5)+
  scale_x_datetime(breaks="7 days", date_labels="%m-%d")
#Oddly enough, we have zMix estiamtes but no sensorTemp estimates...
#Look at what's going on 

dataTempProfile %>%
  select(-wtr12.5, -wtr20.0, -wtr5.0)%>%
  filter(dateTime >= "2019-08-05" & dateTime <= "2019-09-20") %>%
  pivot_longer(-(dateTime), names_sep = "r", names_to = c("variable","depth")) %>%
  dplyr::select(-variable)%>%
  mutate(depth=as.numeric(as.character(depth))) %>%
  rename(temp=value)%>%
  ggplot(aes(x=dateTime, y=temp, color=depth)) + 
  geom_point(size=2)+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%m-%d") +
  scale_color_continuous_sequential(palette = "Lajolla", rev=TRUE)

dataTempProfile %>%
  select(-wtr12.5, -wtr20.0, -wtr5.0)%>%
  filter(dateTime >= "2019-08-05" & dateTime <= "2019-09-20") %>%
  pivot_longer(-(dateTime), names_sep = "r", names_to = c("variable","depth")) %>%
  dplyr::select(-variable)%>%
  mutate(depth=as.numeric(as.character(depth))) %>%
  rename(temp=value)%>%
  ggplot(aes(x=dateTime, y=temp, color=depth)) + 
  geom_point(size=2)+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%m-%d") +
  scale_color_continuous_sequential(palette = "Lajolla", rev=TRUE)

dataTempProfile %>%
  mutate(doy=yday(dateTime))%>%
  filter(doy<209)%>%
  ggplot(aes(x=wtr7.5, y=wtr5.0,fill=doy))+
  geom_point(shape=21, size=2)+
  scale_fill_continuous_sequential(palette = "Lajolla", rev=TRUE)
#These late summer wtr temps are not reliable for wtr5.0
#Could we use the relationship between the two depth at the beginning of the season
#to impute some values?

temp_lm_data<-dataTempProfile %>%
  mutate(doy=yday(dateTime))%>%
  filter(doy<209) %>%
  mutate(wtr5.0_predict=wtr7.5*1.6-0.98)

lm1<-lm(wtr5.0~wtr7.5,
        data=temp_lm_data)
summary(lm1)

temp_lm_data %>%
  ggplot(aes(x=wtr7.5, y=wtr5.0_predict,fill=doy))+
  geom_point(shape=21, size=2)+
  geom_abline(slope=1, intercept = 0)+
  scale_fill_continuous_sequential(palette = "Lajolla", rev=TRUE)
