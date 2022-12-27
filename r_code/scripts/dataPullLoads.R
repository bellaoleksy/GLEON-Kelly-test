
# Load raw data -----------------------------------------------------------

gridSearchInput<-read.csv(here("data/preliminaryMetropolisResults/gridSearchInput_wphyto4bella.csv")) 
#Use this to verify that the new load concentration calculations are indeed FLOW WEIGHTED

# ~Zwart loads  -------------------------------------------------

#### loading in nutrient loads from Zwart's dataset ###
dir<-'~/Google Drive/My Drive/Research (common)/Research/Data/R/catchment_metab_wg/results/nutrient load/' # directory of load data
# dir<-'/Users/solomonlab/Google Drive/Research (common)/Research/Data/R/catchment_metab_wg/results/nutrient load/' # directory of load data
files<-list.files(dir) # folders in this dir
files<-files[-grep('README',files)] # get rid of README doc

zwart_load<-data.frame() # data frame to store all load data
for(i in 1:length(files)){ # loops over all files in load directory
  cur<-read.table(file.path(dir,files[i]),header=T,sep='\t',
                  stringsAsFactors = F) # read in lake specific load data
  cur$lake<-strsplit(files[i], split = '_loads.txt')[[1]][1]
  zwart_load<-rbind(zwart_load,cur)
}

zwart_load <- as_tibble(zwart_load) %>%
  mutate(date = as.Date(Date)) %>%
  select(-Date)



## ~Mueggelsee loads -------------------------------------------------

mueggelsee_Q<-read.delim(here("data/metab_data_raw/Mueggelsee/Mueggelsee_Spree_discharge.txt")) %>%
  select(-site) %>%
  mutate(dateTime=ymd(dateTime)) 

mueggelsee_C<-read.delim(here("data/metab_data_raw/Mueggelsee/Mueggelsee_Spree_nutrient_trim.txt")) %>%
  mutate(dateTime=mdy_hm(dateTime),
         dateTime=date(dateTime)) %>%
  select(-site)
mueggelsee_load<-right_join(mueggelsee_C, mueggelsee_Q, by="dateTime") %>%
  filter(dateTime > "2015-01-01" & dateTime < "2015-12-31") %>% #Keep only 2015, where we have metab data
  mutate(dateTime=ymd_hms(paste(dateTime, "12:00:00"))) %>%
  rename(datetime=dateTime,
         flow=discharge) %>%
  relocate(flow, .before = DOC_mgL) %>% #rearranging so flow is after datetime for load estimation software
  mutate(TP_mgL=TP_ugL/1000,
         SRP_mgL=SRP_ugL/1000) %>% #Convert anything in ug/L to mg/L (==g/L3)
  select(-TP_ugL, -SRP_ugL) 

## ~Erken loads -------------------------------------------------

erken_Q<-read.delim(here("data/metab_data_raw/Erken/Erken_InputQ_2018.txt")) %>%
  select(-Site) %>%
  rename(flow=`Inflow..m3.s.`) %>%
  mutate(datetime=ymd_hms(datetime)) 

erken_C<-read.delim(here("data/metab_data_raw/Erken/Erken_StreamChem2018.txt")) %>%
  rename(datetime=Date.Time) %>%
  mutate(datetime=mdy(datetime)) %>%
  select(-Site, -Name)

erken_load<-right_join(erken_C, erken_Q, by="datetime") %>%
  mutate(datetime=ymd_hms(paste(datetime, "12:00:00"))) %>%
  relocate(flow, .before = DOC_mgL) %>% #rearranging so flow is after datetime for load estimation software
  mutate(TP_mgL=TP_ugL/1000,
         TN_mgL=TN_ugL/1000) %>% #Convert anything in ug/L to mg/L (==g/L3)
  select(-TP_ugL, -TN_ugL) %>%
  arrange(datetime)


## ~Acton loads -------------------------------------------------
# 
# acton_Q_marshall<-read.delim(here("data/metab_data_raw/Acton/ActonLake_streams_discharge_marshallsbranch.txt") %>%
#   mutate(dateTime=ymd_hms(dateTime)) 
# # glimpse(acton_Q_marshall)
# 
# acton_Q_fourmile<-read.delim(here("data/metab_data_raw/Acton/ActonLake_streams_discharge_fourmilecreek.txt") %>%
#   mutate(dateTime=ymd_hms(dateTime)) 
# # glimpse(acton_Q_fourmile)
# 
# acton_Q_littlefourmile<-read.delim(here("data/metab_data_raw/Acton/ActonLake_streams_discharge_littlefourmilecreek.txt") %>%
#   mutate(dateTime=ymd_hms(dateTime)) 
# # glimpse(acton_Q_littlefourmile)
# 
# 
# 
# acton_C_marshall<-read.delim(here("data/metab_data_raw/Acton/ActonLake_streams_nutrients_marshallsbranch.txt") %>%
#   mutate(dateTime=dmy_hm(dateTime),
#          dateTime=date(dateTime)) %>%
#   group_by(dateTime, site)%>%
#   summarize_all(.funs=mean) 
# # glimpse(acton_C_marshall)
# 
# acton_C_fourmile<-read.delim(here("data/metab_data_raw/Acton/ActonLake_streams_nutrients_fourmilecreek.txt") %>%
#   select(site:SRP_ugL)%>%
#   mutate(dateTime=dmy_hm(dateTime),
#          dateTime=date(dateTime)) %>%
#   group_by(dateTime, site)%>%
#   summarize_all(.funs=mean)
# glimpse(acton_C_fourmile)
# 
# acton_C_littlefourmile<-read.delim(here("data/metab_data_raw/Acton/ActonLake_streams_nutrients_littlefourmilecreek.txt") %>%
#   select(site:SRP_ugL)%>%
#   mutate(dateTime=dmy_hm(dateTime),
#          dateTime=date(dateTime)) %>%
#   group_by(dateTime, site)%>%
#   summarize_all(.funs=mean)
# glimpse(acton_C_littlefourmile)
# 
# acton_marshall_load<-right_join(acton_C_marshall, acton_Q_marshall, by=c("dateTime","site")) %>%
#   mutate(dateTime=ymd_hms(paste(dateTime, "12:00:00"))) %>%
#   rename(datetime=dateTime,
#          flow=discharge) %>%
#   relocate(flow, .before = TN_ugL) %>% #rearranging so flow is after datetime for load estimation software
#   mutate(TP_mgL=TP_ugL/1000,
#          SRP_mgL=SRP_ugL/1000,
#          TN_mgL=TN_ugL/1000) %>% #Convert anything in ug/L to mg/L (==g/L3)
#   select(-TP_ugL, -SRP_ugL, -TN_ugL) %>%
#   arrange(datetime)
# glimpse(acton_marshall_load)
# 
# acton_littlefourmile_load<-right_join(acton_C_littlefourmile, acton_Q_littlefourmile, by=c("dateTime","site")) %>%
#   mutate(dateTime=ymd_hms(paste(dateTime, "12:00:00"))) %>%
#   rename(datetime=dateTime,
#          flow=discharge) %>%
#   relocate(flow, .before = TN_ugL) %>% #rearranging so flow is after datetime for load estimation software
#   mutate(TP_mgL=TP_ugL/1000,
#          SRP_mgL=SRP_ugL/1000,
#          TN_mgL=TN_ugL/1000) %>% #Convert anything in ug/L to mg/L (==g/L3)
#   select(-TP_ugL, -SRP_ugL, -TN_ugL) 
# glimpse(acton_littlefourmile_load)
# 
# acton_fourmile_load<-right_join(acton_C_fourmile, acton_Q_fourmile, by=c("dateTime","site")) %>%
#   mutate(dateTime=ymd_hms(paste(dateTime, "12:00:00"))) %>%
#   rename(datetime=dateTime,
#          flow=discharge) %>%
#   relocate(flow, .before = TN_ugL) %>% #rearranging so flow is after datetime for load estimation software
#   mutate(TP_mgL=TP_ugL/1000,
#          SRP_mgL=SRP_ugL/1000,
#          TN_mgL=TN_ugL/1000) %>% #Convert anything in ug/L to mg/L (==g/L3)
#   select(-TP_ugL, -SRP_ugL, -TN_ugL) 
# glimpse(acton_fourmile_load)
# 

## ~Taupo loads -------------------------------------------------

Taupo_Q_Hinemaiaia<-read.delim(here("data/metab_data_raw/Taupo/Taupo_Hinemaiaia_discharge.txt")) %>%
  mutate(dateTime=ymd(dateTime)) 
# glimpse(Taupo_Q_Hinemaiaia)

Taupo_Q_Kuratau<-read.delim(here("data/metab_data_raw/Taupo/Taupo_Kuratau_discharge.txt")) %>%
  mutate(dateTime=ymd(dateTime)) 
# glimpse(Taupo_Q_Kuratau)

Taupo_Q_Tauranga<-read.delim(here("data/metab_data_raw/Taupo/Taupo_Tauranga-Taupo-River_discharge.txt")) %>%
  mutate(dateTime=dmy(dateTime),
         discharge=as.numeric(discharge)) 
# glimpse(Taupo_Q_Tauranga)

Taupo_Q_TokaanuPowerStation<-read.delim(here("data/metab_data_raw/Taupo/Taupo_TokaanuPowerStation_discharge.txt")) %>%
  mutate(dateTime=ymd(dateTime))
# glimpse(Taupo_Q_TokaanuPowerStation)

Taupo_Q_Tongariro<-read.delim(here("data/metab_data_raw/Taupo/Taupo_Tongariro_discharge.txt")) %>%
  mutate(dateTime=ymd(dateTime)) 
# glimpse(Taupo_Q_Tongariro)

Taupo_Q_Whangamata<-read.delim(here("data/metab_data_raw/Taupo/Taupo_Whangamata_discharge.txt")) %>%
  mutate(dateTime=ymd_hms(dateTime),
         dateTime=date(dateTime),
         site = replace(site, site=="Whangamata Stm ", 'Whangamata')) %>%
  drop_na()
# glimpse(Taupo_Q_Whangamata)

Taupo_Q_Whareroa<-read.delim(here("data/metab_data_raw/Taupo/Taupo_Whareroa_discharge.txt")) %>%
  mutate(dateTime=ymd(dateTime)) 
# glimpse(Taupo_Q_Whareroa)


Taupo_C_Hinemaiaia<-read.delim(here("data/metab_data_raw/Taupo/Taupo_Hinemaiaia_nutrient.txt")) %>%
  select(streamID, dateTime, TP, TN, DOC)%>%
  mutate(dateTime=ymd_hms(dateTime),
         dateTime=date(dateTime),
         streamID = replace(streamID, streamID=="Hinemaiaia River", 'Hinemaiaia'),
         DOC = replace(DOC, is.na(DOC), mean(DOC, na.rm=TRUE))) #replacing with measured value 
# glimpse(Taupo_C_Hinemaiaia)

Taupo_C_Kuratau<-read.delim(here("data/metab_data_raw/Taupo/Taupo_Kuratau_nutrient.txt")) %>%
  select(streamID, dateTime, TP, TN, DOC)%>%
  mutate(dateTime=ymd_hms(dateTime),
         dateTime=date(dateTime),
         streamID = replace(streamID, streamID=="Kuratau River", 'Kuratau'),
         DOC = replace(DOC, is.na(DOC), mean(DOC, na.rm=TRUE))) #replacing with measured value
# glimpse(Taupo_C_Kuratau)

Taupo_C_Tauranga<-read.delim(here("data/metab_data_raw/Taupo/Taupo_Tauranga-Taupo-River_nutrient.txt")) %>%
  select(streamID, dateTime, TP, TN, DOC)%>%
  mutate(dateTime=ymd_hms(dateTime),
         dateTime=date(dateTime),
         DOC = replace(DOC, is.na(DOC), mean(DOC, na.rm=TRUE))) #replacing with measured value
# glimpse(Taupo_C_Tauranga)

Taupo_C_Waihaha<-read.delim(here("data/metab_data_raw/Taupo/Taupo_Waihaha_nutrient.txt")) %>%
  select(streamID, dateTime, TP, TN, DOC)%>%
  mutate(dateTime=ymd_hms(dateTime),
         dateTime=date(dateTime),
         DOC = replace(DOC, is.na(DOC), mean(DOC, na.rm=TRUE))) #replacing with measured value
# glimpse(Taupo_C_Waihaha)

Taupo_C_Tongariro<-read.delim(here("data/metab_data_raw/Taupo/Taupo_Tongariro_nutrient.txt")) %>%
  select(streamID, dateTime, TP, TN, DOC)%>%
  mutate(dateTime=ymd_hms(dateTime),
         dateTime=date(dateTime),
         streamID = replace(streamID, streamID=="Tongariro River", 'Tongariro')) 
# glimpse(Taupo_C_Tongariro)

Taupo_C_Whangamata<-read.delim(here("data/metab_data_raw/Taupo/Taupo_Whangamata_nutrient.txt")) %>%
  select(streamID, dateTime, TP, TN, DOC)%>%
  mutate(dateTime=ymd_hms(dateTime),
         dateTime=date(dateTime),
         streamID = replace(streamID, streamID=="Whangamata Stm", 'Whangamata')) 
# glimpse(Taupo_C_Whangamata)

Taupo_C_TokaanuPowerStation<-read.delim(here("data/metab_data_raw/Taupo/Taupo_Tokaanu-Power-Station_nutrient.txt")) %>%
  select(streamID, dateTime, TP, TN, DOC)%>%
  mutate(dateTime=ymd_hms(dateTime),
         dateTime=date(dateTime))
# glimpse(Taupo_C_TokaanuPowerStation)


Taupo_C_Whareroa<-read.delim(here("data/metab_data_raw/Taupo/Taupo_Whareroa_nutrient.txt")) %>%
  select(streamID, dateTime, TP, TN, DOC)%>%
  mutate(dateTime=ymd_hms(dateTime),
         dateTime=date(dateTime)) 
# glimpse(Taupo_C_Whareroa)

Taupo_C_Mapara<-read.delim(here("data/metab_data_raw/Taupo/Taupo_Mapara_nutrient.txt")) %>%
  select(streamID, dateTime, TP, TN, DOC)%>%
  mutate(dateTime=ymd_hms(dateTime),
         dateTime=date(dateTime),
         DOC = replace(DOC, is.na(DOC), mean(DOC, na.rm=TRUE))) #replacing with measured value
# glimpse(Taupo_C_Mapara)

Taupo_C_Waihaha<-read.delim(here("data/metab_data_raw/Taupo/Taupo_Waihaha_nutrient.txt")) %>%
  select(streamID, dateTime, TP, TN, DOC)%>%
  mutate(dateTime=ymd_hms(dateTime),
         dateTime=date(dateTime),
         DOC = replace(DOC, is.na(DOC), mean(DOC, na.rm=TRUE))) #replacing with measured value
# glimpse(Taupo_C_Waihaha)

Taupo_C_Whanganui<-read.delim(here("data/metab_data_raw/Taupo/Taupo_Whanganui_nutrient.txt")) %>%
  select(streamID, dateTime, TP, TN, DOC)%>%
  mutate(dateTime=ymd_hms(dateTime),
         dateTime=date(dateTime)) 
# glimpse(Taupo_C_Whanganui)

Taupo_C_Waitahanui<-read.delim(here("data/metab_data_raw/Taupo/Taupo_Waitahanui_nutrient.txt")) %>%
  select(streamID, dateTime, TP, TN, DOC)%>%
  mutate(dateTime=ymd_hms(dateTime),
         dateTime=date(dateTime),
         DOC = replace(DOC, is.na(DOC), mean(DOC, na.rm=TRUE))) #replacing with measured value
# glimpse(Taupo_C_Waitahanui)



Taupo_Hinemaiaia_load<-right_join(Taupo_C_Hinemaiaia, Taupo_Q_Hinemaiaia, by=c("dateTime",
                                                                               "streamID"="site")) %>%
  mutate(dateTime=ymd_hms(paste(dateTime, "12:00:00"))) %>%
  rename(datetime=dateTime,
         flow=discharge) %>%
  relocate(flow, .before = TP) %>% #rearranging so flow is after datetime for load estimation software
  mutate(TP_mgL=TP/1000,
         DOC_mgL=DOC,
         TN_mgL=TN/1000) %>% #Convert anything in ug/L to mg/L (==g/L3)
  select(-TP, -TN, -DOC) %>%
  arrange(datetime)
# glimpse(Taupo_Hinemaiaia_load)


Taupo_Tauranga_load<-right_join(Taupo_C_Tauranga, Taupo_Q_Tauranga, by=c("dateTime",
                                                                         "streamID"="site")) %>%
  mutate(dateTime=ymd_hms(paste(dateTime, "12:00:00"))) %>%
  rename(datetime=dateTime,
         flow=discharge) %>%
  relocate(flow, .before = TP) %>% #rearranging so flow is after datetime for load estimation software
  mutate(TP_mgL=TP/1000,
         DOC_mgL=DOC,
         TN_mgL=TN/1000) %>% #Convert anything in ug/L to mg/L (==g/L3)
  select(-TP, -TN, -DOC) %>%
  mutate(flow=as.numeric(flow))
# glimpse(Taupo_Tauranga_load)

Taupo_Kuratau_load<-right_join(Taupo_C_Kuratau, Taupo_Q_Kuratau, by=c("dateTime",
                                                                      "streamID"="site")) %>%
  mutate(dateTime=ymd_hms(paste(dateTime, "12:00:00"))) %>%
  rename(datetime=dateTime,
         flow=discharge) %>%
  relocate(flow, .before = TP) %>% #rearranging so flow is after datetime for load estimation software
  mutate(TP_mgL=TP/1000,
         DOC_mgL=DOC,
         TN_mgL=TN/1000) %>% #Convert anything in ug/L to mg/L (==g/L3)
  select(-TP, -TN, -DOC) 
# glimpse(Taupo_Kuratau_load)


Taupo_TokaanuPowerStation_load<-right_join(Taupo_C_TokaanuPowerStation,
                                           Taupo_Q_TokaanuPowerStation, by=c("dateTime",
                                                                             "streamID"="site")) %>%
  mutate(dateTime=ymd_hms(paste(dateTime, "12:00:00"))) %>%
  rename(datetime=dateTime,
         flow=discharge) %>%
  relocate(flow, .before = TP) %>% #rearranging so flow is after datetime for load estimation software
  mutate(TP_mgL=TP/1000,
         DOC_mgL=DOC,
         TN_mgL=TN/1000) %>% #Convert anything in ug/L to mg/L (==g/L3)
  select(-TP, -TN, -DOC) 
# glimpse(Taupo_TokaanuPowerStation_load)

Taupo_Tongariro_load<-right_join(Taupo_C_Tongariro, Taupo_Q_Tongariro, by=c("dateTime",
                                                                            "streamID"="site")) %>%
  mutate(dateTime=ymd_hms(paste(dateTime, "12:00:00"))) %>%
  rename(datetime=dateTime,
         flow=discharge) %>%
  relocate(flow, .before = TP) %>% #rearranging so flow is after datetime for load estimation software
  mutate(TP_mgL=TP/1000,
         DOC_mgL=DOC,
         TN_mgL=TN/1000) %>% #Convert anything in ug/L to mg/L (==g/L3)
  select(-TP, -TN, -DOC) 
# glimpse(Taupo_Tongariro_load)


Taupo_Whangamata_load<-full_join(Taupo_C_Whangamata, Taupo_Q_Whangamata, by=c("dateTime",
                                                                              "streamID"="site")) %>%
  mutate(dateTime=ymd_hms(paste(dateTime, "12:00:00"))) %>%
  rename(datetime=dateTime,
         flow=discharge) %>%
  relocate(flow, .before = TP) %>% #rearranging so flow is after datetime for load estimation software
  mutate(TP_mgL=TP/1000,
         DOC_mgL=DOC,
         TN_mgL=TN/1000) %>% #Convert anything in ug/L to mg/L (==g/L3)
  select(-TP, -TN, -DOC) 
# glimpse(Taupo_Whangamata_load)

Taupo_Whareroa_load<-right_join(Taupo_C_Whareroa, Taupo_Q_Whareroa, by=c("dateTime",
                                                                         "streamID"="site")) %>%
  mutate(dateTime=ymd_hms(paste(dateTime, "12:00:00"))) %>%
  rename(datetime=dateTime,
         flow=discharge) %>%
  relocate(flow, .before = TP) %>% #rearranging so flow is after datetime for load estimation software
  mutate(TP_mgL=TP/1000,
         DOC_mgL=DOC,
         TN_mgL=TN/1000) %>% #Convert anything in ug/L to mg/L (==g/L3)
  select(-TP, -TN, -DOC) %>%
  mutate(flow=as.numeric(flow))
# glimpse(Taupo_Whareroa_load)

Taupo_Mapara_load<- Taupo_C_Mapara %>%
  mutate(dateTime=ymd_hms(paste(dateTime, "12:00:00")),
         discharge=0.33) %>% #assuming average flow for 2015-2019
  rename(datetime=dateTime,
         flow=discharge) %>%
  relocate(flow, .before = TP) %>% #rearranging so flow is after datetime for load estimation software
  mutate(TP_mgL=TP/1000,
         DOC_mgL=DOC,
         TN_mgL=TN/1000) %>% #Convert anything in ug/L to mg/L (==g/L3)
  select(-TP, -TN, -DOC)
# glimpse(Taupo_Mapara_load)

Taupo_Waihaha_load<- Taupo_C_Waihaha %>%
  mutate(dateTime=ymd_hms(paste(dateTime, "12:00:00")),
         discharge=0.44) %>% #assuming average flow for 2015-2019
  rename(datetime=dateTime,
         flow=discharge) %>%
  relocate(flow, .before = TP) %>% #rearranging so flow is after datetime for load estimation software
  mutate(TP_mgL=TP/1000,
         DOC_mgL=DOC,
         TN_mgL=TN/1000) %>% #Convert anything in ug/L to mg/L (==g/L3)
  select(-TP, -TN, -DOC)
# glimpse(Taupo_Waihaha_load)

Taupo_Whanganui_load<- Taupo_C_Whanganui %>%
  mutate(dateTime=ymd_hms(paste(dateTime, "12:00:00")),
         discharge=2.30) %>% #assuming average flow for 2015-2019
  rename(datetime=dateTime,
         flow=discharge) %>%
  relocate(flow, .before = TP) %>% #rearranging so flow is after datetime for load estimation software
  mutate(TP_mgL=TP/1000,
         DOC_mgL=DOC,
         TN_mgL=TN/1000) %>% #Convert anything in ug/L to mg/L (==g/L3)
  select(-TP, -TN, -DOC)
# glimpse(Taupo_Whanganui_load)

Taupo_Waihaha_load<- Taupo_C_Waihaha %>%
  mutate(dateTime=ymd_hms(paste(dateTime, "12:00:00")),
         discharge=5.98) %>% #assuming average flow for 2015-2019
  rename(datetime=dateTime,
         flow=discharge) %>%
  relocate(flow, .before = TP) %>% #rearranging so flow is after datetime for load estimation software
  mutate(TP_mgL=TP/1000,
         DOC_mgL=DOC,
         TN_mgL=TN/1000) %>% #Convert anything in ug/L to mg/L (==g/L3)
  select(-TP, -TN, -DOC)
# glimpse(Taupo_Waihaha_load)

Taupo_Waitahanui_load<- Taupo_C_Waitahanui %>%
  mutate(dateTime=ymd_hms(paste(dateTime, "12:00:00")),
         discharge=7.93) %>% #assuming average flow for 2015-2019
  rename(datetime=dateTime,
         flow=discharge) %>%
  relocate(flow, .before = TP) %>% #rearranging so flow is after datetime for load estimation software
  mutate(TP_mgL=TP/1000,
         DOC_mgL=DOC,
         TN_mgL=TN/1000) %>% #Convert anything in ug/L to mg/L (==g/L3)
  select(-TP, -TN, -DOC)
# glimpse(Taupo_Waitahanui_load)

#Filter out some observations for lake Taupo so we are left with spring-fall data of one year
#Hinemaiaia
dontuse <- subset(Taupo_Hinemaiaia_load, datetime < "2017-10-01" | datetime > "2018-05-01")
Taupo_Hinemaiaia_load<-anti_join(Taupo_Hinemaiaia_load, dontuse)
#Tauranga
dontuse <- subset(Taupo_Tauranga_load, datetime < "2017-10-01" | datetime > "2018-05-01")
Taupo_Tauranga_load<-anti_join(Taupo_Tauranga_load, dontuse)
#Kuratau
dontuse <- subset(Taupo_Kuratau_load, datetime < "2017-10-01" | datetime > "2018-05-01")
Taupo_Kuratau_load<-anti_join(Taupo_Kuratau_load, dontuse)
#Waihaha 
dontuse <- subset(Taupo_Waihaha_load, datetime < "2017-10-01" | datetime > "2018-05-01")
Taupo_Waihaha_load<-anti_join(Taupo_Waihaha_load, dontuse)
#Tongariro
dontuse <- subset(Taupo_Tongariro_load, datetime < "2017-10-01" | datetime > "2018-05-01")
Taupo_Tongariro_load<-anti_join(Taupo_Tongariro_load, dontuse)
#Whareroa
dontuse <- subset(Taupo_Whareroa_load, datetime < "2017-10-01" | datetime > "2018-05-01")
Taupo_Whareroa_load<-anti_join(Taupo_Whareroa_load, dontuse)
#Whangamata
dontuse <- subset(Taupo_Whangamata_load, datetime < "2017-10-01" | datetime > "2018-05-01")
Taupo_Whangamata_load<-anti_join(Taupo_Whangamata_load, dontuse)
#Mapara
dontuse <- subset(Taupo_Mapara_load, datetime < "2017-10-01" | datetime > "2018-05-01")
Taupo_Mapara_load<-anti_join(Taupo_Mapara_load, dontuse)
#Waihaha
dontuse <- subset(Taupo_Waihaha_load, datetime < "2017-10-01" | datetime > "2018-05-01")
Taupo_Waihaha_load<-anti_join(Taupo_Waihaha_load, dontuse)
#Whanganui
dontuse <- subset(Taupo_Whanganui_load, datetime < "2017-10-01" | datetime > "2018-05-01")
Taupo_Whanganui_load<-anti_join(Taupo_Whanganui_load, dontuse)
#Waihaha
dontuse <- subset(Taupo_Waihaha_load, datetime < "2017-10-01" | datetime > "2018-05-01")
Taupo_Waihaha_load<-anti_join(Taupo_Waihaha_load, dontuse)
#Waitahanui
dontuse <- subset(Taupo_Waitahanui_load, datetime < "2017-10-01" | datetime > "2018-05-01")
Taupo_Waitahanui_load<-anti_join(Taupo_Waitahanui_load, dontuse)
#Tokaanu Power Station
dontuse <- subset(Taupo_TokaanuPowerStation_load, datetime < "2017-10-01" | datetime > "2018-05-01")
Taupo_TokaanuPowerStation_load<-anti_join(Taupo_TokaanuPowerStation_load, dontuse)

## ~Loch loads -------------------------------------------------

Loch_Q_inlet<-read.csv("~/Google Drive/My Drive/Research (common)/Research/Data/R/LVWS-long-term-data/data_clean/metabolism/LochInOut_Q_UVs_CY2015_17.csv", stringsAsFactors = F) %>%
  rename(inlet_ft3s = X401733105392400,
         outlet_ft3s = X401733105392404) %>%
  mutate(
    datetime = mdy_hms(datetime, tz='MST'),
    datetime = date(datetime),
    inlet = inlet_ft3s / 35.314666212661, #convert to m3/s
    outlet = outlet_ft3s / 35.314666212661) %>%
  select(-inlet_ft3s, -outlet_ft3s ) %>%
  group_by(datetime)%>%
  summarize(flow=mean(inlet, na.rm=TRUE)) %>%
  filter(datetime>"2016-06-01" & datetime < "2016-09-01")
# glimpse(Loch_Q_inlet) 


##############Loch 
Loch_C_inlet <- read.csv("~/Google Drive/My Drive/Research (common)/Research/Data/R/LVWS-long-term-data/data_clean/surface_chem/LVWS_surfacewaterchemistry_master_180211.csv", stringsAsFactors = F)
# glimpse(Loch_C_inlet)
Loch_C_inlet$YEAR <- as.factor(as.character(Loch_C_inlet$YEAR))
Loch_C_inlet$MONTH <- as.numeric(Loch_C_inlet$MONTH)
Loch_C_inlet$DAY <- as.factor(as.character(Loch_C_inlet$DAY))
Loch_C_inlet$DATE <- ymd(as.character(Loch_C_inlet$DATE))

Loch_C_inlet <- Loch_C_inlet %>%
  mutate(SEASON = 
           ifelse(MONTH %in% c(12, 1, 2), "WINTER",
                  ifelse(MONTH %in% c(3, 4, 5), "SPRING",
                         ifelse(MONTH %in% c(6, 7, 8), "SUMMER",
                                ifelse(MONTH %in% c(9, 10, 11), "FALL", "ERROR"))))) %>%
  filter(YEAR=="2016" & SEASON=="SUMMER" ) %>%
  filter(SITE %in% c("LOCH.I")) %>%
  filter(TYPE=="NORMAL"|TYPE=="DUPE") %>%
  rename(DOC_mgL=DOC,
         TN_mgL=TDN,
         TP_ugL=TP_HSWL,
         datetime=DATE) %>%
  mutate(TP_mgL=TP_ugL/1000)%>%
  select(datetime, DOC_mgL, TN_mgL, TP_mgL) 
# glimpse(Loch_C_inlet)



Loch_inlet_load<-right_join(Loch_C_inlet, Loch_Q_inlet, 
                            by=c("datetime")) %>%
  mutate(datetime=ymd_hms(paste(datetime, "12:00:00"))) %>%
  # rename(datetime=dateTime,
  #        flow=discharge) %>%
  relocate(flow, .before = DOC_mgL) %>% #rearranging so flow is after datetime for load estimation software
  arrange(datetime)






# Interpolations & load estimates -----------------------------------------


# ~ Zwart lakes ------------------------------------------------------------

# Units for nutrient load files = kg/day for TP, TN, DOC and m3/s for inflow
zwart_load_trim <- zwart_load %>%
  select(lake, doy, date, inflow, TN_load, TP_load, DOC_load) %>%
  # filter(!lake %in% c("Acton")) %>%#exclude Acton because we have new data
  rename(lakeName=lake)


# zwart_load_summary<-zwart_load_trim %>%
#   filter(doy >= 121 & doy <= 274) %>%
#   group_by(lakeName) %>%
#   summarize(DOC_load_kg_total=sum(DOC_load, na.rm=TRUE),
#             TP_load_kg_total=sum(TP_load, na.rm=TRUE),
#             TN_load_kg_total=sum(TN_load, na.rm=TRUE))%>%
#   mutate(DOC_TP_massmass= DOC_load_kg_total/TP_load_kg_total) 


zwart_load_summary <- zwart_load %>%
  filter(doy >= 121 & doy <= 274) %>%
  select(lake, doy, date, inflow, TN_load, TP_load, DOC_load) %>%
  rename(lakeName=lake) %>%
  group_by(lakeName) %>%
  summarize(DOC_load=sum(DOC_load, na.rm=TRUE)*(1000), #kg/day  * 1000g/1kg = [g/day]
            TP_load=sum(TP_load, na.rm=TRUE)*(1000000),  #kg/day * 1000g/1kg = [mg/day]
            Qin=sum(inflow, na.rm=TRUE)*86400,
            n_days=n()) %>%  # [m3/s] * 86400s/1day = [m3/day]
  mutate(DOC_gm3=(DOC_load/Qin), #Flow weighted inflow DOC concentration from doy 121-274
         TP_mgm3=(TP_load/Qin)) #Flow weighted inflow TP concentration from doy 121-274



# ~ Mueggelsee --------------------------------------------------------------



#Trim data to same time period as metabolism
mueggelsee_load_trim <- mueggelsee_load %>%
  filter(datetime >= "2015-05-01" & datetime <= "2015-10-01")


#Calculate loads
mueggelsee_load_manual<- imputeTS::na_interpolation(mueggelsee_load_trim, "linear") %>%
  arrange(datetime) %>%
  select(datetime, flow, DOC_mgL, TP_mgL, TN_mgL) %>%
  mutate(DOC_load_kgday=DOC_mgL*flow*86400*0.001,
         TP_load_kgday=TP_mgL*flow*86400*0.001,
         TN_load_kgday=TN_mgL*flow*86400*0.001) %>%  #mg/L = g/m3. So m3 cancels out and you multiply by 86400 s in a day and divide by 1000g per kg to day kg/day. 
  mutate(lakeName="Mueggelsee")

#Totals and ratio
mueggelsee_load_summary<-mueggelsee_load_manual%>%
  group_by(lakeName)%>%
  summarize(DOC_load=sum(DOC_load_kgday, na.rm=TRUE)*(1000), #kg/day  * 1000g/1kg = [g/day]
            TP_load=sum(TP_load_kgday, na.rm=TRUE)*(1000000),  #kg/day * 1000g/1kg = [mg/day]
            TN_load=sum(TN_load_kgday, na.rm=TRUE)*(1000000),  #kg/day * 1000g/1kg = [mg/day]
            Qin=sum(flow, na.rm=TRUE)*86400,
            n_days=n()) %>%  # [m3/s] * 86400s/1day = [m3/day]
  mutate(DOC_gm3=(DOC_load/Qin), #Flow weighted inflow DOC concentration from May1-Oct1
         TP_mgm3=(TP_load/Qin), #Flow weighted inflow TP concentration from May1-Oct1
         TN_mgm3=(TN_load/Qin))  #Flow weighted inflow TN concentration from May1-Oct1


# ~ The Loch --------------------------------------------------------------

#Trim data to same time period as metabolism
loch_load_trim <- Loch_inlet_load %>%
  filter(datetime >= "2016-05-01" & datetime <= "2016-10-01")


#Calculate loads
loch_load_manual<- imputeTS::na_interpolation(loch_load_trim, "linear") %>%
  arrange(datetime) %>%
  select(datetime, flow, DOC_mgL, TP_mgL, TN_mgL) %>%
  mutate(DOC_load_kgday=DOC_mgL*flow*86400*0.001,
         TP_load_kgday=TP_mgL*flow*86400*0.001,
         TN_load_kgday=TN_mgL*flow*86400*0.001) %>%  #mg/L = g/m3. So m3 cancels out and you multiply by 86400 s in a day and divide by 1000g per kg to day kg/day. 
  mutate(lakeName="TheLoch")

#Totals and ratio
loch_load_summary<-loch_load_manual%>%
  group_by(lakeName)%>%
  summarize(DOC_load=sum(DOC_load_kgday, na.rm=TRUE)*(1000), #kg/day  * 1000g/1kg = [g/day]
            TP_load=sum(TP_load_kgday, na.rm=TRUE)*(1000000),  #kg/day * 1000g/1kg = [mg/day]
            TN_load=sum(TN_load_kgday, na.rm=TRUE)*(1000000),  #kg/day * 1000g/1kg = [mg/day]
            Qin=sum(flow, na.rm=TRUE)*86400,
            n_days=n()) %>%  # [m3/s] * 86400s/1day = [m3/day]
  mutate(DOC_gm3=(DOC_load/Qin), #Flow weighted inflow DOC concentration from May1-Oct1
         TP_mgm3=(TP_load/Qin), #Flow weighted inflow TP concentration from May1-Oct1
         TN_mgm3=(TN_load/Qin))  #Flow weighted inflow TN concentration from May1-Oct1




# ~ Erken --------------------------------------------------------------

#Trim data to same time period as metabolism
erken_load_trim <- erken_load %>%
  filter(datetime >= "2018-05-01" & datetime <= "2018-10-01")


#Calculate loads
erken_load_manual<- imputeTS::na_interpolation(erken_load_trim, "linear") %>%
  arrange(datetime) %>%
  select(datetime, flow, DOC_mgL, TP_mgL, TN_mgL) %>%
  mutate(DOC_load_kgday=DOC_mgL*flow*86400*0.001,
         TP_load_kgday=TP_mgL*flow*86400*0.001,
         TN_load_kgday=TN_mgL*flow*86400*0.001) %>%  #mg/L = g/m3. So m3 cancels out and you multiply by 86400 s in a day and divide by 1000g per kg to day kg/day. 
  mutate(lakeName="Erken")

#Totals and ratio
erken_load_summary<-erken_load_manual%>%
  group_by(lakeName)%>%
  summarize(DOC_load=sum(DOC_load_kgday, na.rm=TRUE)*(1000), #kg/day  * 1000g/1kg = [g/day]
            TP_load=sum(TP_load_kgday, na.rm=TRUE)*(1000000),  #kg/day * 1000g/1kg = [mg/day]
            TN_load=sum(TN_load_kgday, na.rm=TRUE)*(1000000),  #kg/day * 1000g/1kg = [mg/day]
            Qin=sum(flow, na.rm=TRUE)*86400,
            n_days=n()) %>%  # [m3/s] * 86400s/1day = [m3/day]
  mutate(DOC_gm3=(DOC_load/Qin), #Flow weighted inflow DOC concentration from May1-Oct1
         TP_mgm3=(TP_load/Qin), #Flow weighted inflow TP concentration from May1-Oct1
         TN_mgm3=(TN_load/Qin))  #Flow weighted inflow TN concentration from May1-Oct1

#  ~ Taupo ----------------------------------------------------------------


# --> Hinemaiaia ----------------------------------------------------------


#Trim data to same time period as metabolism
Taupo_Hinemaiaia_load_trim <- Taupo_Hinemaiaia_load %>%
  filter(datetime >= "2017-11-01" & datetime <= "2018-05-01")


#Calculate loads
Taupo_Hinemaiaia_load_manual<- imputeTS::na_interpolation(Taupo_Hinemaiaia_load_trim, "linear") %>%
  arrange(datetime) %>%
  select(datetime, flow, DOC_mgL, TP_mgL, TN_mgL) %>%
  mutate(DOC_load_kgday=DOC_mgL*flow*86400*0.001,
         TP_load_kgday=TP_mgL*flow*86400*0.001,
         TN_load_kgday=TN_mgL*flow*86400*0.001) %>%  #mg/L = g/m3. So m3 cancels out and you multiply by 86400 s in a day and divide by 1000g per kg to day kg/day. 
  mutate(lakeName="Taupo_Hinemaiaia")

#Totals and ratio
Taupo_Hinemaiaia_load_summary<-Taupo_Hinemaiaia_load_manual%>%
  group_by(lakeName)%>%
  summarize(DOC_load=sum(DOC_load_kgday, na.rm=TRUE)*(1000), #kg/day  * 1000g/1kg = [g/day]
            TP_load=sum(TP_load_kgday, na.rm=TRUE)*(1000000),  #kg/day * 1000g/1kg = [mg/day]
            TN_load=sum(TN_load_kgday, na.rm=TRUE)*(1000000),  #kg/day * 1000g/1kg = [mg/day]
            Qin=sum(flow, na.rm=TRUE)*86400,
            n_days=n()) %>%  # [m3/s] * 86400s/1day = [m3/day]
  mutate(DOC_gm3=(DOC_load/Qin), #Flow weighted inflow DOC concentration from Nov1-May1
         TP_mgm3=(TP_load/Qin), #Flow weighted inflow TP concentration from Nov1-May1
         TN_mgm3=(TN_load/Qin))  #Flow weighted inflow TN concentration from Nov1-May1

# --> Tauranga ------------------------------------------------------------


#Trim data to same time period as metabolism
Taupo_Tauranga_load_trim <- Taupo_Tauranga_load %>%
  filter(datetime >= "2017-11-01" & datetime <= "2018-05-01")


#Calculate loads
Taupo_Tauranga_load_manual<- imputeTS::na_interpolation(Taupo_Tauranga_load_trim, "linear") %>%
  arrange(datetime) %>%
  select(datetime, flow, DOC_mgL, TP_mgL, TN_mgL) %>%
  mutate(DOC_load_kgday=DOC_mgL*flow*86400*0.001,
         TP_load_kgday=TP_mgL*flow*86400*0.001,
         TN_load_kgday=TN_mgL*flow*86400*0.001) %>%  #mg/L = g/m3. So m3 cancels out and you multiply by 86400 s in a day and divide by 1000g per kg to day kg/day. 
  mutate(lakeName="Taupo_Tauranga")

#Totals and ratio
Taupo_Tauranga_load_summary<-Taupo_Tauranga_load_manual%>%
  group_by(lakeName)%>%
  summarize(DOC_load=sum(DOC_load_kgday, na.rm=TRUE)*(1000), #kg/day  * 1000g/1kg = [g/day]
            TP_load=sum(TP_load_kgday, na.rm=TRUE)*(1000000),  #kg/day * 1000g/1kg = [mg/day]
            TN_load=sum(TN_load_kgday, na.rm=TRUE)*(1000000),  #kg/day * 1000g/1kg = [mg/day]
            Qin=sum(flow, na.rm=TRUE)*86400,
            n_days=n()) %>%  # [m3/s] * 86400s/1day = [m3/day]
  mutate(DOC_gm3=(DOC_load/Qin), #Flow weighted inflow DOC concentration from Nov1-May1
         TP_mgm3=(TP_load/Qin), #Flow weighted inflow TP concentration from Nov1-May1
         TN_mgm3=(TN_load/Qin))  #Flow weighted inflow TN concentration from Nov1-May1 



# > Kuratau ---------------------------------------------------------------



#Trim data to same time period as metabolism
Taupo_Kuratau_load_trim <- Taupo_Kuratau_load %>%
  filter(datetime >= "2017-11-01" & datetime <= "2018-05-01")


#Calculate loads
Taupo_Kuratau_load_manual<- imputeTS::na_interpolation(Taupo_Kuratau_load_trim, "linear") %>%
  arrange(datetime) %>%
  select(datetime, flow, DOC_mgL, TP_mgL, TN_mgL) %>%
  mutate(DOC_load_kgday=DOC_mgL*flow*86400*0.001,
         TP_load_kgday=TP_mgL*flow*86400*0.001,
         TN_load_kgday=TN_mgL*flow*86400*0.001) %>%  #mg/L = g/m3. So m3 cancels out and you multiply by 86400 s in a day and divide by 1000g per kg to day kg/day. 
  mutate(lakeName="Taupo_Kuratau")

#Totals and ratio
Taupo_Kuratau_load_summary<-Taupo_Kuratau_load_manual%>%
  group_by(lakeName)%>%
  summarize(DOC_load=sum(DOC_load_kgday, na.rm=TRUE)*(1000), #kg/day  * 1000g/1kg = [g/day]
            TP_load=sum(TP_load_kgday, na.rm=TRUE)*(1000000),  #kg/day * 1000g/1kg = [mg/day]
            TN_load=sum(TN_load_kgday, na.rm=TRUE)*(1000000),  #kg/day * 1000g/1kg = [mg/day]
            Qin=sum(flow, na.rm=TRUE)*86400,
            n_days=n()) %>%  # [m3/s] * 86400s/1day = [m3/day]
  mutate(DOC_gm3=(DOC_load/Qin), #Flow weighted inflow DOC concentration from Nov1-May1
         TP_mgm3=(TP_load/Qin), #Flow weighted inflow TP concentration from Nov1-May1
         TN_mgm3=(TN_load/Qin))  #Flow weighted inflow TN concentration from Nov1-May1

# > Tokaanu Power Station (no DOC) ----------------------------------------


#Trim data to same time period as metabolism
Taupo_TokaanuPowerStation_load_trim <- Taupo_TokaanuPowerStation_load %>%
  filter(datetime >= "2017-11-01" & datetime <= "2018-05-01")


#Calculate loads
Taupo_TokaanuPowerStation_load_manual<- imputeTS::na_interpolation(Taupo_TokaanuPowerStation_load_trim, "linear") %>%
  arrange(datetime) %>%
  select(datetime, flow, DOC_mgL, TP_mgL, TN_mgL) %>%
  mutate(DOC_load_kgday=DOC_mgL*flow*86400*0.001,
         TP_load_kgday=TP_mgL*flow*86400*0.001,
         TN_load_kgday=TN_mgL*flow*86400*0.001) %>%  #mg/L = g/m3. So m3 cancels out and you multiply by 86400 s in a day and divide by 1000g per kg to day kg/day. 
  mutate(lakeName="Taupo_TokaanuPowerStation")

#Totals and ratio
Taupo_TokaanuPowerStation_load_summary<-Taupo_TokaanuPowerStation_load_manual%>%
  group_by(lakeName)%>%
  summarize(DOC_load=NA, #kg/day  * 1000g/1kg = [g/day]
            TP_load=sum(TP_load_kgday, na.rm=TRUE)*(1000000),  #kg/day * 1000g/1kg = [mg/day]
            TN_load=sum(TN_load_kgday, na.rm=TRUE)*(1000000),  #kg/day * 1000g/1kg = [mg/day]
            Qin=sum(flow, na.rm=TRUE)*86400,
            n_days=n()) %>%  # [m3/s] * 86400s/1day = [m3/day]
  mutate(DOC_gm3=NA, #Flow weighted inflow DOC concentration from Nov1-May1
         TP_mgm3=(TP_load/Qin), #Flow weighted inflow TP concentration from Nov1-May1
         TN_mgm3=(TN_load/Qin))  #Flow weighted inflow TN concentration from Nov1-May1


# > Tongariro (no DOC) ----------------------------------------------------


#Trim data to same time period as metabolism
Taupo_Tongariro_load_trim <- Taupo_Tongariro_load %>%
  filter(datetime >= "2017-11-01" & datetime <= "2018-05-01")


#Calculate loads
Taupo_Tongariro_load_manual<- imputeTS::na_interpolation(Taupo_Tongariro_load_trim, "linear") %>%
  arrange(datetime) %>%
  select(datetime, flow, DOC_mgL, TP_mgL, TN_mgL) %>%
  mutate(DOC_load_kgday=DOC_mgL*flow*86400*0.001,
         TP_load_kgday=TP_mgL*flow*86400*0.001,
         TN_load_kgday=TN_mgL*flow*86400*0.001) %>%  #mg/L = g/m3. So m3 cancels out and you multiply by 86400 s in a day and divide by 1000g per kg to day kg/day. 
  mutate(lakeName="Taupo_Tongariro")

#Totals and ratio
Taupo_Tongariro_load_summary<-Taupo_Tongariro_load_manual%>%
  group_by(lakeName)%>%
  summarize(DOC_load=NA, #kg/day  * 1000g/1kg = [g/day]
            TP_load=sum(TP_load_kgday, na.rm=TRUE)*(1000000),  #kg/day * 1000g/1kg = [mg/day]
            TN_load=sum(TN_load_kgday, na.rm=TRUE)*(1000000),  #kg/day * 1000g/1kg = [mg/day]
            Qin=sum(flow, na.rm=TRUE)*86400,
            n_days=n()) %>%  # [m3/s] * 86400s/1day = [m3/day]
  mutate(DOC_gm3=NA, #Flow weighted inflow DOC concentration from Nov1-May1
         TP_mgm3=(TP_load/Qin), #Flow weighted inflow TP concentration from Nov1-May1
         TN_mgm3=(TN_load/Qin))  #Flow weighted inflow TN concentration from Nov1-May1


# > Whareroa (no DOC) -----------------------------------------------------


#Trim data to same time period as metabolism
Taupo_Whareroa_load_trim <- Taupo_Whareroa_load %>%
  filter(datetime >= "2017-11-01" & datetime <= "2018-05-01")


#Calculate loads
Taupo_Whareroa_load_manual<- imputeTS::na_interpolation(Taupo_Whareroa_load_trim, "linear") %>%
  arrange(datetime) %>%
  select(datetime, flow, DOC_mgL, TP_mgL, TN_mgL) %>%
  mutate(DOC_load_kgday=DOC_mgL*flow*86400*0.001,
         TP_load_kgday=TP_mgL*flow*86400*0.001,
         TN_load_kgday=TN_mgL*flow*86400*0.001) %>%  #mg/L = g/m3. So m3 cancels out and you multiply by 86400 s in a day and divide by 1000g per kg to day kg/day. 
  mutate(lakeName="Taupo_Whareroa")

#Totals and ratio
Taupo_Whareroa_load_summary<-Taupo_Whareroa_load_manual%>%
  group_by(lakeName)%>%
  summarize(DOC_load=NA, #kg/day  * 1000g/1kg = [g/day]
            TP_load=sum(TP_load_kgday, na.rm=TRUE)*(1000000),  #kg/day * 1000g/1kg = [mg/day]
            TN_load=sum(TN_load_kgday, na.rm=TRUE)*(1000000),  #kg/day * 1000g/1kg = [mg/day]
            Qin=sum(flow, na.rm=TRUE)*86400,
            n_days=n()) %>%  # [m3/s] * 86400s/1day = [m3/day]
  mutate(DOC_gm3=NA, #Flow weighted inflow DOC concentration from Nov1-May1
         TP_mgm3=(TP_load/Qin), #Flow weighted inflow TP concentration from Nov1-May1
         TN_mgm3=(TN_load/Qin))  #Flow weighted inflow TN concentration from Nov1-May1


# > Whangamata (mod Q, no DOC) --------------------------------------------



#Trim data to same time period as metabolism
Taupo_Whangamata_load_trim <- Taupo_Whangamata_load %>%
  filter(datetime >= "2017-11-01" & datetime <= "2018-05-01") %>%
  mutate(flow = replace(flow, is.na(flow), 0.09)) #replacing with modelled mean


#Calculate loads
Taupo_Whangamata_load_manual<- imputeTS::na_interpolation(Taupo_Whangamata_load_trim, "linear") %>%
  arrange(datetime) %>%
  select(datetime, flow, DOC_mgL, TP_mgL, TN_mgL) %>%
  mutate(DOC_load_kgday=DOC_mgL*flow*86400*0.001,
         TP_load_kgday=TP_mgL*flow*86400*0.001,
         TN_load_kgday=TN_mgL*flow*86400*0.001) %>%  #mg/L = g/m3. So m3 cancels out and you multiply by 86400 s in a day and divide by 1000g per kg to day kg/day. 
  mutate(lakeName="Taupo_Whangamata")

#Totals and ratio
Taupo_Whangamata_load_summary<-Taupo_Whangamata_load_manual%>%
  group_by(lakeName)%>%
  summarize(DOC_load=NA, #kg/day  * 1000g/1kg = [g/day]
            TP_load=sum(TP_load_kgday, na.rm=TRUE)*(1000000),  #kg/day * 1000g/1kg = [mg/day]
            TN_load=sum(TN_load_kgday, na.rm=TRUE)*(1000000),  #kg/day * 1000g/1kg = [mg/day]
            Qin=sum(flow, na.rm=TRUE)*86400,
            n_days=n()) %>%  # [m3/s] * 86400s/1day = [m3/day]
  mutate(DOC_gm3=NA, #Flow weighted inflow DOC concentration from Nov1-May1
         TP_mgm3=(TP_load/Qin), #Flow weighted inflow TP concentration from Nov1-May1
         TN_mgm3=(TN_load/Qin))  #Flow weighted inflow TN concentration from Nov1-May1



# > Mapara (mod Q) --------------------------------------------------------


#Trim data to same time period as metabolism
Taupo_Mapara_load_trim <- Taupo_Mapara_load %>%
  filter(datetime >= "2017-11-01" & datetime <= "2018-05-01") 


#Calculate loads
Taupo_Mapara_load_manual<- imputeTS::na_interpolation(Taupo_Mapara_load_trim, "linear") %>%
  arrange(datetime) %>%
  select(datetime, flow, DOC_mgL, TP_mgL, TN_mgL) %>%
  mutate(DOC_load_kgday=DOC_mgL*flow*86400*0.001,
         TP_load_kgday=TP_mgL*flow*86400*0.001,
         TN_load_kgday=TN_mgL*flow*86400*0.001) %>%  #mg/L = g/m3. So m3 cancels out and you multiply by 86400 s in a day and divide by 1000g per kg to day kg/day. 
  mutate(lakeName="Taupo_Mapara")

#Totals and ratio
Taupo_Mapara_load_summary<-Taupo_Mapara_load_manual%>%
  group_by(lakeName)%>%
  summarize(DOC_load=sum(DOC_load_kgday, na.rm=TRUE)*(1000), #kg/day  * 1000g/1kg = [g/day]
            TP_load=sum(TP_load_kgday, na.rm=TRUE)*(1000000),  #kg/day * 1000g/1kg = [mg/day]
            TN_load=sum(TN_load_kgday, na.rm=TRUE)*(1000000),  #kg/day * 1000g/1kg = [mg/day]
            Qin=sum(flow, na.rm=TRUE)*86400,
            n_days=n()) %>%  # [m3/s] * 86400s/1day = [m3/day]
  mutate(DOC_gm3=(DOC_load/Qin), #Flow weighted inflow DOC concentration from Nov1-May1
         TP_mgm3=(TP_load/Qin), #Flow weighted inflow TP concentration from Nov1-May1
         TN_mgm3=(TN_load/Qin))  #Flow weighted inflow TN concentration from Nov1-May1




# > Waihaha stream (mod Q) ------------------------------------------------


#Trim data to same time period as metabolism
Taupo_Waihaha_load_trim <- Taupo_Waihaha_load %>%
  filter(datetime >= "2017-11-01" & datetime <= "2018-05-01") 


#Calculate loads
Taupo_Waihaha_load_manual<- imputeTS::na_interpolation(Taupo_Waihaha_load_trim, "linear") %>%
  arrange(datetime) %>%
  select(datetime, flow, DOC_mgL, TP_mgL, TN_mgL) %>%
  mutate(DOC_load_kgday=DOC_mgL*flow*86400*0.001,
         TP_load_kgday=TP_mgL*flow*86400*0.001,
         TN_load_kgday=TN_mgL*flow*86400*0.001) %>%  #mg/L = g/m3. So m3 cancels out and you multiply by 86400 s in a day and divide by 1000g per kg to day kg/day. 
  mutate(lakeName="Taupo_Waihaha")

#Totals and ratio
Taupo_Waihaha_load_summary<-Taupo_Waihaha_load_manual%>%
  group_by(lakeName)%>%
  summarize(DOC_load=sum(DOC_load_kgday, na.rm=TRUE)*(1000), #kg/day  * 1000g/1kg = [g/day]
            TP_load=sum(TP_load_kgday, na.rm=TRUE)*(1000000),  #kg/day * 1000g/1kg = [mg/day]
            TN_load=sum(TN_load_kgday, na.rm=TRUE)*(1000000),  #kg/day * 1000g/1kg = [mg/day]
            Qin=sum(flow, na.rm=TRUE)*86400,
            n_days=n()) %>%  # [m3/s] * 86400s/1day = [m3/day]
  mutate(DOC_gm3=(DOC_load/Qin), #Flow weighted inflow DOC concentration from Nov1-May1
         TP_mgm3=(TP_load/Qin), #Flow weighted inflow TP concentration from Nov1-May1
         TN_mgm3=(TN_load/Qin))  #Flow weighted inflow TN concentration from Nov1-May1



# > Whanganui (mod Q, no DOC) ---------------------------------------------


#Trim data to same time period as metabolism
Taupo_Whanganui_load_trim <- Taupo_Whanganui_load %>%
  filter(datetime >= "2017-11-01" & datetime <= "2018-05-01") 


#Calculate loads
Taupo_Whanganui_load_manual<- imputeTS::na_interpolation(Taupo_Whanganui_load_trim, "linear") %>%
  arrange(datetime) %>%
  select(datetime, flow, DOC_mgL, TP_mgL, TN_mgL) %>%
  mutate(DOC_load_kgday=DOC_mgL*flow*86400*0.001,
         TP_load_kgday=TP_mgL*flow*86400*0.001,
         TN_load_kgday=TN_mgL*flow*86400*0.001) %>%  #mg/L = g/m3. So m3 cancels out and you multiply by 86400 s in a day and divide by 1000g per kg to day kg/day. 
  mutate(lakeName="Taupo_Whanganui")

#Totals and ratio
Taupo_Whanganui_load_summary<-Taupo_Whanganui_load_manual%>%
  group_by(lakeName)%>%
  summarize(DOC_load=NA, #kg/day  * 1000g/1kg = [g/day]
            TP_load=sum(TP_load_kgday, na.rm=TRUE)*(1000000),  #kg/day * 1000g/1kg = [mg/day]
            TN_load=sum(TN_load_kgday, na.rm=TRUE)*(1000000),  #kg/day * 1000g/1kg = [mg/day]
            Qin=sum(flow, na.rm=TRUE)*86400,
            n_days=n()) %>%  # [m3/s] * 86400s/1day = [m3/day]
  mutate(DOC_gm3=NA, #Flow weighted inflow DOC concentration from Nov1-May1
         TP_mgm3=(TP_load/Qin), #Flow weighted inflow TP concentration from Nov1-May1
         TN_mgm3=(TN_load/Qin))  #Flow weighted inflow TN concentration from Nov1-May1


# > Waitahanui (mod Q) ----------------------------------------------------


#Trim data to same time period as metabolism
Taupo_Waitahanui_load_trim <- Taupo_Waitahanui_load %>%
  filter(datetime >= "2017-11-01" & datetime <= "2018-05-01") 


#Calculate loads
Taupo_Waitahanui_load_manual<- imputeTS::na_interpolation(Taupo_Waitahanui_load_trim, "linear") %>%
  arrange(datetime) %>%
  select(datetime, flow, DOC_mgL, TP_mgL, TN_mgL) %>%
  mutate(DOC_load_kgday=DOC_mgL*flow*86400*0.001,
         TP_load_kgday=TP_mgL*flow*86400*0.001,
         TN_load_kgday=TN_mgL*flow*86400*0.001) %>%  #mg/L = g/m3. So m3 cancels out and you multiply by 86400 s in a day and divide by 1000g per kg to day kg/day. 
  mutate(lakeName="Taupo_Waitahanui")

#Totals and ratio
Taupo_Waitahanui_load_summary<-Taupo_Waitahanui_load_manual%>%
  group_by(lakeName)%>%
  summarize(DOC_load=sum(DOC_load_kgday, na.rm=TRUE)*(1000), #kg/day  * 1000g/1kg = [g/day]
            TP_load=sum(TP_load_kgday, na.rm=TRUE)*(1000000),  #kg/day * 1000g/1kg = [mg/day]
            TN_load=sum(TN_load_kgday, na.rm=TRUE)*(1000000),  #kg/day * 1000g/1kg = [mg/day]
            Qin=sum(flow, na.rm=TRUE)*86400,
            n_days=n()) %>%  # [m3/s] * 86400s/1day = [m3/day]
  mutate(DOC_gm3=(DOC_load/Qin), #Flow weighted inflow DOC concentration from Nov1-May1
         TP_mgm3=(TP_load/Qin), #Flow weighted inflow TP concentration from Nov1-May1
         TN_mgm3=(TN_load/Qin))  #Flow weighted inflow TN concentration from Nov1-May1


# > All Taupo tributaries -------------------------------------


Taupo_load_summary <- bind_rows(Taupo_Hinemaiaia_load_summary,
                                Taupo_Tauranga_load_summary,
                                Taupo_Kuratau_load_summary,
                                Taupo_TokaanuPowerStation_load_summary,
                                Taupo_Tongariro_load_summary,
                                Taupo_Whareroa_load_summary,
                                Taupo_Whangamata_load_summary,
                                Taupo_Mapara_load_summary,
                                Taupo_Waihaha_load_summary,
                                Taupo_Whanganui_load_summary,
                                Taupo_Waitahanui_load_summary) %>%
  mutate(lakeName="Taupo") %>%
  group_by(lakeName) %>%
  mutate(across(DOC_load:TN_mgm3, mean, na.rm=TRUE)) %>%
  distinct(lakeName, .keep_all = TRUE) #For some reason a million duplicates were showing up
  
  # summarize(DOC_load_kg_total=sum(DOC_load_kg_total, na.rm=TRUE),
  #           TP_load_kg_total=sum(TP_load_kg_total,na.rm=TRUE),
  #           TN_load_kg_total=sum(TN_load_kg_total, na.rm=TRUE),
  #           n_days=n(),
  #           DOC_load_kgday_mean=sum(DOC_load_kgday_mean, na.rm=TRUE),
  #           TP_load_kgday_mean=sum(TP_load_kgday_mean, na.rm=TRUE),
  #           TN_load_kgday_mean=sum( TN_load_kgday_mean, na.rm=TRUE)) %>%
  # mutate(DOC_TP_massmass_total = DOC_load_kg_total/TP_load_kg_total,
  #        DOC_TP_massmass_mean = DOC_load_kgday_mean/TN_load_kgday_mean) %>%


# Taupo_total_tribs<-Taupo_load_summary%>%
#   select(lakeName, contains("_total"))
# Taupo_load_tribs <- bind_rows(Taupo_Hinemaiaia_load_summary,
#                               Taupo_Tauranga_load_summary,
#                               Taupo_Kuratau_load_summary,
#                               Taupo_TokaanuPowerStation_load_summary,
#                               Taupo_Tongariro_load_summary,
#                               Taupo_Whareroa_load_summary,
#                               Taupo_Whangamata_load_summary,
#                               Taupo_Mapara_load_summary,
#                               Taupo_Waihaha_load_summary,
#                               Taupo_Whanganui_load_summary,
#                               Taupo_Waitahanui_load_summary) %>%
#   select(lakeName, contains("_total"))

# bind_rows(Taupo_load_tribs) %>% 
#   pivot_longer(DOC_load_kg_total:TN_load_kg_total)%>%
#   ggplot(aes(x=name, y=value, fill=name)) +
#   geom_bar(stat="identity",color="black") +
#   xlab("Nutrient") +
#   scale_y_continuous(expression(Total~nutrient~load~(kg)), labels = scales::comma)+
#   theme(axis.text.x=element_text(angle = 45, hjust = 1),
#         legend.position="none")+
#   scale_fill_manual(values=c("grey20","grey50","grey90"))+
#   # geom_text(label=round(Taupo_met6$value*10^-6,1), vjust=-1.5)+
#   ggtitle("Lake Taupo - individual tributaries")+
#   facet_wrap(.~lakeName, scales="free_y")
# 
# #Totals and ratio-- TABLE
# bind_rows(Taupo_total_tribs,Taupo_load_tribs) %>%
#   # select(lakeName, contains("_total"))%>%
#   mutate(`TP (kg)`=comma(TP_load_kg_total),
#          `TN (kg)`=comma(TN_load_kg_total),
#          `DOC (kg)`=comma(DOC_load_kg_total),
#          `DOC:TP (mass:mass)`= comma(DOC_TP_massmass_total))%>%
#   rename(`Total or individual inflows`=lakeName)%>%
#   select(-DOC_load_kg_total, -TP_load_kg_total, -TN_load_kg_total,  -DOC_TP_massmass_total) %>%
#   hux() %>% 
#   set_bold(row = 1, col = everywhere, value = TRUE) %>% 
#   set_all_borders(TRUE) %>%
#   theme_plain()


# Huisman load estimates --------------------------------------------------

# load_estimates_huisman<-read.csv(here('data/load_estimates/load_estimates_20210816.csv'), header=T,sep=",") #Version with the ORIGINAL Kelly model params
# load_estimates_huisman<-read.csv(here('data/load_estimates/load_estimates_20220726.csv'), header=T,sep=",") #Version with the updated parameter optimization by SEJ
# load_estimates_huisman<-read.csv(here('data/load_estimates/load_estimates_20220801_kellyset.csv'), header=T,sep=",") #Re-ran the kelly set load estimates on this day

# load_estimates_huisman<-load_estimates_huisman[1:2,]
# str(load_estimates_huisman)
# load_estimates_huisman <- load_estimates_huisman %>%
#   # mutate_if(is.character, as.numeric)
#   mutate_at(vars(-variable), as.numeric) %>%
#   pivot_longer(-variable) %>%
#   rename(lakeName=name)%>%
#   separate(lakeName, c("lakeName","delete"),"_") %>% #split into two columns by underscore ("_")
#   select(-delete) %>%
#   mutate(lakeName=  str_to_sentence(lakeName), #capitalize all the lake names if they aren't already
#          lakeName = replace(lakeName, lakeName=="Theloch", 'TheLoch'), #But still need to fix a few names
#          lakeName = replace(lakeName, lakeName=="Crystalbog", 'CrystalBog'),
#          lakeName = replace(lakeName, lakeName=="Fredriksburgslotso", 'FredriksburgSlotso'),
#          lakeName = replace(lakeName, lakeName=="Littlerock", 'LittleRock'),
#          lakeName = replace(lakeName, lakeName=="Skypond", 'SkyPond'),
#          lakeName = replace(lakeName, lakeName=="Stgribso", 'StGribso'),
#          lakeName = replace(lakeName, lakeName=="Troutbog", 'TroutBog'),
#          lakeName = replace(lakeName, lakeName=="Westlong", "WestLong"),
#          lakeName = replace(lakeName, lakeName=="Eastlong", "EastLong"),
#          lakeName = replace(lakeName, lakeName=="Yunyang", "YunYang"))%>%
#   pivot_wider(id_cols=lakeName, names_from=variable, values_from=value)

load_estimates_huisman<-read.csv(here('data/preliminaryMetropolisResults/ExtendedRangeParameter_outputWestimatedloads.csv'), header=T,sep=",") %>%
  select(lakeName,Pin,DOCin) %>%
  rename(TP_mgm3=Pin,
         DOC_gm3=DOCin)

modelled_loads_kg<-left_join(load_estimates_huisman,metadata %>%
                               select(lakeName,`Volume (m3)`, `Lake residence time (year)`), by="lakeName") %>%
  rename(HRT_days=`Lake residence time (year)`,
         V=`Volume (m3)`) %>%
  mutate(HRT_days=HRT_days*365,
         Qin=V/HRT_days,
         TP_load_kg=TP_mgm3*Qin*(1/1000000),
         DOC_load_kg=DOC_gm3*Qin*(1/1000),
         dataset="modelled")


observed_loads_kelly<-read.csv(here('data/preliminaryMetropolisResults/KellyParameter_outputWobservedloads.csv'), header=T,sep=",") %>%
  select(lakeName,Pin,DOCin) %>%
  rename(TP_mgm3=Pin,
         DOC_gm3=DOCin)
# 
# compare <- full_join(observed_loads_kelly%>%
#                        rename(TPin_obs=TP_mgm3,
#                               DOCin_obs=DOC_gm3),
#                      load_estimates_huisman%>%
#                        rename(TPin_mod=TP_mgm3,
#                               DOCin_mod=DOC_gm3)) %>%
#   drop_na()
# 
# compare %>%
#   ggplot(aes(x=TPin_obs, y=TPin_mod))+
#   geom_point()+
#   geom_abline(intercept=0, slope=1)
# 
# compare %>%
#   ggplot(aes(x=DOCin_obs, y=DOCin_mod))+
#   geom_point()+
#   geom_abline(intercept=0, slope=1)

# observed_loads_litRange<-read.csv(here('data/preliminaryMetropolisResults/LitRangeParameter_outputWobservedloads.csv'), header=T,sep=",") %>%
#   select(lakeName,Pin,DOCin) %>%
#   rename(TP_mgm3=Pin,
#          DOC_gm3=DOCin) 
# 
# compare2 <- full_join(observed_loads_kelly%>%
#                        rename(TPin_kelly=TP_mgm3,
#                               DOCin_kelly=DOC_gm3),
#                       observed_loads_litRange%>%
#                        rename(TPin_lit=TP_mgm3,
#                               DOCin_lit=DOC_gm3)) %>%
#   drop_na()
# 
# compare2 %>%
#   ggplot(aes(x=TPin_kelly, y=TPin_lit))+
#   geom_point()+
#   geom_abline(intercept=0, slope=1)
# 
# compare2 %>%
#   ggplot(aes(x=DOCin_kelly, y=DOCin_lit))+
#   geom_point()+
#   geom_abline(intercept=0, slope=1)

# MASTER LOADS ------------------------------------------------------------

##### This script does the same thing that I now do above.

#Flow weighted concentrations
# zwart_sum<-zwart_load %>%
#   rename(lakeName=lake)%>%
#   filter(doy >= 121 & doy <= 274) %>%
#   group_by(lakeName) %>%
#   summarize(DOC_load=sum(DOC_load, na.rm=TRUE), #kg/day 
#             TP_load=sum(TP_load, na.rm=TRUE), #kg/day
#             Qin=sum(inflow, na.rm=TRUE)) %>% #m3/s
#   mutate(DOC_gm3=(DOC_load/Qin)*(1000/86400),
#          TP_mgm3=(TP_load/Qin)*(1000/86400)*1000) 
# 
# mueggelsee_sum<-mueggelsee_load_manual %>%
#   group_by(lakeName) %>%
#   summarize(DOC_load=mean(DOC_load_kgday, na.rm=TRUE), #kg/day 
#             TP_load=mean(TP_load_kgday, na.rm=TRUE), #kg/day
#             Qin=mean(flow, na.rm=TRUE)) %>% #m3/s
#   mutate(DOC_gm3=(DOC_load/Qin)*(1000/86400),
#          TP_mgm3=(TP_load/Qin)*(1000/86400)*1000) 
# 
# loch_sum <- loch_load_manual %>%
#   group_by(lakeName) %>%
#   summarize(DOC_load=mean(DOC_load_kgday, na.rm=TRUE), #kg/day 
#             TP_load=mean(TP_load_kgday, na.rm=TRUE), #kg/day
#             Qin=mean(flow, na.rm=TRUE)) %>% #m3/s
#   mutate(DOC_gm3=(DOC_load/Qin)*(1000/86400),
#          TP_mgm3=(TP_load/Qin)*(1000/86400)*1000) 
# 
# erken_sum <- erken_load_manual %>%
#   group_by(lakeName) %>%
#   summarize(DOC_load=mean(DOC_load_kgday, na.rm=TRUE), #kg/day 
#             TP_load=mean(TP_load_kgday, na.rm=TRUE), #kg/day
#             Qin=mean(flow, na.rm=TRUE)) %>% #m3/s
#   mutate(DOC_gm3=(DOC_load/Qin)*(1000/86400),
#          TP_mgm3=(TP_load/Qin)*(1000/86400)*1000) 
# 
# 
# taupo_sum<-bind_rows(Taupo_Hinemaiaia_load_manual,
#                      Taupo_Tauranga_load_manual,
#                      Taupo_Kuratau_load_manual,
#                      Taupo_TokaanuPowerStation_load_manual,
#                      Taupo_Tongariro_load_manual,
#                      Taupo_Whareroa_load_manual,
#                      Taupo_Whangamata_load_manual,
#                      Taupo_Mapara_load_manual,
#                      Taupo_Waihaha_load_manual,
#                      Taupo_Whanganui_load_manual,
#                      Taupo_Waitahanui_load_manual) %>%
#   group_by(lakeName) %>%
#   # summarize(DOC_load=mean(DOC_load_kgday, na.rm=TRUE), #kg/day #calculating mean daily loads for each tributary
#   #           TP_load=mean(TP_load_kgday, na.rm=TRUE), #kg/day
#   #           Qin=mean(flow, na.rm=TRUE)) %>%#m3/s
#   # ungroup()%>%
#   summarize(DOC_load=sum(DOC_load_kgday, na.rm=TRUE), #kg/day #summing mean daily loads for each tributary
#             TP_load=sum(TP_load_kgday, na.rm=TRUE), #kg/day
#             # DOC_gm3=sum(DOC_gm3, na.rm=TRUE), #g/m3
#             # TP_gm3=sum(TP_gm3, na.rm=TRUE), #g/m3
#             Qin=sum(flow, na.rm=TRUE))%>% #m3/s
#   mutate(DOC_gm3=(DOC_load/Qin)*(1000/86400), #calculate mean daily inflow concentration
#          TP_mgm3=(TP_load/Qin)*(1000/86400)*1000) %>%
#   mutate(lakeName="Taupo",
#          DOC_gm3=na_if(DOC_gm3,0)) %>% #Make sure the DOC values that got forced to zero are actually NAs
#   group_by(lakeName) %>%
#   mutate(across(DOC_load:TP_mgm3, mean, na.rm=TRUE)) 


#Long version of measured dataset
# inflow_conc_summary<-bind_rows(zwart_sum,mueggelsee_sum,loch_sum, taupo_sum, erken_sum) %>%
#   mutate(dataset="measured") 

inflow_conc_summary<-bind_rows(zwart_load_summary,mueggelsee_load_summary,loch_load_summary,
                               Taupo_load_summary, erken_load_summary) %>%
  mutate(dataset="measured") 

inflow_conc_summary_long<- inflow_conc_summary%>%
  pivot_longer(-c(lakeName, dataset))

#Long version of modelled dataset
load_estimates_huisman<-load_estimates_huisman%>%
  mutate(dataset="modelled")

modelled_loads_kg<-left_join(load_estimates_huisman,metadata %>%
                               select(lakeName,`Volume (m3)`, `Lake residence time (year)`), by="lakeName") %>%
  rename(HRT_days=`Lake residence time (year)`,
         V=`Volume (m3)`) %>%
  mutate(HRT_days=HRT_days*365,
         Qin=V/HRT_days,
         TP_load_kg=TP_mgm3*Qin*(1/1000000),
         DOC_load_kg=DOC_gm3*Qin*(1/10000),
         dataset="modelled")

modelled_loads_kg_long<- modelled_loads_kg %>%
  select( -V) %>%
  pivot_longer(-c(lakeName, dataset))

#Join together
load_comparisons_long<-bind_rows(inflow_conc_summary_long,modelled_loads_kg_long)

load_comparisons_wide<-bind_rows(inflow_conc_summary %>%
                                   rename(DOC_load_kg=DOC_load,
                                          TP_load_kg=TP_load,
                                          Qin_m3s=Qin),
                                 modelled_loads_kg) %>%
  select(-4, -(8:10))



