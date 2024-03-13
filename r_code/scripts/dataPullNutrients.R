# Pull individuals lake nutrients 

# Add Nutrients to Metadata ---------------------------------------------------------------

#Run for UNDERC sites
#Source database script
dbdir=file.path("~/Library/CloudStorage/OneDrive-UCB-O365/Research/Data/R/MFE")
# db="MFEdb_20200218.db"
db="MFEdb_20210112.db"
# db="/Volumes/GoogleDrive/My Drive/Research (common)/Research/Data/R/MFE/MFEdb_20210112.db"
#Updating script with 2020 data
sensordb="MFEsensordb.db" 
source("~/Library/CloudStorage/OneDrive-UCB-O365/Research/Data/R/MFE/000_dbUtil.R")
source("~/Library/CloudStorage/OneDrive-UCB-O365/Research/Data/R/MFE/000_sensordbTable.R")

# dbTableList()


# ~UNDERC nuts ---------------------------------------------------


##############~
#Pull UNDERC nutrient data 
###############~
WATER_CHEM<-dbTable(table="WATER_CHEM")
#Summarize all water chemistry to mean value for summer (June-August) 2013
#to match values from Zwart's dataset

# All 2017 lakes ---------------------------------------------------

UNDERCnewts_2017<-WATER_CHEM %>%
  filter(str_detect(sampleID, "DeepHole"))%>%
  filter(str_detect(sampleID, "PML"))%>%
  filter(flag=="0")%>%
  select(-projectID, -sampleID, -QCcode, -flag,
         -comments, -metadataID, -updateID) %>%
  filter(lakeID %in% c("BO", "BR", "CB", 
                       "HB",  "NG", "WL"))%>% 
  mutate(year=year(dateSample),
         month=month(dateSample),
         season= 
           ifelse(month %in% c(10), "shoulder", 
                  ifelse(month %in% c(5, 6, 7, 8, 9), "summer", "error"))) %>%
  filter(year=="2017" & season =="summer") %>%
  pivot_wider(names_from="parameter", values_from="parameterValue",
              values_fn=mean) %>%
  group_by(lakeID)%>%
  summarize_at(vars(nitrate:DOC), mean, na.rm=TRUE) %>%
  rename(DOC_mgL="DOC",
         POC_mgL="POC",
         PON_mgL="PON",
         SRP_ugL="SRP",
         NO3_ugL="nitrate",
         PP_ugL="particulateP",
         TP_ugL="TP",
         TN_ugL="TN")%>%
  mutate(DOC_mgL=DOC_mgL/1000,
         POC_mgL=POC_mgL/1000,
         PON_mgL=PON_mgL/1000) %>%
  mutate(lakeName=NA)%>%
  mutate(
    lakeName = replace(lakeName, lakeID=="BO", 'Bolger'),
    lakeName = replace(lakeName, lakeID=="BR", 'Brown'),
    lakeName = replace(lakeName, lakeID=="CB", 'Cranberry'),
    lakeName = replace(lakeName, lakeID=="HB", 'Hummingbird'),
    lakeName = replace(lakeName, lakeID=="NG", 'Northgate'),
    lakeName = replace(lakeName, lakeID=="WL", 'WestLong'),
    DOC_mgL = replace(DOC_mgL, lakeID=="BR", 11.9)) #Replacing missing value with the mean from all other years
str(UNDERCnewts_2017)

UNDERCcolor_2017 <- dbTable("COLOR") %>%
  filter(depthClass=="PML") %>%
  filter(siteName=="DeepHole") %>%
  filter(flag=="0")%>%
  select(-projectID, -sampleID, -depthTop, -depthBottom, 
         -metadataID, -comments, -flag, -depthClass,
         -updateID, -dateTimeSample) %>%
  filter(lakeID %in% c( "BO", "BR", "CB", 
                        "HB",  "NG",  "WL"))%>% 
  mutate(year=year(dateSample),
         month=month(dateSample),
         season= 
           ifelse(month %in% c(10), "shoulder", 
                  ifelse(month %in% c(5, 6, 7, 8, 9), "summer", "error"))) %>%
  filter(year=="2017" & season =="summer") %>%
  select(-abs440, -year, -month, -season, -siteName) %>%
  group_by(lakeID)%>%
  summarize(abs440=mean(g440, na.rm=TRUE)) %>% #Modified 2022-11-08 because I realized we were pulled the raw absorbance values instead of color
  mutate(lakeName=NA)%>%
  mutate(
    lakeName = replace(lakeName, lakeID=="BO", 'Bolger'),
    lakeName = replace(lakeName, lakeID=="BR", 'Brown'),
    lakeName = replace(lakeName, lakeID=="CB", 'Cranberry'),
    lakeName = replace(lakeName, lakeID=="HB", 'Hummingbird'),
    lakeName = replace(lakeName, lakeID=="NG", 'NorthGate'),
    lakeName = replace(lakeName, lakeID=="WL", 'WestLong')) 

UNDERCnewts_2017<- left_join(UNDERCnewts_2017, UNDERCcolor_2017, by=c("lakeName","lakeID"))


# All 2016 lakes ---------------------------------------------------

UNDERCnewts_2016<-WATER_CHEM %>%
  filter(str_detect(sampleID, "DeepHole"))%>%
  filter(str_detect(sampleID, "PML"))%>%
  filter(flag=="0")%>%
  select(-projectID, -sampleID, -QCcode, -flag,
         -comments, -metadataID, -updateID) %>%
  filter(lakeID %in% c("BA", "WA"))%>% 
  mutate(year=year(dateSample),
         month=month(dateSample),
         season= 
           ifelse(month %in% c(10), "shoulder", 
                  ifelse(month %in% c(5, 6, 7, 8, 9), "summer", "error"))) %>%
  filter(year=="2016" & season =="summer") %>%
  pivot_wider(names_from="parameter", values_from="parameterValue",
              values_fn=mean) %>%
  group_by(lakeID)%>%
  summarize_at(vars(TP:DOC), mean, na.rm=TRUE) %>%
  rename(DOC_mgL="DOC",
         POC_mgL="POC",
         PON_mgL="PON",
         SRP_ugL="SRP",
         NO3_ugL="nitrate",
         PP_ugL="particulateP",
         TP_ugL="TP",
         TN_ugL="TN")%>%
  mutate(DOC_mgL=DOC_mgL/1000,
         POC_mgL=POC_mgL/1000,
         PON_mgL=PON_mgL/1000) %>%
  mutate(lakeName=NA)%>%
  mutate(lakeName = replace(lakeName, lakeID=="BA", 'Bay'),
         lakeName = replace(lakeName, lakeID=="WA", 'Ward')) 
str(UNDERCnewts_2016)

UNDERCcolor_2016 <- dbTable("COLOR") %>%
  filter(depthClass=="PML") %>%
  filter(siteName=="DeepHole") %>%
  filter(flag=="0")%>%
  select(-projectID, -sampleID, -depthTop, -depthBottom, 
         -metadataID, -comments, -flag, -depthClass,
         -updateID, -dateTimeSample) %>%
  filter(lakeID %in% c( "BA","WA"))%>% 
  mutate(year=year(dateSample),
         month=month(dateSample),
         season= 
           ifelse(month %in% c(10), "shoulder", 
                  ifelse(month %in% c(5, 6, 7, 8, 9), "summer", "error"))) %>%
  filter(year=="2016" & season =="summer") %>%
  select(-abs440, -year, -month, -season, -siteName) %>%
  group_by(lakeID)%>%
  summarize(abs440=mean(g440, na.rm=TRUE)) %>% #Modified 2022-11-08 because I realized we were pulled the raw absorbance values instead of color
  mutate(lakeName=NA)%>%
  mutate(lakeName = replace(lakeName, lakeID=="BA", 'Bay'),
         lakeName = replace(lakeName, lakeID=="WA", 'Ward')) 

UNDERCnewts_2016<- left_join(UNDERCnewts_2016, UNDERCcolor_2016, by=c("lakeName","lakeID"))

UNDERCnewts<-bind_rows(UNDERCnewts_2016, UNDERCnewts_2017)


# ~Catchment metab nuts ---------------------------------------------------


###############~
#Pull Zwart catchment_metab data
###############~


catchment_newtz<- read.delim('~/Library/CloudStorage/OneDrive-UCB-O365/Research/Data/R/catchment_metab_wg/data/in_lake_nutrients/GLEON_nutrient_inlake.txt') %>%
  select(-comment)%>%
  separate(dateTime, c("date", "time"), "//") %>%
  mutate(date=date(date),
         DOY=yday(date)) %>%
  filter(DOY > 121 & DOY < 274)%>%
  filter(depth_m <=5) %>% #Both Trout and Mendota have multiple depths. Have to get rid of the super high TP in the hypolimnion!
  mutate(         DOC_mgL = case_when(site != 'Trout' ~ DOC_mgL, 
                                      site == 'Trout' & DOC_mgL < 10 ~ DOC_mgL),# getting rid of DOC outlier for Trout -->
                  TP_ugL = case_when(site != 'Trout' ~ TP_ugL, 
                                     site == 'Trout' & TP_ugL > 0 ~ TP_ugL)) %>%
  group_by(site)%>%
  summarize_at(vars(TP_ugL:DOC_mgL), mean, na.rm=TRUE)%>%
  rename("lakeName"=site)%>%
  mutate_if(is.numeric, funs(ifelse(is.nan(.), NA, .))) %>% #replace NaN with NA
  unite("NO3_ugN_L", NO3_NO2_ugN_L:NO3_ugN_L, na.rm = TRUE, remove = FALSE)%>% #merge these two columns, exclude NAs
  select(-NO2_ugNL, -NH4_ugN_L, -NO3_NO2_ugN_L) %>%
  mutate(lakeID=NA,
         NO3_ugN_L=as.numeric(NO3_ugN_L))%>%
  mutate(lakeID = replace(lakeID, lakeName=="Acton", 'ACT'),
         lakeID = replace(lakeID, lakeName=="Vortsjarv", 'VORTS'),
         lakeID = replace(lakeID, lakeName=="Langtjern", 'LANG'),
         lakeID = replace(lakeID, lakeName=="Crampton", 'CR'),
         lakeID = replace(lakeID, lakeName=="EastLong", 'EL'),
         lakeID = replace(lakeID, lakeName=="Feeagh", 'FEE'),
         lakeID = replace(lakeID, lakeName=="Morris", 'MO'),
         lakeID = replace(lakeID, lakeName=="Lilli", 'LILLI'),
         lakeID = replace(lakeID, lakeName=="Harp", 'HARP'),
         lakeID = replace(lakeID, lakeName=="Lillsjoliden", 'LILLS'),
         lakeID = replace(lakeID, lakeName=="Nastjarn", 'NAST'),
         lakeID = replace(lakeID, lakeName=="Ovre", 'OVRE'),
         lakeID = replace(lakeID, lakeName=="Struptjarn", 'STRU'),
         lakeID = replace(lakeID, lakeName=="Mangstrettjarn", 'MANG'),
         lakeID = replace(lakeID, lakeName=="Mendota", 'MEN'),
         lakeID = replace(lakeID, lakeName=="Trout", 'TROUT'),
         lakeName = replace(lakeName, lakeName=="Lilli", 'Lillinonah'),
         DOC_mgL = replace(DOC_mgL, lakeName=="Lillinonah", 4.21))  ## renaming for joining purposes
glimpse(catchment_newtz)




# ~Jordan nuts -------------------------------------------------

##############Jordan

jordannuts <- read.delim(here("data/metab_data_raw/Jordan/JordanPond_nutrient.txt")) %>%
  separate(dateTime, c("date", "time"), " ") %>%
  mutate(date=date(date),
         DOY=yday(date)) %>%
  filter(DOY > 121 & DOY < 274)%>%
  select(DOC_mgL, TP_ugL, TN_ugL, chla_ugL, secchi_m) %>%
  mutate(lakeID="JOR",
         lakeName="Jordan") %>%
  group_by(lakeID, lakeName)%>%
  summarize_at(vars(DOC_mgL:secchi_m), mean, na.rm=TRUE) %>%
  ungroup()
str(jordannuts)  

# ~Barco nuts -------------------------------------------------

##############Barco
Barcnuts <- read.csv(here("data/metab_data_raw/NEON sites/BARC/BARC_surfChem_2018-2019.csv")) %>% select(-X) %>%
  separate(dateTime, c("date", "time"), " ") %>%
  mutate(date=date(date),
         DOY=yday(date)) %>%
  filter(DOY > 121 & DOY < 274)%>%
  mutate(lakeName="Barco") %>%
  group_by(lakeID, lakeName)%>%
  summarize_at(vars(cond_uScm:abs280), mean, na.rm=TRUE) %>%
  mutate(TN_ugL=TN_mgL*1000,
         TDN_ugL=TDN_mgL*1000,
         TP_ugL=TP_mgL*1000,
         TDP_ugL=TDP_mgL*1000,
         orthoP_ugL=orthoP_mgL*1000,
         NH4N_ugL=NH4N_mgL*1000)%>%
  select(-TDN_mgL,-TP_mgL,-TDP_mgL,-orthoP_mgL,-TOC_mgL, -TN_mgL, -NH4N_mgL)%>%
  rename(NH4_ugL_N=NH4N_ugL)%>%
  ungroup()
str(Barcnuts)

# ~Little Rock nuts -------------------------------------------------

##############Little Rock Lake
Lironuts <- read.csv(here("data/metab_data_raw/NEON sites/LIRO/LIRO_surfChem_2018-2019.csv")) %>% select(-X) %>%
  separate(dateTime, c("date", "time"), " ") %>%
  mutate(date=date(date),
         DOY=yday(date)) %>%
  filter(DOY > 121 & DOY < 274)%>%
  mutate(lakeName="LittleRock") %>%
  group_by(lakeID, lakeName)%>%
  summarize_at(vars(cond_uScm:abs280), mean, na.rm=TRUE) %>%
  mutate(TN_ugL=TN_mgL*1000,
         TDN_ugL=TDN_mgL*1000,
         TP_ugL=TP_mgL*1000,
         TDP_ugL=TDP_mgL*1000,
         orthoP_ugL=orthoP_mgL*1000,
         NH4N_ugL=NH4N_mgL*1000)%>%
  select(-TDN_mgL,-TP_mgL,-TDP_mgL,-orthoP_mgL,-TOC_mgL, -TN_mgL, -NH4N_mgL)%>%
  rename(NH4_ugL_N=NH4N_ugL)%>%
  ungroup()
str(Lironuts)

# ~Prairie lake nuts -------------------------------------------------

##############Prairie Lake
Prlanuts <- read.csv(here("data/metab_data_raw/NEON sites/PRLA/PRLA_surfChem_2018-2019.csv")) %>% select(-X) %>%
  separate(dateTime, c("date", "time"), " ") %>%
  mutate(date=date(date),
         DOY=yday(date)) %>%
  filter(DOY > 121 & DOY < 274)%>%
  mutate(lakeName="PrairieLake") %>%
  group_by(lakeID, lakeName)%>%
  summarize_at(vars(cond_uScm:abs280), mean, na.rm=TRUE) %>%
  mutate(TN_ugL=TN_mgL*1000,
         TDN_ugL=TDN_mgL*1000,
         TP_ugL=TP_mgL*1000,
         TDP_ugL=TDP_mgL*1000,
         orthoP_ugL=orthoP_mgL*1000,
         NH4N_ugL=NH4N_mgL*1000)%>%
  select(-TDN_mgL,-TP_mgL,-TDP_mgL,-orthoP_mgL,-TOC_mgL, -TN_mgL, -NH4N_mgL)%>%
  rename(NH4_ugL_N=NH4N_ugL)%>%
  ungroup()
str(Prlanuts)

# ~Prairie pothole nuts -------------------------------------------------

##############Prairie Pothole
Prlonuts <- read.csv(here("data/metab_data_raw/NEON sites/PRPO/PRPO_surfChem_2018-2019.csv")) %>% select(-X) %>%
  separate(dateTime, c("date", "time"), " ") %>%
  mutate(date=date(date),
         DOY=yday(date)) %>%
  filter(DOY > 121 & DOY < 274)%>%
  mutate(lakeName="PrairiePothole") %>%
  group_by(lakeID, lakeName)%>%
  summarize_at(vars(cond_uScm:abs280), mean, na.rm=TRUE) %>%
  mutate(TN_ugL=TN_mgL*1000,
         TDN_ugL=TDN_mgL*1000,
         TP_ugL=TP_mgL*1000,
         TDP_ugL=TDP_mgL*1000,
         orthoP_ugL=orthoP_mgL*1000,
         NH4N_ugL=NH4N_mgL*1000)%>%
  select(-TDN_mgL,-TP_mgL,-TDP_mgL,-orthoP_mgL,-TOC_mgL, -TN_mgL, -NH4N_mgL)%>%
  rename(NH4_ugL_N=NH4N_ugL)%>%
  ungroup()
str(Prlonuts)

# ~Sugg nuts -------------------------------------------------

##############Sugg
SUGGnuts <- read.csv(here("data/metab_data_raw/NEON sites/SUGG/SUGG_surfChem_2018-2019.csv")) %>% select(-X)%>%
  separate(dateTime, c("date", "time"), " ") %>%
  mutate(date=date(date),
         DOY=yday(date)) %>%
  filter(DOY > 121 & DOY < 274)%>%
  mutate(lakeName="Suggs") %>%
  group_by(lakeID, lakeName)%>%
  summarize_at(vars(cond_uScm:abs280), mean, na.rm=TRUE) %>%
  mutate(TN_ugL=TN_mgL*1000,
         TDN_ugL=TDN_mgL*1000,
         TP_ugL=TP_mgL*1000,
         TDP_ugL=TDP_mgL*1000,
         orthoP_ugL=orthoP_mgL*1000,
         NH4N_ugL=NH4N_mgL*1000)%>%
  select(-TDN_mgL,-TP_mgL,-TDP_mgL,-orthoP_mgL,-TOC_mgL, -TN_mgL, -NH4N_mgL)%>%
  rename(NH4_ugL_N=NH4N_ugL)%>%
  ungroup()



# ~Erken nuts -------------------------------------------------

##############Erken
erkennuts <- read.delim(here("data/metab_data_raw/Erken/Erken_LakeChem2018.txt")) %>%
  separate(Date.Time, c("date", "time"), " ") %>%
  mutate(date=mdy(date),
         DOY=yday(date)) %>%
  filter(DOY > 121 & DOY < 274)%>%
  mutate(lakeID="ERK",
         lakeName="Erken") %>%
  rename(chla_ugL=chl_ugL)%>%
  filter(!DOC_mgL=="NaN")%>%
  slice_tail(n = 36) %>% #get rid of blank rows
  mutate_if(is.factor, as.numeric, na.rm = TRUE) %>% #convert factors to numbers
  group_by(lakeID, lakeName)%>%
  summarize_at(vars(DOC_mgL:chla_ugL), mean, na.rm=TRUE) %>%
  ungroup()
str(erkennuts)

# ~Schulzensee nuts -------------------------------------------------

schulzenseenuts <- read.delim(here("data/metab_data_raw/Schulzensee/Schulzensee_nutrient.txt")) %>%
  # separate(dateTime, c("date", "time"), " ") %>%
  mutate(date=mdy(date),
         DOY=yday(date)) %>%
  filter(DOY > 121 & DOY < 274)%>%
  rename(lakeName=Site)%>%
  select(lakeName:DIC_mgL)%>% 
  group_by(lakeName)%>%
  summarize_at(vars(DOC_mgL:DIC_mgL), mean, na.rm=TRUE)
str(schulzenseenuts)

# ~Gollinsee nuts -------------------------------------------------

Gollinseenuts <- read.delim(here("data/metab_data_raw/Gollinsee/Gollinsee_nutrient.txt")) %>%
  # separate(dateTime, c("date", "time"), " ") %>%
  mutate(date=mdy(date),
         DOY=yday(date)) %>%
  filter(DOY > 121 & DOY < 274)%>%
  rename(lakeName=site)%>%
  select(lakeName:DIC_mgL)%>% 
  group_by(lakeName)%>%
  summarize_at(vars(DOC_mgL:DIC_mgL), mean, na.rm=TRUE)
str(Gollinseenuts)
# ~Feeagh nuts -------------------------------------------------

##############Feeagh
#Already included in Zwart dataset
# feeaghnuts <- read.delim(here("data/metab_data_raw/Feeagh/Feeagh_nutrient.txt")) %>%
#   mutate(lakeID="FEE",
#          lakeName="Feeagh") %>%
#   rename(DOC_mgL=DOC,
#          TN_ugL=TN,
#          TP_ugL=TP,
#          NO3_ugL=NO3,
#          SRP_ugL=SRP,
#          abs440=A440,
#          secchi_m=SD,
#          kD=Kd) %>% 
#   select(-X, -X.1, -X.2, -X.3, -X.4, -X.5, -X.6, -X.7, -site, -dateTime)%>%
#   mutate_if(is.factor, as.numeric, na.rm = TRUE) %>% #convert factors to numbers
#   group_by(lakeID, lakeName)%>%
#   summarize_at(vars(DOC_mgL:kD), mean, na.rm=TRUE)
# str(feeaghnuts)

# ~Mueggelsee nuts -------------------------------------------------

##############Mueggelsee
meugnuts <- read.delim(here("data/metab_data_raw/Mueggelsee/Mueggelsee_nutrient.txt")) %>%
  separate(dateTime, c("date", "time"), " ") %>%
  mutate(date=mdy(date),
         DOY=yday(date)) %>%
  filter(DOY > 121 & DOY < 274)%>%
  mutate_if(is.factor, as.numeric, na.rm = TRUE) %>% #convert factors to numbers
  mutate_if(is.integer, as.numeric, na.rm = TRUE) %>% #convert integers to numbers
  mutate(lakeID="MEUG",
         lakeName="Mueggelsee") %>%
  rename(DOC_mgL=DOC, #mg_L
         TN_ugL=TN, #mg_L
         TP_ugL=TP, #ug_L
         SRP_ugL=SRP,#ug_L
         NO3_ugL_N=NO3_N,#ug_L
         NH4_ugL_N=NH4_N) %>%#ug_L
  mutate(TN_ugL=TN_ugL*1000)%>%
  group_by(lakeID, lakeName)%>%
  summarize_at(vars(DOC_mgL:NH4_ugL_N), mean, na.rm=TRUE) %>%
  ungroup()
str(meugnuts)

# ~P1 nuts -------------------------------------------------

##############Prairie1
p1nuts <- read.delim(here("data/metab_data_raw/Prairie1/P1_nutrient.txt")) %>%
  separate(dateTime, c("date", "time"), " ") %>%
  mutate(date=date(date),
         DOY=yday(date)) %>%
  filter(DOY > 121 & DOY < 274)%>%
  mutate(lakeName="P1") %>%
  rename(abs254=UV.254nm) %>% 
  group_by(lakeID, lakeName)%>%
  summarize_at(vars(DOC_mgL:abs254), mean, na.rm=TRUE) %>%
  ungroup() %>%
  mutate(TP_ugL=35)    # mg SRP m-3, value from literature
str(p1nuts)

# ~P8 nuts -------------------------------------------------

##############Prairie8
p8nuts <- read.delim(here("data/metab_data_raw/Prairie8/P8_nutrient.txt")) %>%
  separate(dateTime, c("date", "time"), " ") %>%
  mutate(date=date(date),
         DOY=yday(date)) %>%
  filter(DOY > 121 & DOY < 274)%>%
  mutate(lakeName="P8") %>%
  rename(abs254=UV.254nm) %>% 
  group_by(lakeID, lakeName)%>%
  summarize_at(vars(DOC_mgL:abs254), mean, na.rm=TRUE) %>%
  ungroup()%>%
  mutate(TP_ugL=30)    # mg SRP m-3, value from literature
str(p8nuts)


# ~Simoncouche nuts -------------------------------------------------


simoncouchenuts <- read.csv(here("data/metab_data_raw/Simoncouche/Simoncouche_nutrient.csv")) %>%
  separate(dateTime, c("date", "time"), " ") %>%
  mutate(date=date(date),
         DOY=yday(date)) %>%
  filter(DOY > 121 & DOY < 274)%>%
  filter(site=="epilimnion") %>%
  select(-site) %>%
  select(!contains("_")) %>%
  mutate(lakeName="Simoncouche") %>%
  rename(TP_ugL=TP,
         TN_ugL=TN,
         DOC_mgL=DOC) %>% 
  group_by(lakeName)%>%
  summarize_at(vars(DOC_mgL:TP_ugL), mean, na.rm=TRUE) %>%
  ungroup()
str(simoncouchenuts)


# ~Croche nuts -------------------------------------------------


crochenuts <- read.csv(here("data/metab_data_raw/Croche/Croche_nutrient.csv")) %>%
  separate(dateTime, c("date", "time"), " ") %>%
  mutate(date=date(date),
         DOY=yday(date)) %>%
  filter(DOY > 121 & DOY < 274)%>%
  filter(site=="epilimnion") %>%
  select(-site) %>%
  select(!contains("_")) %>%
  mutate(lakeName="Croche") %>%
  rename(TP_ugL=TP,
         TN_ugL=TN,
         DOC_mgL=DOC) %>% 
  group_by(lakeName)%>%
  summarize_at(vars(DOC_mgL:TP_ugL), mean, na.rm=TRUE) %>%
  ungroup()
str(crochenuts)


# ~Taupo -------------------------------------------------


tauponuts <- read.delim(here("data/metab_data_raw/Taupo/Taupo_nutrient.txt")) %>%
  separate(dateTime, c("date", "time"), " ") %>%
  mutate(date=date(date),
         DOY=yday(date)) %>%
  filter(DOY > 121 & DOY < 274)%>%
  select(-LakeID)%>%
  mutate(lakeID="TAU",
         lakeName="Taupo") %>%
  rename(secchi_m=Secchi,
         NO3_ugL=NO3.N,
         TP_ugL=TP,
         TN_ugL=TN,
         chla_ugL=Chlorophylla,
         DOC_mgL=DOC) %>% 
  select(!contains("Method")) %>%
  select(-NH4.N) %>%
  group_by(lakeID, lakeName)%>%
  summarize_at(vars(secchi_m:DOC_mgL), mean, na.rm=TRUE) %>%
  ungroup()
str(tauponuts)


# ~LVWS nuts --------------------------------------------------------------


##############Loch and Sky
lochvalenuts <- read.csv("~/Library/CloudStorage/OneDrive-UCB-O365/Research/Data/R/LVWS-long-term-data/data_clean/surface_chem/LVWS_surfacewaterchemistry_master_180211.csv", stringsAsFactors = F)

lochvalenuts$NO3_NREL <- as.numeric(as.character(lochvalenuts$NO3_NREL))
lochvalenuts$FLDCOND <- as.numeric(as.character(lochvalenuts$FLDCOND))
lochvalenuts$PO4<- as.numeric(as.character(lochvalenuts$PO4))
lochvalenuts$YEAR <- as.factor(as.character(lochvalenuts$YEAR))
lochvalenuts$MONTH <- as.numeric(lochvalenuts$MONTH)
lochvalenuts$DAY <- as.factor(as.character(lochvalenuts$DAY))
str(lochvalenuts)
lochvalenuts$DATE <- ymd(as.character(lochvalenuts$DATE))

lochvalenuts <- lochvalenuts %>%
  mutate(NO3_ugNperL = (NO3_NREL * 1000 * (14.006/62.005))) %>%
  mutate(SEASON = 
           ifelse(MONTH %in% c(12, 1, 2), "WINTER",
                  ifelse(MONTH %in% c(3, 4), "SPRING",
                         ifelse(MONTH %in% c(5, 6, 7, 8, 9), "SUMMER",
                                ifelse(MONTH %in% c(10, 11), "FALL", "ERROR"))))) %>%
  filter(YEAR=="2016" & SEASON=="SUMMER" ) %>%
  filter(SITE %in% c("LOCH.LS","SKY.LS")) %>%
  filter(TYPE=="NORMAL"|TYPE=="DUPE") %>%
  rename(DOC_mgL=DOC,
         chla_ugL=CHL_A,
         TP_ugL=TP_HSWL,
         cond_uScm=FLDCOND,
         NO3_ugL_N=NO3_ugNperL,
         lakeID=SITE) %>%
  mutate(lakeID = case_when(lakeID == 'SKY.LS' ~ 'SKY', # renaming for joining purposes
                            lakeID == 'LOCH.LS' ~ 'LOCH',
                            TRUE ~ lakeID),
         lakeName=NA,
         lakeName = replace(lakeName, lakeID=="SKY", 'SkyPond'),
         lakeName = replace(lakeName, lakeID=="LOCH", 'TheLoch')) %>%
  select(lakeID, lakeName, DOC_mgL, chla_ugL, TP_ugL, NO3_ugL_N, cond_uScm) %>%
  group_by(lakeID, lakeName)%>%
  summarize_at(vars(DOC_mgL:cond_uScm), mean, na.rm=TRUE)






# ~Castle nuts --------------------------------------------------------------
castlenuts <- data.frame(lakeName="Castle",
                         DOC_mgL= 7.68,  
                         TP_ugL= 4.3) 
#Values from literature

# ~Almberga nuts  --------------------------------------------------------------

almberganuts <- read.delim(here("data/metab_data_raw/Almberga/Almberga_nutrient.txt")) %>%
  # separate(dateTime, c("date", "time"), " ") %>%
  mutate(date=mdy(date),
         DOY=yday(date)) %>%
  filter(DOY > 121 & DOY < 274)%>%
  rename(lakeName=lakeID,
         abs440=a440) %>% 
  select(lakeName, DOC_mgL, TN_ugL, NO3_ugL_N) %>%
  group_by(lakeName)%>%
  summarize_at(vars(DOC_mgL:NO3_ugL_N), mean, na.rm=TRUE) %>%
  mutate(TP_ugL=5) #value from literature
str(almberganuts)
# ~Erken nuts --------------------------------------------------------------
erkennuts <- read.delim(here("data/metab_data_raw/Erken/Erken_LakeChem2018.txt")) %>%
  separate(Date.Time, c("date", "time"), " ") %>%
  mutate(date=mdy(date),
         DOY=yday(date)) %>%
  filter(DOY > 121 & DOY < 274)%>%
  mutate(lakeName="Erken") %>%
  filter(Site=="epilimnion") %>% # stick to only epi values, since we'll likely trim metab data too 
  group_by(lakeName)%>%
  summarize_at(vars(DOC_mgL:chl_ugL), mean, na.rm=TRUE) %>%
  rename(chla_ugL=chl_ugL)
glimpse(erkennuts)
str(erkennuts)
# ~YunYang nuts --------------------------------------------------------------
yylnuts <- read.delim(here("data/metab_data_raw/YunYang/yyl_nutrient.txt")) %>%
  separate(dateTime, c("date", "time"), " ") %>%
  mutate(date=date(date),
         DOY=yday(date)) %>%
  filter(DOY > 121 & DOY < 274)%>%
  mutate(lakeName="YunYang") %>%
  select(lakeName, DOC_mgL, TP_ugL, TN_ugL) %>%
  group_by(lakeName)%>%
  summarize_at(vars(DOC_mgL:TN_ugL), mean, na.rm=TRUE)
glimpse(yylnuts)
str(yylnuts)

# ~Utah nuts --------------------------------------------------------------
utahnuts <- read.delim(here("data/metab_data_raw/Utah/UtahLake_nutrient.txt")) %>%
  # separate(dateTime, c("date", "time"), " ") %>%
  mutate(date=ymd(dateTime),
         DOY=yday(date)) %>%
  filter(DOY > 121 & DOY < 274)%>%
  mutate(lakeName="Utah") %>%
  rename(TP_mgL=`total.phosphate.phosphorus_mgL`)%>%
  select(!contains("method")) %>%
  mutate(TN_ugL=TN_mgL*1000,
         TDP_ugL=TDP_mgL*1000,
         TP_ugL=TP_mgL*1000)%>%
  select(lakeName, DOC_mgL, TP_ugL, TN_ugL, chla_ugL, secchi_m, TDP_ugL) %>%
  group_by(lakeName)%>%
  summarize_at(vars(DOC_mgL:TDP_ugL), mean, na.rm=TRUE)
str(utahnuts)
glimpse(utahnuts)


# ~Oneida nuts --------------------------------------------------------------
oneidanuts <- read.delim(here("data/metab_data_raw/Oneida/oneida_nutrient.txt")) %>%
  filter(!date=="6/29/18")%>%
  # separate(dateTime, c("date", "time"), " ") %>%
  mutate(date=mdy(date),
         DOY=yday(date)) %>%
  filter(DOY > 121 & DOY < 274)%>%
  mutate(lakeName="Oneida") %>%
  group_by(lakeName)%>%
  summarize_at(vars(secchi_m:DOC_mgL), mean, na.rm=TRUE)
str(oneidanuts)
glimpse(oneidanuts)

# ~Solomon dataset --------------------------------------------------------

#Join Solomon metadata
# 
solomonLakeData <- read.csv(here("results/Solomon_output/Table - lake data.csv"))
str(solomonLakeData)
# 
#Trim out duplicates
solomonLakeData_trim <- solomonLakeData %>%
  filter(!lake %in% c("Acton","Crampton","Feeagh","Mendota",
                      "Trout","YuanYang","Muggelsee",
                      "Vortsjarv")) %>%
  rename(lakeName=lake) %>%
  mutate(lakeID=NA,
         lakeID = replace(lakeID, lakeName=="Balaton", 'BAL'),
         lakeID = replace(lakeID, lakeName=="Annie", 'ANN'),
         lakeID = replace(lakeID, lakeName=="CrystalBog", 'CRBO'),
         lakeID = replace(lakeID, lakeName=="FredriksburgSlotso", 'FS'),
         lakeID = replace(lakeID, lakeName=="Hampenso", 'HAMP'),
         lakeID = replace(lakeID, lakeName=="Kentucky", 'KEN'),
         lakeID = replace(lakeID, lakeName=="Mirror", 'MIR'),
         lakeID = replace(lakeID, lakeName=="Onondaga", 'ONO'),
         lakeID = replace(lakeID, lakeName=="Sunapee", 'SUN'),
         lakeID = replace(lakeID, lakeName=="Sparkling", 'SPAR'),
         lakeID = replace(lakeID, lakeName=="Pontchartrain", 'PONT'),
         lakeID = replace(lakeID, lakeName=="Rotoiti", 'ROTOI'),
         lakeID = replace(lakeID, lakeName=="Rotorua", 'ROTOR'),
         lakeID = replace(lakeID, lakeName=="StGribso", 'STGR'),
         lakeID = replace(lakeID, lakeName=="Taihu", 'TAI'),
         lakeID = replace(lakeID, lakeName=="TroutBog", 'TRBO'),
         lakeID = replace(lakeID, lakeName=="Vedstedso", 'VEDS'),
         DOC_mgL = replace(DOC_mgL, lakeName=="Mirror", 2.5), #temporary value from https://doi.org/10.2307/3544603
         # TN_ugL=as.numeric(as.factor(TN_ugL)),
         'Load calc possible'="no",
         "Source"="Solomon et al. 2013") 
glimpse(solomonLakeData_trim)





solomonLakeData_nuts<-solomonLakeData_trim%>%
  select(lakeName, TP_ugL:abs440)

glimpse(solomonLakeData_nuts)

#### Join nutrients to metab

# Compile nutrient data ---------------------------------------------------

newts_full<-bind_rows(UNDERCnewts,oneidanuts,
                      catchment_newtz,jordannuts,yylnuts,utahnuts,castlenuts,
                      Barcnuts,Prlanuts,Prlonuts,erkennuts,meugnuts,p1nuts,
                      p8nuts,tauponuts, lochvalenuts, SUGGnuts,Lironuts,
                      solomonLakeData_nuts, almberganuts, erkennuts,
                      Gollinseenuts, schulzenseenuts, simoncouchenuts,
                      crochenuts) %>%
  mutate(DOC_mgL = replace(DOC_mgL, lakeName=="Oneida", 4.2)) #One datapoint from DCR:
                                                              #"From our in prep manuscript about different types
                                                              #of N limitation in lakes across the NE, 
                                                              #Oneida is 4.2 mg DOC/L in June 2018. "

# metadata_more<-bind_rows(metadata, solomonLakeData_metadata_trimmer)



