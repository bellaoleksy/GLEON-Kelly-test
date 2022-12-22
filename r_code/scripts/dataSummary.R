# load packages
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(ggrepel)
library(stringr)
library(lubridate)
####
# install.packages("googlesheets4")
library(googlesheets4)

head(master_latlong)
#Read in metadata google sheet
metadata<-read_sheet("https://docs.google.com/spreadsheets/d/1is87WT3n_TU76pTyiis3Os08JJiSmWc2G6K4bthJhU8/edit#gid=952562522") 
?read_sheet
glimpse(metadata)
# 

master_latlong <- metadata %>%
    select(7:12) %>%
  rename(lat='Latitude (decimal degrees)',
         long='Longitude (decimal degrees)',
         # lakeID='Lake Name',
         country='Country',
         load_calc='Load calc possible')
glimpse(master_latlong)

world <- ne_countries(scale = "medium", returnclass = "sf")
# gene world map
GLEON_global_map<-ggplot(data = world) +
  geom_sf() +
  labs( x = "Longitude", y = "Latitude") +
  ggtitle("Lakes available for GLEON Kelly model test (n=60)")+
  geom_point(data=master_latlong,aes(x=long,y=lat, fill=load_calc), shape=21, color="black")+
  scale_fill_manual(values=c("#d62828","#003049"),
                    name="Inflow est.?")
  # geom_label_repel(data=master_latlong,
  #                  aes(x=long, y=lat, label = lakeID),
  #                  fill = "transparent", color = 'black',
  #                  size = 2.5)
GLEON_global_map

northamerica<- ne_countries(continent = "north america", returnclass= "sf")
GLEON_na_map<-ggplot(data = northamerica) +
  geom_sf() +
  labs( x = "Longitude", y = "Latitude") +
  ggtitle("Lakes included in GLEON Kelly model test (n=39)\nUSA and Canada Only")+
  geom_point(data=master_latlong %>%
               filter(country %in% c('USA',"Canada","Canada?")),aes(long,lat,fill=load_calc),
             shape=21, color="black", fill="#d62828")+
  scale_fill_manual(values=c("#d62828","#003049"),
                    name="Inflow est.?")+
  scale_color_manual(values=c("#d62828","#003049"),
                    name="Inflow est.?")+
  geom_label_repel(data=master_latlong %>%
                     filter(country %in% c('USA',"Canada","Canada?")),
                   aes(x=long, y=lat, label = lakeID, color=load_calc),
                   fill = "white", 
                   size = 2.5)

GLEON_na_map



##To plot Solomon et al. sites we need the country codes.

europe<- ne_countries(continent = "europe", returnclass= "sf")

GLEON_europe<-ggplot(data = europe) +
  geom_sf() +
  labs( x = "Longitude", y = "Latitude") +
  ggtitle("Lakes included in GLEON Kelly model test (n=14)\nEurope only")+
  geom_point(data=master_latlong,aes(long,lat, fill=load_calc), shape=21, color="black", fill="#d62828") +
  coord_sf(xlim = c(-10, 40), ylim = c(35, 70)) +
  geom_label_repel(data=master_latlong %>%
                     filter(!country %in% c("USA","Canada","Canada?","Taiwan",
                                            "New Zealand","China")),
                   aes(x=long, y=lat, label = lakeID, color=load_calc),
                   fill = "white", 
                   size = 2.5)+
  scale_fill_manual(values=c("#d62828","#003049"),
                    name="Inflow est.?")+
  scale_color_manual(values=c("#d62828","#003049"),
                     name="Inflow est.?")
GLEON_europe


# Compute descriptive statistics by country
countries <- master_latlong %>%
  group_by(country) %>%
  summarize(n=n()) %>%
  pivot_wider(names_from = country, values_from=n)%>%
  mutate(SUM = rowSums(.[1:12])) 
# stable <- stable[, c("Species", "length", "mean", "sd")]
# Summary table plot, medium orange theme
countries.t <- ggtexttable(countries, rows = NULL, 
                           theme = ttheme("mBlack"))

# LSA distribution ----------------------------------------------


LSA <- metadata %>%
  select(`Lake Name`,`Surface area (ha)`) %>%
  rename(lakeID=`Lake Name`,
         LSA_ha=`Surface area (ha)`) %>%
  mutate(LSA_km2=LSA_ha/100,
         LSA_sizeclass=
           ifelse(LSA_km2 >0 & LSA_km2 <=0.01, "< 10^4", 
                  ifelse(LSA_km2 >0.01 & LSA_km2 <=0.1, "10^4-10^5", 
                         ifelse(LSA_km2 >0.1 & LSA_km2 <=1, "10^5-10^6", 
                                ifelse(LSA_km2 >1 & LSA_km2 <=10, "10^6-10^7",
                                       ifelse(LSA_km2 >10 , ">10^7", "error")))))) %>%
  mutate(LSA_sizeclass = factor(LSA_sizeclass,
                             levels = c("< 10^4", "10^4-10^5", "10^5-10^6",
                                        "10^6-10^7",">10^7"))) #Reorder factors for plotting

LSA_plot<-LSA %>%
  group_by(LSA_sizeclass) %>%
  summarize(n=n()) %>%
  ggplot(aes(x=LSA_sizeclass, y=n, fill=LSA_sizeclass))+
  geom_bar(stat="identity",  color="black")+
  geom_text(aes(label=n), vjust=-1) +
  scale_fill_manual(values=c("#003049","#d62828","#f77f00","#fcbf49","#eae2b7"),
                    name="Size class")+
  xlab("Lake surface area size class (m2)")+
  ylab("Count")
  ylim(c(0,25))
LSA_plot



#histogram
ggplot(LSA) + geom_histogram(aes(x = log10(LSA_km2)),
                             bins=30, fill = "grey", color = "black")

#density plot

ggplot(LSA) +
  geom_density(aes(x = log10(LSA_km2))) +
  geom_rug(aes(x = log10(LSA_km2), y = 0), sides="b")





# HRT distribution --------------------------------------------------------

str(HRT)
HRT <- metadata %>%
  select(`Lake Name`,`Lake residence time (year)`) %>%
  rename(lakeID=`Lake Name`,
         HRT_years=`Lake residence time (year)`) %>%
  # mutate(HRT_days=HRT_years*365)
  mutate(HRT_days=HRT_years*365,
         HRT_sizeclass=
           ifelse(HRT_days >0 & HRT_days <=1, "< 1", 
                  ifelse(HRT_days >1 & HRT_days <10, "1-10", 
                         ifelse(HRT_days >10 & HRT_days <100, "10-100", 
                                ifelse(HRT_days >100 & HRT_days <1000, "100-1000",
                                       ifelse(HRT_days >1000 , ">1000", "error")))))) %>%
  mutate(HRT_sizeclass = factor(HRT_sizeclass,
                                levels = c("< 1", "1-10", "10-100",
                                           "100-1000",">1000"))) #Reorder factors for plotting

HRT%>%
  summarise_at(vars(HRT_years),funs(sum(is.na(.)))) 
HRT%>%
  summarise_at(vars(HRT_years),funs(n())) 

HRT_plot<-HRT %>%
  drop_na()%>%
  group_by(HRT_sizeclass) %>%
  summarize(n=n()) %>%
  ggplot(aes(x=HRT_sizeclass, y=n, fill=HRT_sizeclass))+
  geom_bar(stat="identity",  color="black")+
  scale_fill_manual(values=c("#003049","#d62828","#f77f00","#fcbf49","#eae2b7"),
                    name="HRT (days)")+
  xlab("HRT size class (days)")+
  ylab("Count")+
  annotate(geom="text", x=1.5, y=15, label="13 out of 70 lakes currently NA ",
           color="black")+
  ylim(c(0,30))+
  geom_text(aes(label=n), vjust=-1) 
HRT_plot


#histogram
ggplot(HRT) + geom_histogram(aes(x = log10(HRT_days)),
                             bins=30, fill = "grey", color = "black")

#density plot

ggplot(HRT) +
  geom_density(aes(x = log10(HRT_days))) +
  geom_rug(aes(x = log10(HRT_days), y = 0), sides="b")


# Lakes with inflows ------------------------------------------------------


metadata %>%
  select(`Lake Name`,`Load calc possible`)%>%
  pivot_longer(-1)%>%
  select(-name,-`Lake Name`)%>%
  group_by(value)%>%
  mutate(load_calc_possible_n=n())%>%
  distinct() %>% #remove duplicates
  ggplot(aes(x=value, y=load_calc_possible_n, fill=value))+
  geom_bar(stat="identity",  color="black")+
  scale_fill_manual(values=c("#d62828","#003049"))+
  ylab("Number of lakes")+
  xlab("Load calculation possible?")+
  geom_text(aes(label=load_calc_possible_n), vjust=-1) +
  theme(legend.position="none")

metadata %>%
  select(`Lake Name`,`Load calc possible`)%>%
  pivot_longer(-1)%>%
  select(-name,-`Lake Name`)%>%
  group_by(value)%>%
  mutate(load_calc_possible_n=n(),
         load_calc_possible_perc=load_calc_possible_n/70)%>%
  distinct() %>% #remove duplicates
  ggplot(aes(x=value, y=load_calc_possible_perc*100, fill=value))+
  geom_bar(stat="identity",  color="black")+
  scale_fill_manual(values=c("#d62828","#003049"))+
  geom_text(aes(label=round(load_calc_possible_perc*100,0)), vjust=-1) +
  ylab("Percentage of lakes")+
  xlab("Load calculation possible?")+
  theme(legend.position="none")


# Data availability -------------------------------------------------------

metadata %>%
  rename(data_avail=`Data availability`)%>%
  group_by(data_avail) %>%
  summarize(n=n()) %>%
  ggplot(aes(x=data_avail, y=n, fill=data_avail))+
  geom_bar(stat="identity",  color="black")+
  scale_fill_manual(values=c("#003049","#d62828","#f77f00","#fcbf49"),
                    name=" ")+
  xlab("Data status")+
  ylab("# of lakes")+
  ylim(c(0,50))+
  geom_text(aes(label=n), vjust=-1) +
  theme(legend.position="none")



# Add Nutrients to Metadata ---------------------------------------------------------------

#Run for UNDERC sites
#Source database script
dbdir=file.path("/Users/solomonlab/Google Drive/Research (common)/Research/Data/R/MFE")
db="MFEdb_20210112.db"  
sensordb="MFEsensordb.db" 
source("/Users/solomonlab/Google Drive/Research (common)/Research/Data/R/MFE/000_dbUtil.R")
source("/Users/solomonlab/Google Drive/Research (common)/Research/Data/R/MFE/000_sensordbTable.R")

# dbTableList()


# ~UNDERC nuts ---------------------------------------------------


##############~
#Pull UNDERC nutrient data 
###############~
WATER_CHEM<-dbTable(table="WATER_CHEM")

#Summarize all water chemistry to mean value for summer (June-August) 2013
#to match values from Zwart's dataset
UNDERCnewts<-WATER_CHEM %>%
  filter(str_detect(sampleID, "DeepHole"))%>%
  filter(str_detect(sampleID, "PML"))%>%
  filter(flag=="0")%>%
  select(-projectID, -sampleID, -QCcode, -flag,
         -comments, -metadataID, -updateID) %>%
  filter(lakeID %in% c("BA", "BO", "BR", "CB", 
                       "HB",  "NG", "WA", "WL"))%>% #Except EL, CR, MO which are in the Zwart dataset
  mutate(year=year(dateSample),
         month=month(dateSample),
         season= 
           ifelse(month %in% c(5, 9, 10), "shoulder", 
                  ifelse(month %in% c(6, 7, 8), "summer", "error"))) %>%
  filter(year=="2013" & season =="summer") %>%
  pivot_wider(names_from="parameter", values_from="parameterValue",
              values_fn=mean) %>%
  group_by(lakeID)%>%
  summarize_at(vars(particulateP:nitrate), mean, na.rm=TRUE) %>%
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
  mutate(`Lake Name`=NA)%>%
  mutate(`Lake Name` = replace(`Lake Name`, lakeID=="BA", 'Bay'),
         `Lake Name` = replace(`Lake Name`, lakeID=="BO", 'Bolger'),
         `Lake Name` = replace(`Lake Name`, lakeID=="BR", 'Brown'),
         `Lake Name` = replace(`Lake Name`, lakeID=="CB", 'Cranberry'),
         `Lake Name` = replace(`Lake Name`, lakeID=="HB", 'Hummingbird'),
         `Lake Name` = replace(`Lake Name`, lakeID=="NG", 'NorthGate'),
         `Lake Name` = replace(`Lake Name`, lakeID=="WA", 'Ward'),
         `Lake Name` = replace(`Lake Name`, lakeID=="WL", 'WestLong')) 
str(UNDERCnewts)

UNDERCcolor <- dbTable("COLOR") %>%
  filter(depthClass=="PML") %>%
  filter(siteName=="DeepHole") %>%
  filter(flag=="0")%>%
  select(-projectID, -sampleID, -depthTop, -depthBottom, 
         -metadataID, -comments, -flag, -depthClass,
         -updateID, -dateTimeSample) %>%
  filter(lakeID %in% c("BA", "BO", "BR", "CB", 
                       "HB",  "NG", "WA", "WL"))%>% #Except EL, CR, MO which are in the Zwart dataset
  mutate(year=year(dateSample),
         month=month(dateSample),
         season= 
           ifelse(month %in% c(5, 9, 10), "shoulder", 
                  ifelse(month %in% c(6, 7, 8), "summer", "error"))) %>%
  filter(year=="2013" & season =="summer") %>%
  select(-g440, -year, -month, -season, -siteName) %>%
  group_by(lakeID)%>%
  summarize(abs440=mean(abs440, na.rm=TRUE)) %>%
  mutate(`Lake Name`=NA)%>%
  mutate(`Lake Name` = replace(`Lake Name`, lakeID=="BA", 'Bay'),
         `Lake Name` = replace(`Lake Name`, lakeID=="BO", 'Bolger'),
         `Lake Name` = replace(`Lake Name`, lakeID=="BR", 'Brown'),
         `Lake Name` = replace(`Lake Name`, lakeID=="CB", 'Cranberry'),
         `Lake Name` = replace(`Lake Name`, lakeID=="HB", 'Hummingbird'),
         `Lake Name` = replace(`Lake Name`, lakeID=="NG", 'NorthGate'),
         `Lake Name` = replace(`Lake Name`, lakeID=="WA", 'Ward'),
         `Lake Name` = replace(`Lake Name`, lakeID=="WL", 'WestLong')) 

UNDERCnewts<- left_join(UNDERCnewts, UNDERCcolor, by=c("Lake Name","lakeID"))

# ~Catchment metab nuts ---------------------------------------------------


###############~
#Pull Zwart catchment_metab data
###############~


catchment_newtz<- read.delim('/Users/solomonlab/Google Drive/Research (common)/Research/Data/R/catchment_metab_wg/data/in_lake_nutrients/GLEON_nutrient_inlake.txt') %>%
  select(-id,-Comment, -depth_m)%>%
  group_by(site)%>%
  summarize_at(vars(TP:DOC), mean, na.rm=TRUE)%>%
  rename("Lake Name"=site)%>%
  mutate_if(is.numeric, funs(ifelse(is.nan(.), NA, .))) %>% #replace NaN with NA
  rename(DOC_mgL="DOC",
         SRP_ugL="SRP",
         TP_ugL="TP",
         TN_ugL="TN",
         PO4_ugL="PO4") %>% 
  unite("NO3_ugL", NO3_NO2:NO3, na.rm = TRUE, remove = FALSE)%>% #merge these two columns, exclude NAs
  select(-NO2, -NH4, -NO3_NO2, -NO3) %>%
  mutate(lakeID=NA,
         NO3_ugL=as.numeric(NO3_ugL))%>%
  mutate(lakeID = replace(lakeID, `Lake Name`=="Acton", 'ACT'),
         lakeID = replace(lakeID, `Lake Name`=="Vortsjarv", 'VORTS'),
         lakeID = replace(lakeID, `Lake Name`=="Langtjern", 'LANG'),
         lakeID = replace(lakeID, `Lake Name`=="Crampton", 'CR'),
         lakeID = replace(lakeID, `Lake Name`=="EastLong", 'EL'),
         lakeID = replace(lakeID, `Lake Name`=="Feeagh", 'FEE'),
         lakeID = replace(lakeID, `Lake Name`=="Morris", 'MO'),
         lakeID = replace(lakeID, `Lake Name`=="Lilli", 'LILLI'),
         lakeID = replace(lakeID, `Lake Name`=="Harp", 'HARP'),
         lakeID = replace(lakeID, `Lake Name`=="Lillsjoliden", 'LILLS'),
         lakeID = replace(lakeID, `Lake Name`=="Nastjarn", 'NAST'),
         lakeID = replace(lakeID, `Lake Name`=="Ovre", 'OVRE'),
         lakeID = replace(lakeID, `Lake Name`=="Struptjarn", 'STRU'),
         lakeID = replace(lakeID, `Lake Name`=="Mangstrettjarn", 'MANG'),
         lakeID = replace(lakeID, `Lake Name`=="Mendota", 'MEN'),
         lakeID = replace(lakeID, `Lake Name`=="Trout", 'TROUT')) 
str(catchment_newtz)

# ~Acton nuts (NEED) -------------------------------------------------

##############Acton
#Missing in-lake nutrient data 

# ~Almberga nuts (NEED) -------------------------------------------------

##############Almberga
#Missing in-lake nutrient data 

# ~Jordan nuts -------------------------------------------------

##############Jordan
jordannuts <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Jordon/JordanPond_nutrient.txt") %>%
  select(DOC_mgL, TP_ugL, TN_ugL, chla_ugL, secchi_m) %>%
  mutate(lakeID="JOR",
         `Lake Name`="Jordan") %>%
  group_by(lakeID, `Lake Name`)%>%
  summarize_at(vars(DOC_mgL:secchi_m), mean, na.rm=TRUE)
str(jordannuts)  

# ~YuanYang nuts -------------------------------------------------

##############YunYang
YuanYangnuts <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/YunYang/yyl_nutrient.txt") %>%
  select(DOC_mgL, TP_ugL, TN_ugL) %>%
  mutate(lakeID="YYL",
         `Lake Name`="YuanYang") %>%
  group_by(lakeID, `Lake Name`)%>%
  summarize_at(vars(DOC_mgL:TN_ugL), mean, na.rm=TRUE)
str(YuanYangnuts)

# ~Barco nuts -------------------------------------------------

##############Barco
Barcnuts <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/NEON sites/BARC/BARC_surfChem_2018-2019.csv")%>% select(-X) %>%
  mutate(`Lake Name`="Barco") %>%
  group_by(lakeID, `Lake Name`)%>%
  summarize_at(vars(cond_uScm:abs280), mean, na.rm=TRUE) %>%
  mutate(TN_ugL=TDN_mgL*1000,
         TP_ugL=TP_mgL*1000,
         TDP_ugL=TDP_mgL*1000,
         orthoP_ugL=orthoP_mgL*1000)%>%
  select(-TDN_mgL,-TP_mgL,-TDP_mgL,-orthoP_mgL,-TOC_mgL)
str(Barcnuts)

# ~Little Rock nuts -------------------------------------------------

##############Little Rock Lake
Lironuts <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/NEON sites/LIRO/LIRO_surfChem_2018-2019.csv")%>% select(-X) %>%
  mutate(`Lake Name`="LittleRock") %>%
  group_by(lakeID, `Lake Name`)%>%
  summarize_at(vars(cond_uScm:abs280), mean, na.rm=TRUE) %>%
  mutate(TN_ugL=TDN_mgL*1000,
         TP_ugL=TP_mgL*1000,
         TDP_ugL=TDP_mgL*1000,
         orthoP_ugL=orthoP_mgL*1000)%>%
  select(-TDN_mgL,-TP_mgL,-TDP_mgL,-orthoP_mgL,-TOC_mgL)
str(Lironuts)

# ~Prairie lake nuts -------------------------------------------------

##############Prairie Lake
Prlanuts <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/NEON sites/PRLA/PRLA_surfChem_2018-2019.csv")%>% select(-X) %>%
  mutate(`Lake Name`="Prairie") %>%
  group_by(lakeID, `Lake Name`)%>%
  summarize_at(vars(cond_uScm:abs280), mean, na.rm=TRUE) %>%
  mutate(TN_ugL=TDN_mgL*1000,
         TP_ugL=TP_mgL*1000,
         TDP_ugL=TDP_mgL*1000,
         orthoP_ugL=orthoP_mgL*1000)%>%
  select(-TDN_mgL,-TP_mgL,-TDP_mgL,-orthoP_mgL,-TOC_mgL)
str(Prlanuts)

# ~Prairie pothole nuts -------------------------------------------------

##############Prairie Pothole
Prlonuts <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/NEON sites/PRPO/PRPO_surfChem_2018-2019.csv")%>% select(-X) %>%
  mutate(`Lake Name`="PrairiePothole") %>%
  group_by(lakeID, `Lake Name`)%>%
  summarize_at(vars(cond_uScm:abs280), mean, na.rm=TRUE) %>%
  mutate(TN_ugL=TDN_mgL*1000,
         TP_ugL=TP_mgL*1000,
         TDP_ugL=TDP_mgL*1000,
         orthoP_ugL=orthoP_mgL*1000)%>%
  select(-TDN_mgL,-TP_mgL,-TDP_mgL,-orthoP_mgL,-TOC_mgL)
str(Prlanuts)

# ~Sugg nuts -------------------------------------------------

##############Sugg
SUGGnuts <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/NEON sites/SUGG/SUGG_surfChem_2018-2019.csv")%>% select(-X)%>%
  mutate(`Lake Name`="Suggs") %>%
  group_by(lakeID, `Lake Name`)%>%
  summarize_at(vars(cond_uScm:abs280), mean, na.rm=TRUE) %>%
  mutate(TN_ugL=TDN_mgL*1000,
         TP_ugL=TP_mgL*1000,
         TDP_ugL=TDP_mgL*1000,
         orthoP_ugL=orthoP_mgL*1000)%>%
  select(-TDN_mgL,-TP_mgL,-TDP_mgL,-orthoP_mgL,-TOC_mgL)



# ~Erken nuts -------------------------------------------------

##############Erken
erkennuts <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Erken/Erken_LakeChem2018.txt") %>%
  mutate(lakeID="ERK",
         `Lake Name`="Erken") %>%
  rename(DOC_mgL=DOC,
         TN_ugL=TN,
         TP_ugL=TP,
         chla_ugL=Chl) %>%
  select(-X)%>%
  filter(!DOC_mgL=="NaN")%>%
  slice_tail(n = 36) %>% #get rid of blank rows
  mutate_if(is.factor, as.numeric, na.rm = TRUE) %>% #convert factors to numbers
  group_by(lakeID, `Lake Name`)%>%
  summarize_at(vars(DOC_mgL:chla_ugL), mean, na.rm=TRUE)
str(erkennuts)

# ~Feeagh nuts -------------------------------------------------

##############Feeagh
#Already included in Zwart dataset
# feeaghnuts <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Feeagh/Feeagh_nutrient.txt") %>%
#   mutate(lakeID="FEE",
#          `Lake Name`="Feeagh") %>%
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
#   group_by(lakeID, `Lake Name`)%>%
#   summarize_at(vars(DOC_mgL:kD), mean, na.rm=TRUE)
# str(feeaghnuts)

# ~Mueggelsee nuts -------------------------------------------------

##############Mueggelsee
meugnuts <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Mueggelsee/Mueggelsee_nutrient.txt") %>%
  mutate_if(is.factor, as.numeric, na.rm = TRUE) %>% #convert factors to numbers
  mutate(lakeID="MEUG",
         `Lake Name`="Mueggelsee") %>%
  rename(DOC_mgL=DOC,
         TN_ugL=TN,
         TP_ugL=TP,
         SRP_ugL=SRP...soluble.reactive.phosphorus,
         NO3_ugL=NO3_N.nitrate) %>% 
  mutate(TN_ugL=TN_ugL*1000)%>%
  select(-NH4_N.ammonium,-SRSi.soluble.reactive.silica)%>%
  group_by(lakeID, `Lake Name`)%>%
  summarize_at(vars(DOC_mgL:NO3_ugL), mean, na.rm=TRUE)
str(meugnuts)

# ~P1 nuts -------------------------------------------------

##############Prairie1
p1nuts <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Prairie1/P1_nutrient.txt") %>%
  mutate(`Lake Name`="P1") %>%
  rename(UV254=UV.254nm) %>% 
  group_by(lakeID, `Lake Name`)%>%
  summarize_at(vars(DOC_mgL:UV254), mean, na.rm=TRUE)
str(p1nuts)

# ~P8 nuts -------------------------------------------------

##############Prairie8
p8nuts <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Prairie8/P8_nutrient.txt") %>%
  mutate(`Lake Name`="P8") %>%
  rename(UV254=UV.254nm) %>% 
  group_by(lakeID, `Lake Name`)%>%
  summarize_at(vars(DOC_mgL:UV254), mean, na.rm=TRUE)
str(p8nuts)


# ~Simoncouche nuts (NEED) -------------------------------------------------

##############Simoncouche
#Waiting on nutrients


# ~Sunapee nuts (NEED) -------------------------------------------------


##############Sunapee
#Waiting on nutrients

##############Taupo
tauponuts <- read.delim("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/Taupo/Taupo_nutrient.txt") %>%
  select(-LakeID)%>%
  mutate(lakeID="TAU",
         `Lake Name`="Taupo") %>%
  rename(secchi_m=Secchi,
         NO3_ugL=NO3.N,
         TP_ugL=TP,
         TN_ugL=TN,
         chla_ugL=Chlorophylla,
         DOC_mgL=DOC) %>% 
  select(!contains("Method")) %>%
  select(-NH4.N) %>%
  group_by(lakeID, `Lake Name`)%>%
  summarize_at(vars(secchi_m:DOC_mgL), mean, na.rm=TRUE)
str(tauponuts)


# ~LVWS nuts --------------------------------------------------------------


##############Loch and Sky
lochvalenuts <- read.csv("~/Google Drive/Research (common)/Research/Data/R/LVWS-long-term-data/data_clean/surface_chem/LVWS_surfacewaterchemistry_master_180211.csv", stringsAsFactors = F)

lochvalenuts$NO3 <- as.numeric(as.character(lochvalenuts$NO3))
lochvalenuts$FLDCOND <- as.numeric(as.character(lochvalenuts$FLDCOND))
lochvalenuts$PO4<- as.numeric(as.character(lochvalenuts$PO4))
lochvalenuts$YEAR <- as.factor(as.character(lochvalenuts$YEAR))
lochvalenuts$MONTH <- as.numeric(lochvalenuts$MONTH)
lochvalenuts$DAY <- as.factor(as.character(lochvalenuts$DAY))
str(lochvalenuts)
lochvalenuts$DATE <- ymd(as.character(lochvalenuts$DATE))

lochvalenuts <- lochvalenuts %>%
  mutate(NO3_ugNperL = (NO3 * 1000 * (14.006/62.005))) %>%
  mutate(SEASON = 
           ifelse(MONTH %in% c(12, 1, 2), "WINTER",
                  ifelse(MONTH %in% c(3, 4, 5), "SPRING",
                         ifelse(MONTH %in% c(6, 7, 8), "SUMMER",
                                ifelse(MONTH %in% c(9, 10, 11), "FALL", "ERROR"))))) %>%
  filter(YEAR=="2016" & SEASON=="SUMMER" ) %>%
  filter(SITE %in% c("LOCH.LS","SKY.LS")) %>%
  filter(TYPE=="NORMAL"|TYPE=="DUPE") %>%
  rename(DOC_mgL=DOC,
         chla_ugL=CHL_A,
         TP_ugL=TP_HSWL,
         cond_uScm=FLDCOND,
         lakeID=SITE) %>%
  mutate(lakeID = case_when(lakeID == 'SKY.LS' ~ 'SKY', # renaming for joining purposes
                            lakeID == 'LOCH.LS' ~ 'LOCH',
                          TRUE ~ lakeID),
         `Lake Name`=NA,
         `Lake Name` = replace(`Lake Name`, lakeID=="SKY", 'SkyPond'),
         `Lake Name` = replace(`Lake Name`, lakeID=="LOCH", 'TheLoch')) %>%
  select(lakeID, `Lake Name`, DOC_mgL, chla_ugL, TP_ugL, cond_uScm) %>%
  group_by(lakeID, `Lake Name`)%>%
  summarize_at(vars(DOC_mgL:cond_uScm), mean, na.rm=TRUE)





##############Sparkling
sparklingabs <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/NTL/Sparkling/Sparkling_absorbance_2014.csv") %>%
  select(-lakeid, -X, -year4)%>%
  mutate(lakeID="SPARK",
         `Lake Name`="Sparkling") %>%
  group_by(lakeID, `Lake Name`)%>%
  summarize_at(vars(abs440:abs440_percm), mean, na.rm=TRUE)

sparklingnuts <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/data/metab_data_raw/NTL/Sparkling/Sparkling_nuts_2014.csv") %>%
  select(-lakeid, -X)%>%
  mutate(lakeID="SPARK",
         `Lake Name`="Sparkling") %>%
  group_by(lakeID, `Lake Name`)%>%
  summarize_at(vars(DOC_mgL:TP_ugL), mean, na.rm=TRUE)

sparklingnuts<-left_join(sparklingnuts, sparklingabs, by=c("Lake Name","lakeID"))



# ~Solomon dataset --------------------------------------------------------

#Join Solomon metadata
# 
solomonLakeData <- read.csv("~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/Solomon_output/Table - lake data.csv")
# 
# #Cut of lakes that we already have
# #Although might consider keeping Sparkling to save on a model run?
# #Same with Sunapee
solomonLakeData_trim <- solomonLakeData %>%
  filter(!lake %in% c("Acton","Crampton","Feeagh","Mendota","Sparkling",
                      "Sunapee","Trout","YuanYang","Muggelsee",
                      "Vortsjarv")) %>%
  rename(`Lake Name`=lake) %>%
  mutate(lakeID=NA,
         lakeID = replace(lakeID, `Lake Name`=="Balaton", 'BAL'),
         lakeID = replace(lakeID, `Lake Name`=="Annie", 'ANN'),
         lakeID = replace(lakeID, `Lake Name`=="CrystalBog", 'CRBO'),
         lakeID = replace(lakeID, `Lake Name`=="FredriksburgSlotso", 'FS'),
         lakeID = replace(lakeID, `Lake Name`=="Hampenso", 'HAMP'),
         lakeID = replace(lakeID, `Lake Name`=="Kentucky", 'KEN'),
         lakeID = replace(lakeID, `Lake Name`=="Mirror", 'MIR'),
         lakeID = replace(lakeID, `Lake Name`=="Onondaga", 'ONO'),
         lakeID = replace(lakeID, `Lake Name`=="Pontchartrain", 'PONT'),
         lakeID = replace(lakeID, `Lake Name`=="Rotoiti", 'ROTOI'),
         lakeID = replace(lakeID, `Lake Name`=="Rotorua", 'ROTOR'),
         lakeID = replace(lakeID, `Lake Name`=="StGribso", 'STGR'),
         lakeID = replace(lakeID, `Lake Name`=="Taihu", 'TAI'),
         lakeID = replace(lakeID, `Lake Name`=="TroutBog", 'TRBO'),
         lakeID = replace(lakeID, `Lake Name`=="Vedstedso", 'VEDS'),
         TN_ugL=as.numeric(as.factor(TN_ugL)),
         'Load calc possible'="no",
         "Source"="Solomon et al. 2013")
glimpse(solomonLakeData_trim)


solomonLakeData_nuts<-solomonLakeData_trim%>%
  select(`Lake Name`, TP_ugL:lakeID)

glimpse(solomonLakeData_nuts)


# Compile nutrient data ---------------------------------------------------

newts_full<-bind_rows(UNDERCnewts,catchment_newtz,jordannuts,YuanYangnuts,
                      Barcnuts,Prlanuts,erkennuts,meugnuts,p1nuts,
                      p8nuts,tauponuts, lochvalenuts, sparklingnuts,
                      solomonLakeData_nuts)
# metadata_more<-bind_rows(metadata, solomonLakeData_metadata_trimmer)


#Join with metadata
metadata_withnuts<-left_join(metadata, newts_full, by=c("Lake Name","lakeID"))


# metadata_withnuts_full<-left_join(metadata_withnuts_full, newts_full, by=c("Lake Name","lakeID"))


#Boxplots of DOC by lake size class
metadata_withnuts %>%
  rename(LSA_ha=`Surface area (ha)`) %>%
  mutate(LSA_km2=LSA_ha/100,
         LSA_sizeclass=
           ifelse(LSA_km2 >0 & LSA_km2 <=0.01, "< 10^4", 
                  ifelse(LSA_km2 >0.01 & LSA_km2 <=0.1, "10^4-10^5", 
                         ifelse(LSA_km2 >0.1 & LSA_km2 <=1, "10^5-10^6", 
                                ifelse(LSA_km2 >1 & LSA_km2 <=10, "10^6-10^7",
                                       ifelse(LSA_km2 >10 , ">10^7", "error")))))) %>%
  mutate(LSA_sizeclass = factor(LSA_sizeclass,
                                levels = c("< 10^4", "10^4-10^5", "10^5-10^6",
                                           "10^6-10^7",">10^7"))) %>%#Reorder factors for plotting
  # select(lakeID, LSA_km2)
  ggplot(aes(x=LSA_sizeclass, y=DOC_mgL, fill=LSA_sizeclass))+
  geom_boxplot(outlier.shape = NA, alpha=0.5)+
  facet_wrap(.~LSA_sizeclass, scales="free_x", ncol=5)+
  geom_jitter(size=3, shape=21)+
  scale_fill_manual(values=c("#003049","#d62828","#f77f00","#fcbf49","#eae2b7"),
                    name="Size class (ha)")+
  ggtitle("50 out of 70 lakes")


#Scatterplot of DOC by lake size class
metadata_withnuts %>%
  rename(LSA_ha=`Surface area (ha)`) %>%
  mutate(LSA_km2=LSA_ha/100,
         LSA_sizeclass=
           ifelse(LSA_km2 >0 & LSA_km2 <=0.01, "< 10^4", 
                  ifelse(LSA_km2 >0.01 & LSA_km2 <0.1, "10^4-10^5", 
                         ifelse(LSA_km2 >0.1 & LSA_km2 <1, "10^5-10^6", 
                                ifelse(LSA_km2 >1 & LSA_km2 <10, "10^6-10^7",
                                       ifelse(LSA_km2 >10 , ">10^7", "error")))))) %>%
  mutate(LSA_sizeclass = factor(LSA_sizeclass,
                                levels = c("< 10^4", "10^4-10^5", "10^5-10^6",
                                           "10^6-10^7",">10^7"))) %>%#Reorder factors for plotting
  ggplot(aes(x=log10(LSA_ha), y=DOC_mgL, fill=LSA_sizeclass))+
  geom_point(size=3, shape=21)+
  scale_fill_manual(values=c("#003049","#d62828","#f77f00","#fcbf49","#eae2b7"),
                    name="Size class (ha)")+
  scale_color_manual(values=c("#003049","#d62828","#f77f00","#fcbf49","#000000"),
                    name="Size class (ha)")+
  geom_label_repel(aes(x=log10(LSA_ha), y=DOC_mgL, label = lakeID, color=LSA_sizeclass),
                   fill = "white",
                   size = 2.5)+
  ggtitle("50 out of 70 lakes")

#Scatterplot of TP by lake size class
metadata_withnuts %>%
  rename(LSA_ha=`Surface area (ha)`) %>%
  mutate(LSA_km2=LSA_ha/100,
         LSA_sizeclass=
           ifelse(LSA_km2 >0 & LSA_km2 <=0.01, "< 10^4", 
                  ifelse(LSA_km2 >0.01 & LSA_km2 <0.1, "10^4-10^5", 
                         ifelse(LSA_km2 >0.1 & LSA_km2 <1, "10^5-10^6", 
                                ifelse(LSA_km2 >1 & LSA_km2 <10, "10^6-10^7",
                                       ifelse(LSA_km2 >10 , ">10^7", "error")))))) %>%
  mutate(LSA_sizeclass = factor(LSA_sizeclass,
                                levels = c("< 10^4", "10^4-10^5", "10^5-10^6",
                                           "10^6-10^7",">10^7"))) %>%#Reorder factors for plotting
  ggplot(aes(x=log10(LSA_ha), y=log10(TP_ugL), fill=LSA_sizeclass))+
  geom_point(size=3, shape=21)+
  scale_fill_manual(values=c("#003049","#d62828","#f77f00","#fcbf49","#eae2b7"),
                    name="Size class (ha)")+
  ggtitle("34 out of 55 lakes")

newts_full %>%
  ggplot(aes(x=DOC_mgL, y=abs440))+
  geom_point()

