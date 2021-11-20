#Extract zMix from all .RData files and save in respective folders.
#I'm sure there is a way to loop this cleverly, but my attemps failed.
#IAO 2021-03-16
library(tidyverse)
library(lubridate)
#Handy function
extract.from.RData <- function(file, name) {
  #' Function for extracting an object from a .RData file created by R's save() command
  E <- new.env()
  load(file=file, envir=E)
  return(get(name, envir=E, inherits=F))
}
here()
dir<-'/Volumes/GoogleDrive/My Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw' # directory of metabolism data





dataZMixSparkling<-extract.from.RData(file=file.path(dir,'Sparkling/Sparkling.RData'),
                                       name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixSparkling,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Sparkling/SparklingzMix.txt",
            row.names=FALSE)


dataZMixCrystalBog<-extract.from.RData(file=file.path(dir,'CrystalBog/CrystalBog.RData'),
                                       name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixCrystalBog,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/CrystalBog/CrystalBogzMix.txt",
            row.names=FALSE)
dataZMixActon<-extract.from.RData(file=file.path(dir,'Acton/Acton.RData'),
                                       name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixActon,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Acton/ActonzMix.txt",
            row.names=FALSE)
dataZMixAlmberga<-extract.from.RData(file=file.path(dir,'Almberga/Almberga.RData'),
                                  name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixAlmberga,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Almberga/AlmbergazMix.txt",
            row.names=FALSE)
dataZMixAnnie<-extract.from.RData(file=file.path(dir,'Annie/Annie.RData'),
                                     name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixAnnie,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Annie/AnniezMix.txt",
            row.names=FALSE)
dataZMixBalaton<-extract.from.RData(file=file.path(dir,'Balaton/Balaton.RData'),
                                     name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixBalaton,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Balaton/BalatonzMix.txt",
            row.names=FALSE)
dataZMixBarco<-extract.from.RData(file=file.path(dir,'Barco/Barco.RData'),
                                     name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixBarco,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Barco/BarcozMix.txt",
            row.names=FALSE)
dataZMixBay<-extract.from.RData(file=file.path(dir,'Bay/Bay.RData'),
                                  name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixBay,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Bay/BayzMix.txt",
            row.names=FALSE)
dataZMixBolger<-extract.from.RData(file=file.path(dir,'Bolger/Bolger.RData'),
                                  name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixBolger,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Bolger/BolgerzMix.txt",
            row.names=FALSE)
dataZMixBrown<-extract.from.RData(file=file.path(dir,'Brown/Brown.RData'),
                                  name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixBrown,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Brown/BrownzMix.txt",
            row.names=FALSE)
dataZMixCastle<-extract.from.RData(file=file.path(dir,'Castle/Castle.RData'),
                                  name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixCastle,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Castle/CastlezMix.txt",
            row.names=FALSE)
dataZMixCrampton<-extract.from.RData(file=file.path(dir,'Crampton/Crampton.RData'),
                                   name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixCrampton,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Crampton/CramptonzMix.txt",
            row.names=FALSE)
dataZMixCranberry<-extract.from.RData(file=file.path(dir,'Cranberry/Cranberry.RData'),
                                   name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixCranberry,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Cranberry/CranberryzMix.txt",
            row.names=FALSE)
dataZMixEastLong<-extract.from.RData(file=file.path(dir,'EastLong/EastLong.RData'),
                                      name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixEastLong,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/EastLong/EastLongzMix.txt",
            row.names=FALSE)
dataZMixErken<-extract.from.RData(file=file.path(dir,'Erken/Erken.RData'),
                                      name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixErken,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Erken/ErkenzMix.txt",
            row.names=FALSE)
dataZMixFeeagh<-extract.from.RData(file=file.path(dir,'Feeagh/Feeagh.RData'),
                                      name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixFeeagh,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Feeagh/FeeaghzMix.txt",
            row.names=FALSE)
dataZMixFredriksburgSlotso<-extract.from.RData(file=file.path(dir,'FredriksburgSlotso/FredriksburgSlotso.RData'),
                                   name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixFredriksburgSlotso,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/FredriksburgSlotso/FredriksburgSlotsozMix.txt",
            row.names=FALSE)
dataZMixGollinsee<-extract.from.RData(file=file.path(dir,'Gollinsee/Gollinsee.RData'),
                                   name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixGollinsee,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Gollinsee/GollinseezMix.txt",
            row.names=FALSE)
dataZMixHampenso<-extract.from.RData(file=file.path(dir,'Hampenso/Hampenso.RData'),
                                   name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixHampenso,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Hampenso/HampensozMix.txt",
            row.names=FALSE)
dataZMixHarp<-extract.from.RData(file=file.path(dir,'Harp/Harp.RData'),
                                   name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixHarp,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Harp/HarpzMix.txt",
            row.names=FALSE)
dataZMixHummingbird<-extract.from.RData(file=file.path(dir,'Hummingbird/Hummingbird.RData'),
                                   name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixHummingbird,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Hummingbird/HummingbirdzMix.txt",
            row.names=FALSE)
dataZMixJordan<-extract.from.RData(file=file.path(dir,'Jordan/Jordan.RData'),
                                   name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixJordan,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Jordan/JordanzMix.txt",
            row.names=FALSE)
dataZMixKentucky<-extract.from.RData(file=file.path(dir,'Kentucky/Kentucky.RData'),
                                   name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix)) %>%
  mutate(zMix = replace(zMix, zMix==6, 2.38)) #Replace the 6m zMix with the mean

write.table(dataZMixKentucky,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Kentucky/KentuckyzMix.txt",
            row.names=FALSE)
dataZMixLangtjern<-extract.from.RData(file=file.path(dir,'Langtjern/Langtjern.RData'),
                                   name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixLangtjern,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Langtjern/LangtjernzMix.txt",
            row.names=FALSE)
dataZMixLillinonah<-extract.from.RData(file=file.path(dir,'Lillinonah/Lillinonah.RData'),
                                   name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixLillinonah,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Lillinonah/LillinonahzMix.txt",
            row.names=FALSE)
dataZMixLillsjoliden<-extract.from.RData(file=file.path(dir,'Lillsjoliden/Lillsjoliden.RData'),
                                   name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixLillsjoliden,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Lillsjoliden/LillsjolidenzMix.txt",
            row.names=FALSE)
dataZMixLittleRock<-extract.from.RData(file=file.path(dir,'LittleRock/LittleRock.RData'),
                                   name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixLittleRock,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/LittleRock/LittleRockzMix.txt",
            row.names=FALSE)
dataZMixMangstrettjarn<-extract.from.RData(file=file.path(dir,'Mangstrettjarn/Mangstrettjarn.RData'),
                                   name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixMangstrettjarn,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Mangstrettjarn/MangstrettjarnzMix.txt",
            row.names=FALSE)
dataZMixMendota<-extract.from.RData(file=file.path(dir,'Mendota/Mendota.RData'),
                                   name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixMendota,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Mendota/MendotazMix.txt",
            row.names=FALSE)
dataZMixMirror<-extract.from.RData(file=file.path(dir,'Mirror/Mirror.RData'),
                                   name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixMirror,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Mirror/MirrorzMix.txt",
            row.names=FALSE)
dataZMixMorris<-extract.from.RData(file=file.path(dir,'Morris/Morris.RData'),
                                   name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixMorris,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Morris/MorriszMix.txt",
            row.names=FALSE)
dataZMixNastjarn<-extract.from.RData(file=file.path(dir,'Nastjarn/Nastjarn.RData'),
                                   name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixNastjarn,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Nastjarn/NastjarnzMix.txt",
            row.names=FALSE)
dataZMixNorthgate<-extract.from.RData(file=file.path(dir,'Northgate/Northgate.RData'),
                                   name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixNorthgate,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Northgate/NorthgatezMix.txt",
            row.names=FALSE)
dataZMixOneida<-extract.from.RData(file=file.path(dir,'Oneida/Oneida.RData'),
                                   name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixOneida,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Oneida/OneidazMix.txt",
            row.names=FALSE)
dataZMixOnondaga<-extract.from.RData(file=file.path(dir,'Onondaga/Onondaga.RData'),
                                   name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixOnondaga,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Onondaga/OnondagazMix.txt",
            row.names=FALSE)
dataZMixOvre<-extract.from.RData(file=file.path(dir,'Ovre/Ovre.RData'),
                                   name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixOvre,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Ovre/OvrezMix.txt",
            row.names=FALSE)
dataZMixP1<-extract.from.RData(file=file.path(dir,'P1/P1.RData'),
                                   name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixP1,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/P1/P1zMix.txt",
            row.names=FALSE)
dataZMixP8<-extract.from.RData(file=file.path(dir,'P8/P8.RData'),
                                   name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixP8,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/P8/P8zMix.txt",
            row.names=FALSE)
dataZMixPontchartrain<-extract.from.RData(file=file.path(dir,'Pontchartrain/Pontchartrain.RData'),
                                   name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixPontchartrain,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Pontchartrain/PontchartrainzMix.txt",
            row.names=FALSE)
dataZMixPrairieLake<-extract.from.RData(file=file.path(dir,'PrairieLake/PrairieLake.RData'),
                                   name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixPrairieLake,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/PrairieLake/PrairieLakezMix.txt",
            row.names=FALSE)
dataZMixPrairiePothole<-extract.from.RData(file=file.path(dir,'PrairiePothole/PrairiePothole.RData'),
                                   name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixPrairiePothole,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/PrairiePothole/PrairiePotholezMix.txt",
            row.names=FALSE)
dataZMixRotoiti<-extract.from.RData(file=file.path(dir,'Rotoiti/Rotoiti.RData'),
                                   name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixRotoiti,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Rotoiti/RotoitizMix.txt",
            row.names=FALSE)
dataZMixRotorua<-extract.from.RData(file=file.path(dir,'Rotorua/Rotorua.RData'),
                                   name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixRotorua,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Rotorua/RotoruazMix.txt",
            row.names=FALSE)
dataZMixSchulzensee<-extract.from.RData(file=file.path(dir,'Schulzensee/Schulzensee.RData'),
                                   name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixSchulzensee,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Schulzensee/SchulzenseezMix.txt",
            row.names=FALSE)
dataZMixSkyPond<-extract.from.RData(file=file.path(dir,'SkyPond/SkyPond.RData'),
                                   name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixSkyPond,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/SkyPond/SkyPondzMix.txt",
            row.names=FALSE)
dataZMixStGribso<-extract.from.RData(file=file.path(dir,'StGribso/StGribso.RData'),
                                   name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixStGribso,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/StGribso/StGribsozMix.txt",
            row.names=FALSE)
dataZMixStruptjarn<-extract.from.RData(file=file.path(dir,'Struptjarn/Struptjarn.RData'),
                                   name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixStruptjarn,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Struptjarn/StruptjarnzMix.txt",
            row.names=FALSE)
dataZMixSuggs<-extract.from.RData(file=file.path(dir,'Suggs/Suggs.RData'),
                                   name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixSuggs,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Suggs/SuggszMix.txt",
            row.names=FALSE)
dataZMixSunapee<-extract.from.RData(file=file.path(dir,'Sunapee/Sunapee.RData'),
                                   name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixSunapee,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Sunapee/SunapeezMix.txt",
            row.names=FALSE)
dataZMixTaihu<-extract.from.RData(file=file.path(dir,'Taihu/Taihu.RData'),
                                   name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixTaihu,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Taihu/TaihuzMix.txt",
            row.names=FALSE)
dataZMixTaupo<-extract.from.RData(file=file.path(dir,'Taupo/Taupo.RData'),
                                   name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixTaupo,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Taupo/TaupozMix.txt",
            row.names=FALSE)
dataZMixTheLoch<-extract.from.RData(file=file.path(dir,'TheLoch/TheLoch.RData'),
                                  name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixTheLoch,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/TheLoch/TheLochzMix.txt",
            row.names=FALSE)
dataZMixTrout<-extract.from.RData(file=file.path(dir,'Trout/Trout.RData'),
                                  name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixTrout,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Trout/TroutzMix.txt",
            row.names=FALSE)
dataZMixTroutBog<-extract.from.RData(file=file.path(dir,'TroutBog/TroutBog.RData'),
                                  name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixTroutBog,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/TroutBog/TroutBogzMix.txt",
            row.names=FALSE)
dataZMixUtah<-extract.from.RData(file=file.path(dir,'Utah/Utah.RData'),
                                  name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixUtah,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Utah/UtahzMix.txt",
            row.names=FALSE)
dataZMixVedstedso<-extract.from.RData(file=file.path(dir,'Vedstedso/Vedstedso.RData'),
                                  name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixVedstedso,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Vedstedso/VedstedsozMix.txt",
            row.names=FALSE)
dataZMixVortsjarv<-extract.from.RData(file=file.path(dir,'Vortsjarv/Vortsjarv.RData'),
                                  name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixVortsjarv,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Vortsjarv/VortsjarvzMix.txt",
            row.names=FALSE)
dataZMixWard<-extract.from.RData(file=file.path(dir,'Ward/Ward.RData'),
                                  name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixWard,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Ward/WardzMix.txt",
            row.names=FALSE)
dataZMixWestLong<-extract.from.RData(file=file.path(dir,'WestLong/WestLong.RData'),
                                  name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixWestLong,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/WestLong/WestLongzMix.txt",
            row.names=FALSE)
dataZMixYuanYang<-extract.from.RData(file=file.path(dir,'YunYang/YunYang.RData'),
                                     name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixYuanYang,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/YunYang/YunYangzMix.txt",
            row.names=FALSE)

dataZMixActon<-extract.from.RData(file=file.path(dir,'Acton/Acton.RData'),
                                     name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixActon,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Acton/ActonzMix.txt",
            row.names=FALSE)


# dataZMixMeuggelsee<-extract.from.RData(file=file.path(dir,'Mueggelsee/Meuggelsee.RData'),
#                                   name="dataZMix") %>%
#   mutate(solarDay=floor_date(dateTime,"day"))%>%
#   group_by(solarDay) %>%
#   summarize(zMix=max(zMix))
# 
glimpse(dataZMix)
dataZMix <- dataZMix %>%
    mutate(solarDay=floor_date(dateTime,"day"))%>%
    group_by(solarDay) %>%
    summarize(zMix=max(zMix))

write.table(dataZMix,"/Users/solomonlab/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Mueggelsee/MeuggelseezMix.txt",
            row.names=FALSE)

write.table(dataZMix, "~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/MeuggelseezMix.txt", row.names=FALSE)



dataZMixCroche<-extract.from.RData(file=file.path(dir,'Croche/croche.RData'),
                                       name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixCroche,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Croche/crochezMix.txt",
            row.names=FALSE)


dataZMixSimoncouche<-extract.from.RData(file=file.path(dir,'Simoncouche/simoncouche.RData'),
                                   name="dataZMix") %>%
  mutate(solarDay=floor_date(dateTime,"day"))%>%
  group_by(solarDay) %>%
  summarize(zMix=max(zMix))
write.table(dataZMixSimoncouche,"~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw/Simoncouche/simoncouchezMix.txt",
            row.names=FALSE)
