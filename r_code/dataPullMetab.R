dir<-here("results/model_output_raw")
# dir<-'~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/results/model_output_raw' # directory of metabolism data
folders<-list.files(dir) # folders in this dir

bella_GPP<-data.frame() # data frame to store all metab data
for(i in 1:length(folders)){ # loops over all folders in metab directory
  cur<-read.table(file.path(dir,folders[i],paste(folders[i],' GPPFitOut.txt',sep='')),header=T,sep=' ',
                  stringsAsFactors = F) # read in lake specific metab data
  cur$lakeName<-folders[i]
  bella_GPP<-rbind(bella_GPP,cur)
}

bella_ER<-data.frame() # data frame to store all metab data
for(i in 1:length(folders)){ # loops over all folders in metab directory
  cur<-read.table(file.path(dir,folders[i],paste(folders[i],' optimOut.txt',sep='')),header=T,sep=' ',
                  stringsAsFactors = F) # read in lake specific metab data
  cur<-cur%>%select(solarDay,rhoEst) # getting rid of any unnecessary columns
  cur$lakeName<-folders[i]
  bella_ER<-rbind(bella_ER,cur)
}


#Remove Meuggelsee?
folders<-folders[folders != "Mueggelsee"]; 


bella_zMix<-data.frame() # data frame to store all metab data
for(i in 1:length(folders)){ # loops over all folders in metab directory
  cur<-read.table(file.path(dir,folders[i],paste(folders[i],'zMix.txt',sep='')),header=T,sep=' ',
                  stringsAsFactors = F) # read in lake specific metab data
  cur$lakeName<-folders[i]
  bella_zMix<-rbind(bella_zMix,cur)
}

#Add Mueggelsee back in manually
MueggelseeZMix<-read.delim(here("results/model_output_raw/Mueggelsee/MeuggelseezMix.txt"), sep=" ")
MueggelseeZMix$lakeName<-"Mueggelsee"

# bella_zMix %>%
#   filter(lakeName=="Taupo")%>%
#   mutate(zMix = replace(zMix, zMix==150, NA)) %>%
#   summarize(zMix_median=median(zMix, na.rm=TRUE))

bella_zMix <- bind_rows(bella_zMix,MueggelseeZMix) %>%
  mutate(zMix = replace(zMix, zMix==150, 27.85), #getting rid of zMix == 150m, replacing with median value
         zMix = replace(zMix, zMix==4.300000, 3.2))  #replace Utah zMix with zMean 
# View(bella_zMix)

bella_metab<-full_join(bella_GPP, bella_zMix, by=c("solarDay","lakeName"))
bella_metab<-full_join(bella_metab, bella_ER, by=c("solarDay","lakeName")) %>%
  mutate(solarDay=ymd(as.factor(solarDay)),
         lakeName=as.factor(lakeName),
         year=year(solarDay)) %>%
  rename(GPP_mgO2L=GPPFit,
         ER_mgO2L=rhoEst) %>%
  mutate(GPP_mgCL=GPP_mgO2L/32*12*1000,
         ER_mgCL=ER_mgO2L/32*12*1000,
         ER_mgO2m2=ER_mgO2L*zMix,
         GPP_mgO2m2=GPP_mgO2L*zMix,
         ER_mgCm2=ER_mgCL*zMix,
         GPP_mgCm2=GPP_mgCL*zMix)

dontuse <- subset(bella_metab, lakeName=="Taupo" & (solarDay < "2017-10-01" | solarDay > "2018-05-01"))
#Filter out some observations for lake Taupo so we are left with spring-fall data of one year

bella_metab<-anti_join(bella_metab, dontuse)

dontuse2 <- subset(bella_metab, lakeName=="P8" & solarDay > "2019-07-01")
#Filtering out days where the thermistor chain drifted into a macrophyte bed and the model couldn't predict GPP 

bella_metab<-anti_join(bella_metab, dontuse2)




