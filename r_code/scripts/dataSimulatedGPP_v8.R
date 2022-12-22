## Created on July 26, 2022 to create GPP estimates with new optimized parameters from SEJ


# Export data for Stuart (lakes without inflow data) ----------------------

#get names of lakes with modeled inflows
unmodeled_names <- load_comparisons_wide %>%
  filter(dataset=="modelled")

#Making GPP estimates from the Kelly Model from known data
export<-   metadata %>%
  select(lakeName, `Lake residence time (year)`, `Surface area (ha)`,
         `Maximum lake depth (m)`, `Volume (m3)`, `Mean lake depth (m)`) %>%
  filter(lakeName %in% unmodeled_names$lakeName) %>%
  left_join(., newts_full %>%
              select(lakeName, DOC_mgL, TP_ugL), by="lakeName") %>%
  rename(HRT_days=`Lake residence time (year)`,
         zMean=`Mean lake depth (m)`,
         SA_ha=`Surface area (ha)`,
         zMax=`Maximum lake depth (m)`, #maximum lake depth, m
         V_m3=`Volume (m3)`, #volume, m3
         DOC=DOC_mgL, #mg/L=g/m3, in-lake DOC concentrations
         TP=TP_ugL) %>% #ug/L=mg/m3, in-lake TP concentrations
         # DOCin=DOC_gm3) %>% #mg/L=g/m3, estimated DOC concentration of inflow
  mutate(HRT_days=HRT_days*365,#HRT (days)
         Qin=V_m3/HRT_days,
         SA=SA_ha/100) %>% #surface area in km2
  drop_na() 


glimpse(export)
n_distinct(export$lakeName)


export<-left_join(export, master_df %>%
                    select(lakeName, medianGPP, meanGPP, meanzMix),
                  by="lakeName") %>%
  select(-SA_ha)
write.csv(export, file = "data/gridSearchInput_unmodeledInfoLakes.csv", row.names = FALSE)


# Export data for Stuart (sites WITH inflow) ------------------------------


#Making GPP estimates from the Kelly Model from known data
sims2<-   left_join(inflow_conc_summary,metadata %>%
                      select(lakeName, `Lake residence time (year)`, `Surface area (ha)`,
                             `Maximum lake depth (m)`, `Volume (m3)`, `Mean lake depth (m)`), by="lakeName") %>%
  left_join(., newts_full %>%
              select(lakeName, DOC_mgL, TP_ugL), by="lakeName") %>%
  rename(HRT_days=`Lake residence time (year)`,
         zMean=`Mean lake depth (m)`,
         SA_ha=`Surface area (ha)`,
         zMax=`Maximum lake depth (m)`, #maximum lake depth, m
         V_m3=`Volume (m3)`, #volume, m3
         DOC=DOC_mgL, #mg/L=g/m3, in-lake DOC concentrations
         TP=TP_ugL, #ug/L=mg/m3, in-lake TP concentrations
         # DOCin=DOC_gm3) %>% #mg/L=g/m3, estimated DOC concentration of inflow
         DOCin=DOC_volWeighted_gm3, #mg/L=g/m3 estimated DOC concentration of inflow
         TPin=TP_volWeighted_mgm3
         # Qin=Qin_mean,
         ) %>%
  mutate(SA=SA_ha/100, #surface area in km2
         # Pin=TP_mgm3,#ug/L=mg/m3, estimated TP concentration of inflow
         # Qin=Qin*86400, #m3/day
         HRT_days=HRT_days*365, #HRT (days)
         DOC = replace(DOC, lakeName=="Lillinonah", 4.213750),
         Qin = V_m3/HRT_days) %>% #Just to be consistent with how we are calculating Qin for all the other lakes.  
  drop_na() %>%
  distinct(., lakeName, .keep_all = TRUE)
# mutate(DOCin = replace(DOCin, lakeName=="Acton", 6.5907),
#        Pin = replace(Pin, lakeName=="Acton", 97.5))
glimpse(sims2)
n_distinct(sims2$lakeName)


export<-left_join(sims2, master_df %>%
                    select(lakeName, medianGPP, meanGPP, meanzMix),
                  by="lakeName") %>%
  select(-Qin_total,-SA_ha, -Qin_mean)
write.csv(export, file = "data/gridSearchInput.csv", row.names = FALSE)



# 18 inflow sites ---------------------------------------------------------



#Making GPP estimates from the Kelly Model from known data
GPP_subset<-   left_join(inflow_conc_summary,metadata %>%
                      select(lakeName, `Lake residence time (year)`, `Surface area (ha)`,
                             `Maximum lake depth (m)`, `Volume (m3)`, `Mean lake depth (m)`), by="lakeName") %>%
  left_join(., newts_full %>%
              select(lakeName, DOC_mgL, TP_ugL), by="lakeName") %>%
  rename(HRT_days=`Lake residence time (year)`,
         zMean=`Mean lake depth (m)`,
         SA_ha=`Surface area (ha)`,
         zMax=`Maximum lake depth (m)`, #maximum lake depth, m
         V_m3=`Volume (m3)`, #volume, m3
         DOC=DOC_mgL, #mg/L=g/m3, in-lake DOC concentrations
         TP=TP_ugL, #ug/L=mg/m3, in-lake TP concentrations
         DOCin=DOC_gm3) %>% #mg/L=g/m3, estimated DOC concentration of inflow
  mutate(SA=SA_ha/100, #surface area in km2
         Pin=TP_mgm3,#ug/L=mg/m3, estimated TP concentration of inflow
         Qin=Qin*86400, #m3/day
         HRT_days=HRT_days*365) %>%#HRT (days)
  mutate(
    DOCin = replace(DOCin, lakeName=="EastLong", 13.60287933), #replace with modeled estimates for DOCin
    Pin = replace(Pin, lakeName=="EastLong", 18.074),#replace with modeled estimates for TPin
    DOC = replace(DOC, lakeName=="Lillinonah", 4.213750)) %>% 
  drop_na() %>%
  select(-DOC_load, -TP_load, -TP_mgm3, -SA_ha) %>%
  distinct() %>%#remove duplicate rows
  left_join(., bella_metab_summary %>%
              select(lakeName, meanGPP, medianGPP, meanzMix), by="lakeName")


#Add a column to d for incident light - this is just a constant made up number right now
GPP_subset$I0 <- 350

p_kelly <- log(c(kDOC=0.42,kA=0.00022,lA=0.1,pA=1.2,hA=55,mA=2,decay=0.001,cA=0.015,v=0.1,rec=0.95))
p_oleksy <- log(c(kDOC=0.35,kA=0.0001,lA=0.1,pA=1.2,hA=55,mA=2,decay=0.002,cA=0.005,v=0.01,rec=0.95))

p_optim <- read_csv("data/params_20220726.csv") %>%
  slice(1)
# p_optim<-as.numeric(as.character(params))
paramNames<-c("kDOC",
              "kA",
              "lA",
              "pA",
              "hA",
              "mA",
              "decay",
              "cA",
              "v",
              "rec")
names(p_optim)<-paramNames
p_optim <- log(p_optim)

# Take a look at how these compare 
kelly_p <- c(kDOC=0.42,kA=0.00022,lA=0.1,pA=1,hA=55,mA=2,decay=0.001,cA=0.015,v=0.1,rec=0.95) 
oleksy_p <- c(kDOC=0.35,kA=0.0001,lA=0.1,pA=1.2,hA=55,mA=2,decay=0.002,cA=0.005,v=0.01,rec=0.95) 
optim_p <- read_csv("data/params_20220726.csv") %>%
  slice(1)
# comparison <- bind_rows(kelly_p,optim_p)
comparison <- bind_rows(kelly_p, oleksy_p, optim_p)
# sets<-data.frame(c("kelly","optim"))
sets<-data.frame(c("kelly","oleksy","optim"))
names(sets)<-"set"
comparison <- bind_cols(sets,comparison) 

# ~ Make predictions for lakes with inflows --------------------------------------------------------
source('r_code/scripts/paramOptim/huisman.r')
source('r_code/scripts/paramOptim/kellyPredix.r')
source('r_code/scripts/paramOptim/lightAtten.R')
source('r_code/scripts/paramOptim/kellyLoss.R')



### Run predictions with default Kelly parameters first
fits_kelly <- data.frame(kellyPredix(p_kelly,GPP_subset)) %>%
  mutate(lakeName=GPP_subset$lakeName,
         param_set="kelly")

fits_kelly_long<-fits_kelly%>%
  pivot_longer(cols=(1:4))

#model performance kelly
kelly_performance <- fits_kelly %>%
  inner_join(., GPP_subset %>%
               select(lakeName, meanGPP, medianGPP, meanzMix, DOC, TP), by="lakeName") %>%
  group_by(param_set) %>%
  summarize(rmse_GPPmean=round(rmse(meanGPP, GPPHat),1),
         rmse_GPPmedian=round(rmse(medianGPP, GPPHat),1),
         rmse_TP=round(rmse(TP,PHat),1),
         rmse_zMix=round(rmse(meanzMix, zMixHat),1),
         rmse_DOC=round(rmse(DOC,CHat),1),
         percBias_GPPmean=round(percent_bias(meanGPP, GPPHat),2),
         percBias_GPPmedian=round(percent_bias(medianGPP, GPPHat),2),
         percBias_TP=round(percent_bias(TP,PHat),2),
         percBias_zMix=round(percent_bias(meanzMix, zMixHat),2),
         percBias_DOC=round(percent_bias(DOC,CHat),2))

### Run predictions with optim parameters
fits_optim <- data.frame(kellyPredix(p_optim,GPP_subset))%>%
  mutate(lakeName=GPP_subset$lakeName,
         param_set="optim")

fits_optim_long <- fits_optim %>%
  pivot_longer(cols=(1:4)) %>%
  mutate(value=as.numeric(unlist(value)))

#model performance optim
optim_performance <- data.frame(lapply(fits_optim, function(x) unlist(x))) %>% #for some reason columns are in list form, this unlists them
  inner_join(., GPP_subset %>%
               select(lakeName, meanGPP, medianGPP, meanzMix, DOC, TP), by="lakeName") %>%
  group_by(param_set) %>%
  summarize(rmse_GPPmean=round(rmse(meanGPP, GPPHat),1),
            rmse_GPPmedian=round(rmse(medianGPP, GPPHat),1),
            rmse_TP=round(rmse(TP,PHat),1),
            rmse_zMix=round(rmse(meanzMix, zMixHat),1),
            rmse_DOC=round(rmse(DOC,CHat),1),
            percBias_GPPmean=round(percent_bias(meanGPP, GPPHat),2),
            percBias_GPPmedian=round(percent_bias(medianGPP, GPPHat),2),
            percBias_TP=round(percent_bias(TP,PHat),2),
            percBias_zMix=round(percent_bias(meanzMix, zMixHat),2),
            percBias_DOC=round(percent_bias(DOC,CHat),2))


### Run predictions with optim parameters
fits_oleksy <- data.frame(kellyPredix(p_oleksy,GPP_subset))%>%
  mutate(lakeName=GPP_subset$lakeName,
         param_set="oleksy")

fits_oleksy_long <- fits_oleksy %>%
  pivot_longer(cols=(1:4)) %>%
  mutate(value=as.numeric(unlist(value)))

#model performance oleksy
oleksy_performance <- data.frame(lapply(fits_oleksy, function(x) unlist(x))) %>% #for some reason columns are in list form, this unlists them
  inner_join(., GPP_subset %>%
               select(lakeName, meanGPP, medianGPP, meanzMix, DOC, TP), by="lakeName") %>%
  group_by(param_set) %>%
  summarize(rmse_GPPmean=round(rmse(meanGPP, GPPHat),1),
            rmse_GPPmedian=round(rmse(medianGPP, GPPHat),1),
            rmse_TP=round(rmse(TP,PHat),1),
            rmse_zMix=round(rmse(meanzMix, zMixHat),1),
            rmse_DOC=round(rmse(DOC,CHat),1),
            percBias_GPPmean=round(percent_bias(meanGPP, GPPHat),2),
            percBias_GPPmedian=round(percent_bias(medianGPP, GPPHat),2),
            percBias_TP=round(percent_bias(TP,PHat),2),
            percBias_zMix=round(percent_bias(meanzMix, zMixHat),2),
            percBias_DOC=round(percent_bias(DOC,CHat),2))



### Combine
model_predictions <- bind_rows(fits_kelly_long,fits_optim_long,fits_oleksy_long)
model_predictions <- inner_join(model_predictions, GPP_subset %>%
                                   select(lakeName, meanGPP, medianGPP, meanzMix, DOC, TP), by="lakeName")


model_performance <- bind_rows(optim_performance, oleksy_performance, kelly_performance) %>%
  mutate(xpos=Inf,
         ypos=-Inf,
         hjustvar=1.2,
         vjustvar=-0.5) #For position of labels, anchor at top left corner

# ~ Compare results on the GPP_subset ---------------------------------------

A <-
  model_predictions %>%
  filter(name=="GPPHat") %>%
  # filter(param_set=="optim") %>%
  ggplot(aes(x=meanGPP, y=value))+
  geom_point(shape=21, size=3, color="grey50", fill="grey50", alpha=0.5)+
  facet_wrap(~param_set)+
  theme_few()+
  ylab(expression(paste('PRED. GPP (mg C m'^-2,' day'^-1,')')))+
  xlab(expression(paste('OBS. mean GPP (mg C m'^-2,' day'^-1,')')))+
  geom_text_repel(aes(label = lakeName), size = 3, max.overlaps=2)+
  geom_abline(intercept = 0, slope = 1)+
  geom_text(data=model_performance,
            aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,
                label= paste0("RMSE=", rmse_GPPmean)))+
  theme(legend.position="none")

B<-model_predictions %>%
  filter(name=="GPPHat") %>%
  # filter(param_set=="optim") %>%
  ggplot(aes(x=medianGPP, y=value))+
  geom_point(shape=21, size=3, color="grey50", fill="grey50", alpha=0.5)+
  facet_wrap(~param_set)+
  theme_few()+
  ylab(expression(paste('PRED. GPP (mg C m'^-2,' day'^-1,')')))+
  xlab(expression(paste('OBS. median GPP (mg C m'^-2,' day'^-1,')')))+
  geom_text_repel(aes(label = lakeName), size = 3, max.overlaps=2)+
  geom_abline(intercept = 0, slope = 1)+
  geom_text(data=model_performance,
            aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,
                label= paste0("RMSE=", rmse_GPPmedian)))+
  theme(legend.position="none")

C<-model_predictions %>%
  filter(name=="CHat") %>%
  # filter(param_set=="optim") %>%
  filter(lakeName %in% c("Mangstrettjarn","Morris","Ovre","Struptjarn"))%>%
  ggplot(aes(x=DOC, y=value))+
  geom_point(shape=21, size=3, color="grey50", fill="grey50", alpha=0.5)+
  facet_wrap(~param_set)+
  theme_few()+
  geom_abline(intercept = 0, slope = 1)+
  ylab(expression(paste('PRED. lake DOC mg L'^-1,)))+
  xlab(expression(paste('OBS. lake DOC mg L'^-1,)))+
  geom_text_repel(aes(label = lakeName), size = 3, max.overlaps=2)+
  geom_abline(intercept = 0, slope = 1)+
  geom_text(data=model_performance,
            aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,
                label= paste0("RMSE=", rmse_DOC)))+
  theme(legend.position="none")

D<- model_predictions %>%
  filter(name=="PHat") %>%
  # filter(lakeName %in% c("Mangstrettjarn","Morris","Ovre","Struptjarn"))%>%
  # filter(param_set=="optim") %>%
  ggplot(aes(x=TP, y=value))+
  geom_point(shape=21, size=3, color="grey50", fill="grey50", alpha=0.5)+
  facet_wrap(~param_set)+
  theme_few()+
  geom_abline(intercept = 0, slope = 1)+
  ylab(expression(paste('PRED. lake TP ug L'^-1,)))+
  xlab(expression(paste('OBS. lake TP ug L'^-1,)))+
  geom_text_repel(aes(label = lakeName), size = 3, max.overlaps=2)+
  geom_abline(intercept = 0, slope = 1)+
  geom_text(data=model_performance,
            aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,
                label= paste0("RMSE=", rmse_TP)))+
  theme(legend.position="none")


E<- model_predictions %>%
  filter(name=="zMixHat") %>%
  # filter(param_set=="optim") %>%
  # filter(lakeName %in% c("Mangstrettjarn","Morris","Ovre","Struptjarn"))%>%
  ggplot(aes(x=meanzMix, y=value))+
  geom_point(shape=21, size=3, color="grey50", fill="grey50", alpha=0.5)+
  facet_wrap(~param_set)+
  theme_few()+
  geom_abline(intercept = 0, slope = 1)+
  ylab("PRED. zMix (m)")+
  xlab("OBS. zMix (m)")+
  geom_text_repel(aes(label = lakeName), size = 3, max.overlaps=2)+
  geom_abline(intercept = 0, slope = 1)+
  geom_text(data=model_performance ,
            aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,
                label= paste0("RMSE=", rmse_zMix)))+
  theme(legend.position="none")

gg_title = str_wrap("GPP, DOC, TP, zMix predictions on lakes with empirical inflow data", width=40)
predictions_kelly_vs_optim_parameters_plot<-A/B/C/D/E+plot_annotation(title = gg_title) 
predictions_kelly_vs_optim_parameters_plot
ggsave(here("figures/CaryMeeting/GPP_predictions_18lakes_empirical_inflow_data.png"), dpi=600, width=6,height=13.5, units="in")


# What are some of the lakes where predictions are way too high?
model_predictions %>%
  filter(name=="GPPHat") %>%
  filter(param_set=="optim") %>%
  filter(medianGPP < 1000) %>%
  ggplot(aes(x=medianGPP, y=value))+
  geom_point(shape=21, size=3)+
  # facet_wrap(~param_set)+
  theme_few()+
  ylab(expression(paste('PRED. GPP (mg C m'^-2,' day'^-1,')')))+
  xlab(expression(paste('OBcanyS. median GPP (mg C m'^-2,' day'^-1,')')))+
  geom_text_repel(aes(label = lakeName), size = 3, max.overlaps=2)+
  geom_abline(intercept = 0, slope = 1)+
  geom_text(data=model_performance%>% filter(param_set=="optim"),
            aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,
                label= paste0("RMSE=", rmse_GPPmedian)))+
  theme(legend.position="none")


#Just GPP
model_predictions %>%
  filter(name=="GPPHat") %>%
  filter(param_set %in% c("kelly","optim")) %>%
  # filter(value>1) %>%
  mutate(value=case_when(value<1 ~ 1,
          TRUE ~ value)) %>%
  ggplot(aes(x=log10(medianGPP), y=log10(value)))+
  geom_point(shape=21, size=3, color="black", fill="grey50", alpha=0.5)+
  facet_wrap(~param_set)+
  theme_few()+
  ylab(expression(paste('log[10] PRED. GPP (mg C m'^-2,' day'^-1,')')))+
  xlab(expression(paste('log[10] OBS. median GPP (mg C m'^-2,' day'^-1,')')))+
  # geom_text_repel(aes(label = lakeName), size = 3, max.overlaps=2)+
  geom_abline(intercept = 0, slope = 1, linetype="dashed")+
  geom_text(data=model_performance%>%
              filter(param_set %in% c("kelly","optim")),
            aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,
                label= paste0("RMSE=", rmse_GPPmedian)))+
  theme(legend.position="none")
ggsave(here("figures/SIL/GPP_predictions_18lakes_empirical_inflow_data.png"), dpi=600, width=7.5,height=4, units="in")


# Predictions for full set of lakes  --------------------------------------


#ALL LAKES
source(here("r_code/dataPullLoads.R")) #need the load_estimates_huisman and modelled_loads_kg df's

GPP_fullset <-   left_join(modelled_loads_kg,metadata %>%
                             select(lakeName, `Surface area (ha)`,
                                    `Maximum lake depth (m)`, `Mean lake depth (m)`), by="lakeName") %>%
  left_join(., newts_full %>%
              select(lakeName, DOC_mgL, TP_ugL), by="lakeName") %>%
  rename(zMean=`Mean lake depth (m)`,
         SA_ha=`Surface area (ha)`,
         zMax=`Maximum lake depth (m)`, #maximum lake depth, m
         # V_m3=V, #volume, m3
         DOC=DOC_mgL, #mg/L=g/m3, in-lake DOC concentrations
         TP=TP_ugL, #ug/L=mg/m3, in-lake TP concentrations
         DOCin=DOC_gm3) %>% #mg/L=g/m3, estimated DOC concentration of inflow
  mutate(SA=SA_ha/100, #surface area in km2
         Pin=TP_mgm3) %>% #ug/L=mg/m3, estimated TP concentration of inflow
  mutate(
    DOCin = replace(DOCin, lakeName=="EastLong", 13.60287933), #replace with modeled estimates for DOCin
    Pin = replace(Pin, lakeName=="EastLong", 18.074),#replace with modeled estimates for TPin
    DOC = replace(DOC, lakeName=="Lillinonah", 4.213750),
    TP = replace(TP, lakeName=="Almberga", 5),#replace with very low value that was used in the load estimate 
    TP = replace(TP, lakeName=="P1", 35), #SRP value
    TP = replace(TP, lakeName=="P8", 30),  #SRP value
    zMax = ifelse(is.na(zMax), zMean, zMax)) %>%#for lakes without zMax, replace with zMean
  drop_na() %>%
  select(-DOC_load_kg, -TP_load_kg, -TP_mgm3, -SA_ha) %>%
  distinct() %>%#remove duplicate Erken row
  left_join(., bella_metab_summary %>%
              select(lakeName, meanGPP, medianGPP, meanzMix), by="lakeName")



#Add a column to d for incident light - this is just a constant made up number right now
GPP_fullset$I0 <- 350

p_kelly <- log(c(kDOC=0.42,kA=0.00022,lA=0.1,pA=1.2,hA=55,mA=2,decay=0.001,cA=0.015,v=0.1,rec=0.95))
p_oleksy <- log(c(kDOC=0.35,kA=0.0001,lA=0.1,pA=1.2,hA=55,mA=2,decay=0.002,cA=0.005,v=0.01,rec=0.95))

p_optim <- read_csv("data/params_20220726.csv") %>%
  slice(1)
# p_optim<-as.numeric(as.character(params))
paramNames<-c("kDOC",
              "kA",
              "lA",
              "pA",
              "hA",
              "mA",
              "decay",
              "cA",
              "v",
              "rec")
names(p_optim)<-paramNames
p_optim <- log(p_optim)

# Take a look at how these compare 
# kelly_p <- c(kDOC=0.42,kA=0.00022,lA=0.1,pA=1.2,hA=55,mA=2,decay=0.001,cA=0.015,v=0.1,rec=0.95)
# optim_p <- read_csv("data/params_20220726.csv") %>%
#   slice(1)
# comparison <- bind_rows(kelly_p,optim_p)



# ~ Make predictions for full set --------------------------------------------------------
source('r_code/scripts/paramOptim/huisman.r')
source('r_code/scripts/paramOptim/kellyPredix.r')
source('r_code/scripts/paramOptim/lightAtten.R')
source('r_code/scripts/paramOptim/kellyLoss.R')



### Run predictions with default Kelly parameters first
fits_kelly <- data.frame(kellyPredix(p_kelly,GPP_fullset)) %>%
  mutate(lakeName=GPP_fullset$lakeName,
         param_set="kelly")

fits_kelly_long<-fits_kelly%>%
  pivot_longer(cols=(1:4))

#model performance kelly
kelly_performance <- fits_kelly %>%
  inner_join(., GPP_fullset %>%
               select(lakeName, meanGPP, medianGPP, meanzMix, DOC, TP), by="lakeName") %>%
  group_by(param_set) %>%
  summarize(rmse_GPPmean=round(rmse(meanGPP, GPPHat),1),
            rmse_GPPmedian=round(rmse(medianGPP, GPPHat),1),
            rmse_TP=round(rmse(TP,PHat),1),
            rmse_zMix=round(rmse(meanzMix, zMixHat),1),
            rmse_DOC=round(rmse(DOC,CHat),1),
            percBias_GPPmean=round(percent_bias(meanGPP, GPPHat),2),
            percBias_GPPmedian=round(percent_bias(medianGPP, GPPHat),2),
            percBias_TP=round(percent_bias(TP,PHat),2),
            percBias_zMix=round(percent_bias(meanzMix, zMixHat),2),
            percBias_DOC=round(percent_bias(DOC,CHat),2))

### Run predictions with optim parameters
fits_optim <- data.frame(kellyPredix(p_optim,GPP_fullset))%>%
  mutate(lakeName=GPP_fullset$lakeName,
         param_set="optim")

fits_optim_long <- fits_optim %>%
  pivot_longer(cols=(1:4)) %>%
  mutate(value=as.numeric(unlist(value)))


#model performance optim
optim_performance <- data.frame(lapply(fits_optim, function(x) unlist(x))) %>% #for some reason columns are in list form, this unlists them
  inner_join(., GPP_fullset %>%
               select(lakeName, meanGPP, medianGPP, meanzMix, DOC, TP), by="lakeName") %>%
  group_by(param_set) %>%
  summarize(rmse_GPPmean=round(rmse(meanGPP, GPPHat),1),
            rmse_GPPmedian=round(rmse(medianGPP, GPPHat),1),
            rmse_TP=round(rmse(TP,PHat),1),
            rmse_zMix=round(rmse(meanzMix, zMixHat),1),
            rmse_DOC=round(rmse(DOC,CHat),1),
            percBias_GPPmean=round(percent_bias(meanGPP, GPPHat),2),
            percBias_GPPmedian=round(percent_bias(medianGPP, GPPHat),2),
            percBias_TP=round(percent_bias(TP,PHat),2),
            percBias_zMix=round(percent_bias(meanzMix, zMixHat),2),
            percBias_DOC=round(percent_bias(DOC,CHat),2))

### Run predictions with optim parameters
fits_oleksy <- data.frame(kellyPredix(p_oleksy,GPP_fullset))%>%
  mutate(lakeName=GPP_fullset$lakeName,
         param_set="oleksy")

fits_oleksy_long <- fits_oleksy %>%
  pivot_longer(cols=(1:4)) %>%
  mutate(value=as.numeric(unlist(value)))



#model performance oleksy
oleksy_performance <- data.frame(lapply(fits_oleksy, function(x) unlist(x))) %>% #for some reason columns are in list form, this unlists them
  inner_join(., GPP_fullset %>%
               select(lakeName, meanGPP, medianGPP, meanzMix, DOC, TP), by="lakeName") %>%
  group_by(param_set) %>%
  summarize(rmse_GPPmean=round(rmse(meanGPP, GPPHat),1),
            rmse_GPPmedian=round(rmse(medianGPP, GPPHat),1),
            rmse_TP=round(rmse(TP,PHat),1),
            rmse_zMix=round(rmse(meanzMix, zMixHat),1),
            rmse_DOC=round(rmse(DOC,CHat),1),
            percBias_GPPmean=round(percent_bias(meanGPP, GPPHat),2),
            percBias_GPPmedian=round(percent_bias(medianGPP, GPPHat),2),
            percBias_TP=round(percent_bias(TP,PHat),2),
            percBias_zMix=round(percent_bias(meanzMix, zMixHat),2),
            percBias_DOC=round(percent_bias(DOC,CHat),2))



### Combine
model_predictions <- bind_rows(fits_kelly_long,fits_optim_long,fits_oleksy_long)
model_predictions <- inner_join(model_predictions, GPP_fullset %>%
                                  select(lakeName, meanGPP, medianGPP, meanzMix, DOC, TP), by="lakeName")


model_performance <- bind_rows(optim_performance, oleksy_performance, kelly_performance) %>%
  mutate(xpos=Inf,
         ypos=-Inf,
         hjustvar=1.2,
         vjustvar=-0.5) #For position of labels, anchor at top left corner





# ~ Compare results on the GPP_subset ---------------------------------------

A <-
  model_predictions %>%
  filter(name=="GPPHat") %>%
  filter(param_set=="oleksy") %>%
  ggplot(aes(x=meanGPP, y=value))+
  geom_point(shape=21, size=3, color="grey50", fill="grey50", alpha=0.5)+
  # facet_wrap(~param_set)+
  theme_few()+
  ylab(expression(paste('PRED. GPP (mg C m'^-2,' day'^-1,')')))+
  xlab(expression(paste('OBS. mean GPP (mg C m'^-2,' day'^-1,')')))+
  geom_text_repel(aes(label = lakeName), size = 3, max.overlaps=2)+
  geom_abline(intercept = 0, slope = 1)+
  geom_text(data=model_performance%>%
              filter(param_set=="kelly"),
            aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,
                label= paste0("RMSE=", rmse_GPPmean)))+
  theme(legend.position="none")

B<-model_predictions %>%
  filter(name=="GPPHat") %>%
  filter(param_set=="oleksy") %>%
  ggplot(aes(x=medianGPP, y=value))+
  geom_point(shape=21, size=3, color="grey50", fill="grey50", alpha=0.5)+
  # facet_wrap(~param_set)+
  theme_few()+
  ylab(expression(paste('PRED. GPP (mg C m'^-2,' day'^-1,')')))+
  xlab(expression(paste('OBS. median GPP (mg C m'^-2,' day'^-1,')')))+
  geom_text_repel(aes(label = lakeName), size = 3, max.overlaps=2)+
  geom_abline(intercept = 0, slope = 1)+
  geom_text(data=model_performance%>%
              filter(param_set=="kelly"),
            aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,
                label= paste0("RMSE=", rmse_GPPmedian)))+
  theme(legend.position="none")

C<-model_predictions %>%
  filter(name=="CHat") %>%
  filter(param_set=="oleksy") %>%
  ggplot(aes(x=DOC, y=value))+
  geom_point(shape=21, size=3, color="grey50", fill="grey50", alpha=0.5)+
  # facet_wrap(~param_set)+
  theme_few()+
  geom_abline(intercept = 0, slope = 1)+
  ylab(expression(paste('PRED. lake DOC mg L'^-1,)))+
  xlab(expression(paste('OBS. lake DOC mg L'^-1,)))+
  geom_text_repel(aes(label = lakeName), size = 3, max.overlaps=2)+
  geom_abline(intercept = 0, slope = 1)+
  geom_text(data=model_performance%>%
              filter(param_set=="kelly"),
            aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,
                label= paste0("RMSE=", rmse_DOC)))+
  theme(legend.position="none")

D<- model_predictions %>%
  filter(name=="PHat") %>%
  filter(param_set=="oleksy") %>%
  ggplot(aes(x=TP, y=value))+
  geom_point(shape=21, size=3, color="grey50", fill="grey50", alpha=0.5)+
  # facet_wrap(~param_set)+
  theme_few()+
  geom_abline(intercept = 0, slope = 1)+
  ylab(expression(paste('PRED. lake TP ug L'^-1,)))+
  xlab(expression(paste('OBS. lake TP ug L'^-1,)))+
  geom_text_repel(aes(label = lakeName), size = 3, max.overlaps=2)+
  geom_abline(intercept = 0, slope = 1)+
  geom_text(data=model_performance%>%
              filter(param_set=="kelly"),
            aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,
                label= paste0("RMSE=", rmse_TP)))+
  theme(legend.position="none")


E<- model_predictions %>%
  filter(name=="zMixHat") %>%
  filter(param_set=="oleksy") %>%
  ggplot(aes(x=meanzMix, y=value))+
  geom_point(shape=21, size=3, color="grey50", fill="grey50", alpha=0.5)+
  # facet_wrap(~param_set)+
  theme_few()+
  geom_abline(intercept = 0, slope = 1)+
  ylab("PRED. zMix (m)")+
  xlab("OBS. zMix (m)")+
  geom_text_repel(aes(label = lakeName), size = 3, max.overlaps=2)+
  geom_abline(intercept = 0, slope = 1)+
  geom_text(data=model_performance%>%
              filter(param_set=="kelly"),
            aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,
                label= paste0("RMSE=", rmse_zMix)))+
  theme(legend.position="none")

gg_title = str_wrap("GPP, DOC, TP, zMix predictions using modelled inflow data", width=40)
predictions_kelly_vs_optim_parameters_plot<-A/B/C/D/E+plot_annotation(title = gg_title) 
predictions_kelly_vs_optim_parameters_plot
ggsave(here("figures/GPP_predictions_using_modeled_inflow_data.png"), dpi=600, width=6,height=13.5, units="in")


gg_title = str_wrap("GPP, DOC, TP, zMix predictions using modelled inflow data", width=30)
predictions_kelly_vs_optim_parameters_plot<-B/C/D/E+plot_annotation(title = gg_title) 
predictions_kelly_vs_optim_parameters_plot
ggsave(here("figures/SIL/GPP_predictions_using_modeled_inflow_data_kellyset.png"), dpi=600, width=3,height=11, units="in")



#Just GPP
model_predictions %>%
  filter(name=="GPPHat") %>%
  filter(param_set %in% c("kelly","optim")) %>%
  ggplot(aes(x=medianGPP, y=value))+
  geom_point(shape=21, size=3, color="grey50", fill="grey50", alpha=0.5)+
  facet_wrap(~param_set)+
  theme_few()+
  ylab(expression(paste('PRED. GPP (mg C m'^-2,' day'^-1,')')))+
  xlab(expression(paste('OBS. median GPP (mg C m'^-2,' day'^-1,')')))+
  geom_text_repel(aes(label = lakeName), size = 3, max.overlaps=2)+
  geom_abline(intercept = 0, slope = 1)+
  geom_text(data=model_performance%>%
              filter(param_set %in% c("kelly","optim")),
            aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,
                label= paste0("RMSE=", rmse_GPPmedian)))+
  theme(legend.position="none")


# LOAD ESTIMATES FOR LAKES WITH KNOWN LOADS ----------------------------------------------------------


#KELLY SET
GPP_subset_load_estimates<-
  # read.csv(here("data/load_estimates/load_estimates_20220726.csv")) %>%
  # read.csv(here("data/load_estimates/load_estimates_20220801_oleksyset.csv")) %>%
  read.csv(here("data/load_estimates/load_estimates_20220801_kellyset.csv")) %>%
  select(variable, Erken_fit, Mueggelsee_fit,Acton_fit,
         Taupo_fit, TheLoch_fit,Mangstrettjarn_fit,
         Harp_fit, Langtjern_fit,Lillinonah_fit,
         Lillsjoliden_fit, Feeagh_fit, Morris_fit, Ovre_fit, Struptjarn_fit, Trout_fit,
         Vortsjarv_fit, EastLong_fit) %>%
  mutate_at(vars(-variable), as.numeric) %>%
  pivot_longer(-variable) %>%
  rename(lakeName=name)%>%
  separate(lakeName, c("lakeName","delete"),"_") %>% #split into two columns by underscore ("_")
  select(-delete) %>%
  mutate(lakeName=  str_to_sentence(lakeName), #capitalize all the lake names if they aren't already
         lakeName = replace(lakeName, lakeName=="Theloch", 'TheLoch'), #But still need to fix a few names
         lakeName = replace(lakeName, lakeName=="Eastlong", "EastLong")) %>%
  pivot_wider(names_from="variable", values_from="value") %>%
  rename(TPin_est=TP_mgm3,
         DOCin_est=DOC_gm3)


#"_est" means modelled using new optimized parameters
G<-GPP_subset %>%
  inner_join(GPP_subset_load_estimates) %>%
  ggplot(aes(x=DOCin,y=DOCin_est))+
  geom_point(shape=21,size=3)+
  geom_abline(intercept = 0, slope = 1)+
  geom_text_repel(aes(label = lakeName), size = 3, max.overlaps=2)+
  labs(y="Modelled DOC in (g/m3)",
       x="Measured DOC in (g/m3)") +
  theme_few()

H<-GPP_subset %>%
  inner_join(GPP_subset_load_estimates) %>%
  ggplot(aes(x=Pin,y=TPin_est))+
  geom_point(shape=21,size=3)+
  geom_abline(intercept = 0, slope = 1)+
  geom_text_repel(aes(label = lakeName), size = 3, max.overlaps=2)+
  labs(y="Modelled TP in (mg/m3)",
       x="Measured TP in (mg/m3)") +
  theme_few()


# I<-GPP_subset %>%
#   inner_join(GPP_subset_load_estimates) %>%
#   ggplot(aes(x=DOCin/Pin,y=DOCin_est/TPin_est))+
#   geom_point(shape=21,size=3)+
#   geom_abline(intercept = 0, slope = 1)+
#   geom_text_repel(aes(label = lakeName), size = 3, max.overlaps=2)+
#   labs(y="DOC/TP ratio (modelled)",
#        x="DOC/TP ratio (measured)") +
#   theme_few()

J<-GPP_subset %>%
  inner_join(GPP_subset_load_estimates) %>%
  ggplot(aes(x=DOCin*Qin*(1/1000),y=DOCin_est*Qin*(1/1000)))+
  geom_point(shape=21,size=3)+
  geom_abline(intercept = 0, slope = 1)+
  geom_text_repel(aes(label = lakeName), size = 3, max.overlaps=10)+
  scale_x_log10(expression(Measured~DOC~load~(kg)), labels = scales::comma)+
  scale_y_log10(expression(Modeled~DOC~load~(kg)), labels = scales::comma)+
  theme_few()

K<-GPP_subset %>%
  inner_join(GPP_subset_load_estimates) %>%
  ggplot(aes(x=Pin*Qin*(1/1000000),y=TPin_est*Qin*(1/1000000)))+
  geom_point(shape=21,size=3)+
  geom_abline(intercept = 0, slope = 1)+
  geom_text_repel(aes(label = lakeName), size = 3, max.overlaps=10)+
  labs(y="Estimated P loads (kg)",
       x="Measured P loads (kg)")+
  scale_x_log10(expression(Measured~TP~load~(kg)), labels = scales::comma)+
  scale_y_log10(expression(Modeled~TP~load~(kg)), labels = scales::comma)+
  theme_few()


gg_title = str_wrap("Modeled vs. measured loads using original Kelly model parameter set", width=40)
inflow_comparison_plot<-G/H/J/K+plot_annotation(title = gg_title) 
inflow_comparison_plot
ggsave(here("figures/inflow_C_P_comparisons_kellyset.png"), dpi=600, width=4, height=10, units="in")

J+K
ggsave(here("figures/SIL/inflow_C_P_comparisons_kellyset.png"), dpi=600, width=8, height=4, units="in")


#OPTIM SET
GPP_subset_load_estimates<-
  read.csv(here("data/load_estimates/load_estimates_20220726.csv")) %>%
  # read.csv(here("data/load_estimates/load_estimates_20220801_oleksyset.csv")) %>%
  # read.csv(here("data/load_estimates/load_estimates_20220801_kellyset.csv")) %>%
  select(variable, Erken_fit, Mueggelsee_fit,Acton_fit,
         Taupo_fit, TheLoch_fit,Mangstrettjarn_fit,
         Harp_fit, Langtjern_fit,Lillinonah_fit,
         Lillsjoliden_fit, Feeagh_fit, Morris_fit, Ovre_fit, Struptjarn_fit, Trout_fit,
         Vortsjarv_fit, EastLong_fit) %>%
  mutate_at(vars(-variable), as.numeric) %>%
  pivot_longer(-variable) %>%
  rename(lakeName=name)%>%
  separate(lakeName, c("lakeName","delete"),"_") %>% #split into two columns by underscore ("_")
  select(-delete) %>%
  mutate(lakeName=  str_to_sentence(lakeName), #capitalize all the lake names if they aren't already
         lakeName = replace(lakeName, lakeName=="Theloch", 'TheLoch'), #But still need to fix a few names
         lakeName = replace(lakeName, lakeName=="Eastlong", "EastLong")) %>%
  pivot_wider(names_from="variable", values_from="value") %>%
  rename(TPin_est=TP_mgm3,
         DOCin_est=DOC_gm3)


#"_est" means modelled using new optimized parameters
G<-GPP_subset %>%
  inner_join(GPP_subset_load_estimates) %>%
  ggplot(aes(x=DOCin,y=DOCin_est))+
  geom_point(shape=21,size=3)+
  geom_abline(intercept = 0, slope = 1)+
  geom_text_repel(aes(label = lakeName), size = 3, max.overlaps=2)+
  labs(y="Modelled DOC in (g/m3)",
       x="Measured DOC in (g/m3)") +
  theme_few()

H<-GPP_subset %>%
  inner_join(GPP_subset_load_estimates) %>%
  ggplot(aes(x=Pin,y=TPin_est))+
  geom_point(shape=21,size=3)+
  geom_abline(intercept = 0, slope = 1)+
  geom_text_repel(aes(label = lakeName), size = 3, max.overlaps=2)+
  labs(y="Modelled TP in (mg/m3)",
       x="Measured TP in (mg/m3)") +
  theme_few()


# I<-GPP_subset %>%
#   inner_join(GPP_subset_load_estimates) %>%
#   ggplot(aes(x=DOCin/Pin,y=DOCin_est/TPin_est))+
#   geom_point(shape=21,size=3)+
#   geom_abline(intercept = 0, slope = 1)+
#   geom_text_repel(aes(label = lakeName), size = 3, max.overlaps=2)+
#   labs(y="DOC/TP ratio (modelled)",
#        x="DOC/TP ratio (measured)") +
#   theme_few()

J<-GPP_subset %>%
  inner_join(GPP_subset_load_estimates) %>%
  ggplot(aes(x=DOCin*Qin*(1/1000),y=DOCin_est*Qin*(1/1000)))+
  geom_point(shape=21,size=3)+
  geom_abline(intercept = 0, slope = 1)+
  geom_text_repel(aes(label = lakeName), size = 3, max.overlaps=10)+
  scale_x_log10(expression(DOC~load~(kg)), labels = scales::comma)+
  scale_y_log10(expression(DOC~load~(kg)), labels = scales::comma)+
  theme_few()

K<-GPP_subset %>%
  inner_join(GPP_subset_load_estimates) %>%
  ggplot(aes(x=Pin*Qin*(1/1000000),y=TPin_est*Qin*(1/1000000)))+
  geom_point(shape=21,size=3)+
  geom_abline(intercept = 0, slope = 1)+
  geom_text_repel(aes(label = lakeName), size = 3, max.overlaps=10)+
  labs(y="Estimated P loads (kg)",
       x="Measured P loads (kg)")+
  scale_x_log10(expression(TP~load~(kg)), labels = scales::comma)+
  scale_y_log10(expression(TP~load~(kg)), labels = scales::comma)+
  theme_few()


gg_title = str_wrap("Modeled vs. measured loads using optimized parameter set (2022-07-26)", width=40)
inflow_comparison_plot<-G/H/J/K+plot_annotation(title = gg_title) 
inflow_comparison_plot
ggsave(here("figures/inflow_C_P_comparisons_optimset.png"), dpi=600, width=4, height=10, units="in")



# Export predictions ------------------------------------------------------

# preds_full<-inner_join(fits_kelly_long, GPP_fullset %>%
#              select(lakeName, meanGPP, medianGPP, meanzMix, DOC, TP), by="lakeName") %>%
#   pivot_wider(names_from=name,values_from=value)

preds_full<-left_join(fits_optim_long, GPP_fullset, by="lakeName")



# ~ Compare results on the GPP_subset ---------------------------------------

A <-
  fits_optim_long %>%
  left_join(.,GPP_fullset %>%
              select(lakeName, meanGPP, medianGPP, meanzMix, DOC, TP), by="lakeName") %>%
  filter(name=="GPPHat") %>%
  # filter(param_set=="optim") %>%
  ggplot(aes(x=meanGPP, y=value))+
  geom_point(shape=21, size=3, color="grey50", fill="grey50", alpha=0.5)+
  # facet_wrap(~param_set)+
  theme_few()+
  ylab(expression(paste('PRED. GPP (mg C m'^-2,' day'^-1,')')))+
  xlab(expression(paste('OBS. mean GPP (mg C m'^-2,' day'^-1,')')))+
  geom_text_repel(aes(label = lakeName), size = 3, max.overlaps=2)+
  geom_abline(intercept = 0, slope = 1)+
  # geom_text(data=model_performance,
  #           aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,
  #               label= paste0("RMSE=", rmse_GPPmean)))+
  theme(legend.position="none")

B<-  fits_optim_long %>%
  left_join(.,GPP_fullset %>%
              select(lakeName, meanGPP, medianGPP, meanzMix, DOC, TP), by="lakeName") %>%
  filter(name=="GPPHat") %>%
  # filter(param_set=="optim") %>%
  ggplot(aes(x=medianGPP, y=value))+
  geom_point(shape=21, size=3, color="grey50", fill="grey50", alpha=0.5)+
  # facet_wrap(~param_set)+
  theme_few()+
  ylab(expression(paste('PRED. GPP (mg C m'^-2,' day'^-1,')')))+
  xlab(expression(paste('OBS. median GPP (mg C m'^-2,' day'^-1,')')))+
  geom_text_repel(aes(label = lakeName), size = 3, max.overlaps=2)+
  geom_abline(intercept = 0, slope = 1)+
  # geom_text(data=model_performance,
  #           aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,
  #               label= paste0("RMSE=", rmse_GPPmedian)))+
  theme(legend.position="none")

C<-  fits_optim_long %>%
  left_join(.,GPP_fullset %>%
              select(lakeName, meanGPP, medianGPP, meanzMix, DOC, TP), by="lakeName") %>%
  filter(name=="CHat") %>%
  # filter(param_set=="optim") %>%
  ggplot(aes(x=DOC, y=value))+
  geom_point(shape=21, size=3, color="grey50", fill="grey50", alpha=0.5)+
  # facet_wrap(~param_set)+
  theme_few()+
  geom_abline(intercept = 0, slope = 1)+
  ylab(expression(paste('PRED. lake DOC mg L'^-1,)))+
  xlab(expression(paste('OBS. lake DOC mg L'^-1,)))+
  geom_text_repel(aes(label = lakeName), size = 3, max.overlaps=2)+
  geom_abline(intercept = 0, slope = 1)+
  # geom_text(data=model_performance,
  #           aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,
  #               label= paste0("RMSE=", rmse_DOC)))+
  theme(legend.position="none")

D<-   fits_optim_long %>%
  left_join(.,GPP_fullset %>%
              select(lakeName, meanGPP, medianGPP, meanzMix, DOC, TP), by="lakeName") %>%
  filter(name=="PHat") %>%
  # filter(param_set=="optim") %>%
  ggplot(aes(x=TP, y=value))+
  geom_point(shape=21, size=3, color="grey50", fill="grey50", alpha=0.5)+
  # facet_wrap(~param_set)+
  theme_few()+
  geom_abline(intercept = 0, slope = 1)+
  ylab(expression(paste('PRED. lake TP ug L'^-1,)))+
  xlab(expression(paste('OBS. lake TP ug L'^-1,)))+
  geom_text_repel(aes(label = lakeName), size = 3, max.overlaps=2)+
  geom_abline(intercept = 0, slope = 1)+
  # geom_text(data=model_performance,
  #           aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,
  #               label= paste0("RMSE=", rmse_TP)))+
  theme(legend.position="none")


E<-   fits_optim_long %>%
  left_join(.,GPP_fullset %>%
              select(lakeName, meanGPP, medianGPP, meanzMix, DOC, TP), by="lakeName") %>%
  filter(name=="zMixHat") %>%
  # filter(param_set=="optim") %>%
  ggplot(aes(x=meanzMix, y=value))+
  geom_point(shape=21, size=3, color="grey50", fill="grey50", alpha=0.5)+
  # facet_wrap(~param_set)+
  theme_few()+
  geom_abline(intercept = 0, slope = 1)+
  ylab("PRED. zMix (m)")+
  xlab("OBS. zMix (m)")+
  geom_text_repel(aes(label = lakeName), size = 3, max.overlaps=2)+
  geom_abline(intercept = 0, slope = 1)+
  # geom_text(data=model_performance,
  #           aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,
  #               label= paste0("RMSE=", rmse_zMix)))+
  theme(legend.position="none")

gg_title = str_wrap("GPP, DOC, TP, zMix predictions using modelled inflow data", width=40)
predictions_kelly_vs_optim_parameters_plot<-A/B/C/D/E+plot_annotation(title = gg_title) 
predictions_kelly_vs_optim_parameters_plot
# ggsave(here("figures/GPP_predictions_using_modeled_inflow_data.png"), dpi=600, width=6,height=13.5, units="in")
