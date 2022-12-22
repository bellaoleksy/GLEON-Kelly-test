## Created on January 30, 2022 to streamline the previous scripts with this name now that we have optim parameters...


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
GPP_subset$I0 <- 1000

p_kelly <- log(c(kDOC=0.42,kA=0.00022,lA=0.1,pA=1.2,hA=55,mA=2,decay=0.001,cA=0.015,v=0.1,rec=0.95))
p_optim <- read_csv("data/params_20220131.csv") %>%
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

# INTRODUCE NECESSARY FUNCTIONS ---------------------------------------
source('r_code/scripts/paramOptim/huisman.r')
source('r_code/scripts/paramOptim/kellyPredix.r')



# MAKE PREDICTIONS --------------------------------------------------------


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
  pivot_longer(cols=(1:4))


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



### Combine
model_predictions <- bind_rows(fits_kelly_long,fits_optim_long)
model_predictions <- inner_join(model_predictions, GPP_subset %>%
                                   select(lakeName, meanGPP, medianGPP, meanzMix, DOC, TP), by="lakeName")


model_performance <- bind_rows(optim_performance, kelly_performance) %>%
  mutate(xpos=-Inf,
         ypos=Inf,
         hjustvar=-0.1,
         vjustvar=1.5) #For position of labels, anchor at top left corner

# Compare results on the GPP_subset ---------------------------------------


A<-
  model_predictions %>%
  filter(name=="GPPHat") %>%
  ggplot(aes(x=meanGPP, y=value))+
  geom_point(shape=21, size=3)+
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
  ggplot(aes(x=meanGPP, y=value))+
  geom_point(shape=21, size=3)+
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
  ggplot(aes(x=DOC, y=value))+
  geom_point(shape=21, size=3)+
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
  ggplot(aes(x=TP, y=value))+
  geom_point(shape=21, size=3)+
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
  ggplot(aes(x=meanzMix, y=value))+
  geom_point(shape=21, size=3)+
  facet_wrap(~param_set)+
  theme_few()+
  geom_abline(intercept = 0, slope = 1)+
  ylab("PRED. zMix (m)")+
  xlab("OBS. zMix (m)")+
  geom_text_repel(aes(label = lakeName), size = 3, max.overlaps=2)+
  geom_abline(intercept = 0, slope = 1)+
  geom_text(data=model_performance,
            aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,
                label= paste0("RMSE=", rmse_zMix)))+
  theme(legend.position="none")


A/B/C/D/E



# LOAD ESTIMATES FOR LAKES WITH KNOWN LOADS ----------------------------------------------------------

GPP_subset %>%
  inner_join(GPP_subset_load_estimates) %>%
  ggplot(aes(x=DOCin,y=DOCin_est))+
  geom_point(shape=21,size=3)+
  geom_abline(intercept = 0, slope = 1)

GPP_subset %>%
  inner_join(GPP_subset_load_estimates) %>%
  ggplot(aes(x=Pin,y=TPin_est))+
  geom_point(shape=21,size=3)+
  geom_abline(intercept = 0, slope = 1)+
  geom_text_repel(aes(label = lakeName), size = 3, max.overlaps=2)

GPP_subset %>%
  inner_join(GPP_subset_load_estimates) %>%
  ggplot(aes(x=DOCin/Pin,y=DOCin_est/TPin_est))+
  geom_point(shape=21,size=3)+
  geom_abline(intercept = 0, slope = 1)+
  geom_text_repel(aes(label = lakeName), size = 3, max.overlaps=2)


GPP_subset %>%
  inner_join(GPP_subset_load_estimates) %>%
  ggplot(aes(x=DOCin*Qin*(1/1000),y=DOCin_est*Qin*(1/1000)))+
  geom_point(shape=21,size=3)+
  geom_abline(intercept = 0, slope = 1)+
  geom_text_repel(aes(label = lakeName), size = 3, max.overlaps=10)+
  scale_x_log10(expression(DOC~load~(kg)), labels = scales::comma)+
  scale_y_log10(expression(DOC~load~(kg)), labels = scales::comma)

GPP_subset %>%
  inner_join(GPP_subset_load_estimates) %>%
  ggplot(aes(x=Pin*Qin*(1/1000000),y=TPin_est*Qin*(1/1000000)))+
  geom_point(shape=21,size=3)+
  geom_abline(intercept = 0, slope = 1)+
  geom_text_repel(aes(label = lakeName), size = 3, max.overlaps=10)+
  labs(y="Estimated P loads (kg)",
       x="Measured P loads (kg)")+
  scale_x_log10(expression(TP~load~(kg)), labels = scales::comma)+
  scale_y_log10(expression(TP~load~(kg)), labels = scales::comma)
