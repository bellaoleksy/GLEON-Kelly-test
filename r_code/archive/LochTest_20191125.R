library(tidyverse)
#Load Loch test metabolism output ----------------------------------------------------------------


loch <- read.csv('/Users/solomonlab/Google Drive/Research (common)/Research/Data/R/MFE/GLEON/results/Loch 20191125/LOCH_2016 GPPFitOut.txt', sep=' ')
loch$lakeID <- 'LOCH'
####DATE CONVERSION###
library(lubridate) #To tell R that my dates are dates
loch$solarDay <- ymd(as.character(loch$solarDay))
str(loch$solarDay)

loch <- loch %>%
  mutate(solarDay = ymd(solarDay))

loch_optimout <- read.csv('/Users/solomonlab/Google Drive/Research (common)/Research/Data/R/MFE/GLEON/results/Loch 20191125/LOCH_2016 optimOut.txt', sep=' ')
loch_optimout$lakeID <- 'LOCH'
loch_optimout$solarDay <- ymd(as.character(loch_optimout$solarDay))
loch_optimout <- loch_optimout %>%
  mutate(solarDay = ymd(solarDay))

loch_full <- left_join(loch, loch_optimout, by = c("solarDay","lakeID"))


ggplot(loch_full, aes(x=solarDay, y=GPP)) + 
  geom_point()+
  geom_errorbar(aes(ymin=GPP-GPPSd, ymax=GPP+GPPSd), width=.2,
                position=position_dodge(0.05)) +
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
  ylab("GPP (mg L-1 d-1)")+
  ggtitle("The Loch 2016 metabolism")

loch_full %>%
  mutate(rhoEst = rhoEst * -1) %>%
  filter(rhoEst < 0)%>%
ggplot(aes(x=solarDay, y=rhoEst)) + 
  geom_point()+
  geom_errorbar(aes(ymin=rhoEst-rhoSd, ymax=rhoEst+rhoSd), width=.2,
                position=position_dodge(0.05)) +
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
  ylab("ER (mg L-1 d-1)")+
  ggtitle("The Loch 2016 metabolism")

loch_full %>%
  mutate(NEP = GPP - rhoEst) %>%
  ggplot(aes(x=solarDay, y=NEP)) + 
  geom_point()+
  # geom_errorbar(aes(ymin=rhoEst-rhoSd, ymax=rhoEst+rhoSd), width=.2,
                # position=position_dodge(0.05)) +
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
  ylab("NEP (mg L-1 d-1)")+
  ggtitle("The Loch 2016 metabolism")


loch_full %>%
  mutate(NEP = GPP - rhoEst) %>%
  summarize(meanNEP = mean(NEP))
