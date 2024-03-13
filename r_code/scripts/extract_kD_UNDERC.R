data <- read.csv("data/kD/MO_EL_kD_2013.csv")

#East Long first
EL <- data %>%
  filter(lakeID=="EL")
unique(EL$depthTop)


EL %>%
  ggplot(aes(x=depthTop, y=log(PAR)))+
  geom_point()+
  facet_wrap(~dateSample)
#Smaller depth intervals probably better

EL_calc <- EL %>%
  select(dateSample, depthTop, PAR) %>%
  pivot_wider(names_from = depthTop,
              names_prefix = "depth_",
              values_from = PAR,
              values_fn = mean) %>% #duplicate entries for some depths
  group_by(dateSample) %>%
  summarize(kD_1.5m = (log(depth_0)-log(depth_1.5))/1.5,
         kD_2m = (log(depth_0)-log(depth_2))/2)

summary(EL_calc)

#Morris
MO <- data %>%
  filter(lakeID=="MO")
unique(MO$depthTop)


MO %>%
  ggplot(aes(x=depthTop, y=log(PAR)))+
  geom_point()+
  facet_wrap(~dateSample)
#Smaller depth intervals probably better

MO_calc <- MO %>%
  select(dateSample, depthTop, PAR) %>%
  pivot_wider(names_from = depthTop,
              names_prefix = "depth_",
              values_from = PAR,
              values_fn = mean) %>% #duplicate entries for some depths
  group_by(dateSample) %>%
  summarize(kD_1.5m = (log(depth_0)-log(depth_1.5))/1.5,
            kD_2m = (log(depth_0)-log(depth_2))/2)

summary(MO_calc)


