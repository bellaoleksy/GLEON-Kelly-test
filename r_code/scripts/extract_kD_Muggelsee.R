data <- read.csv("data/kD/IGB_Muggelsee_PAR_2015.csv", skip=9) %>%
  mutate(datetime=ymd_hms(end.of.interval),
         month=month(datetime),
         hour=hour(datetime),
         date=date(datetime))

data_trim <- data %>%
  filter(month %in% c("5","6","7","8","9","10")) %>%
  filter(hour > 11 & hour < 15) %>%
  group_by(date) %>%
  summarize(mean_daily_kD = mean(X, na.rm=TRUE))

summary(data_trim)
#Put the mean in the kD_compiled spreadsheet