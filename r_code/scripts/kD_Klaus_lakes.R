data <- read.table("data/kD/kd_values_2011-2015_Klaus.txt") %>%
  filter(Lake_kd %in% c("1","4","5","7","8")) %>%
  mutate(lakeName = case_when(Lake_kd=="1" ~ "Övre Björntjärn",
                               Lake_kd=="4" ~ "Mangstrettjärnen",
                               Lake_kd=="5" ~ "Nästjärnen",
                               Lake_kd=="7" ~ "Lillsjölidtjärnen",
                               Lake_kd=="8" ~ "Struptjärn"))

View(data)


data_summary <- data %>%
  mutate(date=ymd(Date_kd),
         year=year(date)) %>%
  group_by(lakeName,year) %>%
  summarize(mean_kD=mean(kd_m, na.rm=TRUE))
