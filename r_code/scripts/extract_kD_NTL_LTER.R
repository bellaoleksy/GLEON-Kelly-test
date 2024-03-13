# Package ID: knb-lter-ntl.259.18 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER: Light Extinction 1981 - current.
# Data set creator:  John Magnuson - University of Wisconsin-Madison 
# Data set creator:  Stephen Carpenter - University of Wisconsin-Madison 
# Data set creator:  Emily Stanley - University of Wisconsin-Madison 
# Metadata Provider:  NTL Information Manager - University of Wisconsin-Madison 
# Contact:    -  NTL LTER  - ntl.infomgr@gmail.com
# Stylesheet for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@Virginia.edu 
#
#install package tidyverse if not already installed
if(!require(tidyverse)){ install.packages("tidyverse") }  
library("tidyverse") 
infile1 <- trimws("https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/259/18/6e9645b25882672e7316357890553e19") 
infile1 <-sub("^https","http",infile1)
# This creates a tibble named: dt1 
dt1 <-read_delim(infile1  
                 ,delim=","   
                 ,skip=1 
                 ,quote='"'  
                 , col_names=c( 
                   "lakeid",   
                   "sampledate",   
                   "extcoef",   
                   "lightext_flag",   
                   "comments"   ), 
                 col_types=list( 
                   col_character(),  
                   col_date("%Y-%m-%d"),  
                   col_number() ,  
                   col_character(),  
                   col_character()), 
                 na=c(" ",".","NA","")  )


# Observed issues when reading the data. An empty list is good!
problems(dt1) 
# Here is the structure of the input data tibble: 
glimpse(dt1) 
# And some statistical summaries of the data 
summary(dt1) 
# Get more details on character variables

summary(as.factor(dt1$lakeid)) 
summary(as.factor(dt1$lightext_flag)) 
summary(as.factor(dt1$comments))

names(dt1)

data1 <- dt1 %>%
  filter(lakeid=="TR") %>%
  mutate(year=year(sampledate)) %>%
  filter(year=="2013") %>%
  drop_na(extcoef)

data2 <- dt1 %>%
  filter(lakeid=="ME") %>%
  mutate(year=year(sampledate),
         month=month(sampledate)) %>%
  filter(month %in% c("5","6","7","8","9")) %>%
  drop_na(extcoef)

export <- bind_rows(data1, data2) %>% select(-month)
write.csv(export, "data/kD/mendota_trout_kD.csv", row.names=FALSE)

data1 %>%
  group_by(lakeid) %>%
  summary()

data2 %>%
  group_by(lakeid) %>%
  summary()
