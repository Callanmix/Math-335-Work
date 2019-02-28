library(tidyverse)
library(lubridate)
library(riem)

car_wash <- read_csv("https://byuistats.github.io/M335/data/carwash.csv")

ymd_hms(car_wash$time, tz = "GMT")


cash <- car_wash %>% 
  mutate(change = force_tz(car_wash$time, tzone = "UTC") %>% 
           with_tz("America/Denver" )) %>% 
  mutate(ceiling = ceiling_date(change, unit = "hour")) %>%
  group_by(ceiling) %>% 
  summarise(price = sum(amount)) 





temp <- riem_measures(station = "RXE",  date_start  = "2016-05-13",  date_end  =  "2016-07-18")

weather <- temp %>% 
  mutate(ceiling = ceiling_date(valid, unit = "hour")) %>% 
  select(station:tmpf, sknt, ceiling) %>% 
  group_by(ceiling) %>% 
  summarise(tempture = mean(tmpf), wind_speed = mean(sknt)) 


bound <- bind_rows(cash, weather)







yday(mdy("March 23, 2016"))

jan31 <- ymd("2013-01-31")
jan31 %>% duration(units = "month") 
