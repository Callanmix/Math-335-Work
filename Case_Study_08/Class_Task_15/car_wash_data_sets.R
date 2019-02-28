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
  summarise(price = sum(amount)) %>% arrange(ceiling)










temp <- riem_measures(station = "RXE",  date_start  = "2016-05-13",  date_end  =  "2016-07-18")


weather <- temp[!is.na(temp$tmpf), ] %>% 
  mutate(ceiling = ceiling_date(valid, unit = "hour")) %>% 
  select(station:tmpf, sknt, ceiling) %>% 
  group_by(ceiling) %>% 
  summarise(tempture = mean(tmpf), wind_speed = mean(sknt)) 


bound <- merge(cash, weather)




bound %>% 
  ggplot(aes(x = ceiling, y = price)) +
  geom_point(color = "red") +geom_smooth(color = "red") +
  geom_point(aes(y = tempture), color = "blue") +geom_smooth(aes(y = tempture), color = "blue") 

bound %>% 
  ggplot(aes(x = ceiling, y = price)) +
  geom_point(color = "red") +geom_smooth(color = "red") 

bound %>% 
  ggplot(aes(x = ceiling, y = tempture)) +
  geom_point(color = "blue") +geom_smooth(color = "blue")

ggpubr::ggarrange(bound %>% 
                    ggplot(aes(x = ceiling, y = price)) +
                    geom_point(color = "red") +geom_smooth(color = "red"),
                    bound %>% 
                       ggplot(aes(x = ceiling, y = tempture)) +
                       geom_point(color = "blue") +geom_smooth(color = "blue"))

