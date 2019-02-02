library(tidyverse)
gun_data <- read_csv(here::here("/Data/full_data_guns.csv"))
library(dplyr)

new_data_gun %>%
  count(intent) %>%  count(education) %>%  count(race)
  



  
new_data_gun <- gun_data %>% 
  select(year:intent, sex:race, place, education) %>% 
  mutate(Season = case_when(month %in% c('01','02','12')~'winter',
                            month %in% c('03','04','05')~'spring',
                            month %in% c('06','07','08')~'summer',
                            TRUE ~ 'fall'))


new_data_gun %>% 
  select(year, count(intent, na.rm = TRUE)) %>% 
  ggplot(aes(x = intent, y = n)) +
  geom_histogram()
  facet_wrap(facets = vars(education), nrow = 1)


