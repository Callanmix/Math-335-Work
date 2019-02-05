library(tidyverse)
gun_data <- read_csv(here::here("/Data/full_data_guns.csv"))
library(dplyr)

new_data_gun %>%
  count(intent, race) %>% 
  ggplot(aes(x = race, y = n), na.rm = TRUE) +
  geom_point(aes(color = n), show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  facet_wrap(facets = vars(intent), nrow = 1) +
  labs(title = 'Deaths per Intent per Race', y = 'Number of Deaths', x = 'Race')



  
new_data_gun <- gun_data %>% 
  select(year:intent, sex:race, place, education) %>% 
  mutate(Season = case_when(month %in% c('01','02','12')~'Winter',
                            month %in% c('03','04','05')~'Spring',
                            month %in% c('06','07','08')~'Summer',
                            TRUE ~ 'Fall'))
new_data_gun %>% 
  count(Season, sex, race) %>% 
  ggplot(aes(x = race, y = n)) +
  geom_point(aes(color = sex)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  facet_wrap(facets = vars(Season), nrow = 1) +
  labs(title = "Deaths per Race Every Season", y = "Nubmer of Deaths", color = 'Gender',
       x = 'Race')

new_data_gun %>% 
  count(education, Season) %>% 
  ggplot(aes(x = education, y = n)) +
  geom_point(aes(color = Season)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  facet_wrap(facets = vars(Season), nrow = 1) +
  labs(title = "Deaths by Education Every Season", y = "Nubmer of Deaths", color = 'Season',
       x = 'Race')
