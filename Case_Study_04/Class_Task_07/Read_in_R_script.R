library(tidyverse)
death <- read_csv(here::here("/Data/NCHS_-_Leading_Causes_of_Death__United_States.csv"))
drugs <- read_csv(here::here("/Data/Youth_Tobacco_Survey__YTS__Data.csv"))


death %>% 
  group_by(State, Year) %>% 
  summarise(ave = mean(Deaths)) %>%
  arrange(ave) %>% filter(ave < 15000) %>% top_n(1)
  ggplot(aes(x = Year, y = ave)) +
  geom_point() +
  geom_line(aes(color = State), show.legend = FALSE)

death %>% 
  group_by(`Cause Name`) %>% 
  summarise(ave = mean(Deaths)) %>% 
  arrange(desc(ave))


drugs %>% 
  group_by(LocationAbbr, YEAR, Education) %>%
  summarise(Ave = mean(Data_Value, na.rm = TRUE)) %>% 
  ggplot(aes(x= YEAR, y = Ave)) +
  geom_point(aes(color = LocationAbbr), show.legend = FALSE) +
  geom_smooth()

