library(tidyverse)
death <- read_csv(here::here("/Data/NCHS_-_Leading_Causes_of_Death__United_States.csv"))

death %>% 
  group_by(State) %>% 
  summarise(ave = mean(Deaths))

death %>% 
  group_by(`Cause Name`) %>% 
  summarise(ave = mean(Deaths)) %>% 
  arrange(desc(ave))

