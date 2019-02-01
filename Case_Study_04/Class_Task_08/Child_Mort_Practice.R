library(tidyverse)
library(devtools)
health <- ourworldindata::financing_healthcare  

health %>% 
  group_by(country, child_mort, continent, year) %>% filter(year >= 1800) %>% 
  ggplot(aes(x = year, y = child_mort)) +
  geom_smooth() +
  labs(title = "Child Mortality per 1000 People", x = "Year", y = "Child Mortality") +
  theme_grey()
