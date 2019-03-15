library(USAboundaries)
library(sf)
library(tidyverse)
library(ggrepel)
library(maps)

permits <- buildings::permits %>% 
  filter(StateAbbr == "ID", variable == "Single Family")

permits %>% 
  group_by(year) %>% 
  summarise(mean = mean(value))

View(us_counties())
