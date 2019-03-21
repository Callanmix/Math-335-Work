library(USAboundaries)
library(sf)
library(tidyverse)
library(ggrepel)
library(maps)
library(ggsfabel)

cities <- us_cities() %>% 
  group_by(state_name) %>% 
  mutate(ranks = order(order(population, decreasing=TRUE))) %>% 
  select(city:state_abbr, name_2010, population:geometry) %>% 
  filter(ranks == 1 | ranks == 2 | ranks == 3) %>% 
  filter(state_name != "Hawaii", state_name != "Alaska") %>% 
  mutate(pop = population/1000)

ID <- us_counties() %>% 
  filter(state_abbr == "ID") 

states <- sf::st_as_sf(map("state", plot = FALSE, fill = TRUE))

ggplot() +
  geom_sf(data = states) +
  geom_sf(data = ID) +
  geom_sf(data = cities, aes(color = pop, labels = city)) +
  geom_sf(data = cities, alpha = .5, aes(size = pop),  show.legend = F) 
  

