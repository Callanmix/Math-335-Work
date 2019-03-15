library(USAboundaries)
library(sf)
library(tidyverse)
library(ggrepel)
library(maps)
library(gghighlight)

cities <- us_cities() %>% 
  group_by(state_name) %>% 
  mutate(ranks = order(order(population, decreasing=TRUE))) %>% 
  select(city:state_abbr, name_2010, population:geometry) %>% 
  filter(ranks == 1 | ranks == 2 | ranks == 3) %>% 
  filter(state_name != "Hawaii", state_name != "Alaska") 

ID <- us_counties() %>% 
  filter(state_abbr == "ID") 

nc <- sf::st_as_sf(map("idaho", plot = FALSE, fill = TRUE))

states <- sf::st_as_sf(map("state", plot = FALSE, fill = TRUE))

ggplot() +
  geom_sf(data = states) +
  geom_sf(data = ID) +
  geom_sf(data = cities, alpha = .63, aes(color = population)) +
  geom_sf(data = cities, alpha = .5, aes(size = population),  show.legend = F) +
  geom_sf_text_repel(data = cities, aes(label = city)) 
  

