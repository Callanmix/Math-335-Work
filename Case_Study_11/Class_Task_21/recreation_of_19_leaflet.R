library(leaflet)
library(USAboundaries)
library(sf)
library(tidyverse)
library(ggrepel)
library(maps)
library(rgdal)

###################################

cities <- us_cities() %>% 
  group_by(state_name) %>% 
  mutate(ranks = order(order(population, decreasing=TRUE))) %>% 
  select(city:state_abbr, name_2010, population:geometry) %>% 
  filter(ranks == 1 | ranks == 2 | ranks == 3) %>% 
  filter(state_name != "Hawaii", state_name != "Alaska") %>% 
  mutate(pop = population/1000)

states <- us_states() %>% 
  filter(statefp != "15", statefp != "02", statefp != "72")

ID <- us_counties() %>% 
  filter(state_abbr == "ID") 

###################################

leaflet() %>%
  addPolygons(data = states, color = "grey",
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>% 
  addPolygons(data = ID, fillColor = topo.colors(10, alpha = NULL), stroke = FALSE) %>% 
  addTiles() %>%
  addMarkers(data = cities, label = ~as.character(city), popup = ~as.character(population))





pchIcons <- function(pch = 0:14, width = 30, height = 30, ...) {
  n <- length(pch)
  files <- character(n)
  # create a sequence of png images
  for (i in seq_len(n)) {
    f <- tempfile(fileext = ".png")
    png(f, width = width, height = height, bg = "transparent")
    par(mar = c(0, 0, 0, 0))
    plot.new()
    points(.5, .5, pch = pch[i], cex = min(width, height) / 8, ...)
    dev.off()
    files[i] <- f
  }
  files
}
