library(leaflet)
library(USAboundaries)
library(sf)
library(tidyverse)
library(ggrepel)
library(maps)
library(rgdal)

permits <- buildings::permits %>% 
  filter(StateAbbr == "ID", variable == "Single Family") 

names(permits)[names(permits) == "state"] <- "statefp"
names(permits)[names(permits) == "county"] <- "countyfp"


idaho_counties <- us_counties(resolution = "low") %>% 
  filter(state_abbr == "ID") %>% 
  transform("countyfp" = as.numeric(countyfp),
            "statefp" = as.numeric(statefp))


merged <- left_join(permits,idaho_counties, by = c("countyfp","statefp")) %>% 
  group_by(countyfp
           


pal <- colorQuantile(
  palette = "Greens",
  domain = merged$value)


leaflet() %>% 
  addPolygons(data = merged$geometry, stroke = FALSE, fillColor = pal(merged$value),
              highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
              fillOpacity = 1, smoothFactor = 1) %>% 
  addLayersControl(baseGroups = c(2000,2005,2010),
                   options = layersControlOptions(collapsed = FALSE))
  # Base groups
  addTiles(group = "OSM (default)") %>%
  addProviderTiles(providers$OpenStreetMap.BlackAndWhite, group = "Toner") %>%
  addProviderTiles(providers$CartoDB.DarkMatter, group = "Toner Lite") %>%
  addLayersControl(
    baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
    options = layersControlOptions(collapsed = FALSE)
  )





