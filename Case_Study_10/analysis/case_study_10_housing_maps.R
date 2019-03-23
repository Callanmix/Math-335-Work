library(USAboundaries)
library(sf)
library(tidyverse)
library(ggrepel)
library(maps)

permits <- buildings::permits %>% 
  filter(StateAbbr == "ID", variable == "Single Family") 

names(permits)[names(permits) == "state"] <- "statefp"
names(permits)[names(permits) == "county"] <- "countyfp"


idaho_counties <- us_counties(resolution = "low") %>% 
  filter(state_abbr == "ID") %>% 
  transform("countyfp" = as.numeric(countyfp),
            "statefp" = as.numeric(statefp))





merged <- left_join(permits,idaho_counties, by = c("countyfp","statefp")) %>% 
  filter(year %in% c(2000:2015)) %>% 
  group_by(countyfp, year) %>% 
  mutate(median = median(value))




ggplot() +
  geom_sf(data = merged,aes(fill = median), color = "red") +
  coord_sf(crs = "+proj=lcc +lat_1=-44 +lat_2=77 +lat_0=63.39 +lon_0=-114 +x_0=6200000 +y_0=3000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") +
  facet_wrap(facets = "year",nrow = 3) +
  scale_fill_gradient(name = "median", trans = "log10") +
  theme(axis.title.x = element_blank(),axis.text.x = element_blank(),
        axis.title.y = element_blank(),axis.text.y = element_blank())
