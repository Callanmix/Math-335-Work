library(USAboundaries)
library(sf)
library(tidyverse)
library(ggrepel)
library(maps)
library(downloader)
library(fs)

file_path <- "https://byuistats.github.io/M335/data/shp.zip"
#################
dams_path <- "https://opendata.arcgis.com/datasets/e163d7da3b84410ab94700a92b7735ce_0.zip?outSR=%7B%22latestWkid%22%3A102605%2C%22wkid%22%3A102605%7D"
df <- tempfile(); uf <- tempfile()
download(dams_path, df, mode = "wb")
unzip(df, exdir = uf)
dams <- read_sf(uf)
file_delete(df); dir_delete(uf) 
##############
water_path <- "https://research.idwr.idaho.gov/gis/Spatial/Hydrography/streams_lakes/c_250k/hyd250.zip"
df <- tempfile(); uf <- tempfile()
download(water_path, df, mode = "wb")
unzip(df, exdir = uf)
water <- read_sf(uf)
file_delete(df); dir_delete(uf) 
#############
well_path <- "https://opendata.arcgis.com/datasets/1abb666937894ae4a87afe9655d2ff83_1.zip?outSR=%7B%22latestWkid%22%3A102605%2C%22wkid%22%3A102605%7D"
df <- tempfile(); uf <- tempfile()
download(well_path, df, mode = "wb")
unzip(df, exdir = uf)
wells <- read_sf(uf)
file_delete(df); dir_delete(uf) 

well_5k <- wells %>% 
  filter(Production >= 5000)

dams_50a <- dams %>% 
  filter(SurfaceAre >= 50)

rivers <- water %>% 
  filter(FEAT_NAME %in% c("Snake River","Henrys Fork"))

id <- us_counties() %>% 
  filter(state_abbr == "ID")

png(here::here("Case_Study_10/Class_Task_20/id_wells.png"))
id_wells <- ggplot() +
  geom_sf(data = id, fill = "lightblue", alpha = .5) +
  geom_sf(data = well_5k, color = "red", alpha = .5, size = 1, shape = 10) +
  geom_sf(data = dams_50a, alpha = .5, color = "yellow", size = .5) +
  geom_sf(data = rivers, size = 1, color = "blue") +
  coord_sf(datum = sf::st_crs(2043))
dev.off()
