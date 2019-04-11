library(tidyverse)
library(gapminder)
library(datasets)
library(maps)
library(USAboundaries)
library(lubridate)
library(riem)
library(sf)
library(leaflet)
library(downloader)
library(gganimate)
library(cartogram)
library(plotly)
library(fs)
################# Data Sets ############################
## Movies
movies <- read_csv("C:/Users/calla/Downloads/suicide-rates-overview-1985-to-2016/tmdb_5000_movies.csv")  
movies <- movies %>% 
  mutate(year = year(ceiling_date(movies$release_date, unit = "year"))) %>% 
  select(budget, genres, original_language, production_companies:runtime, title:year)

## Suicide 1985 - 2016
dat <- read_csv("C:/Users/calla/Downloads/suicide-rates-overview-1985-to-2016/master.csv")

## Gapminde
gap <- gapminder

## World Map Data
map <- "http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip"
df <- tempfile(); uf <- tempfile()
download(map, df, mode = "wb")
unzip(df, exdir = uf)
worldmap <- read_sf(uf) 
file_delete(df); dir_delete(uf) 
worldmap <- worldmap %>% 
  select(NAME, geometry)
names(worldmap)[1] <- "country"

## Death Data World
death_data <- read_csv("C:/Users/calla/Downloads/e15cf5d1-5063-4e19-a676-323f23cde9c6")
death_data$Sums <- rowSums(death_data[4:36], na.rm = TRUE)
names(death_data)[1] <- "country"
names(death_data)[3] <- "year"
death_data <- death_data %>% 
  mutate(suicide_percent = (Suicide/Sums)*100)

options(scipen = 999)


## Lit Rates 
lit_rates <- read_csv("C:/Users/calla/Downloads/cross-country-literacy-rates.csv") %>% 
  filter(Year >= 1980) 
names(lit_rates)[4] <- "literacy_rates"
names(lit_rates)[1] <- "country"
names(lit_rates)[3] <- "year"

## Suicide summary
suicide_info <- dat %>% 
  group_by(year, country) %>% 
  summarise(sum_suicide = sum(suicides_no),
            ave_pop = sum(population),
            suicide_per_pop = ave_pop/sum_suicide,
            percent_suicides = sum_suicide/ave_pop,
            suicide_per_100k = sum(`suicides/100k pop`),
            ave_gdp = mean(`gdp_for_year ($)`),
            gdp_per_cap = ave_gdp/ave_pop,
            gdp_per_suicide = ave_gdp/sum_suicide)

################### Merging ##############################

suicide <- merge(gap, dat, by = c("year","country"))

whole_data <- merge(movies, suicide, by = "year")

final <- merge(suicide, worldmap, by = "country")

suicide_info_world <- merge(suicide_info, worldmap, by = "country")

lit_death <- merge(death_data, lit_rates, by = c("country","year"))
############# Visual ######################
## Movies
movies %>% 
  group_by(year) %>% 
  summarise(mean = mean(vote_average)) %>% 
  ggplot(aes(x = year, y = mean)) +
  geom_point() +
  geom_smooth()

## Lit Rates
 
p <- lit_rates %>%
  merge(worldmap, by = "country") 
p %>% 
  ggplot() +
  geom_sf(data = worldmap) +
  geom_sf(data = p$geometry, aes(fill = "lit_rate"))




## Death Data
death_data %>% 
  group_by(Year) %>% 
  summarise(suicides = sum(Suicide, na.rm = TRUE),
            death = sum(Sums),
            percent = suicides/death) %>% 
  ggplot(aes(x= Year, y = percent)) +
  geom_line() +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Percent of Suicides Deaths by Year",
       subtitle = "Out of Total World Deaths", y = "Percentage", x = "Year") +
  theme_minimal()

death_data %>% 
  group_by(Year) %>% 
  summarise(suicides = sum(Suicide, na.rm = TRUE),
            death = sum(Sums),
            percent = suicides/death) %>% 
  ggplot(aes(x= Year, y = suicides)) +
  geom_line() +
  labs(title = "Number of Total Suicides",
       y = "Number of Suicides", x = "Year") +
  theme_minimal()


## Suicide Info
suicide_info %>% 
  ggplot(aes(sum_suicide, ave_pop)) +
  geom_hex() +
  scale_fill_gradient(name = "count", trans = "log") + 
  facet_wrap(facets = "year") +
  scale_y_continuous(breaks=c(25000000,50000000,75000000,100000000,200000000),
                     labels = c("25 Million","50 Million","75 Million","100 Million","200 Million"),
                     limits = c(0,100000000))
  

################ ggplot ###############################
t1990 <- suicide_info_world %>% 
  filter(year == 1990)
ggplot() +
  geom_sf(data = worldmap, color = "red") +
  geom_sf(data = t1990, aes(fill = sum_suicide), inherit.aes = FALSE) +
  scale_fill_gradient(name = "Sum of Suicide", trans = "log")

t1995 <- suicide_info_world %>% 
  filter(year == 1995)
ggplot() +
  geom_sf(data = worldmap, color = "red") +
  geom_sf(data = t1995, aes(fill = sum_suicide), inherit.aes = FALSE) +
  scale_fill_gradient(name = "Sum of Suicide", trans = "log")

t2000 <- suicide_info_world %>% 
  filter(year == 2000)
ggplot() +
  geom_sf(data = worldmap, color = "red") +
  geom_sf(data = t2000, aes(fill = sum_suicide), inherit.aes = FALSE) +
  scale_fill_gradient(name = "Sum of Suicide", trans = "log")

t2005 <- suicide_info_world %>% 
  filter(year == 2005)
ggplot() +
  geom_sf(data = worldmap, color = "red") +
  geom_sf(data = t2005, aes(fill = sum_suicide), inherit.aes = FALSE) +
  scale_fill_gradient(name = "Sum of Suicide", trans = "log")

t2010 <- suicide_info_world %>% 
  filter(year == 2010)
ggplot() +
  geom_sf(data = worldmap, color = "red") +
  geom_sf(data = t2010, aes(fill = sum_suicide), inherit.aes = FALSE) +
  scale_fill_gradient(name = "Sum of Suicide", trans = "log")

t2015 <- suicide_info_world %>% 
  filter(year == 2015)
ggplot() +
  geom_sf(data = worldmap, color = "red") +
  geom_sf(data = t2015, aes(fill = sum_suicide), inherit.aes = FALSE) +
  scale_fill_gradient(name = "Sum of Suicide", trans = "log")


lit_death$suicide_percent[is.infinite(lit_death$suicide_percent) | is.nan(lit_death$suicide_percent) ] <- NA 
lit_death$literacy_rates[is.infinite(lit_death$literacy_rates) | is.nan(lit_death$literacy_rates) ] <- NA 

a <- lit_death %>% 
  ggplot(aes(literacy_rates, suicide_percent)) +
  geom_point(alpha = .65, show.legend = FALSE, na.rm = TRUE) +
  labs(title = 'Year: {frame_time}', x = 'Suicides as % of Total Deaths', y = 'Literacy Rates %') +
  transition_time(year) +
  ease_aes('linear')

animate(a, nframes = 600, duration = 20, fps = 30, end_pause = 5)




ggplot(suicide_info_world, aes(suicide_per_100k, gdp_per_cap, size = sum_suicide, color = country)) + 
  geom_point(alpha = .65, show.legend = FALSE) + 
  # Here comes the gganimate code
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'Suicide Per 100k') +
  transition_time(year) +
  ease_aes('linear')

b <- ggplot(suicide_info_world, aes(suicide_per_100k, gdp_per_cap, size = sum_suicide,color = country))+
  geom_point(alpha = .65, show.legend = FALSE) + 
  labs(x = "GDP Per Capita",y = "Suicide Per 100,000 People" ) +
# Here comes the gganimate code
labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'Suicide Per 100k') +
  transition_time(year) +
  ease_aes('linear') 

animate(b, nframes = 600, duration = 20, fps = 30, end_pause = 5)

################ Leaflet ###########################
for_leaflet <- suicide_info_world %>% 
  filter(year == 2015)

pal <- colorNumeric(palette = "Greens",
                      domain = for_leaflet$suicide_per_100k)

leaflet() %>% 
  addPolygons(data = worldmap) %>% 
  addProviderTiles(provider = providers$CartoDB.DarkMatter) %>% 
  addPolygons(data = for_leaflet$geometry, stroke = FALSE, 
              fillColor = pal(for_leaflet$suicide_per_100k), fillOpacity = .85) %>% 
  addLegend("bottomright", pal = pal, values = for_leaflet$suicide_per_100k,
            title = "Suicide Per 100K 2015", opacity = .5) %>% 
  addLayersControl(baseGroups = names(for_leaflet$year),
                   options = layersControlOptions(collapsed = FALSE)) 
  