library(tidyverse)
library(ggplot2)
cropland <- read_csv(choose.files())

group_by(cropland, Code) %>% 
ggplot(cropland, mapping = aes(x = Year, y = `(km2)`)) +
  geom_area(aes(fill = Code)) +
  scale_x_continuous(labels = c('10000 BCE','8000 BCE','6000 BCE','4000 BCE','2000 BCE','0','2016'),
                     breaks = c(-10000,-8000,-6000,-4000,-2000,0,2016)) +
  scale_y_continuous(labels = c('1 Billion','2 Billion','3 Billion','4 Billion'),
                     breaks = c(1000000000,2000000000,3000000000,4000000000)) +
  labs(title = "Cropland Use Over Long Term", subtitle = "Total cropland area, measured in hectares",
       x = "Year", y = "Hecta Acres", color = "Region")


