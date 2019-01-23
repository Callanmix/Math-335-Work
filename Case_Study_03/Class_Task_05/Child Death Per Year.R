library(tidyverse)
install.packages("devtools")
devtools::install_github("drsimonj/ourworldindata")
world_data <- ourworldindata::child_mortality%>%
  filter(!is.na(continent))

mort_plot <- group_by(world_data, continent, year) %>% 
  summarise(child_mort = sum(child_mort, na.rm = TRUE))

  ggplot(mort_plot, mapping = aes(x = year, y = child_mort)) +
  geom_area(aes(fill = continent)) +
    scale_fill_brewer(palette = 'Reds') +
    theme_minimal() +
  scale_x_continuous(breaks = c(1800,1825,1850,1875,1900,1925,1950,1975,2000),
                     limits = c(1800,2010)) +
    labs(title = "Child Deaths Per Year", x = 'Year', y = 'Child Mortality Rate', color = 'Continent')
