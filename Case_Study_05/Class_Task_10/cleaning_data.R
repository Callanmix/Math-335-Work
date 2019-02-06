library(tidyverse)
dart_csv <- read_csv("https://github.com/byuistats/data/raw/master/Dart_Expert_Dow_6month_anova/Dart_Expert_Dow_6month_anova.csv")

clean_stocks <- dart_csv %>% 
  separate(contest_period, c("month_beg", "year")) %>% 
  separate(year, c("month_end", "year_end"), sep = -4) %>% 
  mutate(month_beg = str_extract(month_beg,".*[:alpha:]")) %>% 
  filter(year_end <= "1998")
  write_rds(here::here("Case_Study_05/Class_Task_10/Tidy_stock.rds"))


clean_stocks %>% 
  group_by(year_end, variable) %>% 
  summarise(mean = mean(value)) %>% 
  ggplot(aes(x = year_end, y = mean)) +
  geom_point(size = 2) +
  geom_line(aes(color = variable, group = variable), size = 2)


clean_stocks %>% 
  filter(variable == "DJIA") %>% 
  group_by(month_end) %>% 
  spread(year_end,value) %>% 
  select(-1,-3) %>% 
  summarise_all(sum, na.rm = TRUE)
