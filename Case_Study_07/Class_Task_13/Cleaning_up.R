library(tidyverse)

dart_csv <- read_csv("https://github.com/byuistats/data/raw/master/Dart_Expert_Dow_6month_anova/Dart_Expert_Dow_6month_anova.csv")

clean_stocks <- dart_csv %>% 
  separate(contest_period, c("month_beg", "year"), sep = "-") %>%  
  separate(year, c("month_end", "year_end"), sep = -4) %>% 
  mutate(month_beg = str_extract(month_beg,".*[:alpha:]"), 
         month_end = case_when(month_end == "Dec." ~ "December",
                              month_end == "Febuary" ~ "February",
                              TRUE ~ month_end)) 
  
 





clean_stocks %>% 
  group_by(month_end) %>% 
  summarise(ave = mean(value)) 


clean_stocks %>% 
  ggplot(aes(x = month_end, y = value)) +
  geom_violin(aes(group = month_end, fill = month_end), show.legend = FALSE)



