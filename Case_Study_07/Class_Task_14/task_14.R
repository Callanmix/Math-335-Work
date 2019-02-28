library(tidyverse)
library(stringi)
scriptures <- read_csv(here::here("Data/lds-scripture.csv"))




scriptures %>% 
  group_by(verse_short_title, volume_title) %>% 
  summarise(number = stri_stats_latex(scripture_text)["Words"]) %>% 
  ungroup() %>% 
  group_by(volume_title) %>% 
  summarize(mean = mean(number)) %>%  
  ggplot(aes(y = volume_title, x = mean)) +
  geom_point()







scriptures %>% 
  filter(volume_title %in% c("New Testament", "Book of Mormon")) %>% 
  group_by(volume_title) %>% 
  summarise(text = str_c(scripture_text, collapse = " "),
            count = str_count(text, "Jesus")) %>%
  select(volume_title, count) %>% 
  view()






  
  
flevels <- scriptures %>% filter(volume_title == "Book of Mormon") %>% .$book_title %>% unique()

scriptures %>% 
  filter(volume_title == "Book of Mormon") %>% 
  group_by(book_title, verse_id) %>% 
  mutate(number = stri_stats_latex(scripture_text)["Words"]) %>% 
  ungroup() %>% 
  ggplot(aes(x = factor(book_title, levels = flevels), y = number)) +
  geom_jitter(width = .25, height = 0, alpha = .5, color = "darkgrey") +
  geom_boxplot(outlier.colour = NA, fill = NA) +
  labs(x = "Books", y = "Number of Words per Verse")






