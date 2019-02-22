library(tidyverse)
library(stringi)
script <- read_csv(here::here("Data/lds-scripture.csv"))


script %>% 
  filter($volume_lds_url == "ot") %>% 
  stri_stats_latex(scripture_text)


stri_stats_latex(filter(script$volume_lds_url == "ot"))

str_locate_all(script$scripture_text, pattern = "\\w")

script %>% 
  filter(volume_lds_url == "ot") %>% 
  group_by(scripture_text) %>% 
  str_count(pattern = "\\b")









str_count("cody and elena", "\\w")

stringi::stri_stats_latex("cody and elena")[4]


