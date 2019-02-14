library(tidyverse)
library(readr)
library(stringr)
text <- read_lines("https://byuistats.github.io/M335/data/randomletters.txt")
text2 <- read_lines("https://byuistats.github.io/M335/data/randomletters_wnumbers.txt")

text %>% str_split("") %>% unlist() %>% 
  .[c(1, seq(0,str_length(text),1700))] %>% str_flatten() %>%  
  str_extract(., pattern = ".*\\.")

text2 %>% str_extract_all("[:digit:]{1,2}") %>% map(as.numeric) %>% unlist() %>%
  {LETTERS[.]} %>% str_flatten()

text %>% str_remove_all("[:blank:]|[\\.]") %>% 
  str_extract_all(pattern = "[aeiou]{7}")


LETTERS[2]
# remove all "a"and "e" and then count
#str_remove_all
text %>% str_remove_all("a|e") %>% str_count()
text %>% str_count(pattern = "jim")
