library(tidyverse)
library(downloader)
library(gridExtra)
library(ggplot2)
download(mode = "wb")
dart_csv <- read_csv("https://github.com/byuistats/data/raw/master/Dart_Expert_Dow_6month_anova/Dart_Expert_Dow_6month_anova.csv")
dart_dta <- read_dta("https://github.com/byuistats/data/raw/master/Dart_Expert_Dow_6month_anova/Dart_Expert_Dow_6month_anova.dta")
dart_sav <- read_sav("https://github.com/byuistats/data/raw/master/Dart_Expert_Dow_6month_anova/Dart_Expert_Dow_6month_anova.sav")
dart_rds <- read_rds(gzcon(url("https://github.com/byuistats/data/raw/master/Dart_Expert_Dow_6month_anova/Dart_Expert_Dow_6month_anova.RDS")))
dart_xlsx <- download(mode = "wb", url = "https://github.com/byuistats/data/raw/master/Dart_Expert_Dow_6month_anova/Dart_Expert_Dow_6month_anova.xlsx")


p1 <- dart_csv %>% 
  ggplot(aes(x = variable, y = value)) +
  geom_jitter(aes(color = variable))

p2 <- dart_csv %>% 
  ggplot(aes(x = variable, y = value)) +
  geom_boxplot() 

p3 <- dart_csv %>%   
  group_by(variable) %>% summarise(ave = mean(value)) %>% 
  ggplot(aes(y = ave, x = variable)) +
  geom_point()

grid.arrange(p1,p2,p3)  

