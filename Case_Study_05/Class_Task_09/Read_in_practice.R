library(tidyverse)
library(downloader)
library(gridExtra)
bob <- tempfile()
dart_csv <- read_csv("https://github.com/byuistats/data/raw/master/Dart_Expert_Dow_6month_anova/Dart_Expert_Dow_6month_anova.csv")
dart_dta <- read_dta("https://github.com/byuistats/data/raw/master/Dart_Expert_Dow_6month_anova/Dart_Expert_Dow_6month_anova.dta")
dart_sav <- read_sav("https://github.com/byuistats/data/raw/master/Dart_Expert_Dow_6month_anova/Dart_Expert_Dow_6month_anova.sav")
dart_rds <- read_rds(gzcon(url("https://github.com/byuistats/data/raw/master/Dart_Expert_Dow_6month_anova/Dart_Expert_Dow_6month_anova.RDS")))
download("https://github.com/byuistats/data/raw/master/Dart_Expert_Dow_6month_anova/Dart_Expert_Dow_6month_anova.xlsx", bob, mode = "wb")
dart_xlsx <- readxl::read_xlsx(bob)

all.equal(dart_csv,dart_dta)
all.equal(dart_dta,dart_rds)
all.equal(dart_rds,dart_sav)
all.equal(dart_sav,dart_xlsx)

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

png(filename="3_graph_stock.png")
grid.arrange(p1,p2,p3)  
dev.off()
