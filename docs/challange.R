pacman::p_load(tidyverse, ggthemes)
dat <- read_csv("https://www.dropbox.com/s/wq9e3wqzow500uo/master_marathon.csv?dl=1")
# If you can't handle the full data set then use the following.
dat <- read_csv("https://www.dropbox.com/s/dp80lkd1n89gzma/master_marathon_1m.csv?dl=1")
#or
dat <- read_csv("https://byuistats.github.io/M335/data/runners_100k.csv")

######################Challange 1###################

mutate <- dat %>% 
  mutate(hours = chiptime/60)

con <- count(mutate, vars = hours)


mutate %>% 
  ggplot(aes(x = hours)) +
  geom_histogram(na.rm = TRUE, binwidth = .001)



  

##########################Challange 2################################

mutate %>% 
  ggplot(aes(x = hours, y = age)) +
  geom_bin2d(na.rm = TRUE, bins = 50)

mutate %>%
  mutate(group = case_when(age<=20 ~ "Under 20",
                           age<=30 ~ "21-30",
                           age<=40 ~ "31-40",
                           age<=50 ~ "41-50",
                           age<=60 ~ "51-60",
                           age>60 ~ "60+")) %>% 
  ggplot(aes(x = age, y = hours)) +
  geom_point(na.rm = TRUE) +
  facet_wrap(facets = "group")

mutate %>% 
  ggplot(aes(x = hours, y = age)) +
  geom_point(na.rm = TRUE) +
  geom_smooth(na.rm = TRUE)













#############################Challange 3####################################

mutate %>% 
  ggplot(aes(x = hours, y = age))+
  geom_point(na.rm = TRUE, aes(color = gender)) +
  facet_wrap(facets = "country")


mutate %>% 
  group_by(country) %>% 
  summarise(med = median(hours))
  ggplot(aes(x = hours, y = age))+
  geom_point(na.rm = TRUE, aes(color = gender)) 

    
    
mutate %>% 
  group_by(marathon) %>% 
  summarise(med = median(hours)) %>% 
  arrange(med) 
