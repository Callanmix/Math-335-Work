library(tidyverse)
library(Lahman)
library("blscrapeR")
LahmanData




names <- Master %>% 
  select(playerID,nameGiven)

cash <- Salaries %>% 
  select(playerID, teamID, salary, yearID)

college <- CollegePlaying %>% 
  group_by(playerID) %>% 
  arrange(playerID, desc(yearID)) %>% 
  mutate(last_year = row_number(desc(yearID))) %>% 
  filter(last_year == 1) %>% select(playerID, schoolID)

school <- Schools %>% 
  select(schoolID,state)

inflat <- inflation_adjust(2017)

tab <- merge(names,cash, by = "playerID")
tab1 <- merge(school, college, by = "schoolID")
tab3 <- merge(tab, tab1, by = "playerID")

tab3 %>% 
  group_by(nameGiven) %>% 
  summarise(ave = mean(salary))
  
#Salaries, collegeplaying, schools, master


final_tab <- tab3 %>% 
  rename("year"="yearID") %>% 
  merge(inflat, by = "year") %>% 
  group_by(nameGiven) %>% 
  mutate(new = salary/adj_value) %>% 
  select(year:state, adj_value, new)

MLB_Graph <- final_tab %>% 
  filter(state == "UT") %>% 
  ggplot(aes(y = salary, x = schoolID)) + 
  geom_boxplot(aes(group = schoolID, fill = schoolID), show.legend = FALSE) +
  labs(title = "BYU MLB Salaries Comparted to Other Utah Schools", x = "School", y = "Salary") +
  scale_y_continuous(breaks = c(1000000,2000000,3000000,4000000,5000000),
                     labels = c("1 Million","2 Million","3 Million","4 Million","5 Million"))

write_rds(MLB_Graph,here::here("Case_Study_06/Class_Task_12/MLB_Graph.rds"))

