library(tidyverse)
flights <- nycflights13::flights

arrival <- group_by(flights, origin, carrier == "DL") %>% 
  summarise(ave = mean(arr_delay, na.rm = TRUE))

ggplot(arrival, aes(x = origin, y = ave)) +
  geom_line(aes(color = 'red', alpha = .6), show.legend = FALSE) +
  stat_summary(fun.y = "mean", geom = "point") +
  labs(title = "Average Arrival Delay When Departing With Delta",
       caption = "Line = Margin of Error", x = "Departing", y = "Minutes") +
  theme_gray()


  
dep <- group_by(flights, dest, sched_dep_time <= '1200', carrier) %>% 
  summarise(mean = mean(dep_delay, na.rm = TRUE)) 
  
filter(dep, mean >= quantile(mean, .75, na.rm = TRUE)) %>% 
ggplot(dep, aes(x = carrier, y = mean)) +
  geom_point(aes(color = dest), show.legend = FALSE) 

flights %>%is.na() %>% sum()


flights$dep_delay %>% quantile(probs =c(.25,.5,.75), na.rm = TRUE)


