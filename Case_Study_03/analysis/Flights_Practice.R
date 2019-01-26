library("tidyverse")
library("ggrepel")
flights <- nycflights13::flights

arrival <- group_by(flights, origin, carrier == "DL") %>% 
  summarise(ave = median(arr_delay, na.rm = TRUE))

ggplot(arrival, aes(x = origin, y = ave)) +
  geom_line(aes(color = 'red', alpha = .6), show.legend = FALSE) +
  stat_summary(fun.y = "mean", geom = "point") +
  labs(title = "Median Arrival Delay When Departing With Delta",
       caption = "Line = Margin of Error", x = "Departing", y = "Minutes") +
  theme_gray()


  
best_flight <- group_by(flights, origin, sched_dep_time <= '1200', carrier) %>%  
                summarise(quan = quantile(dep_delay, .75, na.rm = TRUE)) %>% 
                arrange(quan) %>%  top_n(1)
               


ggplot(best_flight, mapping = aes(x = origin, y = quan), na.rm = TRUE) +
  geom_jitter(aes(color = carrier), show.legend = FALSE) +
  geom_text_repel(aes(label = carrier,color = "white"), show.legend = FALSE) +
  labs(title = "Lowest Delay Times by Carrier by Origin at 75th Percentile",
          x = "Origin", y = "Minutes") +
  theme_gray()


worst <- group_by(flights, dest) %>%
  summarise(arr_delay_med = median(arr_delay, na.rm = TRUE),
            dep_delay_med = median(dep_delay, na.rm = TRUE)) %>% 
  arrange(desc(arr_delay_med)) %>% top_n(5, arr_delay_med)

ggplot(worst, aes(x = dest, y = arr_delay_med)) +
  geom_point(aes(color = dest, size = arr_delay_med, alpha = .5), show.legend = FALSE) +
  labs(title = "Five Worst Destination Airports for Arrival Delays", x = "Destination",
       y = "Arrival Delay (Minutes)") +
  theme_minimal()

