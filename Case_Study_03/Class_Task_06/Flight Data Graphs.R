library(tidyverse)
flights <- nycflights13::flights

group_flights <- group_by(flights, origin, month) %>% 
  summarise(ave = mean(dep_delay), average = mean(arr_delay), dis = mean(distance))

ggplot(group_flights, aes(x = month, y = dis)) +
  geom_point(aes(color = origin)) +
  geom_path(aes(mapping = origin))+
  scale_x_continuous(labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', ' Aug', 'Sep',
                                'Oct', 'Nov', 'Dec'), breaks = c(1,2,3,4,5,6,7,8,9,10,11,12)) +
  labs(title = "Average Distance Traveled From Airport", x = "Months", y = "Miles", 
       color = "Airports")

leave <- group_by(flights, dest, month) %>% 
  summarise(ave = mean(dep_delay), average = mean(arr_delay), dis = mean(distance))

ggplot(filter(leave, quantile(average >= .75)), aes(x = month, y = average)) +
  geom_jitter(aes(color = dest), show.legend = FALSE) +
  scale_x_continuous(labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', ' Aug', 'Sep',
                                'Oct', 'Nov', 'Dec'), breaks = c(1,2,3,4,5,6,7,8,9,10,11,12)) +
  labs(title = "Average Arrival Delay per Airport", x = "Months", y = "Minutes", 
       color = "Airports")







tail <- group_by(flights, carrier, tailnum) %>% 
  summarise(sum = sum(distance))

ggplot(tail, aes(y = sum, x = carrier)) +
  geom_jitter(aes(color = carrier, alpha = .6), show.legend = FALSE) +
  labs(title = "Sum of Miles by Plane per Carrier", y = "Miles", x = "Carrier") +
  theme_grey()











dist <- group_by(flights, month) %>% 
  summarise(ave = sum(distance))

ggplot(dist, mapping = aes(y = ave, x = month)) +
  geom_line(aes(color = ave), show.legend = NULL) +
  theme_minimal() +
  scale_y_continuous(labels = c('25 Million', '26 Million','27 Million', '28 Million','29 Million',
                                '30 Million', '31 Million','32 Million'), breaks = c(25000000, 26000000,
                                27000000, 28000000, 29000000,30000000, 31000000, 32000000)) +
  scale_x_continuous(labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', ' Aug', 'Sep',
                     'Oct', 'Nov', 'Dec'), breaks = c(1,2,3,4,5,6,7,8,9,10,11,12)) +
  labs(title = "Total Flight Distance Per Month", x = "Months", y = "Distance")
