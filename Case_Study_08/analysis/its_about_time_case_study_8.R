library(tidyverse)
library(lubridate)
library(riem)

sale <- read_csv("https://byuistats.github.io/M335/data/sales.csv")

dat_time <- sale %>% 
  mutate(fix_time = force_tz(Time, tzone = "GMT") %>% with_tz("America/Denver")) %>% 
  mutate(hour = ceiling_date(fix_time, unit = 'hour'),
         just_hours = format(as.POSIXct(hour) ,format = "%H:%M:%S"),
         day = ceiling_date(fix_time, unit = 'day'),
         week = ceiling_date(fix_time, unit = 'week'),
         month = ceiling_date(fix_time, unit = 'month'),
         weekdays = weekdays(as.Date(.$fix_time))) %>% 
  .[-15631, ] %>% 
  filter(month != "2016-05-01 MDT", Name != "Missing")


###### By hour of day and faceted by Name

dat_time %>% 
  filter(just_hours >= "09:00:00", Name != "Missing") %>% 
  group_by(Name, just_hours) %>% 
  summarise(price = median(Amount)) %>% 
  ggplot(aes(x = just_hours, y = price)) +
  geom_point(aes(color = Name)) +
  geom_path(aes(group = Name, color = Name)) + 
  facet_wrap(facets = "Name") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks = c("08:00:00","12:00:00","16:00:00","20:00:00"))

######## By hour of day and boxplot amount under 250

dat_time %>% 
  filter(just_hours >= "08:00:00", Name != "Missing", Amount <= 250) %>% 
  group_by(just_hours) %>% 
  ggplot(aes(x = just_hours, y = Amount)) +
  geom_jitter(aes(color = Name)) +
  geom_boxplot(aes(group = just_hours)) +
  facet_wrap(facets = "Name") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks = c("08:00:00","12:00:00","16:00:00","20:00:00"))
 
######### Geom_violin for hours of day and amount 

dat_time %>% 
  filter(just_hours >= "08:00:00", Name != "Missing", Amount <= 250) %>% 
  group_by(just_hours) %>% 
  ggplot(aes(x = just_hours, y = Amount)) +
  geom_violin(aes(group = just_hours, fill = just_hours), show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks = c("08:00:00","12:00:00","16:00:00","20:00:00"))


########## By Weekday

dat_time %>% 
  filter(just_hours >= "08:00:00", Name != "Missing", Amount <= 150, Amount >=-150) %>%
  group_by(just_hours) %>% 
  ggplot(aes(x = just_hours, y = Amount)) +
  geom_jitter(aes(color = Name), alpha = .5) +
  geom_boxplot(aes(group = just_hours)) +
  facet_wrap(facets = "weekdays", scales = "free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks = c("08:00:00","12:00:00","16:00:00","20:00:00"))

dat_time %>% 
  filter(just_hours >= "08:00:00", Name != "Missing", Amount <= 150, Amount >=-150) %>%
  group_by(just_hours) %>% 
  ggplot(aes(x = just_hours, y = Amount)) +
  geom_jitter(aes(color = weekdays), alpha = .5) +
  geom_boxplot(aes(group = just_hours), fill = NA, outlier.shape = NA) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks = c("08:00:00","12:00:00","16:00:00","20:00:00"))

############

dat_time %>% 
  filter(just_hours >= "09:00:00", Name != "Missing") %>%
  ggplot(aes(x = week, y = Amount)) +
  geom_point()


############### By Weekday

dat_time %>% 
  filter(just_hours >= "09:00:00", Name != "Missing") %>%
  group_by(day, Name, weekdays) %>% 
  summarise(median = median(Amount)) %>% 
  ggplot(aes(x = day, y = median)) +
  geom_smooth() +
  geom_point(aes(color = Name)) +
  facet_wrap(facets = "weekdays")


dat_time %>% 
  filter(just_hours >= "07:00:00", Name != "Missing") %>%
  ggplot(aes(x = factor(weekdays, levels = c("Sunday","Monday","Tuesday",
                                             "Wednesday","Thursday",
                                             "Friday","Saturday")), y = just_hours)) +
  geom_jitter(alpha = .5) +
  facet_wrap(facets = "Name") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_discrete(breaks = c("08:00:00","12:00:00","16:00:00","20:00:00"))

############### By week

dat_time %>% 
  filter(just_hours >= "07:00:00", Name != "Missing") %>%
  group_by(week, Name) %>% 
  summarise(sd = sd(Amount)) %>% filter(week != "2016-07-24") %>% 
  ggplot(aes(x = week, y = sd)) +
  geom_point(aes(color = Name)) +
  geom_line(aes(group = Name))
  

################ By Month

dat_time %>% 
  group_by(Name, month, day) %>% 
  summarise(mean = mean(Amount), sd = sd(Amount), median = median(Amount)) %>% 
  ggplot(aes(y = mean, x = day)) +
  geom_point(aes(color = Name)) +
  geom_line(aes(group = Name, color =  Name), size = .75) +
  facet_wrap(facets = "month", scales = "free")


dat_time %>% 
  group_by(month, day, Name) %>% 
  summarise(sum = sum(Amount)) %>% 
  ggplot(aes(y = sum, x = day)) +
  geom_point(aes(color = Name), show.legend = FALSE) +
  geom_smooth(alpha = .05) +
  facet_wrap(facets = "Name")


dat_time %>% 
  filter(just_hours >= "07:00:00", Name != "Missing") %>%
  ggplot(aes(x = factor(weekdays, levels = c("Sunday","Monday","Tuesday",
                                             "Wednesday","Thursday",
                                             "Friday","Saturday")), y = just_hours)) +
  geom_jitter(alpha = .5, aes(color = month), show.legend = FALSE) +
  facet_wrap(facets = "month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_discrete(breaks = c("08:00:00","10:00:00","12:00:00","14:00:00",
                              "16:00:00","18:00:00","20:00:00","22:00:00")) +
  labs(title = "Sales Traffic by Hour and Month", x = "Days of the Week", y = "Time of Day")
