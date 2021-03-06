---
title: "Case Study 3"
author: "Callan Mix"
date: "January 26, 2019"
output:
  html_document:  
    keep_md: true
    toc: true
    toc_float: true
    code_folding: hide
    fig_height: 6
    fig_width: 12
    fig_align: 'center'
---






```r
flights <- nycflights13::flights
```

## Data Wrangling


```r
arrival <- group_by(flights, origin, carrier == "DL") %>% 
  summarise(ave = median(arr_delay, na.rm = TRUE))

best_flight <- group_by(flights, origin, sched_dep_time <= '1200', carrier) %>%  
                summarise(quan = quantile(dep_delay, .75, na.rm = TRUE)) %>% 
                arrange(quan) %>%  top_n(1)
               
worst <- group_by(flights, dest) %>%
  summarise(arr_delay_med = median(arr_delay, na.rm = TRUE),
            dep_delay_med = median(dep_delay, na.rm = TRUE)) %>% 
  arrange(desc(arr_delay_med)) %>% top_n(5, arr_delay_med)
```

## Data Visualization


```r
ggplot(arrival, aes(x = origin, y = ave)) +
  geom_line(aes(color = 'red', alpha = .6), show.legend = FALSE) +
  stat_summary(fun.y = "mean", geom = "point") +
  labs(title = "Median Arrival Delay When Departing With Delta",
       caption = "Line = Margin of Error", x = "Departing", y = "Minutes") +
  theme_gray()
```

![](Flights_Graphs_files/figure-html/plot_data-1.png)<!-- -->
This first graph shows the arrival delay when departing with Delta.


```r
ggplot(best_flight, mapping = aes(x = origin, y = quan), na.rm = TRUE) +
  geom_jitter(aes(color = carrier), show.legend = FALSE) +
  geom_text_repel(aes(label = carrier,color = "white"), show.legend = FALSE) +
  labs(title = "Lowest Delay Times by Carrier by Origin at 75th Percentile",
          x = "Origin", y = "Minutes") +
  theme_gray()
```

![](Flights_Graphs_files/figure-html/unnamed-chunk-2-1.png)<!-- -->
Here I tried to show the lowest delay times at the 75th percentile, but I am not sure if it was the best way to show the data.


```r
ggplot(worst, aes(x = dest, y = arr_delay_med)) +
  geom_point(aes(color = dest, size = arr_delay_med, alpha = .5), show.legend = FALSE) +
  labs(title = "Five Worst Destination Airports for Arrival Delays", x = "Destination",
       y = "Arrival Delay (Minutes)") +
  theme_minimal()
```

![](Flights_Graphs_files/figure-html/unnamed-chunk-3-1.png)<!-- -->
Finally, we have a graph the shows the worst destination airports. Again I am not sure that this graph was the best was to show the data, but I was not sure any other way to use besides a bar graph.

## Conclusions

I learned a lot doing this assignment. The hardest part for me is organizing the data in such a way that I can plot a meaningful graph that communicates relavent information. If there was a better way to do this assignment, I would love to know. 
