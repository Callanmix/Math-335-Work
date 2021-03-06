---
title: "Wealth and Life Expectancy"
author: "Callan Mix"
date: "1/19/2019"
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





## Background

I created two graphs that show GPD per Capita by year and by life expectancy. I have never done a data visualization before of this size. I am brand new to programming and I learned a lot about how to do it. I used a lot of google and the text book. I am not sure if I could have done a different route using less code. 


## Data Visualization


```r
ggplot(filter(gapminder, gdpPercap < 50000), mapping = aes(x = lifeExp, y = gdpPercap)) +
  geom_point(aes(size = pop/100000, color = continent)) +
  facet_wrap(~year,nrow = 1) +
  scale_y_continuous(trans = "sqrt") +
  labs(x = "Life Expectancy",y = "GDP Per Capita",size = "Population (100K)",color = "Continent") +
  theme_bw()
```

![](Wealth_and_Life_Expectancy_files/figure-html/plot_data-1.png)<!-- -->

```r
ggsave("LifeExpectancyPlot.png", width = 15)
```


```r
continent <- gapminder %>%
  group_by(continent, year) %>% 
  summarise(gdpPercap = weighted.mean(gdpPercap, pop))

(plot2 <- group_by(filter(gapminder, gdpPercap < 50000), continent) %>% 
    ggplot(plot2, mapping = aes(x = year,y = gdpPercap)) +
    geom_path(aes(color = continent, group = country)) +
    geom_path(continent, mapping = aes(x = year, y = gdpPercap)) +
    geom_point(aes(size = pop/100000, color = continent)) +
    geom_point(continent, mapping = aes(x = year, y = gdpPercap)) +
    scale_y_continuous(trans = "sqrt") +
    facet_wrap(~continent,nrow = 1)) +
  labs(x = "Year",y = "GDP Per Capita",size = "Population (100K)",color = "Continent") +
  theme_bw()
```

![](Wealth_and_Life_Expectancy_files/figure-html/plot_data2-1.png)<!-- -->

```r
ggsave("continentplot.png", width = 15)
```
