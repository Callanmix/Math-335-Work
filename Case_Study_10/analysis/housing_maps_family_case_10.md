---
title: "Case Study 10: Building the past"
author: "Callan Mix"
date: "March 23, 2019"
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
permits <- buildings::permits %>% 
  filter(StateAbbr == "ID", variable == "Single Family") 

names(permits)[names(permits) == "state"] <- "statefp"
names(permits)[names(permits) == "county"] <- "countyfp"


idaho_counties <- us_counties(resolution = "low") %>% 
  filter(state_abbr == "ID") %>% 
  transform("countyfp" = as.numeric(countyfp),
            "statefp" = as.numeric(statefp))
```

## Background

You have been asked to support a story for the local paper (that has a web presence) that looks back on the housing collapse and the early effects of residential construction. You have data on residential building permits from 1980 through 2010 for each county in the United States. Your colleague that is writing the article would like a few maps and graphics that highlight the single family building permit patterns across your state as well as the patterns in the US.

Remember the big story is the collapse of new building permits at the initial stages of the mortgage crisis. Make sure your graphics highlight the collapse in a clear and honest manner.


## Data Wrangling


```r
merged <- left_join(permits,idaho_counties, by = c("countyfp","statefp")) %>% 
  filter(year %in% c(2000:2015)) %>% 
  group_by(countyfp, year) %>% 
  mutate(median = median(value))
```

## Data Visualization


```r
ggplot() +
  geom_sf(data = merged,aes(fill = median), color = "red") +
  coord_sf(crs = "+proj=lcc +lat_1=-44 +lat_2=77 +lat_0=63.39 +lon_0=-114 +x_0=6200000 +y_0=3000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") +
  facet_wrap(facets = "year",nrow = 3) +
  scale_fill_gradient(name = "median", trans = "log10") +
  theme(axis.title.x = element_blank(),axis.text.x = element_blank(),
        axis.title.y = element_blank(),axis.text.y = element_blank())
```

![](housing_maps_family_case_10_files/figure-html/plot_data-1.png)<!-- -->

## Conclusions
