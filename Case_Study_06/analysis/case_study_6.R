library(tidyverse)
library(readr)
library(stringr)
library(buildings)
build <- buildings::buildings0809
climate <- buildings::climate_zone_fips

new <- left_join(build, climate, by = c("FIPS.county","FIPS.state"))

new %>% filter(Type == "Food_Beverage_Service") %>% 
  mutate(subgroup = case_when(str_detect(ProjectTitle %>% str_to_lower(),
                              a) 
                              
                              
                              
a <- not_restaurants %>% str_trim() %>% str_to_lower() %>% str_c(collapse = "|")
 

  
  
  
  
permits <- buildings::permits
rest <- buildings::restaurants 
  







not_restaurants <- c("development","Food preperation center", "Food Services center","bakery","Grocery","concession","Cafeteria", "lunchroom","school","facility"," hall ")
standalone_retail <- c("Wine","Spirits","Liquor","Convenience","drugstore","Flying J", "Rite Aid ","walgreens ","Love's Travel ")
full_service_type <- c("Ristorante","mexican","pizza ","steakhouse"," grill ","buffet","tavern"," bar ","waffle","italian","steak house")
quick_service_type <- c("coffee"," java "," Donut ","Doughnut"," burger ","Ice Cream ","custard ","sandwich ","fast food "," bagel ")
quick_service_names <- restaurants$Restaurant[restaurants$Type %in% c("coffee","Ice Cream","Fast Food")]
full_service_names <- restaurants$Restaurant[restaurants$Type %in% c("Pizza","Casual Dining","Fast Casual")]
## After the above assignments the below rules need to be implemented
# Over 4,000 ADD and NEW construction get assigned to Sit Down Restaurants
# Under 4,000 sqft NEW construction get assigned to Fast Food
# all Type == "Food/Beverage Service" that don't get grouped based on the above are called "Unknown"
