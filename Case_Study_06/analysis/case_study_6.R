library(tidyverse)
library(readr)
library(stringr)
library(buildings)

## Loading Data


build <- buildings::buildings0809
climate <- buildings::climate_zone_fips

permits <- buildings::permits %>% filter(state == 16) %>% rename("FIPS.state" = "state")
rest <- buildings::restaurants 


new <- left_join(build, climate, by = c("FIPS.county","FIPS.state"))
inter <- left_join(new, permits, by = c("FIPS.state"))

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


not_rest <- not_restaurants %>% str_trim() %>% str_to_lower() %>% str_c(collapse = "|")
standalone <- standalone_retail %>% str_trim() %>% str_to_lower() %>% str_c(collapse = "|")
full_types <- full_service_type %>% str_trim() %>% str_to_lower() %>% str_c(collapse = "|")
full_names <- full_service_names %>% str_trim() %>% str_to_lower() %>% str_c(collapse = "|")

  
##Mutating Data


##Question Number One

new1 <- inter %>% filter(Type == "Food_Beverage_Service") %>% 
  mutate(subgroup = case_when(str_detect(ProjectTitle %>% str_to_lower(), not_rest) ~ "not_restaurant",
                              str_detect(ProjectTitle %>% str_to_lower(), standalone) ~ "standalone_retail",
                              str_detect(ProjectTitle %>% str_to_lower(), full_types) ~ "full_service_type",
                              str_detect(ProjectTitle %>% str_to_lower(), full_names) ~ "full_service_names",
                              TRUE ~ "other")) %>%  
  mutate(type_restaurant = case_when(SqFt >= 4000 ~ "Full Service Restaurant",
                              SqFt <= 4000 ~ "Quick Service Restaurant"))




new1 %>% 
 ggplot(aes(x = year, y = value)) +
  geom_jitter() 

                            
##I took a picture of what we did in class for help above.                              
                              




## Question number 2
all_data <- inter %>%  
  mutate(subgroup = case_when(str_detect(ProjectTitle %>% str_to_lower(), not_rest) ~ "not_restaurant",
                              str_detect(ProjectTitle %>% str_to_lower(), standalone) ~ "standalone_retail",
                              str_detect(ProjectTitle %>% str_to_lower(), full_types) ~ "full_service_type",
                              str_detect(ProjectTitle %>% str_to_lower(), full_names) ~ "full_service_names",
                              TRUE ~ "other")) %>%  
  mutate(type_restaurant = case_when(SqFt >= 4000 ~ "Full Service Restaurant",
                                     SqFt <= 4000 ~ "Quick Service Restaurant"))


