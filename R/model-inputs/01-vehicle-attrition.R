source("R/00-setup.R")


#' This script will set up the survival curves for heavy vehicles, based on tablebuilder
#' data from the motor vehicle census (2013-2021 census data)


# Importing data -----------------------------------

rego_data <- bind_rows(

  read_csv("data-raw/tablebuilder/final/MVUS-2013-type-no-registered.csv",
           skip = 9) %>% 
    clean_names() %>% 
    mutate(series = 2013,
           counting = "NA") %>% 
    select(year_of_manufacture, vehicle_type, counting, count, annotations, series),
  
  read_csv("data-raw/tablebuilder/final/MVUS-2014-type-no-registered.csv",
           skip = 6) %>% 
    clean_names() %>% 
    mutate(series = 2014),
  
  read_csv("data-raw/tablebuilder/final/MVUS-2015-type-no-registered.csv",
           skip = 6) %>% 
    clean_names() %>% 
    mutate(series = 2015),
  
  read_csv("data-raw/tablebuilder/final/MVUS-2016-type-no-registered.csv",
           skip = 6) %>% 
    clean_names() %>% 
    mutate(series = 2016),
  
  read_csv("data-raw/tablebuilder/final/MVUS-2017-type-no-registered.csv",
           skip = 6) %>% 
    clean_names() %>% 
    mutate(series = 2017),
  
  read_csv("data-raw/tablebuilder/final/MVUS-2018-type-no-registered.csv",
           skip = 6) %>% 
    clean_names() %>% 
    mutate(series = 2018),
  
  read_csv("data-raw/tablebuilder/final/MVUS-2019-type-no-registered.csv",
           skip = 6) %>% 
    clean_names() %>% 
    mutate(series = 2019),
  
  read_csv("data-raw/tablebuilder/final/MVUS-2020-type-no-registered.csv",
           skip = 6) %>% 
    clean_names() %>% 
    mutate(series = 2020),
  
  read_csv("data-raw/tablebuilder/final/MVUS-2021-type-no-registered.csv",
           skip = 6) %>% 
    clean_names() %>% 
    mutate(series = 2021)) %>% 
  
  
  select(-annotations) %>% 
  #filter(counting %in% c("Vehicles", "Vehicle Level")) %>%
  
  mutate(vehicle_type = case_when(
    vehicle_type %in% c("Fire Appliances", "Trucks with Machinery Mounted", 
                        "Other Non Freight Carrying Trucks", "Tow Trucks") ~ "Non-freight carrying trucks",
    vehicle_type %nin% c("Fire Appliances", "Trucks with Machinery Mounted", 
                         "Other Non Freight Carrying Trucks", "Tow Trucks") ~ vehicle_type)) %>% 
  
  group_by(vehicle_type, year_of_manufacture, series) %>% 
  summarise(count = sum(count))



# Wrangling data ------------------------------------

attrition <- rego_data %>% 
  filter(#vehicle_type == "Prime Movers",
         year_of_manufacture %nin% c("Total", "Not stated")) %>% 
  mutate(year_of_manufacture = as.numeric(year_of_manufacture)) %>% 
  arrange(year_of_manufacture) %>% 
  ungroup() %>% 
  group_by(year_of_manufacture, vehicle_type) %>% 
  mutate(perc_change = count / lag(count)) %>% 
  filter(year_of_manufacture %in% 1970:2012) %>% 
  mutate(age = series - year_of_manufacture) %>% 
  
  #now filtering out first year (and 2021 because it seems strangely different)
  #and getting averages
  filter(series %nin% c(2013, 2021)) %>% 
  group_by(age, vehicle_type) %>% 
  summarise(perc_change = mean(perc_change, na.mr = FALSE)) %>% 
  
  #and because the first year of rego data is dodgy we will set attrition to 1 (no attrition)
  mutate(perc_change = case_when(
    perc_change >= 1 ~ 1,
    perc_change < 1 ~ perc_change))




#and adding ages for 0 and 1 

young_vehicles <- bind_rows(
  attrition %>% 
    filter(age == 2) %>% 
    mutate(age = 1),
  
  attrition %>% 
    filter(age == 2) %>% 
    mutate(age = 0))
  

attrition <- bind_rows(attrition, young_vehicles)
  
#adjusting to make another column for cumulative rates

#Rego data was turned into attrition in the vehicle attrition script
#backing out cumulative attrition rates from this:
attrition <- attrition %>% 
  arrange(vehicle_type, age) %>% 
  group_by(vehicle_type)

i <- 1
while (i <= nrow(attrition)){
  if (attrition$age[i] == 0){
    attrition$cumulative_attr[i] <- 1
  } else {
    attrition$cumulative_attr[i] <- attrition$cumulative_attr[i-1] * attrition$perc_change[i]
  }
  i <- i + 1
}

# Plotting data --------------------------------------------

attrition %>% 
  ggplot(aes(x = age, 
             y = cumulative_attr, 
             colour = vehicle_type)) +
  geom_point() +
  geom_smooth() +
  scale_x_continuous_grattan(limits = c(0, 50)) +
  #geom_smooth() +
  facet_wrap(~vehicle_type) +
  theme_grattan() +
  grattan_colour_manual() +
  labs(title= "Vehicle attrition rates by segment",
       subtitle = "Proportion of vehicles surviving from pervious age/year",
       x = "Vehicle age")


# Final changes for compatability with model

survival_curves <- attrition %>% 
  rename("proportion_surviving" = cumulative_attr,
         "type" = vehicle_type) %>% 
  mutate(type = case_when(
    type == "Buses > 9 seats" ~ "buses",
    type == "Heavy Rigid Trucks > 4.5t GVM" ~ "heavy_rigid",
    type == "Light Rigid Trucks up to 4.5t GVM" ~ "light_rigid",
    type == "Prime Movers" ~ "articulated",
    type == "Non-freight carrying trucks" ~ "non_freight")) 


write_rds(survival_curves, "data/attrition.rds")








