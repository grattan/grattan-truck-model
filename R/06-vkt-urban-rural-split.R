
# Urban/rural proportions --------------------

source("R/00-setup.R")

# For the health costs and our model, we need to assign urban/rural driving kilometres
# to each vehicle. We can base these estimates from ABS motor vehicle use survey data 

# because we wat to avoid COVID effects, we're also basing this off of 2018 ABS data

urban_rural <- read_xlsx("data/2018-MVUS-area-of-use.xlsx",
          sheet = "r-input",
          skip = 1) %>% 
  clean_names()


# we're going to base our estimates on intrastate travel, and assume the proportions
# hold for interstate travelling vehicles
urban_rural <- urban_rural %>% 
  filter(area %in% c("Capital city", "Other urban areas", 
                     "Other areas", "Total intrastate"),
         vehicle_type %in% c("Rigid trucks", "Articulated trucks",
                             "Non-freight carrying trucks", "Buses")) %>% 
  select(vehicle_type, area, diesel) %>% 
  mutate(urban_rural = case_when(
    area %in% c("Other urban areas", "Capital city") ~ "urban",
    area == "Other areas" ~ "rural",
    area == "Total intrastate" ~ "total")) %>% 
  select(-area) %>% 
  group_by(vehicle_type, urban_rural) %>% 
  summarise(diesel = sum(diesel)) %>% 
  pivot_wider(names_from = urban_rural,
              values_from = diesel) %>% 
  mutate(urban_share = urban / total,
         rural_share = rural / total) %>% 
  select(-rural, -total, -urban) %>% 
  rename("metro" = urban_share,
         "non_metro" = rural_share) %>% 
  pivot_longer(cols = 2:3,
               values_to = "share",
               names_to = "region") 



# there's also no strong evidence to suggest that these fractions change over time
# or as a vehicle ages, so they will be constant in our analysis. 
  


