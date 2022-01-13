# Pollutant estimates 

source("R/00-setup.R")
source("R/07-fleet-turnover.R")


#' To get the pollutant estimates, first we're going to assign pollutant emissions 
#' values to vehicles based on the vehicle type and year of manufacture. The values used are unpublished 
#' BITRE estimates (passed on by David Cosgrove), as determined from a combination of
#' models (include the COPERT suite for example). 


#all emissions factors are reported as g/L of fuel used
emissions_factors <- read_xlsx("data/bitre-fuel-factors-updated.xlsx",
          sheet = "Sheet1") %>% 
  rename("pollutant_class" = type) %>% 
  select(fuel_type, pollutant_class, year, NOx, PM10, NMVOC, SOx) %>% 
  rename("nox" = NOx,
         "pm10" = PM10,
         "sox" = SOx,
         "voc" = NMVOC,
         "pollutant_year" = year) 




all_fleets <- all_fleets %>% 
  mutate(pollutant_class = case_when(
  type == "light_rigid" ~ "medium_truck",
  type == "heavy_rigid" ~ "medium_truck",
  type == "articulated" ~ "heavy_truck",
  type == "buses" ~ "buses",
  type == "non_freight" ~ "medium_truck"),
  #and adding in the pollutant ages
  pollutant_year = case_when(
    sales_year > 2015 ~ "post 2015",
    sales_year %in% (2009:2015) ~ "2009-2015",
    sales_year %in% (2003:2008) ~ "2003-2008",
    sales_year %in% (1996:2002) ~ "1996-2002",
    sales_year < 1996 ~ "pre 1996"))



#Joining the emissions factors to vehicles
all_emissions <- left_join(all_fleets, emissions_factors) 


all_emissions %>% 
  #calculating the total over time and converting to tonnes
  group_by(fleet_year, vkt_scenario, region) %>% 
  summarise(total_nox_t = sum(nox * fuel_used * total / 1000000),
            total_pm10_t = sum(pm10 * fuel_used * total / 1000000)) %>% 
  select(-total_pm10_t) %>% 
  pivot_wider(names_from = vkt_scenario,
              values_from = total_nox_t) %>% 
  ggplot() +
  geom_ribbon(aes(x = fleet_year,
                  ymin = vkt_lower / 1000,
                  ymax = vkt_upper / 1000, 
                  fill = region),
              alpha = 0.2,
              colour = NA) +
  geom_line(aes(x = fleet_year,
                y = vkt_central / 1000,
                colour = region)) +
  scale_y_continuous_grattan(limits = c(0, 200)) +
  theme_grattan() +
  grattan_fill_manual() +
  grattan_colour_manual() + 
  labs(title = "NOx emissions are likely to rise without action",
       subtitle = "Total NOx emissions from heavy vehicles ('000 tonnes)",
       x = NULL)




all_emissions %>% 
  #calculating the total over time and converting to tonnes
  group_by(fleet_year, vkt_scenario, region) %>% 
  summarise(total_nox_t = sum(nox * fuel_used * total / 1000000),
            total_pm10_t = sum(pm10 * fuel_used * total / 1000000)) %>% 
  select(-total_nox_t) %>% 
  pivot_wider(names_from = vkt_scenario,
              values_from = total_pm10_t) %>% 
  ggplot() +
  geom_ribbon(aes(x = fleet_year,
                  ymin = vkt_lower / 1000,
                  ymax = vkt_upper / 1000,
                  fill = region),
              alpha = 0.2,
              colour = NA) +
  geom_line(aes(x = fleet_year,
                y = vkt_central / 1000,
                colour = region)) +
  scale_y_continuous_grattan(limits = c(0, 5)) +
  theme_grattan() +
  grattan_fill_manual() +
  grattan_colour_manual() + 
  labs(title = "PM10",
       subtitle = "Exhaust PM10 emissions from heavy vehicles ('000 tonnes)",
       x = NULL)


