
#Scenario - euro 6 

#' This script uses the model to produce forecasts for emissions under euro 6 scenarios:
#' both a scenario where euro 6 comes into force in 2024, and where it comes into force in 2027
source("R/08-pollutant-emissions.R")


#' Our `all_emissions` dataset from the previous script has all the baseline forecast
#' information for the pollutant emissions, with pollutant values recorded in grams per litre
#' of fuel consumed


#' First we want to duplicate this dataset to create our euro 6 and baseline scenarios
#' We also want to add our Euro 6 assumptions - for specifically, that PM10 matter
#' rates will decrease by 60% with euro 6 for new vehicles compared to the 2015 and younger pollutant rates,
#' and that NOx emissions will decrease by 80% compared to 2015 and younger vehicle rates
#' these values are based on the limit values of euro 5 vs 6 
#' (https://www.infrastructure.gov.au/sites/default/files/migrated/vehicles/environment/forum/files/heavy-vehicle-emission-standards-for-cleaner-air.pdf page 24)

euro_scenarios <- bind_rows(
  all_emissions %>% 
    mutate(scenario = "Baseline"),
  
  all_emissions %>% 
    mutate(scenario = "Euro 6 (2024)",
           nox = case_when(
             sales_year >= 2024 ~ nox * 0.2,
             sales_year < 2024 ~ nox),
           pm10 = case_when(
             sales_year >= 2024 ~ pm10 * 0.4,
             sales_year < 2024 ~ pm10)),
  
  all_emissions %>% 
    mutate(scenario = "Euro 6 (2027)",
           nox = case_when(
             sales_year >= 2027 ~ nox * 0.2,
             sales_year < 2027 ~ nox),
           pm10 = case_when(
             sales_year >= 2027 ~ pm10 * 0.4,
             sales_year < 2027 ~ pm10)))






















# And now forecasting the emissions under each of these scenarios:

euro_results <- euro_scenarios %>% 
  group_by(vkt_scenario, fleet_year, scenario, ) %>% 
  summarise(total_nox_t = sum(nox * fuel_used * total / 1000000),
            total_pm10_t = sum(pm10 * fuel_used * total / 1000000)) 

#first for NOx
nox_scenarios <- euro_results %>% 
  select(-total_pm10_t) %>% 
  pivot_wider(names_from = vkt_scenario,
              values_from = total_nox_t) 
#and for NOx

pm_scenarios <- euro_results %>% 
  select(-total_nox_t) %>% 
  pivot_wider(names_from = vkt_scenario,
              values_from = total_pm10_t) 

# And plotting results ------------------------------------------

# Setting some colours 

colours <- c("Baseline" = grattan_red, 
             "Euro 6 (2024)" = grattan_yellow,
             "Euro 6 (2027)" = grattan_orange)


# First for NOx

nox_scenarios %>% 
  mutate(scenario = factor(scenario,
                           levels = names(colours))) %>% 
  ggplot() +
  geom_ribbon(aes(x = fleet_year,
                  ymin = vkt_lower / 1000,
                  ymax = vkt_upper / 1000,
                  fill = scenario),
              alpha = 0.15,
              colour = NA) +
  geom_line(aes(x = fleet_year,
                y = vkt_central / 1000,
                colour = scenario)) +
  scale_y_continuous_grattan(limits = c(0, 200)) +
  theme_grattan() +
  scale_colour_manual(values = colours) + 
  scale_fill_manual(values = colours) +
  labs(title = "NOx emissions are likely to rise without action",
       subtitle = "Total NOx emissions from heavy vehicles ('000 tonnes)",
       x = NULL) +
  
  geom_text(aes(
    x = 2020,
    y = 185,
    label = "Baseline"),
    hjust = "left",
    fontface = "bold",
    colour = grattan_red,
    check_overlap = TRUE) +
  
  geom_text(aes(
    x = 2020,
    y = 175,
    label = "Euro 6 (2027)"),
    hjust = "left",
    fontface = "bold",
    colour = grattan_orange,
    check_overlap = TRUE) +
  
  geom_text(aes(
    x = 2020,
    y = 165,
    label = "Euro 6 (2024)"),
    hjust = "left",
    fontface = "bold",
    colour = grattan_yellow,
    check_overlap = TRUE)

grattan_save(filename = "atlas/nox_euro.pdf",
             type = "fullslide",
             save_ppt = TRUE)



# And for PM 

pm_scenarios %>% 
  ggplot() +
  geom_ribbon(aes(x = fleet_year,
                  ymin = vkt_lower / 1000,
                  ymax = vkt_upper / 1000,
                  fill = scenario),
              alpha = 0.2,
              colour = NA) +
  geom_line(aes(x = fleet_year,
                y = vkt_central / 1000,
                colour = scenario)) +
  scale_y_continuous_grattan(limits = c(0, 4)) +
  theme_grattan() +
  scale_colour_manual(values = colours) + 
  scale_fill_manual(values = colours) +
  labs(title = "PM10 emissions can be significantly reduce with Euro 6",
       subtitle = "Total PM10 emissions from heavy vehicles ('000 tonnes)",
       x = NULL) +
  
  geom_text(aes(
    x = 2040,
    y = 2.75,
    label = "Baseline"),
    hjust = "right",
    fontface = "bold",
    colour = grattan_red,
    check_overlap = TRUE) +
  
  geom_text(aes(
    x = 2040,
    y = 2.5,
    label = "Euro 6 (2027)"),
    hjust = "right",
    fontface = "bold",
    colour = grattan_orange,
    check_overlap = TRUE) +
  
  geom_text(aes(
    x = 2040,
    y = 2.25,
    label = "Euro 6 (2024)"),
    hjust = "right",
    fontface = "bold",
    colour = grattan_yellow,
    check_overlap = TRUE)

grattan_save(filename = "atlas/pm_euro.pdf",
             type = "fullslide",
             save_pptx = TRUE)













