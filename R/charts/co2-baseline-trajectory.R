
# Forecast co2 chart - baseline (by sector)

source("R/00-setup.R")
policy_outcomes <- read_rds("data/policy_outcomes.rds")

#Importing DISER data + getting only trucks + buses-------------------------

co2_historical <- read_xlsx("data-raw/diser-co2-forecasts.xlsx",
                           sheet = "Sheet1") %>% 
  pivot_longer(cols = (2:11),
               values_to = "emissions_mt",
               names_to = "sector") %>% 
  clean_names() %>% 
  mutate(forecast_flag = case_when(
    year < 2021 ~ "historical",
    year >= 2021 ~ "forecast")) %>% 
  filter(sector %in% c("Rigid trucks", "Articulated trucks", "Buses")) %>% 
  filter(year <= 2020)



# Importing our data -----------------------------

co2_forecast <- policy_outcomes %>% 
  filter(scenario == "baseline",
         vkt_scenario == "vkt_central",
         pollutant == "ex_nox_l") %>% 
  group_by(scenario, fleet_year, fuel_class) %>% 
  summarise(co2 = sum(co2) / 1000000000000) %>% 
  rename("sector" = fuel_class,
         "year" = fleet_year,
         "forecast_flag" = scenario,
         "emissions_mt" = co2)  %>% 
  mutate(forecast_flag = "forecast") %>% 
  filter(sector != "Non-freight carrying trucks",
         year >= 2021)



# Binding data ------------------------------------

co2_all <- bind_rows(co2_historical, co2_forecast) %>% 
 mutate(sector = factor(sector, levels = c("Buses", "Rigid trucks", "Articulated trucks")))


# Plotting ----------------------

co2_all %>% 
  ggplot() + 
  
  # Historical component
  geom_area(data = co2_all %>% 
              filter(year %in% 2000:2021),
            aes(x = year, 
                y = emissions_mt, 
                fill = sector,
                colour = sector),
            alpha = 0.95) +
  
  # Forecast component
  geom_area(data = co2_all %>% 
              filter(year %in% 2021:2030),
            aes(x = year, 
                y = emissions_mt, 
                fill = sector,
                colour = sector),
            linetype = "dashed",
            size = 0.7,
            alpha = 0.5) +
  
  theme_grattan() +
  grattan_fill_manual(3) + 
  grattan_colour_manual(3) +
  scale_y_continuous_grattan(limits = c(0, 27.5),
                             breaks = c(0, 10, 20)) +
  scale_x_continuous_grattan(limits = c(1999, 2040),
                             breaks = c(2000, 2010, 2020, 2030)) +
  
  # Adding labels 
 # grattan_label(data = . %>% slice(1),
#                aes(x = 2000,
#                    y = 23,
#                    label = "Historical emissions"),
#                fontface = "bold",
#                colour = grattan_black,
#                hjust = "left") +
  
 # grattan_label(data = . %>% slice(1),
#                aes(x = 2018,
#                    y = 29,
#                    label = "Forecast emissions"),
##                fontface = "bold",
  #              colour = grattan_grey4,
   #             hjust = "left") +
  
  #Adding dots and callouts for emissions values
  geom_point(data = . %>% 
               filter(year %in% c(2000, 2021, 2030)) %>% 
               group_by(year) %>% 
               arrange(desc(sector)) %>% 
               mutate(emissions_mt = cumsum(emissions_mt)),
             
             aes(x = year, 
                 y = emissions_mt,
                 fill = sector,
                 colour = sector),
             size = 3) +
  
  # Labelling end points
  grattan_label(data = . %>% 
                         filter(year %in% c(2000, 2021, 2030)) %>% 
                         group_by(year) %>% 
                         arrange(desc(sector)) %>% 
                         mutate(emissions_mt = cumsum(emissions_mt)) %>% 
                         filter(sector == "Buses"),
                      
                      aes(label = paste0(round(emissions_mt), "Mt"),
                          x = year + 0.4, 
                          y = emissions_mt + 1.5),
                      colour = grattan_grey4,
                      fontface = "bold") +
  
  # Labelling categories
  grattan_label(data = . %>% 
                  filter(year %in% c(2030)) %>% 
                  group_by(year) %>% 
                  arrange(desc(sector)) %>%
                  mutate(emissions_mt = cumsum(emissions_mt)),
                
                aes(label = sector,
                    colour = sector,
                    x = year + 1, 
                    y = emissions_mt -0.3),
                fontface = "bold",
                hjust = "left") +
  
  
  labs(title = "Emissions from the heavy vehicle sector are set to rise",
       subtitle = "Heavy vehicle emissions (Mt)",
       x = NULL)


grattan_save(filename = "atlas/co2-forecast.pdf",
             type = "normal",
             save_pptx = TRUE)




