
# Carbon emissions

source("R/07-fleet-turnover.R")



# First calculating the CO2 emitted based on the fleet model

co2 <- all_fleets %>% 
  group_by(fleet_year, fuel_class, vkt_scenario) %>% 
  summarise(total_co2_mt = sum(co2 * total / 1000000000000)) 



#Importing data + getting only trucks + buses-------------------------

co2_forecasts <- read_xlsx("data/diser-co2-forecasts.xlsx",
                           sheet = "Sheet1") %>% 
  pivot_longer(cols = (2:11),
               values_to = "emissions_mt",
               names_to = "sector") %>% 
  clean_names() %>% 
  mutate(forecast_flag = case_when(
    year < 2021 ~ "historical",
    year >= 2021 ~ "forecast"
  ))


hdv_co2 <- co2_forecasts %>%   
  filter(sector %in% c("Rigid trucks", "Articulated trucks", "Buses")) %>% 
  mutate(sector = factor(sector, 
                         levels = c("Articulated trucks", "Rigid trucks", "Buses"))) 


hdv_co2 <- bind_rows(
  hdv_co2,
  
  hdv_co2 %>% 
    filter(year == 2021) %>% 
    mutate(forecast_flag = "historical")
)


co2_ours <- co2 %>% 
  mutate(forecast_flag = "Grattan forecast") %>% 
  rename("emissions_mt" = total_co2_mt,
         "year" = fleet_year,
         "sector" = fuel_class) %>% 
  pivot_wider(names_from = vkt_scenario,
              values_from = emissions_mt) %>% 
  mutate("emissions_mt"= vkt_central)


co2_forecasts <- bind_rows(
  co2_ours,
  hdv_co2
)



hdv_co2 <- co2_forecasts %>%   
  filter(sector %in% c("Rigid trucks", "Articulated trucks", "Buses")) %>% 
  mutate(sector = factor(sector, 
                         levels = c("Articulated trucks", "Rigid trucks", "Buses"))) 


#just plotting grattan/historical

hdv_co2 <- hdv_co2 %>% 
  mutate(include = case_when(
    (forecast_flag == "Grattan forecast" & year > 2021) ~ TRUE,
    (forecast_flag == "historical" & year <= 2021) ~ TRUE,
    (forecast_flag == "forecast" & year > 2020 ~ TRUE))) %>% 
  filter(include ==  TRUE)

hdv_co2 <- bind_rows(
  hdv_co2,
  
  hdv_co2 %>% 
    filter(year == 2021) %>% 
    mutate(forecast_flag = "Grattan forecast")) %>% 
  filter(forecast_flag != "forecast")



hdv_co2 %>% 
  
  filter(year <= 2030,
         year >= 2005) %>% 
  
  ggplot() +
  
  #the confidence band
  geom_ribbon(data = co2_forecasts %>% 
                filter(sector %in% c("Rigid trucks", 
                                     "Articulated trucks", "Buses"),
                       forecast_flag == "Grattan forecast",
                       year <= 2031) %>% 
                mutate(sector = factor(sector, 
                                       levels = c("Articulated trucks", 
                                                  "Rigid trucks", "Buses"))),
              aes(
                x = year,
                ymin = vkt_lower,
                ymax = vkt_upper,
                group = sector),
              fill = grattan_red,
              colour = NA,
              alpha = 0.18) +
  
  geom_line(aes(x = year, 
                y = emissions_mt,
                colour = forecast_flag)) +
  
  theme_grattan() + 
  grattan_colour_manual(2, 
                        reverse = TRUE) +
  scale_x_continuous_grattan(breaks = c(2010, 2021, 2030),
                             limits = c(2004, 2033)) +
  scale_y_continuous_grattan(breaks = c(0, 5, 10, 15),
                             limits = c(0, 19)) +
  
  geom_point(data = hdv_co2 %>% 
               filter(year %in% c(2005, 2021, 2030)),
             aes(x = year, 
                 y = emissions_mt,
                 colour = forecast_flag)) +
  
  geom_text(data = hdv_co2 %>% 
              filter(year %in% c(2005, 2021, 2030)),
            aes(x = year + 1,
                y = emissions_mt - 1.2,
                label = round(emissions_mt,1),
                colour = forecast_flag),
            check_overlap = TRUE,
            size = 5,
            fontface = "bold") +
  
  geom_text(data = hdv_co2 %>% 
              filter(sector == "Articulated trucks"),
            aes(
              x = 2005.7,
              y = 13.9,
              label = "Historical \nemissions",
              hjust = 0,
              size = 11),
            colour = grattan_orange,
            fontface = "bold",
            check_overlap = TRUE) +
  
  geom_text(data = hdv_co2 %>% 
              filter(sector == "Articulated trucks"),
            aes(
              x = 2018.7,
              y = 16,
              label = "Forecast \nemissions",
              hjust = 0,
              size = 11),
            colour = grattan_red,
            fontface = "bold",
            check_overlap = TRUE) +
  
  
  facet_wrap(~sector) +
  
  labs(title = "Emissions from heavy vehicles are forecast to rise",
       subtitle = "Annual greenhouse gas emissions (Mt CO2e) from HDVs",
       x = NULL)
