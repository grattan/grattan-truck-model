
# This script estimates the proportion of heavy vehicle pollutant emissions that 
# euro 1 and 2 vehicles are responsible for. (vehicles sold pre-2002)


# Determining total health costs from pollutant emissions based on sales date category (pre/post 2002)

euro1_2_fraction <- policy_outcomes %>% 
  filter(vkt_scenario == "vkt_central",
         fleet_year == 2021,
         scenario == "baseline") %>% 
  mutate(euro = case_when(
    pollutant_year %in% c("1996-2002", "pre 1996") ~ "Pre-2002",
    pollutant_year %nin% c("1996-2002", "pre 1996") ~ "Post-2002")) %>% 
  
  mutate(total_pollution = case_when(
    pollutant %in% c("ex_nox_l", "ex_sox_l", "ex_voc_l", "ex_pm10_l") ~ pollutant_rate * fuel_consumption,
    pollutant %nin% c("ex_nox_l", "ex_sox_l", "ex_voc_l", "ex_pm10_l") ~ pollutant_rate * vkt * total)) %>% 
  
  group_by(euro, pollutant, fuel_class) %>% 
  summarise(total_pollution = sum(total_pollution)) %>% #select(pollutant) %>% unique()
  
  mutate(pollutant_class = case_when(
    pollutant == "ex_nox_l" ~ "nox",
    pollutant %in% c("brake_pm10_km", "tyre_pm10_km", "road_wear_pm10_km", "ex_pm10_l") ~ "pm10",
    pollutant %in% c("ex_pm25_l", "brake_pm25_km", "tyre_pm25_km", "road_wear_pm25_km", 
                     "secon_nox_pm25", "secon_sox_pm25") ~ "pm25",
    pollutant%in% c("ex_sox_l", "ex_voc_l") ~ "other")) %>% 
  
  group_by(pollutant_class, fuel_class, euro) %>% 
  summarise(total_pollution = sum(total_pollution))



#Plotting for various pollutant -----------------------------------------------

euro1_2_fraction %>% 
  mutate(colour = paste0(fuel_class, " ", euro),
         euro = factor(euro, levels = c("Pre-2002", "Post-2002"))) %>% 
  
  
  filter(fuel_class != "Non-freight carrying trucks",
         pollutant_class %in% c("pm25", "nox")) %>% 
  
  
  ggplot(aes(x = euro, 
             y = total_pollution / 1000000,
             fill = colour)) +
  geom_col() +
  scale_y_continuous_grattan() +
  theme_grattan() +
  scale_fill_manual(values = c(grattan_red4, grattan_red,
                               grattan_orange4, grattan_orange,
                               grattan_yellow4, grattan_yellow)) +
  facet_grid(cols = vars(fuel_class),
             rows = vars(pollutant_class),
             scales = "free_y") + 
  labs(title = "Euro 1 and earlier vehicles contribute to a disproportionate share of all pollution",
       subtitle = "Tonnes of pollutant emissions, by Euro class",
       caption = "This accounts for the fact that newer vehicle travel significantly further than older 
       vehicles on avergae. Grattan analysis.",
       x = NULL)


#grattan_save(filename = "atlas/euro-emissions-breakdown",
#             type = "fullslide",
#             save_ppt = TRUE)




