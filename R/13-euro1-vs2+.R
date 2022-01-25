


euro1_2_fraction <- policy_outcomes %>% 
  filter(vkt_scenario == "vkt_central",
         fleet_year == 2021,
         scenario == "baseline") %>% 
  mutate(euro = case_when(
    pollutant_year %in% c("1996-2002", "pre 1996") ~ "Euro 1 or less",
    pollutant_year %nin% c("1996-2002", "pre 1996") ~ "Euro 2 or higher"
  )) %>% 
  
  mutate(total_pollution = case_when(
    pollutant %in% c("ex_nox_l", "ex_sox_l", "ex_voc_l", "ex_pm10_l") ~ pollutant_rate * fuel_consumption,
    pollutant %nin% c("ex_nox_l", "ex_sox_l", "ex_voc_l", "ex_pm10_l") ~ pollutant_rate * vkt * total)) %>% 
  
  group_by(euro, pollutant, fuel_class) %>% 
  summarise(total_pollution = sum(total_pollution)) %>% 
  
  mutate(pollutant_class = case_when(
    pollutant == "ex_nox_l" ~ "nox",
    pollutant %in% c("brake_pm10_km", "tyre_pm10_km", "road_wear_pm10_km") ~ "pm10",
    pollutant %in% c("ex_pm10_l", "brake_pm25_km", "tyre_pm25_km", "road_wear_pm25_km") ~ "pm25"
  )) %>% 
  filter(!is.na(pollutant_class)) %>% 
  
  group_by(pollutant_class, fuel_class, euro) %>% 
  summarise(total_pollution = sum(total_pollution))


euro1_2_fraction %>% 
  mutate(colour = paste0(fuel_class, " ", euro)) %>% 
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
       x = NULL)


#grattan_save(filename = "atlas/euro-emissions-breakdown",
#             type = "fullslide",
#             save_ppt = TRUE)




