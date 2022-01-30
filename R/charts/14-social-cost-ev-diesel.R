

#Assume over the first 10 years of ownership, and sold in 2022

electric_pc <- policy_outcomes %>% 
filter(sales_year > 2021,
       #Going 12 years to be consistent with the TCO estimates
       age <= 12,
       vkt_scenario == "vkt_central",
       scenario == "baseline") %>% 
  #basically saying that this is a single vehicle over the first 10 years of ownership
  mutate(total = 1) %>% 
  #working out the total fuel used/co2
  select(-vkt_scenario, -electric_share, -tyre_improvement, -engine_efficiency, -fuel_consumption,
         -pollutant_year, -pollutant_class, -co2, -co2_ice, -co2_ev)



electric_pc <- bind_rows(
  electric_pc %>% 
    mutate(fuel = "diesel"),
  
  electric_pc %>% 
    mutate(fuel = "electric")) %>% 
  unique()

electric_pc <- electric_pc %>% 
  mutate(co2 = case_when(
    #including upstream emissions
    fuel == "diesel" ~ diesel_fuel_to_co2(vkt * diesel_rate_100) * 1.2,
    fuel == "electric" ~ vkt * ev_consumption * 1000 * ei_g_wh),
    fuel_consumption = case_when(
      fuel == "diesel" ~ diesel_rate_100 / 100 * vkt,
      fuel == "electric" ~ 0),
    pollutant_total = case_when(
      pollutant %in% c("ex_nox_l", "ex_voc_l", "ex_sox_l", "ex_pm10_l", "ex_pm25_l", "secon_nox_pm25", "secon_sox_pm25") ~ pollutant_rate * fuel_consumption / 1000000,
      pollutant %in% c("tyre_pm25_km", "tyre_pm10_km", "brake_pm25_km", "brake_pm10_km", 
                       "road_wear_pm10_km", "road_wear_pm25_km") ~ pollutant_rate * vkt / 1000000),
    health_cost_total = pollutant_total * damage_cost_t) %>% 
  
  
  group_by(fuel_class, fuel, co2, region, vkt, age, sales_year, fleet_year) %>% 
  summarise(health_cost_total = sum(health_cost_total),
            co2 = co2 / 1000000) %>% 
  
  mutate(co2_cost = co2 * 25,
         social_cost = health_cost_total + co2_cost)  %>% 
  unique() %>% 
  group_by(fuel_class, fuel, sales_year, fleet_year) %>% 
  summarise(social_cost = sum(social_cost),
            co2 = sum(co2))
            
    

#Saving the dataset --------------------------------------

write_rds(electric_pc, "data/per_vech_social_cost.rds")



electric_pc %>% 
  filter(fuel_class != "Non-freight carrying trucks") %>% 
  ggplot(aes(x = social_cost, 
             y = fuel_class, 
             fill = fuel, 
             colour = fuel,
             size = social_cost)) +
  geom_point() +
  theme_grattan(chart_fuel = "scatter") +
  grattan_x_continuous(labels = scales::label_dollar(),
                       limits = c(0, 160000),
                       breaks = c(0, 50000, 100000, 150000)) +
  scale_colour_manual(values = c(grattan_red, grattan_blue)) +
  scale_size(range = c(5, 10)) +
  
  grattan_label_repel(data = electric_pc %>% 
                  filter(fuel_class == "Rigid trucks"),
                aes(label = fuel,
                    fontface = "bold"),
                nudge_y = 0.2) +
  
   labs(title = "The social costs of electric trucks are far less than diesel",
       subtitle = "Total social costs from carbon and pollutant emissions over 10 years of vehicle operation",
       x = "10 year social cost",
       y = NULL,
       caption = "Includes pollutant costs and a social cost of carbon of $25/tonne. Assumes a diesel 
       vehicle is Euro V standard. Does not include estimates of noise pollution damage.")
  




  
