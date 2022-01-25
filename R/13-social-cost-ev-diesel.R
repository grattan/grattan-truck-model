

#Assume over the first 10 years of ownership, and sold in 2022

electric_pc <- policy_outcomes %>% 
filter(sales_year == 2022,
       age <= 10,
       vkt_scenario == "vkt_central",
       scenario == "baseline") %>% 
  #basically saying that this is a single vehicle over the first 10 years of ownership
  mutate(total = 1) %>% 
  #working out the total fuel used/co2
  select(-vkt_scenario, -electric_share, -tyre_improvement, -engine_efficiency, -fuel_consumption,
         -pollutant_year, -pollutant_class, -co2_ice, -co2_ev, -co2)

electric_pc <- bind_rows(
  electric_pc %>% 
    mutate(type = "Diesel"),
  
  electric_pc %>% 
    mutate(type = "EV")
) %>% 
  unique()

electric_pc <- electric_pc %>% 
  mutate(co2 = case_when(
    #including upstream emissions
    type == "Diesel" ~ diesel_fuel_to_co2(vkt * diesel_rate_100) * 1.2,
    type == "EV" ~ vkt * ev_consumption * 1000 * ei_g_wh),
    fuel_consumption = case_when(
      type == "Diesel" ~ diesel_rate_100 / 100 * vkt,
      type == "EV" ~ 0),
    pollutant_total = case_when(
      pollutant %in% c("ex_nox_l", "ex_voc_l", "ex_sox_l") ~ pollutant_rate * fuel_consumption / 1000000,
      pollutant == "ex_pm10_l" ~ pollutant_rate * fuel_consumption * 0.95 / 1000000,
      pollutant %in% c("tyre_pm25_km", "tyre_pm10_km", "brake_pm25_km", "brake_pm10_km", 
                       "road_wear_pm10_km", "road_wear_pm25_km") ~ pollutant_rate * vkt / 1000000),
    pollutant_cost = case_when(
      pollutant == "ex_nox_l" ~ pollutant_total * nox_cost,
      pollutant == "ex_sox_l" ~ pollutant_total * sox_cost,
      pollutant == "ex_voc_l" ~ pollutant_total * hc_voc_cost,
      pollutant %in% c("ex_pm10_l", "road_wear_pm25_km", "tyre_pm25_km", "brake_pm25_km") ~ pollutant_total * pm2_5_combustion_cost,
      pollutant %in% c("tyre_pm10_km", "brake_pm10_km", "road_wear_pm10_km") ~ pollutant_total * pm10_nonexhaust_cost)) %>% 
  
  group_by(fuel_class, type, co2, region, vkt, age) %>% 
  summarise(pollutant_cost_total = sum(pollutant_cost),
            co2 = co2 / 1000000) %>% 
  
  mutate(co2_cost = co2 * 25,
         social_cost = pollutant_cost_total + co2_cost)  %>% 
  unique() %>% 
  group_by(fuel_class, type) %>% 
  summarise(social_cost = sum(social_cost),
            co2 = sum(co2))
            
    



electric_pc %>% 
  filter(fuel_class != "Non-freight carrying trucks") %>% 
  ggplot(aes(x = social_cost, 
             y = fuel_class, 
             fill = type, 
             colour = type,
             size = social_cost)) +
  geom_point() +
  theme_grattan(chart_type = "scatter") +
  grattan_x_continuous(labels = scales::label_dollar(),
                       limits = c(0, 160000),
                       breaks = c(0, 50000, 100000, 150000)) +
  scale_colour_manual(values = c(grattan_red, grattan_blue)) +
  scale_size(range = c(5, 10)) +
  
  grattan_label_repel(data = electric_pc %>% 
                  filter(fuel_class == "Rigid trucks"),
                aes(label = type,
                    fontface = "bold"),
                nudge_y = 0.2) +
  
   labs(title = "The social costs of EV trucks are far less than diesel",
       subtitle = "Total social costs from carbon and pollutant emissions over 10 years of vehicle operation",
       x = "10 year social cost",
       y = NULL,
       caption = "Includes pollutant costs and a social cost of carbon of $25/tonne. Assumes a diesel 
       vehicle is Euro V standard. Does not include estimates of noise pollution damage.")
  




  
