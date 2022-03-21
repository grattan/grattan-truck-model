


cost_per_year <- ev_scenarios %>% 
  filter(scenario %in% c("Euro 6 (2027)", "Electric and Euro 6 (2027)"),
         fleet_year >= 2022,
         age <= 10,
         vkt_scenario == "vkt_central",
         fuel_class %in% c("Articulated trucks", "Rigid trucks")) %>%  
  group_by(scenario, vkt_scenario, sales_year, fuel_class) %>%
  # Including adblue in maintenance
  summarise(maintenance_cost_total = sum(maintenance_cost_total) + sum(adblue_cost),
            purchase_price = sum(purchase_price),
            fuel_cost = sum(fuel_cost),
            co2_social_cost = sum(co2_social_cost),
            infrastructure_cost = sum(infrastructure_cost),
            time_weight_penalty = sum(time_weight_penalty),
            health_cost_total = sum(health_cost_total),
            noise_cost = sum(noise_cost)) %>% 
  pivot_longer(cols = 5:12,
               names_to = "cost_type",
               values_to = "cost") %>% 
  #mutate(disc_costs = (1 / (1 + 0.07)^(fleet_year - 2022)) * cost) %>% 
  
  #selecting only the costs relevant for TCO
 #select(-disc_costs) %>% 
  filter(cost_type %in% c("maintenance_cost_total",
                          "purchase_price",
                          "fuel_cost",
                          "infrastructure_cost",
                          "time_weight_penalty"),
         vkt_scenario == "vkt_central") %>% 
  pivot_wider(names_from = scenario,
              values_from = cost) %>% 
  #Calculating the change in costs per year
  mutate(cost_change = (`Euro 6 (2027)` - `Electric and Euro 6 (2027)`) / 1000000) %>% 
  filter(sales_year >= 2024) %>% 
  group_by(sales_year, fuel_class) %>% 
  summarise(cost_change = sum(cost_change)) 



# total electric sales between now and 10 year TCO parity (2029)
trucks_per_year <- ev_scenarios %>% 
  filter(sales_year %in% 2024:2029,
         vkt_scenario == "vkt_central",
         fuel_class %in% c("Articulated trucks", "Rigid trucks"),
         age == 0) %>%
  group_by(sales_year, fuel_class) %>% 
  summarise(electric_total = sum(electric_share * total))


joined <- left_join(trucks_per_year, cost_per_year) %>% 
  mutate(gap = cost_change / electric_total * 1000000)




joined %>% 
  filter(gap < 0,
         sales_year <= 2029) %>% 
  ungroup() %>% 
  summarise(cost_change = sum(cost_change))
