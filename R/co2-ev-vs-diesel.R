

#Assume over the first 10 years of ownership, and sold in 2022

co2_ev_vs_diesel <- policy_outcomes %>% 
filter(sales_year == 2022,
       age <= 10,
       vkt_scenario == "vkt_central",
       scenario == "baseline") %>% 
  #basically saying that this is a single vehicle over the first 10 years of ownership
  mutate(total = 1) %>% 
  #working out the total fuel used/co2
  select(1:14, -type) %>% 
  group_by(fuel_class, age, fleet_year, vkt_scenario, diesel_rate_100, ev_consumption,
           ei_g_wh) %>% 
  summarise(vkt = sum(vkt)) %>% 
  group_by(fuel_class) %>% 
  mutate(ei_g_wh_lagged = lead(ei_g_wh, n = 8, default = 0))


co2_ev_vs_diesel <- bind_rows(
  co2_ev_vs_diesel %>% 
    mutate(type = "diesel",
           co2 = diesel_fuel_to_co2(diesel_rate_100) * vkt / 1000000),
  
  co2_ev_vs_diesel %>% 
    mutate(type = "EV 2022",
           co2 = ev_consumption * 1000 * ei_g_wh * vkt / 1000000),
  
  co2_ev_vs_diesel %>% 
  mutate(type = "EV 2030",
         co2 = ev_consumption * 1000 * ei_g_wh_lagged * vkt / 1000000)) %>% 
  
  group_by(fuel_class, vkt_scenario, type) %>% 
  summarise(co2 = sum(co2))


co2_ev_vs_diesel %>% 
  filter(fuel_class %in% c("Rigid trucks", "Articulated trucks")) %>% 
  mutate(fuel_class = factor(fuel_class, 
                             levels = c("Rigid trucks", "Articulated trucks")),
         type = factor(type, 
                       levels = c("EV 2030", "EV 2022", "diesel"))) %>% 
  ggplot(aes(x = co2,
             y = fuel_class,
             fill = type)) +
  
  geom_col(size = 2,
           alpha = 0.95,
           position = "dodge") +
  scale_fill_manual(values = c(grattan_lightblue, grattan_darkblue, grattan_red)) +
  theme_grattan() +
  
  grattan_label(aes(x = 640,
                    y = "Rigid trucks",
                    label = "EV (sold in 2022)",
                    fontface = "bold"),
                    colour = grattan_darkblue) +  
  
  grattan_label(aes(x = 630,
                    y = "Rigid trucks",
                    label = "Diesel",
                    fontface = "bold"),
                colour = grattan_red,
                nudge_y = 0.3) +
  
  grattan_label(aes(x = 400,
                    y = "Rigid trucks",
                    label = "EV (sold in 2030)",
                    fontface = "bold"),
                colour = grattan_lightblue,
                nudge_y = -0.3) +
  
  #grattan_fill_manual(3) +
  scale_x_continuous_grattan(limits =  c(0, 2000)) +
  labs(title = "CO2 emissions from ZE-HDVs are substantially lower than from diesel vehicles",
       subtitle = "Estimated CO2 emissions (lifetime) from diesel and electric trucks",
       x = "CO2 (tonnes)",
       caption = "Grattan analysis") 
  

grattan_save(type = "normal",
             save_ppt = TRUE,
             filename = "atlas/co2_ev-vs-diesel.pdf")
