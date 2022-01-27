
# This script estimates the CO2 emissions fo diesel and electric trucks, depending on type
# and sales date. 

#Assume only counting emissions over the first 10 years of ownership, when sold in 2022

source("R/00-setup.R")
read_rds("data/policy_outcomes.rds")


#Creating a dataset with emissions estimates, including a lagged variable of grid intensity
# for the vehicle purchased in 2030. 

co2_ev_vs_diesel <- policy_outcomes %>% 
filter(sales_year == 2022,
       age <= 10,
       vkt_scenario == "vkt_central",
       scenario == "baseline",
       #just selecting a single pollutant to remove duplicates for non-pollutant data
       pollutant == "ex_nox_l") %>% 
  #basically saying that this is a single vehicle over the first 10 years of ownership
  mutate(total = 1) %>% 
  #working out the total fuel used/co2
  select(1:14) %>% 
  group_by(fuel_class, age, fleet_year, vkt_scenario, diesel_rate_100, ev_consumption,
           ei_g_wh) %>% 
  summarise(vkt = sum(vkt)) %>% 
  group_by(fuel_class) %>% 
  #lagging the grid intensity variable by 8 years, to assume that this vehicle is purchased
  #8 years in the future (2030)
  mutate(ei_g_wh_lagged = lead(ei_g_wh, n = 8, default = 0))


# Binding together estimates for the different vehicle types (EVs/diesel etc.)

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
  

#grattan_save(type = "normal",
#             save_ppt = TRUE,
#             filename = "atlas/co2_ev-vs-diesel.pdf")
