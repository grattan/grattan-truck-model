
# Policy outcomes CO2 


# Carbon emissions -------------------------------------------------
source("R/00-setup.R")
policy_scenarios <- read_rds("data/policy-scenarios.rds")

# So the co2 is made up of:
# the ICE fraction; where it is the VKTs travelled, multipled by the rate of diesel consumption
# per 100km, divided by 100. Then the efficiency gains are factored in, so multiplied by the tyre
# improvements and the engine improvements. That is then converted to CO2 from L diesel, multiplied by
# the non-electric share, and by the total. 
# The electric share is calculated as energy consumption (listed in kWh/km) * 1000 * grid intensity (in g/wHCO2) 
# multplied by the km travelled. Then it is multiplied by the electric share and the total number of vehicles. 

policy_scenarios <- policy_scenarios %>% 
  mutate(fuel_consumption = diesel_rate_100 * vkt / 100 * tyre_improvement * engine_efficiency * (1 - electric_share) * total,
         co2_ice = total * vkt * diesel_fuel_to_co2(diesel_rate_100 * tyre_improvement * engine_efficiency) * (1 - electric_share),
         co2_ev = total * vkt * electric_share * ev_consumption * 1000 * ei_g_wh,
         co2 = co2_ice + co2_ev)


# Calulating the totals
co2_scenarios <- policy_scenarios %>% 
  filter(pollutant == "ex_nox_l") %>% 
  group_by(fleet_year, fuel_class, vkt_scenario, scenario) %>% 
  summarise(total_co2_mt = sum(co2 / 1000000000000)) 


# Plotting --------------


colour_vals <- c("historical" = grattan_grey4,
                 "baseline" = grattan_darkred,
                 "tyre only" = grattan_red,
                 "Electric targets" = grattan_orange,
                 "engine only" = grattan_darkyellow,
                 "engine and tyre standards" = grattan_lightyellow1,
                 "Electric and engine and tyre standards" = grattan_darkblue)




diser_co2 <- read_xlsx("data-raw/diser-co2-forecasts.xlsx",
                       sheet = "Sheet1") %>% 
  pivot_longer(cols = (2:11),
               values_to = "emissions_mt",
               names_to = "sector") %>% 
  clean_names() %>%   
  filter(sector %in% c("Rigid trucks", "Articulated trucks", "Buses"),
         year <= 2021) %>% 
  mutate(sector = factor(sector, 
                         levels = c("Articulated trucks", 
                                    "Rigid trucks", "Buses")),
         scenario = "historical") %>% 
  rename("fuel_class" = sector,
         "total_co2_mt" = emissions_mt,
         "fleet_year" = year) %>% 
  mutate(vkt_scenario = "vkt_central")


historical_projection_comb <- bind_rows(
  co2_scenarios %>% 
    filter(fleet_year >= 2021),
  diser_co2)



# Plotting -------------------------------------


co2_colours <- c("Electric and engine and tyre standards" = grattan_yellow,
                 "Electric targets" = grattan_red,
                 "engine and tyre standards" = grattan_orange,
                 "baseline" = grattan_darkred,
                 "historical" = grattan_black)



c3_emission_policy_scenarios <- historical_projection_comb %>% 
  group_by(scenario, fleet_year, vkt_scenario) %>% 
  summarise(total_co2_mt = sum(total_co2_mt)) %>% 
  pivot_wider(names_from = vkt_scenario,
              values_from = total_co2_mt) %>% 
  filter(scenario %in% c("historical",
                         "baseline",
                         "Electric targets",
                         "engine and tyre standards",
                         "Electric and engine and tyre standards"),
         fleet_year <= 2040) %>% 
  
  mutate(scenario = factor(scenario, levels = names(co2_colours)),
         line = case_when(scenario == "historical" ~ "a",
                          scenario != "historical" ~ "b")) %>% 
  
  ggplot(aes(x = fleet_year, 
             y = vkt_central,
             colour = scenario,
             linetype = line)) +
  
  geom_ribbon(aes(
    ymin = vkt_lower,
    ymax = vkt_upper,
    fill = scenario),
    alpha= 0.05,
    colour = NA) +
  
  geom_line() +
  
  theme_grattan() +
  scale_y_continuous_grattan(limits = c(0, 32)) +
  scale_x_continuous_grattan(limits = c(2000, 2048),
                             breaks = c(2000, 2020, 2040)) +
  scale_colour_manual(values = co2_colours) +
  scale_fill_manual(values = co2_colours) +
 # labs(title = "A combination of vehicle standards and ZEV mandates can bring down HDV emissions",
#       subtitle = "Annual carbon emissions from HDVs (Mt CO2)",
#       caption = "Historical emissions are from DISER. Forecasts from Grattan analysis. Does not include scope 2 emissions
#       from diesel vehicles.",
#       x = NULL) +
  labs(x = NULL) +
  
  geom_text(aes(x = 2040.5,
                y = 28.2,
                label = "Forecast \nemissions",
                hjust = 0,
                size = 11),
            colour = grattan_darkred,
            fontface = "bold",
            check_overlap = TRUE) +
  
  geom_text(aes(x = 2011,
                y = 23.5,
                label = "Historical",
                hjust = 0,
                size = 11),
            colour = grattan_black,
            fontface = "bold",
            check_overlap = TRUE) +
  
  geom_text(aes(x = 2040.5,
                y = 19,
                label = "ZEV targets",
                hjust = 0,
                size = 11),
            colour = grattan_red,
            fontface = "bold",
            check_overlap = TRUE)  +
  
  geom_text(aes(x = 2040.5,
                y = 22.5,
                label = "Technology \nstandards",
                hjust = 0,
                size = 11),
            colour = grattan_orange,
            fontface = "bold",
            check_overlap = TRUE) +
  
  geom_text(aes(x = 2036,
                y = 14,
                label = "Technology standards \n+ ZEV targets",
                hjust = 0,
                size = 11),
            colour = grattan_yellow,
            fontface = "bold",
            check_overlap = TRUE)
