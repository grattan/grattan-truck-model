
#' Analysing policy scenarios

#' Now we have our policy scenarios, we want to see how they differ in terms of
#' pollutants, health costs, and carbon emissions. 


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


co2_scenarios %>% 
  filter(scenario %in%  c("baseline",
                          "Electric targets",
                          "engine and tyre standards",
                          "engine only",
                          "tyre only",
                          "Electric and engine and tyre standards")) %>% 
  group_by(scenario, fleet_year, vkt_scenario, fuel_class) %>% 
  summarise(total_co2_mt = sum(total_co2_mt)) %>% 
  pivot_wider(names_from = vkt_scenario,
              values_from = total_co2_mt) %>% 
  
  ggplot() +
  
 # geom_ribbon(aes(
#    x = fleet_year,
#    ymin = vkt_lower,
#    ymax = vkt_upper,
#    fill = scenario),
#    colour = NA,
#    alpha = 0.2) +
  
  geom_smooth(aes(x = fleet_year, 
                y = vkt_central, 
                colour = scenario)) +
  
  theme_grattan(legend = "top") +
  scale_y_continuous_grattan(limits = c(0, NA)) +
  scale_x_continuous_grattan() +
  #grattan_colour_manual() +
  #grattan_fill_manual(palette = "diverging") +
  scale_colour_manual(values = colour_vals) +
  scale_fill_manual(values = colour_vals) +
  labs(title = "Total HDV CO2 emissions",
       subtitle = "Annual Co2 (Mt)",
       y = " ",
       x = NULL) +
  facet_wrap(~fuel_class,
             scales = "free_y")


# -------------------------------------

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


co2_colours <- c("Electric and engine and tyre standards" = grattan_yellow,
                 "Electric targets" = grattan_red,
                 "engine and tyre standards" = grattan_orange,
                 "baseline" = grattan_darkred,
                 "historical" = grattan_black)


co2_plot <- historical_projection_comb %>% 
  group_by(scenario, fleet_year, vkt_scenario) %>% 
  summarise(total_co2_mt = sum(total_co2_mt)) %>% 
  pivot_wider(names_from = vkt_scenario,
              values_from = total_co2_mt) %>% 
  filter(scenario %in% c("historical",
                         "baseline",
                         "Electric targets",
                         "engine and tyre standards",
                         "Electric and engine and tyre standards")) %>% 
  
  mutate(scenario = factor(scenario, levels = names(co2_colours))) %>% 
  
  ggplot(aes(x = fleet_year, 
             y = vkt_central,
             colour = scenario)) +
  
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
  labs(title = "A combination of vehicle standards and ZEV mandates can bring down HDV emissions",
       subtitle = "Annual carbon emissions from HDVs (Mt CO2)",
       caption = "Historical emissions are from DISER. Forecasts from Grattan analysis. Does not include scope 2 emissions
       from diesel vehicles.",
       x = NULL) +
  
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

  
co2_plot


#grattan_save(object = co2_plot, 
#              type = "normal",
#              save_pptx = TRUE,
#              filename = "atlas/co2_plot.pdf")


# Pollutant emissions ----------------------------------------

scenario_vals <- c("baseline" = grattan_red,
                   "Electric and Euro 6 (2024)" = grattan_orange,
                   "Euro 6 (2024)" = grattan_darkorange)

#' The current dataset contains the emissions rates for each we are interested in - some
#' are in per km (non-exhaust) and some are in per L (exhaust). Secondary pollutants which
#' are going to be calculated as a conversion rate from primary exhuast pollutants are not
#' yet included
#' 
#' Non-exhuast emissions, including secondary pollutants, tyre wear, brake wear,
#' and re-entrained road dust, are from various other sources. Secondary pollutants
#' are calulcated later, using an offset ratio to estimate the conversion of NOx to 
#' PM2.5 nitrate fraction, and SOx to sulphates. The offset ratios used are 100:1 for NOx to nitrate
#' (PM2.5), and 40:1 for SOx to sulphates (PM2.5). These ratios are from US estimates (west coast),
#' https://www.4cleanair.org/wp-content/uploads/2021/01/01072011-NACAAPM2.5ModelingWorkgroupReport-FINAL_3.pdf 
#' (page 18)
#' 
#' Tyre wear and brake wear figures are US estimates from the MOVES model:
#' https://cfpub.epa.gov/si/si_public_file_download.cfm?p_download_id=525701 (pages 16 and 24)
#' The pm10 fractions are expressed as a pm2.5-10 range, and do not include PM2.5 emissoins. 
#' 
#' Estimates of exhuast emissions are based on per litre of fuel consumed, while 
#' tyre and brake wear etc. are per kilometre travelled
#' 
#' Re-entrained road dust is from.... FILL IN 

#Ratios for NOx to nitrate and SOx to sulphate conversions (PM2.5)
nox_conversion <- 1/100
sox_conversion <- 1/40

#' Calculating the totals for the different fractions 

#Adding rows for the secondary pollutants to the dataset
policy_scenarios <- bind_rows(
  policy_scenarios,
  
  policy_scenarios %>% 
    filter(pollutant %in% c("ex_nox_l", "ex_sox_l")) %>% 
    mutate(pollutant_rate = if_else(
      pollutant == "ex_nox_l", 
      pollutant_rate * nox_conversion,
      pollutant_rate * sox_conversion),
           pollutant = if_else(
             pollutant == "ex_nox_l",
             "secon_nox_pm25",
             "secon_sox_pm25")))



policy_scenarios <- policy_scenarios %>% 
  #we are assuming 92% of the pm10 from diesel exhuast is pm2.5 - the remaining is pm2.5-10)
  #this is from: https://cfpub.epa.gov/si/si_public_file_download.cfm?p_download_id=527145 page 15 
  #all figures are also converted to tonnes
  mutate(pollutant_total = case_when(
    pollutant %in% c("ex_pm10_l", "ex_pm25_l", "ex_sox_l", "ex_voc_l", "ex_nox_l",
                     "secon_nox_pm25", "secon_sox_pm25") ~ pollutant_rate * fuel_consumption / 1000000,
    pollutant %in% c("tyre_pm10_km", "brake_pm10_km", "road_wear_pm10_km", 
                     "tyre_pm25_km", "brake_pm25_km", "road_wear_pm25_km") ~ total * pollutant_rate * vkt / 1000000),
    
    #these names are broad categories designed to be consistent with the BITRE damage cost estimates
    #so we can join them shortly (and are also just helpful categories)
    pollutant_cat = case_when(
      pollutant %in% c("ex_pm25_l", "tyre_pm25_km", "brake_pm25_km", "road_wear_pm25_km", "secon_nox_pm25", "secon_sox_pm25") ~ "pm2_5_combustion",
      pollutant %in% c("tyre_pm10_km", "brake_pm10_km", "road_wear_pm10_km") ~ "pm10_nonexhaust",
      pollutant == "ex_pm10_l" ~ "pm10_secondary",
      pollutant == "ex_nox_l" ~ "nox",
      pollutant == "ex_sox_l" ~ "sox",
        pollutant =="ex_voc_l" ~ "hc_voc"),
    
    pollutant_cat2 = case_when(
      pollutant %in% c("ex_pm25_l", "tyre_pm25_km", "brake_pm25_km", "road_wear_pm25_km", "secon_nox_pm25", "secon_sox_pm25") ~ "pm25",
      pollutant %in% c("tyre_pm10_km", "brake_pm10_km", "road_wear_pm10_km", "ex_pm10_l") ~ "pm10",
      pollutant == "ex_nox_l" ~ "nox",
      pollutant %in% c("ex_sox_l", "ex_voc_l") ~ "other"))

#Checking haven't missed any categories
#policy_scenarios %>% 
#  filter(is.na(pollutant_cat))


pm_nox <- policy_scenarios %>% 
  filter(scenario %in% c("Euro 6 (2024)",
                         "baseline",
                         "Electric and Euro 6 (2024)")) %>% 
  group_by(vkt_scenario, fleet_year, scenario, pollutant_cat) %>% 
  summarise(pollutant_total = sum(pollutant_total)) %>% 
  filter(vkt_scenario == "vkt_central") %>% 
  mutate(scenario = factor(scenario, levels = names(scenario_vals))) %>% 
  
  ggplot(aes(x = fleet_year, 
             y = pollutant_total / 1000,
             colour = scenario)) +
  geom_line() +
  theme_grattan(legend = "top") +
  grattan_colour_manual() +
  scale_y_continuous_grattan(limits = c(0, NA)) +
  scale_x_continuous_grattan(limits = c(2020, 2040),
                             breaks = c(2020, 2030, 2040)) +
  scale_colour_manual(values = scenario_vals) +
  facet_wrap(~pollutant_cat,
             scales = "free_y") +
  
  labs(title = "The introduction of Euro VI will considerably reduce exhaust pollution from HDVs",
       subtitle = "Total exhaust NOx and PM10 emissions from the HDV fleet (thousands of tons)",
       x = NULL)

pm_nox

 # grattan_save(object = pm_nox, 
#               type = "fullslide",
#               save_pptx = TRUE,
#               filename = "atlas/pm10_nox_emissions.pdf")

# Health cost estimates ---------------------------------------------------

#' Health cost data (damage costs) were provided by BITRE, and are based on estimates
#' derived from Jacbon Marsden Assc consulting work into fuel quality and emissions over
#' the 2016-2018 period. 
#' 

# All damage costs are in $ / tonne pollutant in 2016 Aud

damage_costs <- read_xlsx("data-raw/BITRE-damage-costs.xlsx",
                          sheet = "r-input") %>% 
  clean_names() %>% 
  pivot_longer(cols = (2:15),
               values_to = "cost_2016_au",
               names_to = "pollutant") %>% 
  mutate(region = case_when(
    str_detect(pollutant, "_metro") == TRUE ~ "metro",
    str_detect(pollutant, "nonmet") == TRUE ~ "non_metro"),
    pollutant = case_when(
      str_detect(pollutant, "_metro") == TRUE ~ substr(pollutant, start = 1, stop = nchar(pollutant) - 6),
      str_detect(pollutant, "nonmet") == TRUE ~ substr(pollutant, start = 1, stop = nchar(pollutant) - 7))) %>% 
  #and just reordering and inflating to 2020 values
  select(pollutant, region, year, cost_2016_au) %>% 
  mutate(cost_2016_au = cost_2016_au * 1.0646) %>% 
  rename("damage_cost_t" = cost_2016_au,
         "fleet_year" = year,
         "pollutant_cat" = pollutant)

write_rds(damage_costs, "data/damage-costs.rds")

#Joining the health cost data

policy_outcomes <- left_join(policy_scenarios,
                              
                             damage_costs) %>% 
  mutate(health_cost_total = pollutant_total * damage_cost_t)


write_rds(policy_outcomes, "data/policy_outcomes.rds")







health_costs <- policy_outcomes %>%
  group_by(vkt_scenario, fleet_year, fuel_class, age, region, scenario, pollutant_cat2) %>% 
  #assuming 95% of pm10 is pm2.5 for the moment
  #calculating costs - the / 1000000 is to convert from g to tonnes
  summarise(health_cost_total = sum(pollutant_total * damage_cost_t))

write_rds(health_costs, "data/health_costs.rds")

# Previous studies (ie.e https://www.epa.nsw.gov.au/~/media/EPA/Corporate%20Site/resources/air/HealthPartEmiss.ashx)
# have excluded secondary costs because of this uncertainty - that is probably the approach we should take here 
# because we have no real way of establishing this (and it' sporbbaly unlikely to significantly affect results.)
# This makes our estimates more conservative. 



costs_bar <- health_costs %>% 
  filter(scenario %in% c("Euro 6 (2024)",
                         "baseline",
                         "Electric and Euro 6 (2024)")) %>% 
  group_by(scenario, vkt_scenario, fleet_year) %>% 
  summarise(total_cost = sum(health_cost_total)) %>% 
  pivot_wider(values_from = total_cost,
              names_from = vkt_scenario) %>% 
  mutate(scenario = factor(scenario, levels = names(scenario_vals))) %>% 
  
  ggplot(aes(x = fleet_year)) +
  #geom_ribbon(aes(
  #  ymin = vkt_lower / 1000000,
  #  ymax = vkt_upper / 1000000,
  #  fill = scenario),
  #  alpha= 0.2,
  #  colour = NA) +
  geom_col(aes(y = vkt_central / 1000000,
                colour = scenario,
               fill = scenario),
                alpha = 0.8,
           position = "dodge") +
  scale_colour_manual(values = scenario_vals) + 
  scale_fill_manual(values = scenario_vals) +
  scale_y_continuous_grattan(labels = scales::label_dollar(),
                             limits = c(0, 4500)) +
  theme_grattan() +
  labs(title = "Without intervention, health costs from heavy vehicles will rise",
       subtitle = "Estimated annual health cost, ($ millions, undiscounted)",
       x = NULL)



costs_dots <- health_costs %>% 
  filter(scenario %in% c("Euro 6 (2024)",
                         "baseline")) %>% 
  #"Electric and Euro 6 (2024)")) %>% 
  group_by(scenario, vkt_scenario, fleet_year) %>% 
  summarise(total_cost = sum(health_cost_total)) %>% 
  pivot_wider(values_from = total_cost,
              names_from = vkt_scenario) %>% 
  mutate(scenario = factor(scenario, levels = names(scenario_vals))) %>% 
  
  ggplot(aes(x = fleet_year)) +
  geom_ribbon(aes(
    ymin = vkt_lower / 1000000,
    ymax = vkt_upper / 1000000,
    fill = scenario),
    alpha= 0.2,
    colour = NA) +
  
  geom_line(aes(y = vkt_central / 1000000,
                colour = scenario),
            alpha = 0.75,
            size = 0.5) +
  
  geom_point(aes(y = vkt_central / 1000000,
               colour = scenario,
               fill = scenario,
               size = vkt_central),
           alpha = 1) +

  scale_colour_manual(values = scenario_vals) + 
  scale_size_area(max_size = 4) +
  scale_fill_manual(values = scenario_vals) +
  scale_y_continuous_grattan(labels = scales::label_dollar(),
                             limits = c(0, 4000)) +
  theme_grattan() +
  labs(title = "Without intervention, health costs from heavy vehicles will rise",
       subtitle = "Estimated annual health cost, ($ millions, undiscounted)",
       x = NULL)

costs_dots
costs_bar


#grattan_save(object = costs,
#             type = "fullslide",
#             save_pptx = TRUE,
#             filename = "atlas/health_costs.pdf")





