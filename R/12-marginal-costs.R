

#Additional charts

policy_scenarios <- read_rds("data/policy-scenarios.rds")
health_costs <- read_rds("data/health_costs.rds")

vech_colours <- c("Articulated trucks" = grattan_red,
                  "Rigid trucks" = grattan_orange,
                  "Buses" = grattan_yellow)

vech_age_breakdown <- health_costs %>% 
  filter(fleet_year == 2021) %>% 
  pivot_longer(cols = 7:11,
               values_to = "cost",
               names_to = "pollutant") %>% 
  filter(scenario == "baseline") %>% 
  group_by(age, scenario, vkt_scenario, fleet_year, fuel_class) %>% 
  summarise(cost = sum(cost)) %>% 
  pivot_wider(values_from = cost,
              names_from = vkt_scenario) %>% 
  filter(fuel_class %in% c("Articulated trucks", "Rigid trucks", "Buses")) %>% 
  mutate(fuel_class = factor(fuel_class, levels = names(vech_colours))) %>% 
  
  ggplot(aes(x = age)) +
  geom_ribbon(aes(
    ymin = vkt_lower / 1000000,
    ymax = vkt_upper / 1000000,
    fill = scenario),
    alpha= 0.2,
    colour = NA) +
  geom_smooth(aes(y = vkt_central / 1000000,
                  colour = fuel_class),
              span = 0.5,
              se = TRUE,
              alpha = 0.1) +
  scale_colour_manual(values = (vech_colours)) + 
  grattan_fill_manual() +
  scale_y_continuous_grattan(labels = scales::label_dollar(),
                             limits = c(0, 40)) +
  theme_grattan() +
  labs(title = "Older rigid trucks are responsible for significant health costs",
       subtitle = "Estimated annual health cost by vehicle age, ($ millions, undiscounted)",
       x = "Vehicle age") +
  facet_wrap(~fuel_class)


#grattan_save(object = vech_age_breakdown, 
#             type = "fullslide",
#             save_pptx = TRUE,
#             filename = "atlas/vech_age_breakdown.pdf")



 marg_cost <- health_costs %>% 
   filter(fleet_year == 2021) %>% 
   pivot_longer(cols = 7:11,
                values_to = "cost",
                names_to = "pollutant") %>% 
   filter(scenario == "baseline") %>% 
   group_by(age, scenario, vkt_scenario, fleet_year, fuel_class, total) %>% 
   summarise(cost = sum(cost)) %>% 
   mutate(marginal_cost = cost / total) %>% 
   select(-cost) %>% 
   pivot_wider(values_from = marginal_cost,
               names_from = vkt_scenario) %>% 
   filter(fuel_class %in% c("Articulated trucks", "Rigid trucks", "Buses")) %>% 
   mutate(fuel_class = factor(fuel_class, levels = names(vech_colours)))
 
 
 marg_cost %>%   
   ggplot(aes(x = age)) +
   
   #geom_ribbon(aes(
  #   ymin = vkt_lower,
  #   ymax = vkt_upper,
  #   fill = scenario),
  #   alpha= 0.2,
  #   colour = NA) +
   
   geom_smooth(aes(y = vkt_central,
                   colour = fuel_class),
               span = 0.3,
               se = TRUE,
               alpha = 0.1) +
   scale_colour_manual(values = (vech_colours)) + 
   grattan_fill_manual() +
   scale_y_continuous_grattan(labels = scales::label_dollar(),
                              limits = c(0, 11000),
                              breaks = c(0, 5000, 10000)) +
   theme_grattan() +
   labs(title = "Older rigid trucks are responsible for significant health costs",
        subtitle = "Estimated annual health cost by vehicle age, ($ millions, undiscounted)",
        x = "Vehicle age") +
   facet_wrap(~fuel_class)
 
 
 
 
#---------------------------------------------------------------
 
 
policy_outcomes <- read_rds("data/policy_outcomes.rds")

marginal_cost <- policy_outcomes %>% 
  ungroup() %>% 
  filter(scenario == "baseline") %>% 
  select(-type, -electric_share, -ev_consumption, -ei_g_wh, -fuel_type, 
         -scenario, -tyre_improvement, -engine_efficiency, -co2_ice,
         -co2_ev, -co2)
  
non_substituted <- marginal_cost %>%
  filter(fleet_year > 2020) %>% 
  group_by(vkt_scenario, fleet_year, fuel_class, age, region, sales_year) %>% 
  #assuming 95% of pm10 is pm2.5 for the moment
  #calculating costs - the / 1000000 is to convert from g to tonnes
  summarise(ex_pm10 = ex_pm10 * pm10_secondary_cost,
            ex_pm25 = ex_pm25 * pm2_5_combustion_cost,
            ex_sox = ex_sox * sox_cost,
            ex_voc = ex_voc * hc_voc_cost, 
            ex_nox = ex_nox * nox_cost, 
            non_ex_pm10 = non_ex_pm10 * pm10_nonexhaust_cost, 
            non_ex_pm25 = non_ex_pm25 * pm2_5_combustion_cost, 
            secon_pm25 = secon_pm25 * pm2_5_combustion_cost,
            total = sum(total)) %>% 
  pivot_longer(cols = (7:14),
               names_to = "pollutant",
               values_to = "total_cost") %>% 
  mutate(pollutant_class = case_when(
    pollutant %in% c("ex_pm10", "non_ex_pm10") ~ "pm10",
    pollutant %in% c("ex_pm25", "non_ex_pm25", "secon_pm25") ~ "pm25",
    pollutant == "ex_sox" ~ "sox",
    pollutant == "ex_nox" ~ "nox",
    pollutant == "ex_voc" ~ "voc")) %>% 
  
  #filter(pollutant_class == "pm25") %>% 
  
  group_by(sales_year, vkt_scenario, total, fuel_class, fleet_year) %>% 
  summarise(total_cost = sum(total_cost)) %>% 
  
  mutate(marginal_cost = total_cost / total)


# Adding a discount rate of 7%

discount <- function(data, rate) {
  
  i <- 1
  while (i <= nrow(data)) {
    
    data$total_cost[i] =  (data$total_cost[i])  / ((1 + rate)^(data$fleet_year[i] - 2021))
    
    i <- i + 1
  }
  return(data)
}


non_substitued_discounted <- discount(non_substituted, rate = 0.07)




non_substitued_discounted %>% 
  filter(fuel_class %in% c("Articulated trucks", "Rigid trucks")) %>% 
  group_by(sales_year, vkt_scenario, fuel_class) %>% 
  summarise(total_cost = sum(total_cost),
            marginal_cost = sum(marginal_cost),
            #selecting the max number of vehicles - i.e. what is around at the 
            #time of the first fleet year - i.e. 2020
            total = max(total)) %>% 

  filter(vkt_scenario == "vkt_central") %>% 
  ggplot(aes(x = sales_year,
             y = marginal_cost,
             fill = fuel_class)) +
  
  geom_col(alpha = 0.9) +
  
  theme_grattan() +
  grattan_fill_manual(2) +
  scale_y_continuous_grattan(labels = scales::label_dollar()) +
  scale_x_continuous_grattan(limits = c(1980, 2020),
                             breaks = c(1980, 2000, 2020)) +
  grattan_colour_manual(2) +
  labs(title = "The health damage from trucks is huge",
       subtitle = "Estimated health cost of vehicles over their remaining lifetime, by sales date (non-substituted)",
       caption = "A discount rate of 7% is applied. Does not include health damage from re-entrained road dust.",
       x = "Year of vehicle sale") +
  
  facet_wrap(~fuel_class)



#grattan_save(filename = "atlas/non-subbed-cost",
#             type = "fullslide",
#             save_ppt = TRUE)


# Calculating the marginal cost with substitutability -------------------------

#' This is predicated on an assumption that if a vehice is taken off the road:
  #' If the vehicle has technology prior to euro 3, it is replaced with a euro 3 vehicle
  #' If it has technology at or above euro, this is replaced with the same (so no change)
#'  This means that the cost for euro 3 and onwards vehicles will be 0 

policy_scenarios <- read_rds("data/policy-scenarios.rds")

substituted <- policy_scenarios %>% 
  mutate(fuel_consumption = diesel_rate_100 * vkt / 100 * tyre_improvement * engine_efficiency * (1 - electric_share) * total,
         co2_ice = total * vkt * diesel_fuel_to_co2(diesel_rate_100 * tyre_improvement * engine_efficiency) * (1 - electric_share),
         co2_ev = total * vkt * electric_share * ev_consumption * 1000 * ei_g_wh,
         co2 = co2_ice + co2_ev)

#Ratios for NOx to nitrate and SOx to sulphate conversions (PM2.5)
nox_conversion <- 1/100
sox_conversion <- 1/40

#Now we want to substitute the vehicles that have pre-euro 3 technology with the pollutant
# figures of a euro 3 truck

#first we want to get the values for a euro 3 truck

euro_3 <- substituted %>% 
  ungroup() %>% 
  filter(sales_year == 2003) %>% 
  select(fuel_class, ex_nox_l, ex_sox_l, ex_pm10_l, ex_voc_l) %>% 
  unique()

#and now we are going to replace pre-euro 3 truck pollutant values with these figures,
#and integrate that back into the overall dataset

euro_3 <- left_join(
  substituted %>% 
    filter(sales_year < 2003) %>% 
    select(-ex_nox_l, -ex_sox_l, -ex_pm10_l, -ex_voc_l),
  
  euro_3)

substituted <- bind_rows(
  euro_3,
  
  substituted %>% 
    filter(sales_year >= 2003))

#' Calculating the cost totals for the different fractions with our replaced data

substituted <- substituted %>% 
  #we are assuming 92% of the pm10 from diesel exhuast is pm2.5 - the remaining is pm2.5-10)
  #this is from: https://cfpub.epa.gov/si/si_public_file_download.cfm?p_download_id=527145 page 15 
  #all figures are also converted to tonnes
  mutate(ex_pm10 = 0.08 * ex_pm10_l * fuel_consumption / 1000000,
         ex_pm25 = 0.92 * ex_pm10_l * fuel_consumption / 1000000,
         ex_sox = ex_sox_l * fuel_consumption / 1000000,
         ex_voc = ex_voc_l * fuel_consumption / 1000000,
         ex_nox = ex_nox_l * fuel_consumption / 1000000,
         non_ex_pm10 = vkt * (tyre_pm10_km + brake_pm10_km + road_wear_pm10_km) * total / 1000000,
         non_ex_pm25 = vkt * (tyre_pm25_km + brake_pm25_km + road_wear_pm25_km) / 1000000,
         secon_pm25 =  ((ex_nox_l * nox_conversion * fuel_consumption) + (ex_sox_l * sox_conversion * fuel_consumption))  / 1000000) %>% 
  select(-tyre_pm25_km, -tyre_pm10_km, -brake_pm25_km, -brake_pm10_km, -ex_pm10_l, -road_wear_pm10_km, 
         -road_wear_pm25_km, -ex_nox_l, -ex_sox_l, -ex_voc_l)



damage_costs <- read_rds("data/damage-costs.rds")

substituted <- left_join(substituted,
                             
                         damage_costs %>%
                           mutate(pollutant = str_c(pollutant, "_cost")) %>% 
                           pivot_wider(names_from = pollutant,
                                       values_from = damage_cost_t))


marginal_cost_sub <- substituted %>% 
  ungroup() %>% 
  filter(scenario == "baseline") %>% 
  select(-type, -electric_share, -ev_consumption, -ei_g_wh, -fuel_type, 
         -scenario, -tyre_improvement, -engine_efficiency, -co2_ice,
         -co2_ev, -co2)



 #--------
marginal_cost_sub <- marginal_cost_sub %>%
  filter(fleet_year > 2020) %>% 
  group_by(vkt_scenario, fleet_year, fuel_class, age, region, sales_year) %>% 
  #assuming 95% of pm10 is pm2.5 for the moment
  #calculating costs - the / 1000000 is to convert from g to tonnes
  summarise(ex_pm10 = ex_pm10 * pm10_secondary_cost,
            ex_pm25 = ex_pm25 * pm2_5_combustion_cost,
            ex_sox = ex_sox * sox_cost,
            ex_voc = ex_voc * hc_voc_cost, 
            ex_nox = ex_nox * nox_cost, 
            non_ex_pm10 = non_ex_pm10 * pm10_nonexhaust_cost, 
            non_ex_pm25 = non_ex_pm25 * pm2_5_combustion_cost, 
            secon_pm25 = secon_pm25 * pm2_5_combustion_cost,
            total = sum(total)) %>% 
  pivot_longer(cols = (7:14),
               names_to = "pollutant",
               values_to = "total_cost") %>% 
  mutate(pollutant_class = case_when(
    pollutant %in% c("ex_pm10", "non_ex_pm10") ~ "pm10",
    pollutant %in% c("ex_pm25", "non_ex_pm25", "secon_pm25") ~ "pm25",
    pollutant == "ex_sox" ~ "sox",
    pollutant == "ex_nox" ~ "nox",
    pollutant == "ex_voc" ~ "voc")) %>% 
  
  #filter(pollutant_class == "pm25") %>% 
  
  group_by(sales_year, vkt_scenario, total, fuel_class, fleet_year) %>% 
  summarise(total_cost = sum(total_cost)) %>% 
  
  mutate(marginal_cost = total_cost / total)


# Discounting

marginal_cost_sub_disc <- discount(marginal_cost_sub, rate = 0.07)


# Joining substituted and non-substiuted costs to work out the marginal substituted cost


substituted_final <- left_join(non_substitued_discounted,
          
          marginal_cost_sub_disc %>% 
            rename("total_sub_cost" = total_cost,
                   "marginal_sub_cost" = marginal_cost)) %>% 
  
  #and calculating that marginal cost after substitution
  mutate(total_cost = total_cost - total_sub_cost,
         marginal_cost = marginal_cost - marginal_sub_cost) %>% 
  select(-total_sub_cost, -marginal_sub_cost)



substituted_final %>% 
  filter(fuel_class %in% c("Articulated trucks", "Rigid trucks"),
         sales_year <= 2021) %>% 
  group_by(sales_year, vkt_scenario, fuel_class) %>% 
  summarise(total_cost = sum(total_cost),
            marginal_cost = sum(marginal_cost),
            #selecting the max number of vehicles - i.e. what is around at the 
            #time of the first fleet year - i.e. 2020
            total = max(total)) %>% 
  
  filter(vkt_scenario == "vkt_central") %>% 
  ggplot(aes(x = sales_year,
             y = marginal_cost,
             fill = fuel_class)) +
  
  geom_col(alpha = 0.9) +
  
  theme_grattan() +
  grattan_fill_manual(2) +
  scale_y_continuous_grattan(labels = scales::label_dollar()) +
  scale_x_continuous_grattan(limits = c(1980, 2003),
                             breaks = c(1980, 1990, 2000)) +
  grattan_colour_manual(2) +
  labs(title = "Cash for clunkers could significantly reduce health costs from trucks",
       subtitle = "Estimated avoided health cost per vehicle if bought in a cash for clunkers scheme (substituted)",
       caption = "A discount rate of 7% is applied. Does not include health damage from re-entrained road dust. Assumes that
       any vehicle sold prior to 2003 (pre-Euro III standards) is replaced with a Euro III truck. ",
       x = "Year of vehicle sale") +
  
  facet_wrap(~fuel_class,
             scales = "free_y")




#grattan_save(filename = "atlas/subbed-cost",
#             type = "fullslide",
#             save_ppt = TRUE)


















non_substitued_discounted %>% 
  filter(fuel_class %in% c("Articulated trucks", "Rigid trucks")) %>% 
  group_by(sales_year, vkt_scenario, fuel_class) %>% 
  summarise(total_cost = sum(total_cost),
            marginal_cost = sum(marginal_cost),
            #selecting the max number of vehicles - i.e. what is around at the 
            #time of the first fleet year - i.e. 2020
            total = max(total))
  








