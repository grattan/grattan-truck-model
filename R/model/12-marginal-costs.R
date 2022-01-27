
# This script looks at policy scenarios that are aimed to remove old vehicles from the fleet
# For these policies, it's of interest to know what the total health costs from old vehicles are,
# and what the marginal health costs of these vehicles are if they were substituted with a younger 
# vehicle

# Loading data --------------------------------------

policy_outcomes <- read_rds("data/policy_outcomes.rds")
health_costs <- read_rds("data/health_costs.rds")


# Discount rate function 
discount <- function(data, rate) {
  
  i <- 1
  while (i <= nrow(data)) {
    
    data$marginal_cost[i] =  (data$marginal_cost[i])  / ((1 + rate)^(data$fleet_year[i] - 2022))
    
    i <- i + 1
  }
  return(data)
}


# Breakdown of total costs by vehicle age --------------------------------


vech_colours <- c("Articulated trucks" = grattan_red,
                  "Rigid trucks" = grattan_orange,
                  "Buses" = grattan_yellow)



vech_age_breakdown <- policy_outcomes %>% 
  filter(fleet_year == 2022,
         scenario == "baseline") %>% 
  group_by(age, scenario, vkt_scenario, fleet_year, fuel_class) %>% 
  summarise(health_cost_total = sum(health_cost_total)) %>% 
  pivot_wider(values_from = health_cost_total,
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
  #scale_y_continuous_grattan(labels = scales::label_dollar(),
  #                           limits = c(0, 40)) +
  theme_grattan() +
  labs(title = "Older rigid trucks are responsible for significant health costs",
       subtitle = "Estimated annual health cost by vehicle age, ($ millions, undiscounted)",
       x = "Vehicle age") +
  facet_wrap(~fuel_class)


#grattan_save(object = vech_age_breakdown, 
#             type = "fullslide",
#             save_pptx = TRUE,
#             filename = "atlas/vech_age_breakdown.pdf")






# Calculating total costs per vehicle, by age -----------------------------------


# Now working out the social cost per vehicle for a specific year (2022), and how that is 
# distributed by vehicle age

 cost_per_vehicle <- policy_outcomes %>% 
   filter(fleet_year == 2022,
          scenario == "baseline") %>% 
   mutate(marginal_cost = health_cost_total / total) %>% 
   
   group_by(age, scenario, vkt_scenario, fleet_year, fuel_class) %>% 
   summarise(marginal_cost = sum(marginal_cost)) %>% 
   filter(fuel_class %in% c("Articulated trucks", "Rigid trucks", "Buses")) %>% 
   mutate(fuel_class = factor(fuel_class, levels = names(vech_colours)))
 
 
cost_per_vehicle %>%   
   ggplot(aes(x = age)) +
   
   #geom_ribbon(aes(
  #   ymin = vkt_lower,
  #   ymax = vkt_upper,
  #   fill = scenario),
  #   alpha= 0.2,
  #   colour = NA) +
   
   geom_smooth(aes(y = marginal_cost,
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


 
 
 
 
# Estimating marginal costs of vehicles if taken off the road ------------------
# This is calculated first without replacement - i.e., what is the social cost of a vehicle 
 # over it's remaining life?
 

rem_life_cost <- policy_outcomes %>% 
  
  ungroup() %>% 
  filter(scenario == "baseline") %>% 
  mutate(marginal_cost = health_cost_total / total) %>% 
  
  group_by(sales_year, fleet_year, scenario, vkt_scenario, fuel_class) %>% 
  summarise(marginal_cost = sum(marginal_cost)) %>% 
  filter(fuel_class %in% c("Articulated trucks", "Rigid trucks", "Buses")) %>% 
  mutate(fuel_class = factor(fuel_class, levels = names(vech_colours)))


# Adding a discount rate of 7%


non_substitued_discounted <- discount(rem_life_cost, rate = 0.07)


non_substitued_discounted %>% 
  filter(fuel_class %in% c("Articulated trucks", "Rigid trucks")) %>% 
  group_by(sales_year, vkt_scenario, fuel_class) %>% 
  summarise(marginal_cost = sum(marginal_cost)) %>% 

  filter(vkt_scenario == "vkt_central") %>% 
  ggplot(aes(x = sales_year,
             y = marginal_cost,
             fill = fuel_class)) +
  
  geom_col(alpha = 0.9) +
  
  theme_grattan() +
  grattan_fill_manual(2) +
  scale_y_continuous_grattan(labels = scales::label_dollar(),
                             limits = c(0, 170000)) +
  scale_x_continuous_grattan(limits = c(1980, 2022),
                             breaks = c(1980, 2000, 2020)) +
  grattan_colour_manual(2) +
  labs(title = "The health damage from trucks is huge",
       subtitle = "Estimated health cost of vehicles over their remaining lifetime, by sales date (non-substituted)",
       caption = "A discount rate of 7% is applied. Does not include health damage from re-entrained road dust.",
       x = "Year of vehicle sale") +
  
  facet_wrap(~fuel_class)



# Calculating the marginal cost with substitutability -------------------------

#' This is predicated on an assumption that if a vehice is taken off the road:
  #' If the vehicle has technology prior to euro 3, it is replaced with a euro 3 vehicle
  #' If it has technology at or above euro, this is replaced with the same (so no change)
#'  This means that the cost for euro 3 and onwards vehicles will be 0 

#Now we want to substitute the vehicles that have pre-euro 3 technology with the pollutant
# figures of a euro 3 truck

#first we want to get the values for a euro 3 truck


euro_3 <- policy_outcomes %>% 
  ungroup() %>% 
  filter(sales_year == 2003) %>% 
  distinct(fuel_class, pollutant, pollutant_rate) %>% 
  rename("pollutant_rate2" = pollutant_rate) 


#and now we are going to replace pre-euro 3 truck pollutant values with these figures,
#and integrate that back into the overall dataset

substituted <- left_join(
  policy_outcomes,
  euro_3) %>% 
  mutate(pollutant_rate = if_else(
    pollutant_rate >= pollutant_rate2,
    pollutant_rate2, 
    pollutant_rate2)) %>% 
  select(-pollutant_rate2)


#' Calculating the cost totals for the different fractions with our replaced data

substituted <- substituted %>% 
  mutate(pollutant_total = case_when(
    pollutant %in% c("ex_pm10_l", "ex_pm25_l", "ex_sox_l", "ex_voc_l", "ex_nox_l",
                     "secon_nox_pm25", "secon_sox_pm25") ~ pollutant_rate * fuel_consumption / 1000000,
    pollutant %in% c("tyre_pm10_km", "brake_pm10_km", "road_wear_pm10_km", 
                     "tyre_pm25_km", "brake_pm25_km", "road_wear_pm25_km") ~ total * pollutant_rate * vkt / 1000000)) %>% 
  
  mutate(health_cost_euro_3 = pollutant_total * damage_cost_t) %>% 
  select(-health_cost_total)



# Calculating the remaining health cost after we 'replace' the vehicle with a 
# Euro 3 equivalent

substituted <- left_join(policy_outcomes %>% 
                           select(scenario, vkt_scenario, fuel_class, fleet_year, 
                                  sales_year, total, region, pollutant,health_cost_total),
                        substituted) %>% 
  mutate(avoided_health_cost = health_cost_total - health_cost_euro_3)


# Now calculating the marginal social benefit per vehicle after replacement

substituted <- substituted %>% 
  ungroup() %>% 
  filter(scenario == "baseline") %>% 
  mutate(marginal_cost = avoided_health_cost / total) %>% 
  group_by(sales_year, fleet_year, scenario, vkt_scenario, fuel_class) %>% 
  summarise(marginal_cost = sum(marginal_cost)) %>% 
  filter(fuel_class %in% c("Articulated trucks", "Rigid trucks", "Buses")) %>% 
  mutate(fuel_class = factor(fuel_class, levels = names(vech_colours)))


# Adding a discount rate of 7%

substituted_discounted <- discount(substituted, rate = 0.07)




# Plotting the result -------------

substituted_discounted %>% 
  filter(vkt_scenario == "vkt_central",
         fuel_class %in% c("Rigid trucks", "Articulated trucks")) %>% 
  group_by(sales_year, scenario, vkt_scenario, fuel_class) %>% 
  summarise(marginal_cost = sum(marginal_cost)) %>% 
  
  ggplot(aes(x = sales_year,
             y = marginal_cost,
             fill = fuel_class)) +
  
  geom_col(alpha = 0.9) +
  
  theme_grattan() +
  grattan_fill_manual(2) +
  grattan_colour_manual(2) +
  scale_y_continuous_grattan(labels = scales::label_dollar())+
  scale_x_continuous_grattan(limits = c(1980, 2003),
                             breaks = c(1980, 2000, 2020)) +

  labs(title = "Cash for clunkers could significantly reduce health costs from trucks",
       subtitle = "Estimated avoided health cost per vehicle if bought in a cash for clunkers scheme (substituted)",
       caption = "A discount rate of 7% is applied. Does not include health damage from re-entrained road dust. Assumes that
       any vehicle sold prior to 2003 (pre-Euro III standards) is replaced with a Euro III truck. ",
       x = "Year of vehicle sale") +
  
  facet_wrap(~fuel_class, scales = "free_y")











