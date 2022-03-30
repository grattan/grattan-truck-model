
# This script looks at policy scenarios that are aimed to remove old vehicles from the fleet
# For these policies, it's of interest to know what the total health costs from old vehicles are,
# and what the marginal health costs of these vehicles are if they were substituted with a younger 
# vehicle

# Loading data --------------------------------------
source("R/00-setup.R")
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



# Chart for report -----------------------------------------------------
vech_colours <- c("Articulated trucks" = grattan_red,
                  "Rigid trucks" = grattan_orange,
                  "Buses" = grattan_yellow)

# Estimating marginal costs of vehicles if taken off the road ------------------
# This is calculated first without replacement - i.e., what is the social cost of a vehicle 
# over it's remaining life?

#This is the chart that is in the the report 

rem_life_cost <- policy_outcomes %>% 
  ungroup() %>% 
  filter(scenario == "baseline") %>% 
  mutate(marginal_cost = health_cost_total / total) %>% 
  
  group_by(sales_year, fleet_year, scenario, vkt_scenario, fuel_class) %>% 
  summarise(marginal_cost = sum(marginal_cost)) %>% 
  filter(fuel_class %in% c("Articulated trucks", "Rigid trucks", "Buses")) %>% 
  mutate(fuel_class = factor(fuel_class, levels = names(vech_colours))) %>% 
  #need to filter to 2022 onwards 
  filter(fleet_year >= 2022)


# Adding a discount rate of 7%
non_substitued_discounted <- discount(rem_life_cost, rate = 0.07)

non_substitued_discounted_p <- non_substitued_discounted %>% 
  filter(fuel_class %in% c("Articulated trucks", "Rigid trucks")) %>% 
  group_by(sales_year, vkt_scenario, fuel_class) %>% 
  summarise(marginal_cost = sum(marginal_cost)) 


# Plotting -------

c2_health_costs_per_truck <- non_substitued_discounted_p %>% 
  filter(vkt_scenario == "vkt_central") %>% 
  ggplot(aes(x = sales_year,
             y = marginal_cost / 1000,
             fill = fuel_class)) +
  
  geom_col(alpha = 0.9) +
  
  theme_grattan() +
  theme(strip.text.y = element_blank()) +
  grattan_fill_manual(2, rev = TRUE) +
  scale_y_continuous_grattan(labels = scales::label_dollar(suffix = "K"),
                             limits = c(0, 190),
                             breaks = c(0, 50, 100, 150)) +
  scale_x_continuous_grattan(limits = c(1980, 2022),
                             breaks = c(1980, 2000, 2020)) +
  
  grattan_label(data = non_substitued_discounted_p %>% 
                  filter(sales_year == 1980),
                
                aes(label = fuel_class,
                    colour = fuel_class,
                    y = 170),
                fontface = "bold",
                hjust = "left") +
  
  grattan_colour_manual(2, rev = TRUE) +
 # labs(title = "The health damage from trucks is huge",
#       subtitle = "Estimated health cost of vehicles over their remaining lifetime, by sales date (non-substituted)",
#       caption = "A discount rate of 7% is applied. Does not include health damage from re-entrained road dust.",
 #      x = "Year of manufacture") +
  labs(x = "Year of manufacture") +
  facet_grid(rows = vars(fuel_class))

#grattan_save(filename = "atlas/non-subbed-cost.pdf",
#             type = "wholecolumn",
#             save_ppt = TRUE)







#' Splitting by 'rural only' and 'Urban only' vehicles -----------------------
#' This is slightly fiddly but we do this by scaling up the costs by adding in the rural/urban percentages

#' Scaling the Urban km's/costs
rural_urban_only <- policy_outcomes %>% 
  filter(scenario == "baseline",
         vkt_scenario == "vkt_central",
         fleet_year %in% (2020:2040)) %>% 
  group_by(scenario, vkt_scenario, region, fuel_class, fleet_year, sales_year, total, age, vkt) %>% 
  summarise(health_cost_total = sum(health_cost_total)) %>% 
  arrange(fleet_year, sales_year, age, fuel_class) %>% 
  group_by(fleet_year, sales_year, fuel_class, total) %>% 
  #' Adding a scaling factor (what to multiply the km by if all the km were in the 
  #' given region (urban/rural))
  summarise(vkt_scale = sum(vkt) / vkt,
            #' Calculating the health cost for an individual vehicle
            health_cost_total = health_cost_total / total,
            vkt = vkt,
            region = region) %>% 
  select(-total) %>% 
  mutate(scaled_costs = health_cost_total * vkt_scale) %>% 
  ungroup() %>% 
  # discounting from 2022
  filter(fleet_year >= 2022) %>% 
  mutate(scaled_costs_disc = (1 / (1 + 0.07)^(fleet_year - 2022)) * scaled_costs) %>% 
  ungroup() %>% 
  count(sales_year, fuel_class, region, wt = scaled_costs_disc) 
  
  
# Plotting results
rural_urban_only %>% 
  filter(fuel_class %in% c("Articulated trucks", "Rigid trucks")) %>% 
  ggplot(aes(x = sales_year,
             y = n / 1000,
             fill = fuel_class)) +
  
  geom_col(alpha = 0.9) +
  
  theme_grattan() +
  theme(strip.text.y = element_blank()) +
  grattan_fill_manual(2, rev = TRUE) +
  scale_y_continuous_grattan(labels = scales::label_dollar(suffix = "K"),
                             limits = c(0, 300),
                             breaks = c(0, 100, 200)) +
  scale_x_continuous_grattan(limits = c(1980, 2022),
                             breaks = c(1980, 2000, 2020)) +
  
  grattan_label(data = . %>% 
                  filter(sales_year == 1980, region == "metro"),
                aes(label = fuel_class,
                    colour = fuel_class,
                    y = 280),
                fontface = "bold",
                hjust = "left") +
  
  grattan_colour_manual(2, rev = TRUE) +
  # labs(title = "The health damage from trucks is huge",
  #       subtitle = "Estimated health cost of vehicles over their remaining lifetime, by sales date (non-substituted)",
  #       caption = "A discount rate of 7% is applied. Does not include health damage from re-entrained road dust.",
  #      x = "Year of manufacture") +
  labs(x = "Year of manufacture") +
  facet_grid(rows = vars(fuel_class),
             cols = vars(region))

#  mutate(marginal_cost = health_cost_total / total) %>% 
  
#  group_by(sales_year, fleet_year, scenario, vkt_scenario, fuel_class) %>% 
#  summarise(marginal_cost = sum(marginal_cost)) %>% 
 # filter(fuel_class %in% c("Articulated trucks", "Rigid trucks", "Buses")) %>% 
 # mutate(fuel_class = factor(fuel_class, levels = names(vech_colours)))


# Adding a discount rate of 7%
#non_substitued_discounted <- discount(rem_life_cost, rate = 0.07)

#non_substitued_discounted_p <- non_substitued_discounted %>% 
#  filter(fuel_class %in% c("Articulated trucks", "Rigid trucks")) %>% 
#  group_by(sales_year, vkt_scenario, fuel_class) %>% 
#  summarise(marginal_cost = sum(marginal_cost)) 
































# Other charts and analaysis ---------------------------------

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
  
  geom_text(data = . %>% filter(fuel_class == "Rigid trucks") %>% slice(1),
            aes(x = 1980,
                y = 75000,
                label = "Rigid trucks"),
            colour = grattan_red,
            hjust = "left",
            fontface = "bold",
            check_overlap = TRUE) +
  geom_text(data = . %>% filter(fuel_class == "Articulated trucks") %>% slice(1),
            aes(x = 1980,
                y = 100000,
                label = "Articulated trucks"),
            colour = grattan_orange,
            hjust = "left",
            fontface = "bold",
            check_overlap = TRUE) +
  
  theme_grattan() +
  grattan_fill_manual(2) +
  grattan_colour_manual(2) +
  scale_y_continuous_grattan(labels = scales::label_dollar())+
  scale_x_continuous_grattan(limits = c(1980, 2003),
                             breaks = c(1980, 2000, 2020)) +
  
  theme(strip.text.x = element_blank()) +
  #labs(title = "Cash for clunkers could significantly reduce health costs from trucks",
  #     subtitle = "Estimated avoided health cost per vehicle if bought in a cash for clunkers scheme (substituted)",
  #     caption = "A discount rate of 7% is applied. Does not include health damage from re-entrained road dust. Assumes that
  #     any vehicle sold prior to 2003 (pre-Euro III standards) is replaced with a Euro III truck. ",
  #     x = "Year of vehicle sale") +
  labs(x = "Year of vehicle sale") +
  
  facet_grid(rows = vars(fuel_class))











