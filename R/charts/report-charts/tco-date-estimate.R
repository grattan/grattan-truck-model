
# This script prepares the charts estimating when total cost of ownership will 
# be reached for electric HDVs with diesel

source("R/costs-modelling/estimate_tco.R")


#' Running the function
#' (is there a way to do this through `map` to generate heaps of options with different
#' price scenarios etc.?)


# Binding various runs - sensitivity testing -----------------------------
tco_scenarios <- bind_rows(
  estimate_tco() %>% 
    mutate(cost_scen = "base"),
  estimate_tco(diesel_price = 1.53) %>% 
    mutate(cost_scen = "high-fuel"),
  estimate_tco(diesel_price = 1.13) %>% 
    mutate(cost_scen = "low-fuel"),
  #20\% higher infrastructure costs
  estimate_tco(infr_costs = infrastructure_costs_all %>% 
                 mutate(infrastructure_cost = infrastructure_cost * 1.15)) %>% 
    mutate(cost_scen = "high-infr"),
  #20% lower infra costs
  estimate_tco(infr_costs = infrastructure_costs_all %>% 
                 mutate(infrastructure_cost = infrastructure_cost * 0.85)) %>% 
    mutate(cost_scen = "low-infr"),
  #Combinations of infrastructure costs and fuel costs
  estimate_tco(infr_costs = infrastructure_costs_all,
               electricity_price = 0.19) %>% 
    mutate(cost_scen = "high-elec"),
  #20% lower infra costs
  estimate_tco(infr_costs = infrastructure_costs_all,
               electricity_price = 0.11) %>% 
    mutate(cost_scen = "low-elec"))


# Sumarising the data -------------------------------

tco_summarised <- tco_scenarios %>% 
  filter(age <= 10) %>% 
  pivot_longer(cols = 10:15,
               names_to = "cost_type",
               values_to = "cost") %>% 
  #Discounting at 4%
  mutate(cost = (1 / (1 + 0.07)^(fleet_year - 2022)) * cost,
         total_cost = (1 / (1 + 0.07)^(fleet_year - 2022)) * total_cost) %>% 
  
  group_by(sales_year, fuel, cost_type, fuel_class, cost_scen) %>% 
  summarise(cost = sum(cost))


tco_chart <- tco_summarised %>% 
  filter(fuel_class %in% c("Articulated trucks", "Rigid trucks")) %>% 
  group_by(sales_year, fuel, fuel_class, cost_scen) %>% 
  summarise(cost = sum(cost)) %>% 
  filter(sales_year <= 2030)  %>% 
  pivot_wider(names_from = fuel,
              values_from = cost) %>% 
  mutate(incr_cost = (electric - diesel) / diesel) 



# Plotting the estimates ----------------------------------------------

cost_colours <- c("social_cost" = grattan_black,
                  "co2" = grattan_grey3,
                  "time_weight_penalty" = grattan_lightyellow,
                  "adblue_cost" = grattan_yellow,
                  "maintenance_cost" = grattan_orange,
                  "infrastructure_cost" = grattan_darkorange,
                  "fuel_cost" = grattan_red,
                  "purchase_price" = grattan_darkred)


# Plotting as a simple point estimate of when TCO is reached ----------------


c3_tco_estimate <- tco_chart %>% 
  mutate(fuel_class = factor(fuel_class, levels = c("Articulated trucks", "Rigid trucks"))) %>% 
  #filtering for the first incremental cost that is past TCO parity
  group_by(fuel_class, cost_scen) %>% 
  filter(incr_cost < 0) %>% 
  filter(incr_cost == max(incr_cost)) %>% 
  #and creating confidence bounds
  ungroup() %>% 
  group_by(fuel_class) %>% 
  mutate(min_year = min(sales_year),
         max_year = max(sales_year)) %>% 

  # Plotting
  ggplot(aes(colour = fuel_class, 
             fill = fuel_class)) +
  
  geom_linerange(data = . %>% group_by(fuel_class),
                 aes(y = fuel_class,
                     xmax = max_year,
                     xmin = min_year),
            alpha = 0.05, size = 14)  +
  
  geom_point(data = . %>% filter(cost_scen == "base"),
             aes(x = sales_year, 
                 y = fuel_class),
             size = 8) +
  
  geom_richtext(data = . %>% filter(fuel_class == "Rigid trucks") %>% ungroup() %>% slice(1),
                aes(x = 2024,
                    y = "Rigid trucks",
                    label = glue("<span style= 'font-size:15pt; color:{grattan_grey3}'>We estimate that an average<br>",
                                 "<span style= 'font-size:15pt; color:{grattan_red}'>**rigid truck**",
                                 "<span style= 'font-size:15pt; color:{grattan_grey3}'> will reach<br>TCO parity around<br>2026-2030")),
                hjust = "left",
                lineheight = 1.5,
                fill = "white", label.color = NA,
                nudge_y = 0.3) +
  
  geom_richtext(data = . %>% filter(fuel_class == "Articulated trucks") %>% ungroup() %>% slice(1),
                aes(x = 2030,
                    y = "Articulated trucks",
                    label = glue("<span style= 'font-size:15pt; color:{grattan_grey3}'>And that an average<br>",
                                 "<span style= 'font-size:15pt; color:{grattan_orange}'>**articulated truck**",
                                 "<span style= 'font-size:15pt; color:{grattan_grey3}'> will<br>reach TCO parity<br>around 2025-29")),
                hjust = "right",
                lineheight = 1.5,
                fill = "white", label.color = NA,
                nudge_y = 0.3) +
  
  theme_grattan() +
  grattan_x_continuous(limits = c(2022, 2034)) +
  grattan_fill_manual(2) + 
  grattan_colour_manual(2) +
  
  labs(x = NULL)












