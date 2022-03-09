
# This script prepares the charts estimating when total cost of ownership will 
# be reached for electric HDVs with diesel

source("R/costs-modelling/estimate_tco.R")


#' Running the function
#' (is there a way to do this through `map` to generate heaps of options with different
#' price scenarios etc.?)


# Binding various runs -----------------------------
tco_scenarios <- bind_rows(
  estimate_tco() %>% 
    mutate(cost_scen = "base"),
  estimate_tco(diesel_price = 1.63) %>% 
    mutate(cost_scen = "v-high-fuel"),
  estimate_tco(diesel_price = 0.93) %>% 
    mutate(cost_scen = "v-low-fuel"),
  #20\% higher infrastructure costs
  estimate_tco(infr_costs = infrastructure_costs_all %>% 
                 mutate(infrastructure_cost = infrastructure_cost * 1.15)) %>% 
    mutate(cost_scen = "high-infr"),
  #20% lower infra costs
  estimate_tco(infr_costs = infrastructure_costs_all %>% 
                 mutate(infrastructure_cost = infrastructure_cost * 0.85)) %>% 
    mutate(cost_scen = "low-infr"),
  #Combinations of infrastructure costs and fuel costs
  estimate_tco(infr_costs = infrastructure_costs_all %>% 
                 mutate(infrastructure_cost = infrastructure_cost * 1.15),
               electricity_price = 0.2) %>% 
    mutate(cost_scen = "high-infr-high-elec"),
  #20% lower infra costs
  estimate_tco(infr_costs = infrastructure_costs_all %>% 
                 mutate(infrastructure_cost = infrastructure_cost * 0.85),
               electricity_price = 0.1) %>% 
    mutate(cost_scen = "low-infr-low-elec"))


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


c3_tco_estimate <- tco_chart %>% 
  ggplot(aes(x = sales_year,
             y = incr_cost,
             colour = fuel_class,
             fill = fuel_class)) +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  
  geom_smooth(alpha = 0.2) +
  # geom_point() +
  theme_grattan() + 
  theme(strip.text.y = element_blank()) +
  grattan_colour_manual(2) + 
  grattan_fill_manual(2) +
  scale_y_continuous_grattan(limits = c(-0.35, 0.35),
                             label = scales::label_percent()) +
  geom_text(data =. %>% 
            filter(sales_year == 2022),
          aes(x = 2022,
              y = 0.33,
              label = fuel_class),
          hjust = "left",
          fontface = "bold",
          check_overlap = TRUE,
          size = 7) +
  
  geom_point(data = . %>% filter(fuel_class == "Articulated trucks"),
             colour = grattan_grey3,
             aes(x = 2026.58, y = 0),
             size = 2.5) +
  
  geom_richtext(data = . %>% filter(fuel_class == "Articulated trucks") %>% ungroup() %>% slice(1),
                aes(x = 2027,
                    y = 0.15,
                    label = glue("<span style= 'font-size:13pt; color:{grattan_grey3}'>We estimate an average<br>",
                                 "<span style= 'font-size:13pt; color:{grattan_orange}'>**articulated truck** ",
                                 "<span style= 'font-size:13pt; color:{grattan_grey3}'>will<br>reach TCO parity around <br> 2026-28")),
                hjust = "left",
                lineheight = 1.5,
                fill = "white", label.color = NA) +
  
  facet_grid(rows = vars(fuel_class)) +
  labs(x = NULL)
  
 # labs(title = "Total cost of ownership parity is approaching quickly for articulated trucks",
#       subtitle = "12 year total cost of ownership estimate",
#       caption = "Based on Grattan analysis. A discount rate of 4% is applied. Costs reflect vehicle running costs and infrastructure costs associated with charging. 
#       Assumed infrastructure costs are highly conservative, and reflective of very low volume electric fleets (under 100 trucks).",
#       x = NULL)

#-------------------

#grattan_save(filename = "atlas/place-holder-tco-chart.pdf",
#             type = "wholecolumn",
#             save_ppt = TRUE)




