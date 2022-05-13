

# Willingness to pay tables
#' This script creates tables (and charts) of how much we think the government should 
#' be willing to pay for vehicles in a scrappage/buyback scheme, depending on how far the 
#' vehicle travelled (in 2022), the age of the vehicle, and it's type


source("R/00-setup.R")

# Reading data

policy_outcomes <- read_rds("data/policy_outcomes.rds")
discount_rate <- 0.07

# Wrangling data 

#' What we're after is a relatively simple estimate of remainig lifetime costs for each sales year/vehicle class 
#' combination. We can then scale this to create an estimate of the public costs of each vehicle based on 
#' how far it travels in the base year (2022 in our case)

lifetime_costs <- policy_outcomes %>% 
  filter(fleet_year >= 2022,
         sales_year <= 2022,
         fuel_class %in% c("Articulated trucks", "Rigid trucks"),
         scenario == "baseline",
         vkt_scenario == "vkt_central") %>% 
  select(fuel_class, fleet_year, sales_year, total, region, vkt, pollutant, pollutant_year,
         health_cost_total) %>% 
  count(fuel_class, fleet_year, sales_year, total, region, vkt, wt = health_cost_total, name = "health_cost_total") %>% 
  group_by(fuel_class, sales_year, region) %>% 
  # 'saving a total of how many vehicles were around in the base year (2022). Using this figure,
  #' as opposed to how many vehicles are around in the current year means we factor in the 
  #' likelihood of a vehicle being reitred over time
  mutate(max_total = max(total)) %>% 
  # Converting the health costs into a 'per vehicle' health cost by dividing by the total vehicles in each category
  mutate(health_cost_total = health_cost_total / max_total) %>% 
  select(-total, -max_total) %>% 
  mutate(health_cost_total = health_cost_total / (1 + discount_rate)^(fleet_year - 2022)) # discounting health costs at 7\%



#' Now we want to create three estimates: one estimate where we have an 'average' vehicle across rural
#' and urban use; and scaled versions of 'urban only' and 'rural only' use

#' Scaling for 'urban only' and 'rural only' use
rural_urban_scaled <- lifetime_costs %>% 
  ungroup() %>% 
  group_by(fuel_class, fleet_year, sales_year) %>% 
  mutate(vkt_scale = sum(vkt) / vkt,  #creating a scaling factor
         health_cost_total = vkt_scale * health_cost_total,
         vkt = vkt * vkt_scale) %>%  # and using that factor to upscale the costs
  select(-vkt_scale) 


region_average_costs <- lifetime_costs %>% 
  group_by(fuel_class, fleet_year, sales_year) %>% 
  summarise(vkt = sum(vkt),
            health_cost_total = sum(health_cost_total)) %>% 
  mutate(region = "average")


#' Now binding our two datasets together to have the three options,
#' and checking the km travelled are all equivalent for each (should be if the
#' scaling worked properly)
#' 
lifetime_costs_all <- bind_rows(rural_urban_scaled,
          region_average_costs) %>% 
  arrange(fuel_class, fleet_year, sales_year)




# Scaling estimates based on km travelled ----------------------------------

#' Now we want to create estimates for our charts of how the health costs increase
#' with an increase in the km travelled. This will be a linear relationship (before discounting)
#' given that the health cost per km travelled is a linear relationship. 

km_scenarios <- seq(from = 0, to = 150000, by = 10000)

wtp_scenarios <- lifetime_costs_all %>% 
  mutate(cost_per_km = health_cost_total / vkt) %>% 
  ungroup() %>% 
  # and now crossing it with scenarios of how far a truck travelled
  crossing(km_scenarios) %>% 
  mutate(health_cost_scenario = cost_per_km * km_scenarios) %>% # estimating health cost over scenarios
  count(fuel_class, sales_year, km_scenarios, region, wt = health_cost_scenario, name = "health_cost_scenario")




# Adding vehicle replacements: i.e., what is the overall health benefit if the vehicles
# are instead replaced with a Euro IV vehicle

lifetime_costs_all %>% 
  mutate(cost_per_km = health_cost_total / vkt) %>% 
  ungroup() 

#' For this we want to add a column, 'cost_per_km_of_replacement' which represents a Euro IV vehicle
#' (i.e. a 2010 vehicle) 

#' Calculating the cost per km in each fleet year for a Euro IV vehicle
replacement_costs <- lifetime_costs_all %>% 
  mutate(cost_per_km = health_cost_total / vkt) %>% 
  ungroup() %>% 
  filter(sales_year == 2010) %>% 
  mutate(replacement_cost_per_km = cost_per_km) %>% 
  select(fuel_class, fleet_year, region, replacement_cost_per_km) 

# Generating all sales years and joining
sales_year <- seq(from = 1980, to = 2022)
all_replacement_costs <- crossing(sales_year, replacement_costs)

wtp_scenarios_w_replacement <- left_join(lifetime_costs_all %>% 
            mutate(cost_per_km = health_cost_total / vkt) %>% 
            ungroup(), 
          all_replacement_costs) %>% 
  mutate(wtp_per_km = cost_per_km - replacement_cost_per_km) %>% # calculating net cost
  crossing(km_scenarios) %>% 
  mutate(health_cost_scenario = wtp_per_km * km_scenarios) %>% # estimating health cost over scenarios
  count(fuel_class, sales_year, km_scenarios, region, wt = health_cost_scenario, name = "health_cost_scenario") %>% 
  # because replaced with Euro IV vehicle results meaningless for anything after 2007 (the numbers post 2010 are non-zero 
  # because of different driving patterns/attrition etc but don't panic, these effects shouldn't effect the other results
  # meaningfully)
  filter(sales_year <= 2007)




# Plotting --------------------------------

#' We also want to add averages to the plot (the average km travelled value for each 
#' option just for some context)
average_vkts <- policy_outcomes %>% 
  filter(fuel_class %in% c("Articulated trucks", "Rigid trucks"),
         fleet_year == 2022,
         scenario == "baseline",
         vkt_scenario == "vkt_central",
         pollutant == "ex_nox_l") %>% #just selecting a single pollutant to avoid duplicate rows %>% 
  ungroup() %>%
  select(fuel_class, region, sales_year, vkt) %>% 
  count(fuel_class, sales_year, wt = vkt, name = "average_vkt")
  
# And sorting our colours
colours <- c("art_average" = grattan_red,
             "art_metro" = grattan_grey1,
             "art_nonmet" = grattan_grey1,
             "rig_average" = grattan_orange,
             "rig_met" = grattan_grey1,
             "rig_nonmet" = grattan_grey1)
wtp_scenarios_w_replacement <- wtp_scenarios_w_replacement %>% 
  mutate(colours = case_when(
    fuel_class == "Articulated trucks" & region == "average" ~ "art_average",
    fuel_class == "Articulated trucks" & region == "metro" ~ "art_metro",
    fuel_class == "Articulated trucks" & region == "non_metro" ~ "art_nonmet",
    fuel_class == "Rigid trucks" & region == "average" ~ "rig_average",
    fuel_class == "Rigid trucks" & region == "metro" ~ "rig_met",
    fuel_class == "Rigid trucks" & region == "non_metro" ~ "rig_nonmet"))



# Final plot ----------------------

c2_wtp_scenarios <- wtp_scenarios_w_replacement %>%
  left_join(average_vkts) %>% 
  # Selected years include one from each euro level pre Euro IV
                          #pre-euro  #pre-euro   #I    #III
  filter(sales_year %in% c(1990,      1995,     2000,  2005)) %>% 
  
  ggplot(aes(x = km_scenarios, 
             y = health_cost_scenario,
             colour = colours)) +
  
  geom_line(aes(linetype = region)) +
  #Plotting the point of an 'average vehicle of that age'
  geom_point(aes(x = average_vkt,
                 y = average_vkt * (health_cost_scenario / km_scenarios)),
             size = 4) +
  
  # Dollar figure labels
  grattan_label(data = . %>% filter(region == "average"),
                aes(x = average_vkt + 9000,
                y = average_vkt * (health_cost_scenario / km_scenarios) - 1000,
                label = paste0("$", signif(average_vkt * (health_cost_scenario / km_scenarios), digits = 2) %>%  comma())),
                size = 12) +
  
  # Vehicle type labels
  grattan_label(data = . %>% filter(sales_year == 1990, region == "average"),
                aes(y = 250000, 
                    x = 25000,
                    label  = fuel_class,
                    colour = colours),
                hjust = "center",
                fontface = "bold") +
  
  # Vehicle year labels
  grattan_label(data = . %>% filter(sales_year == 1990, region == "average"),
                aes(y = 232000, 
                    x = 25000,
                    label  = paste0("Manufactured in: 1990")),
                colour = grattan_grey4,
                size = 16,
                hjust = "center") +
  grattan_label(data = . %>% filter(sales_year > 1990, region == "average"),
                aes(y = 250000, 
                    x = 3000,
                    label = sales_year),
                colour = grattan_grey4,
                size = 16,
                hjust = "left") +
  
  # Urban/rural labels
  grattan_label(data = . %>% filter(fuel_class == "Articulated trucks", sales_year == 1995) %>% slice(1),
                aes(y = 185000, 
                    x = 35000,
                    label  = "Urban only\ndriving"),
                colour = grattan_grey3,
                hjust = "right",
                size = 14) +
  grattan_label(data = . %>% filter(fuel_class == "Articulated trucks", sales_year == 1995) %>% slice(1),
                aes(y = 13000, 
                    x = 26000,
                    label  = "Rural only driving"),
                colour = grattan_grey3,
                hjust = "left",
                size = 14) +
  grattan_label(data = . %>% filter(fuel_class == "Articulated trucks", sales_year == 1995) %>% slice(1),
                aes(y = 135000, 
                    x = 35000,
                    label  = "Urban/rural\naverage"),
                colour = grattan_red,
                hjust = "left",
                size = 14) +
  
  # Other formatting
  geom_hline(yintercept = 0) + 
  grattan_y_continuous(limits = c(0, 255000),
                       labels = label_dollar()) +
  grattan_x_continuous(limits = c(0, 55000),
                       labels = label_comma(),
                       breaks = c(0, 25000, 50000)) +
  
  scale_color_manual(values = colours) +
  scale_fill_manual(values = colours) +
  scale_linetype_manual(values = c("solid", "solid", "dashed")) +
  theme_grattan(chart_type = "scatter") +
  theme(strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        plot.subtitle = element_markdown()) +
 
  
  labs(
  #title = "Willlingness-to-pay estimates",
  #     subtitle = glue("<span style='font-size:18pt'>Estimated public benefit of scrapping vehicles under a buyback scheme, by year of vehicle manufacture<br>",
  #                     "![Red dot](R/charts/dot-for-chart.svg)",
  #                     "<span style='font-size:15pt; color:{grattan_grey3}'> Estimate for a vehicle travelling fleet average kms"),
       y = NULL,
       x = "Km traveled in the past year") +
  
  facet_grid(cols = vars(sales_year),
             rows = vars(fuel_class))

















