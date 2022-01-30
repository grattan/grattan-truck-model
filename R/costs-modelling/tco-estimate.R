
#' This script estimates the total cost of ownership of EVs and Diesel vehicles
#' at various points in time. 
#' The key parts of the model are:

  #' cost of truck purchase (EV/diesel CSIRO data)
    #' (https://aemo.com.au/-/media/files/electricity/nem/planning_and_forecasting/inputs-assumptions-methodologies/2021/csiro-ev-forecast-report.pdf)
  
  #' cost of truck operation (electricity/diesel costs + adblue)
    #' various estimates
  
  #' cost of maintenance (as a per km estimate from ICCT, cross checked with US data)
    #' (https://theicct.org/wp-content/uploads/2021/11/tco-bets-europe-1-nov21.pdf page 14)
  
  #' infrastructure cost estimates for charging (estimates from ICCT, will take conservative approach)
    #' (https://theicct.org/wp-content/uploads/2021/06/ICCT_EV_HDVs_Infrastructure_20190809.pdf)
  
  #' weight penalty of ev's (approx 3-6% of total weight, assume only half trucks are fully loaded)
  #' time penalty (ICCT assumes 3% but we might need to think about this)
    #' (https://theicct.org/wp-content/uploads/2021/06/ICCT_EV_HDVs_Infrastructure_20190809.pdf)
  
  #' social costs of diesel trucks from pollution
  #' social cost of electricity (externality costs) from generation source
  #' carbon emissions under each scenario


discount_costs <- function(data, rate) {
  i <- 1
  while (i <= nrow(data)) {
    
    data$cost[i] =  (data$cost[i])  / ((1 + rate)^(data$fleet_year[i] - 2022))
    
    i <- i + 1
  }
  return(data)
}

policy_outcomes <- read_rds("data/policy_outcomes.rds")


# Simplifying down to the data we need for the EV scenarios ---------------

tco_estimate <- policy_outcomes %>% 
      filter(scenario == "baseline",
             vkt_scenario == "vkt_central",
             sales_year > 2021, age <= 12) %>% 
      mutate(total = 1,
             electric = "electric",
             diesel = "diesel") %>% 
  filter(pollutant == "ex_nox_l") %>% 
  group_by(fuel_class, fleet_year, sales_year, age, diesel_rate_100, ev_consumption, ei_g_wh, diesel, electric) %>% 
  summarise(vkt = sum(vkt)) %>% 
  unique() %>% 
  pivot_longer(cols = 8:9,
               names_to = "fuel") %>% 
  select(-value)
  

# Adding upfront vehicle costs ---------------------------------------------

#Upfront cost data
upfront_costs <- read_rds("data/upfront_costs.rds") %>% 
  rename("sales_year" = year)

# First adding upfront costs
tco_estimate <- left_join(tco_estimate, 
                          upfront_costs) %>% 
  
  mutate(purchase_price = if_else(age == 0, cost, 0)) %>% 
  select(-cost)



# Adding fuel costs --------------------------------------------

# Price data electricity/fuel and adblue consumption
diesel_price <- 1.33
electricity_price <- 0.15
adblue_price <- 0.55

tco_estimate <- tco_estimate %>% 
  mutate(fueling_cost = if_else(fuel == "electric",
                                #Electric costs
                                vkt * ev_consumption * electricity_price,
                                #diesel costs
                                vkt / 100 * diesel_rate_100 * diesel_price,),
         #Adblue costs
         adblue_cost = if_else(fuel == "diesel",
                               vkt / 100 * diesel_rate_100 * 0.05 * adblue_price,
                               0))



# Maintenance costs --------------------------------------------

#' Maintenance costs are assumed to follow ICCT assumptions, where electric vehicles 
#' have  a cost 30% lower than conventional vehicles. This is broadly consistent with US estimates,
#' which estimate that EVs have approximately 25% lower lifetime maintenance costs 
  #' Base maintenance cost estimates are from: https://www.atap.gov.au/parameter-values/road-transport/2-vehicle-operating-cost-voc-components
  #' We assume EV costs are 30% lower, and there is no oil requirement (following ICCT : https://theicct.org/wp-content/uploads/2021/06/ICCT_EV_HDVs_Infrastructure_20190809.pdf
  #' Given there are many types of trucks in each category specified by ATAP, we take reoughyl central estimates
# Oil + lubricant costs are estimated from ICCT, converted from euro. It is assumed that rigid trucks
# cost for these are 50% of articulated trucks, in line with fuel consumption

#Buses and non-freight are assumed the same values as rigid trucks. 

inflation_13_21 <- 1.15
euro_aus_conversion <- 1.58

maintenance <- tribble( ~fuel_class,                            ~fuel,           ~maintenance_cost_km,
                        #Diesel                                                # maintenance component       # oil/lubricant component
                         "Articulated trucks",                  "diesel",                0.25         +        0.75/100 * euro_aus_conversion,
                         "Rigid trucks",                        "diesel",                0.13         +        0.75/100 * euro_aus_conversion * 0.5,
                         "Buses",                               "diesel",                0.13         +        0.75/100 * euro_aus_conversion * 0.5,
                         "Non-freight carrying vehicles",       "diesel",                0.13         +        0.75/100 * euro_aus_conversion * 0.5,
                        
                        #Electric vehicles
                        "Articulated trucks",                  "electric",               0.25  *  0.7,
                        "Rigid trucks",                        "electric",               0.13  *  0.7,
                        "Buses",                               "electric",               0.13  *  0.7,
                        "Non-freight carrying vehicles",       "electric",               0.13  *  0.7) %>% 
   
  #Costs are reported as 2013 costs, so inflating 
  mutate(maintenance_cost_km = maintenance_cost_km * inflation_13_21)

tco_estimate <- left_join(tco_estimate,
                           maintenance) %>% 
  mutate(maintenance_cost = maintenance_cost_km * vkt) %>% 
  select(-maintenance_cost_km)



# Infrastructure costs -----------------------------------------------

#' ICCT: (https://theicct.org/wp-content/uploads/2021/06/ICCT_EV_HDVs_Infrastructure_20190809.pdf)
#' Assuming the 'low volume' estimates for charging infrastructure costs for each category gives:
  #' Articulated : US$180,000/vehicle at low volumes, $113,000 med and $70,000 high volumes
  #' Rigid       : US$82,000/vehicle at low volumes, $40,000 med and $27,000 high volumes
#' We will assume Costs for buses/non-freight follow rigid costs and all follow low volume cost estimates for now
#' ICCT defines low volume as 100 trucks, medium as 1,000 and high as 10,000 trucks;
#' which is approx 1.2 slow chargers and 0.3 ultra-fast chargers per articulated truck,
#' and 1.18 slow and 0.2 ultra-fast chargers per rigid truck
#' In reality it's probably reasonable to shift the costs to medium volumes by 2027 or so (assuming uptake around 
#' 2-10% of new sales); but we will do this in the CBA model based on the share of trucks 

us_aus_conversion <- 1.42

tco_estimate <- tco_estimate %>% 
  mutate(infrastructure_cost = case_when(
    fuel == "electric" & age == 0 & fuel_class == "Articulated trucks" ~ 180000 * us_aus_conversion,
    fuel == "electric" & age == 0 & fuel_class == "Rigid trucks" ~ 82000 * us_aus_conversion,
    fuel == "electric" & age != 0 ~ 0,
    fuel != "electric" ~ 0))




# Time and weight penalties -----------------------------------------------

#' ICCT assumptions use a value of 3-6\% as a load weight penalty for EV's. They assume
#' that vehicles travel with a full load 50% of the time, resulting in an overall penalty 
#' of 1.5-3% for electric vehicles (that 1.5-3% more vehicles are required.)

#' They also assume a time penalty of 3% - it's not entirely clear where this value comes
#' from, but it's what we will start off with - 1.5% for weight and 3% for time


tco_estimate <- tco_estimate %>% 
  mutate(time_weight_penalty = if_else(
    fuel == "electric",
    1.015 * 0.03 * (purchase_price + fueling_cost + adblue_cost + maintenance_cost + infrastructure_cost),
    0))


# Adding together all as a total ----------------------

# Adding for the totals
tco_estimate <- tco_estimate %>% 
  mutate(total_cost = purchase_price + fueling_cost + adblue_cost + maintenance_cost + infrastructure_cost + time_weight_penalty)


# Plotting the estimates ----------------------------------------------

cost_colours <- c("social_cost" = grattan_black,
                  "co2" = grattan_grey3,
                  "time_weight_penalty" = grattan_lightyellow,
                  "adblue_cost" = grattan_yellow,
                  "maintenance_cost" = grattan_orange,
                  "infrastructure_cost" = grattan_darkorange,
                  "fueling_cost" = grattan_red,
                  "purchase_price" = grattan_darkred)


tco_summarised <- tco_estimate %>% 
  pivot_longer(cols = 10:15,
               names_to = "cost_type",
               values_to = "cost") %>% 
  #Discounting at 4%
  discount_costs(rate = 0.04) %>% 
  
  group_by(sales_year, fuel, cost_type, fuel_class) %>% 
  summarise(cost = sum(cost))
  
tco_summarised %>% 
   filter(sales_year %in% c(2022, 2025, 2028),
         fuel_class %in% c("Articulated trucks", "Rigid trucks")) %>% 
  mutate(cost_type = factor(cost_type,
                            levels = names(cost_colours))) %>% 
    ggplot(aes(x = fuel, 
             y = cost,
             fill = cost_type,
             colour = cost_type)) +
  
  geom_col(position = "stack") +
  grattan_fill_manual() + 
  grattan_colour_manual() +
  theme_grattan(legend = "top") +
  scale_y_continuous_grattan(label = scales::label_dollar()) +
  
  facet_grid(cols = vars(sales_year),
             rows = vars(fuel_class),
             scales = "free_y") +
  
  labs(title = "Total cost of ownership parity is approaching",
       subtitle = "Total cost of ownership estimates - 12 year cost",
       x = NULL)


tco_summarised %>% 
  filter(fuel_class %in% c("Articulated trucks", "Rigid trucks")) %>% 
  group_by(sales_year, fuel, fuel_class) %>% 
  summarise(cost = sum(cost)) %>% 
  filter(sales_year <= 2028) %>% 
  
  ggplot(aes(x = sales_year,
             y = cost,
             colour = fuel)) +
  geom_line() +
  geom_point() +
  theme_grattan() + 
  grattan_colour_manual(2) + 
  scale_y_continuous_grattan(limits = c(0, NA),
                     label = scales::label_dollar()) +
  
  facet_wrap(~fuel_class,
             scales = "free_y") +
  
  labs(title = "Total cost of ownership parity is approaching quickly for articulated trucks",
       subtitle = "12 year total cost of ownership estimate",
       caption = "Based on Grattan analysis. A discount rate of 4% is applied. Costs reflect vehicle running costs and infrastructure costs associated with charging. 
       Assumed infrastructure costs are highly conservative, and reflective of very low volume electric fleets (under 100 trucks).")
  





# Total + social costs of ownership ========================================


social_cost <- read_rds("data/per_vech_social_cost.rds")

all_costs <- left_join(tco_estimate, social_cost) %>% 
  select(-total_cost) %>% 
  pivot_longer(cols = 10:16,
               names_to = "cost_type",
               values_to = "cost") %>% 
  filter(sales_year %in% c(2022, 2025, 2028),
         fuel_class %in% c("Articulated trucks", "Rigid trucks")) 

# Discounting and summarising ------------

all_costs <- discount_costs(all_costs, rate = 0.04) %>% 
  group_by(sales_year, fuel, cost_type, fuel_class) %>% 
  summarise(cost = sum(cost)) %>% 
  mutate(cost_type = factor(cost_type,
                            levels = names(cost_colours))) 


# Plotting -------------------------------

all_costs %>% 
  ggplot(aes(x = fuel, 
             y = cost,
             fill = cost_type,
             colour = cost_type)) +
  
  geom_col(position = "stack") +
  scale_fill_manual(values = cost_colours) + 
  scale_colour_manual(values = cost_colours) +
  theme_grattan(legend = "top") +
  scale_y_continuous_grattan(label = scales::label_dollar()) +
  
  facet_grid(cols = vars(sales_year),
             rows = vars(fuel_class),
             scales = "free") +
  
  labs(title = "Subsidising electric vehicles will be worth it by 2025",
       subtitle = "Total financial and social cost of ownership estimates - 12 year cost",
       caption = "Discounted using a 4% discount rate")
