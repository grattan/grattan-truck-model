#' TCO function 
#' 
#' Turing the TCO estimate script into a function so we can run it with various different input
#' cost scenarios to sensitivity test


#' TCO Chart 


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

source("R/00-setup.R")


discount_costs <- function(data, rate) {
  i <- 1
  while (i <= nrow(data)) {
    
    data$cost[i] =  (data$cost[i])  / ((1 + rate)^(data$fleet_year[i] - 2022))
    
    i <- i + 1
  }
  return(data)
}

policy_outcomes <- read_rds("data/policy_outcomes.rds")



# Pulling out all the assumptions used as inputs -----------------------------

inflation_13_21 <- 1.15
euro_aus_conversion <- 1.58

#Maintenance data 

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


#Infrastructure cost data 
# ICCT assume that 'low volume' is 0-1,000 trucks, 'medium volume' is 1,000-10,000, and higher is 10,000 + trucks. 
# comparing this to our ev sales targets, that corresponds to about:
#' Rigid trucks: high cost pre 2024, medium 2024 - 2025, and low 2025 onwards
#' Articulated trucks: high pre 2030, medium 2030-2035 and low beyond that


us_aus_conversion <- 1.40

infr_cost_rid_art <- tribble( ~fuel,        ~age,   ~volume,       ~fuel_class,                      ~infrastructure_cost,          ~year,
                              "electric",    0,        "low",      "Articulated trucks",            180000 * us_aus_conversion,       2022,
                              "electric",    0,        "medium",   "Articulated trucks",            113000 * us_aus_conversion,       2030,
                              "electric",    0,        "high",     "Articulated trucks",            70000 * us_aus_conversion,        2035,
                              "electric",    0,        "low",      "Rigid trucks",                  82000 * us_aus_conversion,        2022,
                              "electric",    0,        "medium",   "Rigid trucks",                  40000 * us_aus_conversion,        2024,
                              "electric",    0,        "high",     "Rigid trucks",                  27000 * us_aus_conversion,        2026)

# Interpolating between data points
art <- infr_cost_rid_art %>% 
  filter(fuel_class == "Articulated trucks") 

rig <- infr_cost_rid_art %>% 
  filter(fuel_class == "Rigid trucks") 

#Final interpolated data
infrastructure_costs_all <- bind_rows(
  approx(art$year, 
         art$infrastructure_cost,
         n = 14) %>% 
    as.tibble() %>% 
    mutate(fuel_class = "Articulated trucks"),
  
  approx(rig$year, 
         rig$infrastructure_cost,
         n = 14) %>% 
    as.tibble() %>% 
    mutate(fuel_class = "Rigid trucks")) %>% 
  
  rename("sales_year" = x,
         "infrastructure_cost" = y) %>% 
  mutate(age = 0,
         fuel = "electric")


#Upfront costs 
#Upfront cost data
upfront_costs_all <- read_rds("data/upfront_costs.rds") %>% 
  rename("sales_year" = year)



# Final input data ----------------------------------------

euro_aus_conversion <- 1.58
us_aus_conversion <- 1.42
euro_aus_conversion <- 1.58
us_aus_conversion <- 1.42

infrastructure_costs_all
maintenance


estimate_tco <- function(
  base_data = policy_outcomes,
  #Prices
  diesel_price = 1.33,
  electricity_price = 0.15,
  adblue_price = 0.55,
  infr_costs = infrastructure_costs_all,
  maintenance_costs = maintenance,
  upfront_costs = upfront_costs_all,
  # Conversions
  euro_aus_conversion = 1.58,
  us_aus_conversion = 1.42) {

  
  
  # Simplifying down to the data we need for the EV scenarios ---------------
  
  tco_estimate <- base_data %>% 
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
    select(-value) %>% 
  
  # Correcitng for line haul removal 
    
  #' Scaling articulated truck km travelled to `remove' line-haul operations from the data
  #' This is done because line haul is not considered feasible for the TCO estimate 
  #' In the time frame we are discussing (it is too uncertain), and otherwise including
  #' it probably overstates the TCO point (brings it too far forward). 
  #' We assume that instead of traveling ~350-500km per day (7 day - 5 day estimate over 10 years),
  #' use of non-line-haul trucks is 10\% lower (315 - 450 km/day). This is line with what is considered
  #' technically feasibly by the ICCT (on the safe side - they assume up to 500km/day reasonable)
  #' This is also conservative given that a time and weight penalty is also added to costs, which mainly
  #' reflect constraints on line haul

    mutate(vkt = case_when(
      fuel_class == "Articulated trucks" ~ vkt * 0.9,
      fuel_class != "Articulaetd trucks" ~ vkt))
  
  
  # Adding upfront vehicle costs ---------------------------------------------
  
  # First adding upfront costs
  tco_estimate <- left_join(tco_estimate, 
                            upfront_costs) %>% 
    
    mutate(purchase_price = if_else(age == 0, cost, 0)) %>% 
    select(-cost)
  
  
  
  # Adding fuel costs --------------------------------------------
  
  tco_estimate <- tco_estimate %>% 
    mutate(fuel_cost = if_else(fuel == "electric",
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
  
  tco_estimate <- left_join(tco_estimate,
                            maintenance_costs) %>% 
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
  
  tco_estimate <- left_join(tco_estimate, infr_costs) %>% 
    mutate(infrastructure_cost = if_else(
      is.na(infrastructure_cost) == TRUE,
      0, infrastructure_cost))
  
  
  # Time and weight penalties -----------------------------------------------
  
  #' ICCT assumptions use a value of 3-6\% as a load weight penalty for EV's. They assume
  #' that vehicles travel with a full load 50% of the time, resulting in an overall penalty 
  #' of 1.5-3% for electric vehicles (that 1.5-3% more vehicles are required.)
  
  #' They also assume a time penalty of 3% - it's not entirely clear where this value comes
  #' from, but it's what we will start off with - 1.5% for weight and 3% for time
  
  
  tco_estimate <- tco_estimate %>% 
    mutate(time_weight_penalty = if_else(
      fuel == "electric",
      1.015 * 0.03 * (purchase_price + fuel_cost + adblue_cost + maintenance_cost + infrastructure_cost),
      0))
  
  
  # Adding together all as a total ----------------------
  
  # Adding for the totals
  tco_estimate <- tco_estimate %>% 
    mutate(total_cost = purchase_price + fuel_cost + adblue_cost + maintenance_cost + infrastructure_cost + time_weight_penalty)
  
  
  
  return(tco_estimate)
  
}




#' Running the function
#' (is there a way to do this through `map` to generate heaps of options with different
#' price scenarios etc.?)


tco_scenarios <- bind_rows(
  estimate_tco() %>% 
    mutate(cost_scen = "base"),
  
  #estimate_tco(electricity_price = 0.2) %>% 
  #  mutate(cost_scen = "high-elec"),
  
 # estimate_tco(electricity_price = 0.1) %>% 
 #   mutate(cost_scen = "low-elec"),
  
 # estimate_tco(diesel_price = 1.43) %>% 
#    mutate(cost_scen = "high-fuel"),
  
#  estimate_tco(diesel_price = 1.23) %>% 
#    mutate(cost_scen = "low-fuel"),
  
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
    mutate(cost_scen = "low-infr-low-elec"),
  
#  estimate_tco(infr_costs = infrastructure_costs_all %>% 
#                 mutate(infrastructure_cost = infrastructure_cost * 1.15),
#               electricity_price = 0.1) %>% 
#    mutate(cost_scen = "high-infr-low-elec"),
  
  #20% lower infra costs
 # estimate_tco(infr_costs = infrastructure_costs_all %>% 
#                 mutate(infrastructure_cost = infrastructure_cost * 0.85),
#               electricity_price = 0.2) %>% 
#    mutate(cost_scen = "low-infr-high-elec")
  
  )
