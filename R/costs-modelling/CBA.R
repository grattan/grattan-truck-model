
#' Zero emissions targets + incentives CBA ==================================

#' This script estimates the social and private costs and benefits of a baseline and 
#' a EV mandate scenario (basically a CBA). Conceptually, the main factors that are 
#' included in the analysis are:

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


policy_outcomes <- read_rds("data/policy_outcomes.rds")



# Separating diesel/electric into separate rows -----------------
  # (this is a slightly painful process)

#First duplicating and binding 
ev_scenarios <- bind_rows(
  # The electric share
  policy_outcomes %>% 
    mutate(total = total * electric_share,
           fuel = "electric"),
  
  #The diesel share 
  policy_outcomes %>% 
    mutate(total = total * (1 - electric_share),
           fuel = "diesel")) %>% 
  
  # Sorting out the co2, fuel consumption, electricity use and exhaust emissions/health costs columns
  mutate(co2 = if_else(fuel == "diesel", co2_ice, co2_ev),
         fuel_consumption = if_else(fuel == "diesel", fuel_consumption, 0),
         elec_use_kw = if_else(fuel == "electric", total * vkt * ev_consumption, 0),
         #assume exhaust emissions 0 for EVs
         pollutant_rate = if_else(pollutant %in% c("ex_nox_l", "ex_pm10_l", "ex_pm25_l", "ex_voc_l", "ex_sox_l", "secon_nox_pm25", "secon_sox_pm25") & fuel == "electric",
                                  0, pollutant_rate),
         
         #calculating the totals for pollutants
         pollutant_total = if_else(
           #If in one of the per litre categories
           pollutant %in% c("ex_nox_l", "ex_pm10_l", "ex_pm25_l", "ex_voc_l", "ex_sox_l", "secon_nox_pm25", "secon_sox_pm25"),
           #the calculate as L fuel * rate
           fuel_consumption * pollutant_rate,
           # Else it is a per km category, so calculate as per km travelled
           vkt * pollutant_rate),
         
         # And multiply by damage costs (convert from g to tonnes) for total costs 
         health_cost_total = damage_cost_t * pollutant_total / 1000000) 




# Simplifying down to the data we need for the EV scenarios ---------------

ev_scenarios <- ev_scenarios %>% 
  filter(scenario %in% c("baseline",
                         "Euro 6 (2027)",
                         "Electric targets")) %>% 
  #and remove 0 vehicle columns
  filter(total != 0) %>% 
  
  
  #Simplifying the dataset down to the key elements, first broken up by region
    #(we are doing in two stages so we don't double count things like VKTs which are 
    #duplicated over rows)
  group_by(scenario, vkt_scenario, fleet_year, fuel_class, sales_year, fuel, age, 
           total, electric_share, ei_g_wh, vkt, region, co2, elec_use_kw, fuel_consumption) %>% 
  summarise(health_cost_total = sum(health_cost_total))  %>% 
  #we are grouping again to now summarise down from the region estimate to overall
  group_by(scenario, vkt_scenario, fleet_year, fuel_class, fuel, sales_year, age, total, ei_g_wh,
           electric_share) %>% 
  summarise(vkt = sum(vkt),
            health_cost_total = sum(health_cost_total),
            co2_t = sum(co2) / 1000000,
            fuel_consumption = sum(fuel_consumption),
            elec_use_kw = sum(elec_use_kw))




# Adding upfront cost estimates for EVs -------------------------------------

upfront_vehicle_costs <- read_rds("data/upfront_costs.rds") %>% 
  rename("sales_year" = year,
         "purchase_price" = cost) 


ev_scenarios <- left_join(ev_scenarios, 
                          upfront_vehicle_costs) %>% 
  #Assuming there is 'no' upfront cost before EVs are an option (because there 
  #is no price differential to be counted), and making sure upfront cost only
  #applied to 0 age vehicles
  mutate(purchase_price = if_else(is.na(purchase_price) | age != 0, 0, purchase_price))


# Diesel and electricity prices -------------------------------------

#' Based on the ATA estimates, we are going to assume an upper bound electricity price
#' (wholesale) of \$0.05-$0.15/kWh, and an average diesel price of $1.33/L (taking into account
#' the rebate on fuel excise)
#' These costs are assumed to stay steady over time, but in reality it's likely that diesel
#' prices might rise 

diesel_price <- 1.33
electricity_price <- 0.15

ev_scenarios <- ev_scenarios %>% 
  mutate(fuel_cost = if_else(fuel == "diesel",
                             fuel_consumption * diesel_price,
                             elec_use_kw * electricity_price))

# Adblue costs -------------------------------------------

# Acccording to Yarra (https://www.yara.com.au/chemical-and-environmental-solutions/adblue-for-vehicles/adblue-for-commercial-vehicles/#:~:text=In%20general%2C%20expect%20a%20consumption,100%20km%20on%20the%20road.)
# commercial adblue consumption rates tend to be 4-6% of diesel consumption rates. 
# Adblue prices in bulk are likely to be around the 50-60c/L mark (https://www.abc.net.au/news/2021-12-14/adblue-prices-increase-as-haulage-companies-call-for-a-fix/100697712)
# For this analysis we will assume 55c/L, and 5% consumption rate across the whole fleet

# NEED TO FIND OUT ADBLUE CONSUMPTION RATES FOR OLDER VEHICLES ETC. 

ev_scenarios <- ev_scenarios %>% 
  mutate(adblue_cost = if_else(fuel == "diesel",
                               fuel_consumption * 0.05 * 0.55,
                               0),
         
         
      

# Co2 cost -------------------------------------------------------
# Assuming a social cost of carbon, applied at a rate of $25 to start with (per tonne)
        co2_social_cost = 25 * co2_t)




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

maintenance_costs <- tribble(~fuel_class,                            ~fuel,           ~maintenance_cost_km,
                              #Diesel                                                # maintenance component       # oil/lubricant component
                              "Articulated trucks",                  "diesel",                0.25         +        0.75/100 * euro_aus_conversion,
                              "Rigid trucks",                        "diesel",                0.13         +        0.75/100 * euro_aus_conversion * 0.5,
                              "Buses",                               "diesel",                0.13         +        0.75/100 * euro_aus_conversion * 0.5,
                              "Non-freight carrying trucks",         "diesel",                0.13         +        0.75/100 * euro_aus_conversion * 0.5,
                              
                              #Electric vehicles
                              "Articulated trucks",                  "electric",               0.25  *  0.7,
                              "Rigid trucks",                        "electric",               0.13  *  0.7,
                              "Buses",                               "electric",               0.13  *  0.7,
                              "Non-freight carrying trucks",         "electric",               0.13  *  0.7) %>% 
  
  #Costs are reported as 2013 costs, so inflating 
  mutate(maintenance_cost_km = maintenance_cost_km * inflation_13_21) 

ev_scenarios <- left_join(ev_scenarios,
                          maintenance_costs) %>% 
  mutate(maintenance_cost_total = (maintenance_cost_km * vkt * total)) %>% 
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

ev_scenarios <- ev_scenarios %>% 
  mutate(infrastructure_cost = case_when(
    fuel == "electric" & age == 0 & fuel_class == "Articulated trucks" ~ 180000 * us_aus_conversion,
    fuel == "electric" & age == 0 & fuel_class == "Rigid trucks" ~ 82000 * us_aus_conversion,
    fuel == "electric" & age == 0 & fuel_class == "Non-freight carrying trucks" ~ 82000 * us_aus_conversion,
    fuel == "electric" & age == 0 & fuel_class == "Buses" ~ 82000 * us_aus_conversion,
    # Assume buses and non-freight have same costs as rigid
    fuel == "electric" & age != 0 ~ 0,
    fuel != "electric" ~ 0))



# Time and weight penalties -----------------------------------------------

#' ICCT assumptions use a value of 3-6\% as a load weight penalty for EV's. They assume
#' that vehicles travel with a full load 50% of the time, resulting in an overall penalty 
#' of 1.5-3% for electric vehicles (that 1.5-3% more vehicles are required.)

#' They also assume a time penalty of 3% - it's not entirely clear where this value comes
#' from, but it's what we will start off with - 1.5% for weight and 3% for time
#' 
#' The way the data is structured means that we need to carry these costs through at 
#' each step of the analysis, instead of applying at a single point. 

ev_scenarios <- ev_scenarios %>% 
  mutate(time_weight_penalty = if_else(
    fuel == "electric",
    1.015 * 0.03 * (purchase_price + fuel_cost + adblue_cost + maintenance_cost + infrastructure_cost),
    0))



# Playing around with outputs ------------------------------------------


cost_colours <- c("social_cost" = grattan_black,
                  "co2" = grattan_grey3,
                  "time_weight_penalty" = grattan_lightyellow,
                  "adblue_cost" = grattan_yellow,
                  "maintenance_cost" = grattan_orange,
                  "infrastructure_cost" = grattan_darkorange,
                  "fuel_cost" = grattan_red,
                  "purchase_price" = grattan_darkred)



# Summarising and discounting  ---------------------------------------

ev_summarised <- ev_scenarios %>% 
  relocate(c(co2_t, fuel_consumption, elec_use_kw), 
           .before = health_cost_total) %>% 
  pivot_longer(cols = 15:21,
               names_to = "cost_type",
               values_to = "cost") %>% 
  filter(fleet_year >= 2022) %>% 
  
  #discounting 
  mutate(discounted_cost = (1 / (1 + 0.04)^(fleet_year - 2022)) * cost) %>% 

  group_by(scenario, vkt_scenario) %>% 
  # Total in billions
  summarise(total_cost = sum(cost) / 1000000000,
            total_disc_cost = sum(discounted_cost / 1000000000)) %>% 
  filter(vkt_scenario == "vkt_central")












           