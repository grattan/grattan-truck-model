
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


#' 


policy_outcomes <- read_rds("data/policy_outcomes.rds")


# Simplifying down to the data we need for the EV scenarios 
ev_scenarios <- policy_outcomes %>% 
  filter(scenario %in% c("baseline",
                         "Euro 6 (2027)",
                         "Electric targets")) %>% 
  #estimating electricity use - this figure is in Wh 
  mutate(elec_use = electric_share * total * vkt * ev_consumption * 1000) %>% 
  
  #simplifying the health costs to a total figure per vehicle group
           group_by(scenario, vkt_scenario, fleet_year, fuel_class, sales_year, age, 
                    total, electric_share, ei_g_wh,) %>% 
           summarise(health_cost_total = sum(health_cost_total),
                     co2 = sum(co2),
                     elec_use = sum(elec_use),
                     fuel_consumption = sum(fuel_consumption))



# Adding upfront cost estimates for EVs -------------------------------------

ev_costs <- read_rds("data/ev_costs.rds") %>% 
  rename("sales_year" = year)

ev_scenarios <- left_join(ev_scenarios, 
                          ev_costs) %>% 
  mutate(elec_upfront_cost = if_else(age == 0,
                  electric_share * total * incr_cost, 0)) %>% 
  select(-incr_cost)



# Diesel and electricity prices -------------------------------------

#' Based on the ATA estimates, we are going to assume an upper bound electricity price
#' (wholesale) of \$0.05-$0.15/kWh, and an average diesel price of $1.33/L (taking into account
#' the rebate on fuel excise)
#' These costs are assumed to stay steady over time, but in reality it's likely that diesel
#' prices might rise 

diesel_price <- 1.33
electricity_price <- 0.10

ev_scenarios <- ev_scenarios %>% 
  mutate(total_diesel_cost = fuel_consumption * diesel_price,
         # we / 1000 to convert to kWh here
         total_electricity_cost = elec_use * electricity_price / 1000)


# Adblue costs -------------------------------------------

# Acccording to Yarra (https://www.yara.com.au/chemical-and-environmental-solutions/adblue-for-vehicles/adblue-for-commercial-vehicles/#:~:text=In%20general%2C%20expect%20a%20consumption,100%20km%20on%20the%20road.)
# commercial adblue consumption rates tend to be 4-6% of diesel consumption rates. 
# Adblue prices in bulk are likely to be around the 50-60c/L mark (https://www.abc.net.au/news/2021-12-14/adblue-prices-increase-as-haulage-companies-call-for-a-fix/100697712)
# For this analysis we will assume 55c/L, and 5% consumption rate across the whole fleet

# NEED TO FIND OUT ADBLUE CONSUMPTION RATES FOR OLDER VEHICLES ETC. 

ev_scenarios <- ev_scenarios %>% 
  mutate(adblue_cost = fuel_consumption * 0.05 * 0.55,

# Co2 cost -------------------------------------------------------
# Using a rate of $25 to start with (per tonne)
        co2_social_cost = 25 * co2 / 1000000,
        elec_upfront_cost = if_else(
            is.na(elec_upfront_cost),
            0, elec_upfront_cost),
        total_cost = co2_social_cost + adblue_cost + total_diesel_cost + total_electricity_cost + elec_upfront_cost + health_cost_total,
        social_cost = co2_social_cost + health_cost_total)


# Maintenance costs -----------------------------------------------












# Playing around with outputs ------------------------------------------


test_output <- ev_scenarios %>% 
  group_by(scenario, vkt_scenario, fleet_year) %>% 
  summarise(total_cost = sum(total_cost)) 


discount_cba <- function(data, rate) {
  i <- 1
  while (i <= nrow(data)) {
    
    data$total_cost[i] =  (data$total_cost[i])  / ((1 + rate)^(data$fleet_year[i] - 2022))
    
    i <- i + 1
  }
  return(data)
}


# Plotting

discount_cba(test_output, 0.04) %>% 
  filter(vkt_scenario == "vkt_central") %>% 
  ggplot(aes(x = fleet_year, 
             y = total_cost / 1000000000,
             colour = scenario)) +
  geom_point() +
  scale_y_continuous_grattan(limits = c(0, NA))
  
  



           