
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


# Simplifying down data where we can --------------

ev_scenarios <- policy_outcomes %>% 
  filter(scenario %in% c("baseline",
                         "Electric targets",
                         "Euro 6 (2027)",
                         "Electric and Euro 6 (2027)")) %>% 
  #and remove 0 vehicle columns
  filter(total != 0) 


# Separating diesel/electric into separate rows -----------------
  # (this is a slightly painful process)

#First duplicating and binding 
ev_scenarios <- bind_rows(
  # The electric share
  ev_scenarios %>% 
    mutate(total = total * electric_share,
           fuel = "electric"),
  
  #The diesel share 
  ev_scenarios %>% 
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
  rename("sales_year" = year) %>% 
  pivot_wider(names_from = fuel,
              values_from = cost) %>% 
  # Adding it as an incremental cost 
  mutate(incr_cost = electric - diesel,
         fuel = "electric") %>% 
  select(-diesel, -electric) %>% 
  rename("purchase_price" = incr_cost)


ev_scenarios <- left_join(ev_scenarios, 
                          upfront_vehicle_costs) %>% 
  mutate(purchase_price = if_else(is.na(purchase_price) | age != 0, 
                                  0, 
                                  purchase_price * total))


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
        co2_social_cost = 35 * co2_t)




# Maintenance costs --------------------------------------------

#' Maintenance costs are assumed to follow ICCT assumptions, where electric vehicles 
#' have  a cost 30% lower than conventional vehicles. This is broadly consistent with US estimates,
#' which estimate that EVs have approximately 25% lower lifetime maintenance costs 
#' Base maintenance cost estimates are from: https://www.atap.gov.au/parameter-values/road-transport/2-vehicle-operating-cost-voc-components
#' We assume EV costs are 30% lower, and there is no oil requirement (following ICCT : https://theicct.org/wp-content/uploads/2021/06/ICCT_EV_HDVs_Infrastructure_20190809.pdf
#' Given there are many types of trucks in each category specified by ATAP, we take roughyl central estimates
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


#(might need to convert this to a dataset and a join to speed up processing, 
#currently very very slow/struggles)

us_aus_conversion <- 1.42


infrastructure_costs <- tribble( ~fuel,        ~age,   ~volume,       ~fuel_class,                      ~infrastructure_cost,
                                 "electric",    0,        "low",      "Articulated trucks",            180000 * us_aus_conversion,
                                 "electric",    0,        "medium",   "Articulated trucks",            113000 * us_aus_conversion,
                                 "electric",    0,        "high",     "Articulated trucks",            70000 * us_aus_conversion,
                                 "electric",    0,        "low",      "Rigid trucks",                  82000 * us_aus_conversion,
                                 "electric",    0,        "medium",   "Rigid trucks",                  40000 * us_aus_conversion,
                                 "electric",    0,        "high",     "Rigid trucks",                  27000 * us_aus_conversion,
                                 "electric",    0,        "low",      "Non-freight carrying trucks",   82000 * us_aus_conversion,
                                 "electric",    0,        "medium",   "Non-freight carrying trucks",   40000 * us_aus_conversion,
                                 "electric",    0,        "high",     "Non-freight carrying trucks",   27000 * us_aus_conversion,
                                 "electric",    0,        "low",      "Buses",                         82000 * us_aus_conversion,
                                 "electric",    0,        "medium",   "Buses",                         40000 * us_aus_conversion,
                                 "electric",    0,        "high",     "Buses",                         27000 * us_aus_conversion,)


ev_scenarios <- left_join(
    ev_scenarios %>% 
      mutate(volume = case_when(
        fleet_year < 2030 ~ "low",
        fleet_year < 2035 ~ "medium",
        fleet_year >= 2035 ~ "high")),
    
    infrastructure_costs) %>% 
  
  # Multiplying by total to get total costs 
  mutate(infrastructure_cost = if_else(is.na(infrastructure_cost), 
                                             0,
                                             total * infrastructure_cost))



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
    1.015 * 0.03 * (purchase_price + fuel_cost + maintenance_cost_total + infrastructure_cost),
    0))



# Externality costs from electricity generation (health) -------------------

#' Although zero-emission trucks are cleaner than diesel trucks, electricity generation
#' can still pose harm to health as it can produce emissions similarly. 
#' Based on: (https://www.atse.org.au/wp-content/uploads/2019/01/the-hidden-costs-of-electricity.pdf)
#' we are assuming a health externality cost of:
  #' $1/MWh for gas
  #' $3/MH for black coal
  #' $12/MWh for brown coal 
#' These estimates are calculated as the difference between the estimates of greenhouse gas externality costs
#' and total costs (specified as greenhouse gas + health costs)
#' Based on current power generation: (https://www.aer.gov.au/wholesale-markets/wholesale-statistics/generation-capacity-and-output-by-fuel-source-nem)
#' We assume 49.3% black coal, 16.9% brown coal, 5.6% gas in the current grid. 
#' So weighting these three we get externality cost of :

externality_cost_mwh <- 3 * (49.3 / (49.3 + 16.9 + 5.6)) + 
                        12 * (16.9 / (49.3 + 16.9 + 5.6)) + 
                        1 * (5.6 / (49.3 + 16.9 + 5.6))

#' We will assume that this cost scales down proportionally with the emissions intensity of the grid, 
#' which is a proxy for the shares of these emissions intensive generation modes.

base_emission_intensity <- ev_scenarios %>% 
  filter(fleet_year == 2022) %>% 
  head(1) %>% 
  pull(ei_g_wh)


ev_scenarios <- ev_scenarios %>% 
  mutate(health_cost_total = health_cost_total + (elec_use_kw * 0.001 * externality_cost_mwh * (ei_g_wh / base_emission_intensity)) )



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


# With no changes to Euro scenario --------------------------------

ev_scenarios %>% 
  mutate(costs = purchase_price + infrastructure_cost + time_weight_penalty,
         benefits = health_cost_total + fuel_cost + adblue_cost + co2_social_cost + maintenance_cost_total) %>% 
  group_by(scenario, vkt_scenario, fleet_year) %>% 
  summarise(costs = sum(costs),
            benefits = sum(benefits)) %>% 
  filter(scenario %in% c("baseline", "Electric targets")) %>% 
  pivot_wider(names_from = scenario,
              values_from = c(costs, benefits)) %>% 
  filter(fleet_year >= 2022) %>% 
  mutate(costs = `costs_Electric targets` - costs_baseline,
         benefits = `benefits_Electric targets` - benefits_baseline) %>% 
  select(vkt_scenario, fleet_year, costs, benefits) %>% 
  
  #Discounting 
  mutate(disc_costs = (1 / (1 + 0.07)^(fleet_year - 2022)) * costs,
         disc_benefits = (1 / (1 + 0.07)^(fleet_year - 2022)) * benefits) %>% 
  group_by(vkt_scenario) %>% 
  summarise(disc_costs = sum(disc_costs) / 1000000000,
            disc_benefits = sum(disc_benefits) / 1000000000,
            cba = sum(disc_benefits) / -sum(disc_costs))




# Against Euro scenario (2027) --------------------------------------

ev_scenarios %>% 
  mutate(costs = purchase_price + infrastructure_cost + time_weight_penalty,
         benefits = health_cost_total + fuel_cost + adblue_cost + co2_social_cost + maintenance_cost_total) %>% 
  group_by(scenario, vkt_scenario, fleet_year) %>% 
  summarise(costs = sum(costs),
            benefits = sum(benefits)) %>% 
  filter(scenario %in% c("Euro 6 (2027)", "Electric and Euro 6 (2027)")) %>% 
  pivot_wider(names_from = scenario,
              values_from = c(costs, benefits)) %>% 
  filter(fleet_year >= 2022) %>% 
  mutate(costs = `costs_Electric and Euro 6 (2027)` - `costs_Euro 6 (2027)`,
         benefits = `benefits_Electric and Euro 6 (2027)` - `benefits_Euro 6 (2027)`) %>% 
  select(vkt_scenario, fleet_year, costs, benefits) %>% 
  
  #Discounting 
  mutate(disc_costs = (1 / (1 + 0.07)^(fleet_year - 2022)) * costs,
         disc_benefits = (1 / (1 + 0.07)^(fleet_year - 2022)) * benefits) %>% 
  group_by(vkt_scenario) %>% 
  summarise(disc_costs = sum(disc_costs) / 1000000000,
            disc_benefits = sum(disc_benefits) / 1000000000,
            cba = sum(disc_benefits) / -sum(disc_costs))



# Data for the table in report -- summarised costs and benefits --------------

# First for euro 6 scenario

cba_summary_e_6 <- ev_scenarios %>% 
  filter(scenario %in% c("Euro 6 (2027)", "Electric and Euro 6 (2027)"),
         fleet_year >= 2022,
         vkt_scenario == "vkt_central") %>%  
  group_by(scenario, vkt_scenario, fleet_year) %>%
  # Including adblue in maintenance
  summarise(maintenance_cost_total = sum(maintenance_cost_total) + sum(adblue_cost),
            purchase_price = sum(purchase_price),
            fuel_cost = sum(fuel_cost),
            co2_social_cost = sum(co2_social_cost),
            infrastructure_cost = sum(infrastructure_cost),
            time_weight_penalty = sum(time_weight_penalty),
            health_cost_total = sum(health_cost_total)) %>% 
  pivot_longer(cols = 4:10,
               names_to = "cost_type",
               values_to = "cost") %>% 
  mutate(disc_costs = (1 / (1 + 0.07)^(fleet_year - 2022)) * cost) %>% 
  group_by(scenario, cost_type) %>% 
  summarise(disc_costs = sum(disc_costs)) %>% 
  
  pivot_wider(names_from = scenario,
              values_from = disc_costs) %>% 
  
  mutate(avoided_cost_b = (`Euro 6 (2027)` - `Electric and Euro 6 (2027)`) / 1000000000)


# Non-euro 6 scenario 

cba_summary_base <- ev_scenarios %>% 
  filter(scenario %in% c("baseline", "Electric targets"),
         fleet_year >= 2022,
         vkt_scenario == "vkt_central") %>%  
  group_by(scenario, vkt_scenario, fleet_year) %>%
  # Including adblue in maintenance
  summarise(maintenance_cost_total = sum(maintenance_cost_total) + sum(adblue_cost),
            purchase_price = sum(purchase_price),
            fuel_cost = sum(fuel_cost),
            co2_social_cost = sum(co2_social_cost),
            infrastructure_cost = sum(infrastructure_cost),
            time_weight_penalty = sum(time_weight_penalty),
            health_cost_total = sum(health_cost_total)) %>% 
  pivot_longer(cols = 4:10,
               names_to = "cost_type",
               values_to = "cost") %>% 
  mutate(disc_costs = (1 / (1 + 0.07)^(fleet_year - 2022)) * cost) %>% 
  group_by(scenario, cost_type) %>% 
  summarise(disc_costs = sum(disc_costs)) %>% 
  
  pivot_wider(names_from = scenario,
              values_from = disc_costs) %>% 
  
  mutate(avoided_cost_b = (`baseline` - `Electric targets`) / 1000000000)


# Plotting --------------------------------------------

colour_vals <- c("Infrast-\nructure costs" = grattan_grey5,
                 "Vehicle costs" = grattan_darkred,
                 "Time + weight penalty" = grattan_red,
                 "Health costs" = grattan_yellow,
                 "Abated CO2" = grattan_lightyellow,
                 "Mainte-\nnance costs" = grattan_lightblue,
                 "Fuel costs" = grattan_darkblue)


# Euro 6
cba_summary_e_6 %>% 
  mutate(cost_type = case_when(
    cost_type == "infrastructure_cost" ~ "Infrast-\nructure costs",
    cost_type == "purchase_price" ~ "Vehicle costs",
    cost_type == "time_weight_penalty" ~ "Time + weight penalty",
    cost_type == "health_cost_total" ~ "Health costs",
    cost_type == "co2_social_cost" ~ "Abated CO2",
    cost_type == "maintenance_cost_total" ~ "Mainte-\nnance costs",
    cost_type == "fuel_cost" ~ "Fuel costs"),
    cost_type = factor(cost_type, levels = names(colour_vals))) %>% 
  
 arrange(cost_type) %>% 
  mutate(cost_start = lag(cumsum(avoided_cost_b),
                          default = 0),
         cost_end = cumsum(avoided_cost_b),
         xmin = 1,
         xmin = cumsum(xmin),
         xmax = xmin + 1) %>%  
  
  ggplot() +
  
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = grattan_grey4) +
  
  geom_rect(aes(ymin = cost_start,
                ymax = cost_end,
                xmin = xmin - 0.4,
                xmax = xmax - 0.6,
                x = cost_type,
                fill = cost_type,
                colour = cost_type),
            alpha = 0.95) +
  
  geom_segment(data = . %>% 
                 filter(cost_type != "Fuel costs"),
               aes(x = xmin - 0.4,
                   xend = xmax + 0.4,
                   yend = cost_end,
                   y = cost_end)) +
  
  theme_grattan() +
  scale_x_discrete(labels = label_wrap(8)) +
  scale_y_continuous_grattan(labels = scales::label_dollar(suffix = "b"),
                             limits = c(-15, 10)) +
  scale_fill_manual(values = colour_vals) +
  scale_colour_manual(values = colour_vals) +
  
  
  geom_segment(aes(y = 0, yend = 6.91, x = 6, xend = 6), colour = grattan_grey3) +
  geom_segment(aes(y = 6.91, yend = 6.91, x = 6, xend = 6.1), colour = grattan_grey3) +
  geom_segment(aes(y = 0, yend = 0 , x = 6, xend = 6.1), colour = grattan_grey3) +
  
  grattan_label(aes(x = 6 - 0.1,
                    y = 4,
                    label = "Estimated net \nbenefit of $6.9b"),
                hjust = "right",
                fontface = "bold",
                colour = grattan_grey4) +
  
  grattan_label(data = . %>% 
                  filter(avoided_cost_b < 0),
                aes(x = cost_type,
                    y = cost_end - 1.4,
                    label = paste0("-$", round(abs(avoided_cost_b), digits = 1), "b"),
                    colour = cost_type)) +
  grattan_label(data = . %>% 
                  filter(avoided_cost_b > 0),
                aes(x = cost_type,
                    y = cost_end + 1.4,
                    label = paste0("$", round(abs(avoided_cost_b), digits = 1), "b"),
                    colour = cost_type)) +
  
  
  labs(title = "The benefits of accelerating zero emission truck uptake outweighs the costs",
       subtitle = "Estimated costs and benefits of zero emissions targets for heavy vehicles",
       x = NULL,
       caption = "Details of CBA metholody are included in appendix XX. Calculated using a 7% discount rate.")



  
#grattan_save(filename = "atlas/cba-results.pdf",
#             type = "normal",
#             save_pptx = TRUE)

# Non-euro 6 (the health costs savings are significantly larger - \$3-4b vs ~\$ 1b)
cba_summary_base %>% 
  mutate(cost_type = factor(cost_type, 
                            levels = cba_summary_base %>% 
                              arrange(avoided_cost_b) %>% 
                              pull(cost_type))) %>% 
  arrange(cost_type) %>% 
  mutate(cumulative_cost = cumsum(avoided_cost_b)) %>% 
  
  ggplot() +
  geom_col(aes(x = cost_type,
               y = avoided_cost_b,
               fill = cost_type))

           