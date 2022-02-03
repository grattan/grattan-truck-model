
#' This script estimates paypack periods for investment in improved tyre and engine technology. 
#' This is based on incremental cost estimates from the ICCT. 

source("R/00-setup.R")

#' US estimates
#' US estimates are neatly contained here:
#' https://theicct.org/sites/default/files/publications/ICCT_position-brief_HDVenginetech-India_jun2015.pdf (page 1)
#' They are from a baseline 2010 vehicle for a 2025 period (page 2), and are 
#' reported as $2014 US
#' 
#' In the dataset below hte incr_cost is reported as $/% and the perc_potential
#' refers to the total stated gains that can be acheieved

#' Inflation/exchange factors

us_to_aus <- 1.41
us_2014_2021 <- 1.17
eu_2016_2021 <- 1.052
eu_to_aus <- 1.6

us_costs <- tribble(
  ~technology,          ~cost_type,    ~incr_cost,  ~perc_potential,
  "Aero",                  "US",           894,             0.16,
  "Tyres",                 "US",           251,             0.08,
  "Weight \nreduction",    "US",           9666,            0.02,
  "Transmission",          "US",           2184,            0.02,
  "Engine",                "US",           334,             0.24) %>% 
  #and converting to 2021 Aus dollars
  mutate(incr_cost = incr_cost * us_to_aus * us_2014_2021)


#' EU estimates
#' The EU estimates are from two papers - one paper dedicated to the efficiency gains
#' that can be achieved through individual technology, and the other paper to the costs
#' It seems very far to link the two, given the ICCT describes them as 'companion pieces'. 
#' Effectiveness: https://theicct.org/publications/fuel-efficiency-technology-european-heavy-duty-vehicles-baseline-and-potential-2020
#' Costs: https://theicct.org/sites/default/files/publications/ICCT_EU-HDV-tech-2025-30_20180116.pdf
#' 
#' By collating the improvement expected of different tech packages, and the costs, we
#' can calculate the incremental costs for each technology. 
#' 
#' We are only taking esimtates of long haul trucks, to be (relatively) consistent
#' with the US data (which is for sleeper cab tractor-trucks)
#' All EU costs are in euros (2016)
#' 
#' For each dataset, all the relevant tech packages that are estimated are included - 
#' we'll work out a way to pick which ones to include later!
#' 
#' For the package effectiveness and costs, 'long haul' and 2025 year values
#' were used for all. Packages for Transmission are not included in the EU data

eu_engine <- tribble(
  ~technology,    ~package,                 ~cost_type,       ~cost,  ~perc_potential,
  "Engine",       "2017 best in class",         "EU",           284,           0.033,
  "Engine",       "2020+",                      "EU",           2065,          0.095,
  "Engine",       "2020+ with WHR",             "EU",           5763,          0.117,
  "Engine",       "Engine long-term",           "EU",           7081,          0.181) 


#' For estimates involving both tractors and trailers, the costs are assumed with a 
#' 1.4 trailer:tractor ratio (consistent with ICCT assumptions)
eu_aero <- tribble(
  ~technology,    ~package,                 ~cost_type,          ~cost,      ~perc_potential,
  "Aero",       "Incremental",                 "EU",           335 + 1005*1.4,      0.053,
  "Aero",       "Moderate",                    "EU",           446 + 1194*1.4,      0.074,
  "Aero",       "Advanced",                    "EU",           669 + 1194*1.4,      0.085,
  "Aero",       "Long-term",                   "EU",           1673 + 1194*1.4,     0.132) 

eu_tyres <- tribble(
  ~technology,    ~package,                 ~cost_type,           ~cost,      ~perc_potential,
  "Tyres",       "Incremental",                 "EU",           113 + 137*1.4,      0.028,
  "Tyres",       "Moderate",                    "EU",           113 + 172*1.4,      0.058,
  "Tyres",       "Advanced",                    "EU",           122 + 172*1.4,      0.070,
  "Tyres",       "Long-term",                   "EU",           141 + 172*1.4,      0.084) 

eu_weight <- tribble(
  ~technology,                   ~package,      ~cost_type,         ~cost,      ~perc_potential,
  "Weight \nreduction",       "Incremental",     "EU",           81 + 39*1.4,          0.003,
  "Weight \nreduction",       "Moderate",        "EU",           255 + 168*1.4,        0.06,
  "Weight \nreduction",       "Advanced",        "EU",           1044 + 1023*1.4,      0.016,
  "Weight \nreduction",       "Long-term",       "EU",           3755 + 5285*1.4,      0.034) 



#' Binding the EU data and calculting incremental costs

eu_all <- bind_rows(
  eu_engine,
  eu_aero,
  eu_tyres,
  eu_weight) %>% 
  mutate(cost = cost * eu_2016_2021 * eu_to_aus,
         incr_cost = cost / (perc_potential*100)) %>% 
  filter(package %in% c("Engine long-term", "Advanced"))



#' Binding US and EU data ---------------------------------------------

all_technology <- bind_rows(
  eu_all,
  us_costs) %>% 
  # we're only interested in the incr_cost for these estimates
  select(-package, -cost, -perc_potential)
  

# Determining the payback period -------------------------------------

#' We have our costs as an icnremental cost - that is, as a 1\% improvement to efficiency
#' So we want to work out how long it would take for a new vehicle to save the same amount of money
#' as the technology costs. 
#' Our estimates are only for long haul trucks (articulated), so we will use that data 

vkts <- read_rds("data/vkt.rds")
fuel_cons <- read_rds("data/fuel_consumption.rds")
diesel_cost <- 1.33

payback <- left_join(vkts, fuel_cons) %>% 
  filter(fuel_class == "Articulated trucks",
         sales_year == 2022) %>% 
  #calculating a new diesel rate with the 1\% improvement
  mutate(new_rate = diesel_rate_100 * 0.99,
         old_cost_total = vkt * diesel_rate_100 / 100 * diesel_cost,
         new_cost_total = vkt * new_rate / 100 * diesel_cost,
         savings = old_cost_total - new_cost_total,
         cumulative_savings = cumsum(savings)) %>% 
  select(age, fuel_class, cumulative_savings)



all_technology %>% 
  filter(technology %in% c("Engine", "Tyres"))


payback %>% 
  # savings from engine improvements of 5\% and tyre of 3\%
  mutate(engine_5 = cumulative_savings * 5,
         tyre_3 = cumulative_savings * 3)




total_savings <- full_join(payback, 
          
          all_technology %>% 
            filter(technology %in% c("Engine", "Tyres")) %>% 
            mutate(fuel_class = "Articulated trucks")) %>% 
  
  mutate(savings = cumulative_savings - incr_cost)


#' Engine standards we assume 3% per year form baseline - tyres 1.5% per year from baseline (
#' baseline is 2022 Euro 5 engine)















