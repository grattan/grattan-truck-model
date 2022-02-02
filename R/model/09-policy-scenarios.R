
#' Policy scenarios ===================================================
all_fleets <- read_rds("data/all_fleets.rds")

#' This script takes the baseline BAU case as estimated in the 08-fleet-turnover
#' script, and applies different policy scenarios to that estimate. 

#' Policy scenarios include:
    #' Euro 6 - both in 2024 and 2027
    #' Tyre standards
    #' Engine standards
    #' Electric vehicle targets
#' As well as different combinations of the policies

baseline <- all_fleets %>% 
  mutate(scenario = "baseline",
         #assuming no tyre/engine improvements
         tyre_improvement = 1,
         engine_efficiency = 1)



#' Euro 6 --------------------------------

#' First we want to duplicate this dataset to create our euro 6 and baseline scenarios
#' We also want to add our Euro 6 assumptions - for specifically, that PM10 matter
#' rates will decrease by 60% with euro 6 for new vehicles compared to the 2015 and younger pollutant rates,
#' and that NOx emissions will decrease by 80% compared to 2015 and younger vehicle rates
#' these values are based on the limit values of euro 5 vs 6 
#' (https://www.infrastructure.gov.au/sites/default/files/migrated/vehicles/environment/forum/files/heavy-vehicle-emission-standards-for-cleaner-air.pdf page 24)
#' Given the lack of data, we assume no change to other pollutant rates

euro_scenarios <- bind_rows(
  
  baseline %>% 
    mutate(scenario = "Euro 6 (2024)",
           
           pollutant_rate = ifelse(
             pollutant == "ex_nox_l" & sales_year >= 2024, 
             pollutant_rate * 0.2, 
             pollutant_rate),
           
           pollutant_rate = ifelse(
             pollutant %in% c("ex_pm10_l", "ex_pm25_l")  & sales_year >= 2024, 
             pollutant_rate * 0.4, 
             pollutant_rate)),
  
  baseline %>% 
    mutate(scenario = "Euro 6 (2027)",
           pollutant_rate = ifelse(
             pollutant == "ex_nox_l" & sales_year >= 2027, 
             pollutant_rate * 0.2, 
             pollutant_rate),
           
           pollutant_rate = ifelse(
             pollutant %in% c("ex_pm10_l", "ex_pm25_l")  & sales_year >= 2027, 
             pollutant_rate * 0.4, 
             pollutant_rate))
  ) %>% 
  
  #assuming no tyre or engine improvements
  mutate(tyre_improvement = 1,
         engine_efficiency = 1)



#' Electric vehicle targets ==================================================

#' The electric vehicle targets are specified as:
    #' Between 2024 and 2028, targets only apply to operators with over XX vehicles in 
    #' their fleet (approximately yy% of rigid trucks and zz% of articulated)
    #' After 2028, targets apply to operators with over XX vehicles (approximately 
    #' yy% of rigid trucks and zz% of articulated)
    #' 
  #' Rigid truck targets are:
    #' 2% in 2024
    #' 5% by 2025
    #' 10% by 2027
    #' 30% by 2030, 
    #' 60% by 2035
  #' Articulated truck targets are:
    #' 1% by 2024,
    #' 3% by 2027
    #' 10% by 2030, 
  
#' it is assume that all operators meet their targets with electric vehicles. While 
#' this might not be true (some might meet through hydrogen etc.), it's unlikely to affect
#' the overall results (and it is very likely a substantial proportion will be electric.)


electric_targets <- read_xlsx("data-raw/electric-uptake.xlsx",
          sheet = "electric-targets") %>% 
  mutate(electric_target = electric_target / 100)



ev_scenario <- left_join(baseline, 
                         electric_targets) %>% 
  #replacing with 0 if no target applies
  mutate(electric_target = if_else(is.na(electric_target), 0, electric_target),
         share_applied = if_else(is.na(share_applied), 0, share_applied)) %>% 
  
  # Assuming that if the BAU is higher than the target, it applies instead,
  # and collapsing the share applied and target into one value across the fleet
  mutate(electric_share = pmax(electric_target * share_applied, electric_share),
         scenario = "Electric targets") %>% 
  select(-electric_target, -share_applied)



#' Electric and Euro 6 =======================================================
#' Combining both euro 6 and electric scenarios 

ev_euro_scenarios <- left_join(euro_scenarios, 
                      electric_targets) %>% 
  
  #replacing with 0 if no target applies
  mutate(electric_target = if_else(is.na(electric_target), 0, electric_target),
         share_applied = if_else(is.na(share_applied), 0, share_applied)) %>% 
  
  # Assuming that if the BAU is higher than the target, it applies instead,
  # and collapsing the share applied and target into one value across the fleet
  mutate(electric_share = pmax(electric_target * share_applied, electric_share),
         scenario = paste0("Electric and ", scenario)) %>% 
  select(-electric_target, -share_applied)




#' Tyre and Engine standards ==================================================

# First, because there is already an assumed efficiency improvement in the original fleet,
# simulation, we need to remove this and assume all vehicle efficiency from 2021 onwards is constant 
# We'll do this by deleting the fuel consumption column, updating the fuel consumption 
# data for this purpose, and rejoining the column

# Because we're doing engine improvements relative to a 2021 baseline, but we're also assuming that
# the vehicles continue to improve by 0.5% YoY prior to the engine standard implementation,
# we are only holding figures after 2024 constant at 2021 levels. Before the engine standard is
# applied, this will give the effect that vehicle fuel consumption drops over 2022/2023,
# only to bounce back to 2021 levels in 2024. But it will work once the engine efficiency is applied

source("R/model-inputs/04-fuel-consumption.R")

constant_fuel_cons <- all_fuel_consumption %>% 
  filter(sales_year <= 2021,
         fuel_class != "Non-freight carrying vehicles") %>% 
  arrange(fuel_class, age, sales_year) %>% 
  group_by(fuel_class, age) %>% 
  complete(sales_year = (2024:2060)) %>% 
  arrange(fuel_class, age, sales_year) %>% 
  na.locf()

constant_fuel_cons <- bind_rows(
  constant_fuel_cons,
  
  all_fuel_consumption %>% 
    filter(fuel_class == "Non-freight carrying vehicles"),
  
  all_fuel_consumption %>% 
    filter(sales_year %in% (2022:2023)) %>% 
  arrange(fuel_class, age, sales_year))


engine_fleets <- left_join(
  baseline %>% 
    select(-diesel_rate_100,
           -engine_efficiency),
  
  constant_fuel_cons) 
  



#' Engine standards ---------------------

# Our engine regulation assumes we can make a 25% improvement to engine efficiency 
# by 2028. This is very ambitious currently and we need to check if it's realistic. 

efficiency_targets <- seq(from = 1980, to = 2040) %>% 
  as_tibble() %>% 
  rename(year = "value") %>% 
  mutate(engine_efficiency = case_when(
    year < 2024 ~ 1,
    year == 2024 ~ 0.95,
    year == 2025 ~ 0.90,
    year == 2026 ~ 0.85,
    year == 2027 ~ 0.80,
    year >= 2028 ~ 0.75))

# Now creating a dataset for each vehicle type
# Note that as the efficiency targets do not apply to non-freight carrying vehiles,
# these are assumed to have an unchanged efficiency over time (100)
efficiency_targets <- crossing(efficiency_targets,
                               
                               baseline %>% 
                                 ungroup() %>% 
                                 select(fuel_class) %>% 
                                 unique()) %>% 
  mutate(engine_efficiency = case_when(
    fuel_class == "Non-freight carrying trucks" ~ 1,
    fuel_class != "Non-freight carrying trucks" ~ engine_efficiency))

# Applying the targets to the fleet, assuming they are perfectly met. 
# Now creating this as our scenario for engine only improvements ---------------

engine_only <- left_join(engine_fleets,
                         
                         efficiency_targets %>% 
                           rename("sales_year" = year)) %>% 
  mutate(tyre_improvement = 1,
         scenario = "engine only")


# Tyre standards -------------------------------------

# For tyre standards, we are going to assume that a total of 8% improvement from a 
# baseline 2020 vehicle are available by 2028, corresponding to 2% improvements per year
# when starting in 2024

# We are going to assume this is on top of the 0.5% YoY assumption, given those improvements
# are more likely to come from engine and other improvements

tyre_only <- baseline %>%
  mutate(scenario = "tyre only",
         #assuming no engine improvement beyong the 0.5%/y
         engine_efficiency = 1) %>% 
  select(-tyre_improvement)

# Across the fleet we are assuming that between 2024 an 2028 we can get 2% improvements
# per year to fuel consumption from tyre tech

tyre_targets <- seq(from = 1980, to = 2040) %>% 
  as_tibble() %>% 
  rename(year = "value") %>% 
  mutate(tyre_improvement = case_when(
    year < 2024 ~ 1,
    year == 2024 ~ 0.98,
    year == 2025 ~ 0.96,
    year == 2026 ~ 0.94,
    year == 2027 ~ 0.92,
    year >= 2028 ~ 0.92))

tyre_targets <- crossing(
  tyre_targets,
  
  all_fleets %>% 
    ungroup() %>% 
    select(fuel_class) %>% 
    unique()) %>% 
  #at the moment this is an assumption it only applies to new vehicles
  rename("sales_year" = year)

tyre_only <- left_join(
  tyre_only,
  
  tyre_targets)

#' Tyre and engine standards combined ---------------------

#' Assuming a scenario where both engine and tyre standards are incorperated 
#' Taking our engine improvement dataset

engine_and_tyre <- engine_only %>% 
  select(-tyre_improvement) %>% 
  mutate(scenario = "engine and tyre standards")

engine_and_tyre <- left_join(
  engine_and_tyre,
  
  tyre_targets)



#' Tyre, engine and electric scenarios combined ===========================

engine_tyre_electric <- left_join(engine_and_tyre, 
                                  electric_targets) %>% 
  
  #replacing with 0 if no target applies
  mutate(electric_target = if_else(is.na(electric_target), 0, electric_target),
         share_applied = if_else(is.na(share_applied), 0, share_applied)) %>% 
  
  # Assuming that if the BAU is higher than the target, it applies instead,
  # and collapsing the share applied and target into one value across the fleet
  mutate(electric_share = pmax(electric_target * share_applied, electric_share),
         scenario = paste0("Electric and ", scenario)) %>% 
  select(-electric_target, -share_applied) 







#' Joining all scenarios =======================================

policy_scenarios <- bind_rows(
  baseline,
  euro_scenarios,
  ev_scenario,
  ev_euro_scenarios,
  engine_only,
  tyre_only,
  engine_and_tyre,
  engine_tyre_electric)  %>% 
  #re-ordering variables for ease of use and making neater
  relocate(scenario, vkt_scenario, fuel_class, fleet_year, sales_year, total, region, age) %>% 
  select(-share)


write_rds(policy_scenarios, "data/policy-scenarios.rds")












