
#' The previous script looked at business as usual emissions - this script is going to look
#' at how the policy options we are considering compare to that business as usual scenario

source("R/07-fleet-turnover.R")

#' Policies under consideration:
  #' tyre and engine regulation (shorter term)
  #' Full vehicle CO2 regulation for specific vehicle segments (longer term)
  #' Electric sales targets (fleet or manufacturer level)
  #' 


# Baseline fleet

co2_baseline <- all_fleets %>% 
  mutate(tyre_improvement = 1,
         engine_efficiency = 1,
         co2_scenario = "baseline") 


# Dataset with no improvements to edit ----------------------------------------

# First, because there is already an assumed efficiency improvement in the original fleet,
# simulation, we need to remove this and assume all vehicle efficiency from 2021 onwards is constant 
# We'll do this by deleting the fuel consumption column, updating the fuel consumption 
# data for this purpose, and rejoining the column

# Because we're doing engine improvements relative to a 2021 baseline, but we're also assuming that
# the vehicles continue to improve by 0.5% YoY prior to the engine standard implementation,
# we are only holding figures after 2024 constant at 2021 levels. Before the engine standard is
# applied, this will give the effect that vehicle fuel consumption drops over 2022/2023,
# only to bounce back to 2021 levels in 2024. But it will work once the engine efficiency is applied

source("R/04-fuel-consumption.R")
constant_fuel_cons <- all_fuel_consumption %>% 
  filter(sales_year <= 2021) %>% 
  arrange(type, age, sales_year) %>% 
  group_by(type, age) %>% 
  complete(sales_year = (2024:2050)) %>% 
  arrange(type, age, sales_year) %>% 
  na.locf() %>% 
  rename("fuel_class" = type)

constant_fuel_cons <- bind_rows(
  constant_fuel_cons,
  
  all_fuel_consumption %>% 
    filter(sales_year %in% (2022:2023)) %>% 
    rename("fuel_class" = type)) %>% 
  arrange(fuel_class, age, sales_year)


engine_fleets <- left_join(
  all_fleets %>% 
    select(-diesel_rate_100),
  
  constant_fuel_cons) %>% 
  
  # and now we are going to get rid of the `fuel_used` and `co2` columns for the moment,
  # because they are no longer accurate (remove to avoid making an error)
  select(-fuel_used, -co2)

# Engine regulation --------------------------------------------------------

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
         
         all_fleets %>% 
           ungroup() %>% 
           select(fuel_class) %>% 
           unique()) %>% 
  mutate(efficiency = case_when(
    fuel_class == "Non-freight carrying trucks" ~ 1,
    fuel_class != "Non-freight carrying trucks" ~ efficiency))

# Applying the targets to the fleet, assuming they are perfectly met. 
# Now creating this as our scenario for engine only improvements ---------------

engine_only <- left_join(engine_fleets,
          
          efficiency_targets %>% 
            rename("sales_year" = year)) %>% 
  mutate(tyre_improvement = 1,
         co2_scenario = "engine only")


  
# Tyre standards --------------------------------------------------------------

# For tyre standards, we are going to assume that a total of 8% improvement from a 
# baseline 2020 vehicle are available by 2028, corresponding to 2% improvements per year
# when starting in 2024

# We are going to assume this is on top of the 0.5% YoY assumption, given those improvements
# are more likely to come from engine and other improvements

tyre_only <- all_fleets %>% 
  select(-co2, -fuel_used) %>% 
  mutate(co2_scenario = "tyre only",
         #assuming no engine improvement beyong the 0.5%/y
         engine_efficiency = 1)

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



# tyre and engine standards combined --------------------------------------

# Assuming a scenario where both engine and tyre standards are incorperated 

# Taking our engine improvement dataset

engine_and_tyre <- engine_only %>% 
  select(-tyre_improvement) %>% 
  mutate(co2_scenario = "engine and tyre")

engine_and_tyre <- left_join(
  engine_and_tyre,
  
  tyre_targets)



# Calculating the CO2 emitted --------------------------------------------


co2_scenarios <- bind_rows(
  engine_only,
  tyre_only,
  engine_and_tyre)

#And calculting the co2 emitted
co2_scenarios <- co2_scenarios %>% 
  mutate(co2 = diesel_fuel_to_co2(diesel_rate_100 * tyre_improvement * engine_efficiency) * vkt) %>% 
  mutate(fuel_used = (vkt / 100) * diesel_rate_100 * tyre_improvement * engine_efficiency)


# Joining with the baseline and calulating the totals
co2_scenarios <- bind_rows(
  co2_scenarios,
  co2_baseline) %>% 
  group_by(fleet_year, fuel_class, vkt_scenario, co2_scenario) %>% 
  summarise(total_co2_mt = sum(co2 * total / 1000000000000)) 


# Plotting ----------------------------------------------

colour_vals <- c("baseline" = grattan_darkred,
                 "tyre only" = grattan_red,
                 "engine only" = grattan_orange,
                 "engine and tyre" = grattan_yellow,
                 "historical" = grattan_grey4)

  
co2_scenarios %>% 
  group_by(co2_scenario, fleet_year, vkt_scenario) %>% 
  summarise(total_co2_mt = sum(total_co2_mt)) %>% 
  pivot_wider(names_from = vkt_scenario,
              values_from = total_co2_mt) %>% 
  
  ggplot() +

  geom_ribbon(aes(
    x = fleet_year,
    ymin = vkt_lower,
    ymax = vkt_upper,
    fill = co2_scenario),
    colour = NA,
    alpha = 0.2) +
  
  geom_line(aes(x = fleet_year, 
                y = vkt_central, 
                colour = co2_scenario)) +
  
  theme_grattan(legend = "top") +
  scale_y_continuous_grattan(limits = c(0, 33)) +
  scale_x_continuous_grattan() +
  scale_colour_manual(values = colour_vals) +
  scale_fill_manual(values = colour_vals) +
  labs(title = "Total HDV CO2 emissions",
       subtitle = "Annual Co2 (Mt)",
       y = " ",
       x = NULL)


# -------------------------------------

diser_co2 <- read_xlsx("data/diser-co2-forecasts.xlsx",
                           sheet = "Sheet1") %>% 
  pivot_longer(cols = (2:11),
               values_to = "emissions_mt",
               names_to = "sector") %>% 
  clean_names() %>%   
  filter(sector %in% c("Rigid trucks", "Articulated trucks", "Buses"),
         year <= 2021) %>% 
  mutate(sector = factor(sector, 
                         levels = c("Articulated trucks", 
                                    "Rigid trucks", "Buses")),
         co2_scenario = "historical") %>% 
  rename("fuel_class" = sector,
         "total_co2_mt" = emissions_mt,
         "fleet_year" = year) %>% 
  mutate(vkt_scenario = "vkt_central")


historical_projection_comb <- bind_rows(
  co2_scenarios %>% 
    filter(fleet_year >= 2021),
  diser_co2)


historical_projection_comb %>% 
  filter(vkt_scenario == "vkt_central") %>% 
  group_by(co2_scenario, fleet_year) %>% 
  summarise(total_co2_mt = sum(total_co2_mt)) %>% 
  
  ggplot(aes(x = fleet_year, 
             y = total_co2_mt,
             colour = co2_scenario)) +
  geom_line() +
  theme_grattan(legend = "top") +
  scale_y_continuous_grattan(limits = c(0, 33)) +
  scale_x_continuous_grattan(limits = c(2000, 2035)) +
  scale_colour_manual(values = colour_vals) +
  scale_fill_manual(values = colour_vals) +
  labs(title = "Total HDV CO2 emissions",
       subtitle = "Annual Co2 (Mt)",
       y = " ",
       x = NULL)



