

#' Voucher scheme -------------------------------------------------------
#' This script calculates values that are justifiable for a voucher scheme in terms 
#' of ZE-HDV subsidies

source("R/00-setup.R")
source("R/costs-modelling/estimate_tco.R")



#' Calculating TCO gap---------------------------------------------------

#' For the moment this is just a point estimate, but we'll add scenarios later 

estimated_tco <- estimate_tco()

estimated_tco <- bind_rows(estimate_tco() %>% 
                             mutate(cost_scen = "central"),
                           
                           estimate_tco(electricity_price = 0.17,
                                        diesel_price = 1.28) %>% 
                             mutate(cost_scen = "upper"),
                           
                           estimate_tco(electricity_price = 0.14,
                                        diesel_price = 1.38) %>% 
                             mutate(cost_scen = "lower"))


tco_gap <- estimated_tco %>% 
  filter(age <= 10) %>% 
  pivot_longer(cols = 10:15,
               names_to = "cost_type",
               values_to = "cost") %>% 
  #Discounting at 7%, where year = 0 when the vehicle is sold
  mutate(cost = (1 / (1 + 0.07)^(fleet_year - sales_year)) * cost,
         total_cost = (1 / (1 + 0.07)^(fleet_year - sales_year)) * total_cost) %>% 
  
  group_by(sales_year, fuel, cost_type, fuel_class, cost_scen) %>% 
  summarise(cost = sum(cost)) %>% 
  filter(fuel_class %in% c("Articulated trucks", "Rigid trucks")) %>% 
  ungroup() %>% 
  count(sales_year, fuel, fuel_class, cost_scen, wt = cost, name = "cost") %>% 
  pivot_wider(names_from = fuel, values_from = cost) %>% 
  mutate(tco_gap = electric - diesel,
         tco_gap_half = tco_gap / 2) %>% 
  filter(sales_year %in% (2024:2030)) %>% 
  select(-diesel, -electric) 




#' Public benefit -------------------------------------------------------

#' We also need to estimate the public benefit expected compared to a diesel vehicle
#' For this we need to estimate the pollution, co2 and noise costs in each year for
#' each vehicle type

#' For this we can use the work we have already done in the CBA estimates,
#' and imply simplify these down to represent individual vehicles

source("R/costs-modelling/CBA.R")

#Calcualitng the total social cost for diesel/electric
public_cost_gap <- ev_scenarios %>% 
  ungroup() %>% 
  filter(scenario == "Electric and Euro 6 (2027)",
         fuel_class %in% c("Articulated trucks", "Rigid trucks")) %>% 
  mutate(cost_scen = substr(vkt_scenario, start = 5, stop = 100)) %>% 
  select(scenario, cost_scen, fuel_class, fuel, fleet_year, sales_year, age, total, health_cost_total,
         co2_social_cost, noise_cost) %>% 
  # Discounting and converting to individual vehicle costs
    # We're applying the discounting from the point of sales - i.e. year = 0 when the vehicle is sold
  mutate(health_cost_total = (1 / (1 + 0.07)^(fleet_year - sales_year)) * health_cost_total / total,
         co2_social_cost = (1 / (1 + 0.07)^(fleet_year - sales_year)) * co2_social_cost / total,
         noise_cost = (1 / (1 + 0.07)^(fleet_year - sales_year)) * noise_cost / total) %>% 
  group_by(scenario, cost_scen, fuel_class, fuel, sales_year) %>% 
  summarise(health_cost_total = sum(health_cost_total),
            co2_social_cost = sum(co2_social_cost),
            noise_cost = sum(noise_cost),
            total_public_cost = sum(health_cost_total + co2_social_cost + noise_cost)) %>% 
  filter(sales_year %in% (2024:2030)) %>% 
  # Calculating the gap in public cost between diesel/electric
  select(scenario, cost_scen, fuel_class, fuel, sales_year, total_public_cost) %>% 
  pivot_wider(names_from = fuel, values_from = total_public_cost) %>% 
  mutate(public_cost_gap = diesel - electric) %>% 
  select(-electric, -diesel)




#' Joining the datasets and calculating total spend -------------------------

#Joining sales/targets datasets to get an estimate of EV's sold per year by class

sales <- read_rds("data/sales.rds") %>% 
  mutate(fuel_class = case_when(
    type == "articulated" ~ "Articulated trucks",
    type %in% c("light_rigid", "heavy_rigid") ~ "Rigid trucks")) %>% 
  drop_na(fuel_class) %>% 
  select(fuel_class, sales_year, sales) %>% 
  count(fuel_class, sales_year, wt = sales, name = "sales") %>% 
  filter(sales_year >= 2024)

targets <- read_excel ("data-raw/electric-uptake.xlsx",
                       sheet = "electric-targets")

ev_sales <- left_join(sales, targets) %>% 
  mutate(ev_sales = round(electric_target / 100 * sales, 0)) %>% 
  select(fuel_class, sales_year, ev_sales) %>% 
  filter(sales_year %in% (2024:2030))


# From the estimate_tco results and public costs, estimate the spending 

voucher_estimates <- left_join(public_cost_gap, tco_gap) %>% 
  rowwise() %>% 
  mutate(public_cost_gap = case_when(
    cost_scen == "lower" ~ public_cost_gap * 0.9,
    cost_scen == "upper" ~ public_cost_gap * 1.1,
    cost_scen == "central" ~ public_cost_gap),
    gov_subsidy = if_else(
      min(tco_gap_half, public_cost_gap) > 0,
      min(tco_gap_half, public_cost_gap), 0),
    tco_gap_half = if_else(
      min(tco_gap_half, public_cost_gap) > 0,
      min(tco_gap_half, public_cost_gap), 0)) %>% 
  
  #calculate total spend
  left_join(ev_sales) %>%
  mutate(total_gov_spend = ev_sales * gov_subsidy / 1000000) %>% 

  select(cost_scen, sales_year, fuel_class, tco_gap_half, public_cost_gap, gov_subsidy, total_gov_spend) %>% 
  #rounding and formatting nicely 
  mutate(tco_gap_half = round(tco_gap_half, -2) %>% format(big.mark = ",", scientific=FALSE),
         public_cost_gap = round(public_cost_gap, -2) %>% format(big.mark = ",", scientific=FALSE),
         gov_subsidy = round(gov_subsidy, -2) %>% format(big.mark = ",", scientific=FALSE),
         total_gov_spend = round(total_gov_spend, 1) %>% format(big.mark = ",", scientific=FALSE)) %>% 
  
  pivot_wider(names_from = cost_scen, values_from = c(tco_gap_half, public_cost_gap, gov_subsidy, total_gov_spend)) %>% 
  #Formatting into a table for easier copying into latex
  #values are rounded to nearest hundred for the moment
  mutate(tco_gap_half = paste0("$", tco_gap_half_central, " ($", tco_gap_half_lower, " - $", tco_gap_half_upper, ")"),
         public_cost_gap = paste0("$", public_cost_gap_central, " ($", public_cost_gap_lower, " - $", public_cost_gap_upper, ")"),
         gov_subsidy = paste0("$", gov_subsidy_central, " ($", gov_subsidy_lower, " - $", gov_subsidy_upper, ")"),
         total_gov_spend = paste0("$", total_gov_spend_central, "m ($", total_gov_spend_lower, "m - $", total_gov_spend_upper, "m)")) %>% 
  select(fuel_class, sales_year, tco_gap_half, public_cost_gap, gov_subsidy, total_gov_spend)


#' Converting to a latex table

#install.packages("gt")
library(gt)

voucher_estimates %>% gt::gt() %>% gt::as_latex() %>% clipr::write_clip()


