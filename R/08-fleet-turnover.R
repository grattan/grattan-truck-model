
#' This script creates truck fleets based on past and future sales data
#' 
#' Each vehicle is also assigned a fuel efficiency value, pollutant values, and 
#' driving behaviour (VKTs, fuel consumed etc.)

source("R/00-setup.R")
source("R/00-data-inputs.R")

#Function to build the fleet up. ------------------------------
  #the data argument refers to the joined survival curves and sales data set
  #(built as survival_sales)

  #the sales_year_start variable indicates how far back we consider - i.e. if 
  #it is 1990, we are considering all vehicles sold since 1990. 
  
  #the fleet_year_start and fleet_year_end variables specify what years between
  #(inclusive) that we are building the fleets for. Each takes account fo vehicles sold
  #between 'sales_year_start` up to the year the fleet is being built for. 

survival_sales <- full_join(survival_curves, sales)

fleet_year_start <- 2020
fleet_year_end <- 2040
sales_year_start <- 1980

#This function creates the fleet for a given year (the function inside 
#generates the sales for a given year and a given fleet)

fleet_build_all <- function(fleet_year) {

  #getting the sales for a given year - where the given year is 'sale_year'
    fleet_build_year <- function(sale_year) {
      
      #message("fleet_year is: ", fleet_year)
      #message("sale_year is: ", sale_year)
      
      current_survival_sales <- survival_sales %>% 
        filter(age == fleet_year - sale_year,
               sales_year == sale_year)
      
      return(current_survival_sales)
      
    }

    current_fleet <- map_dfr(.x = (sales_year_start:fleet_year), 
                             .f = fleet_build_year) %>% 
      mutate(fleet_year = fleet_year)
}    


#Mapping over this function to generate a fleet for all the years we are
#interested in 
all_fleets <- map_dfr(.x = fleet_year_start:fleet_year_end,
                      .f = fleet_build_all) 



#Adding fleet details - VKTs, fuel ---------------------------------------

all_fleets <- all_fleets %>% 
  mutate(total = (sales * proportion_surviving)) %>% 
  select(type, sales_year, age, total, fleet_year) %>% 
  filter(total > 0) %>% 
  #assigning the relevant pollutant classes
  
  #' For the pollutant data, we are going to assume that a 'medium' vehicle is
  #' an articulated truck, and a 'medium truck' is a light/heavy rigid truck
  #' NEED TO ACTUALLY CHECK THE DEFINITIONS ON THIS
  mutate(#pollutant_class = case_when(
    #type == "light_rigid" ~ "medium_truck",
    #type == "heavy_rigid" ~ "medium_truck",
    #type == "articulated" ~ "heavy_truck",
    #type == "buses" ~ "buses",
     #type == "non_freight" ~ "medium_truck"
  #),
  fuel_class = case_when(
    type == "light_rigid" ~ "Rigid trucks",
    type == "heavy_rigid" ~ "Rigid trucks",
    type == "articulated" ~ "Articulated trucks",
    type == "buses" ~ "Buses",
    type == "non_freight" ~ "Non-freight carrying trucks"))

fuel_vkt <- fuel_vkt %>% 
  rename("fuel_class" = type) %>% 
  mutate(age = as.numeric(age))

all_fleets <- all_fleets %>% 
  mutate(age = as.integer(age))

#Now joining with our VKT and fuel data (we will deal with pollutant data seperately later)
all_fleets <- left_join(all_fleets, fuel_vkt) 

vkt <- vkt %>% 
  rename("fuel_class" = type)
  
all_fleets <- inner_join(all_fleets, vkt)



# Adjusting fleet activity for GDP projections --------------------------------

# We have estimated vehicle sales based on population growth figures, but this
# is not the only thing that will affect the freight task - economy wide
# conditions are also likely to affect how the road freight task grows. 

# For this, following the approach used in GHG accounts, we are going to use GDP
# forecasts projected in the 2021 intergenerational report to scale the `activity` of the
# freight fleet. 


gdp_growth <- read_xlsx("data/IG-report-data.xlsx",
                        sheet = "gdp-growth") %>% 
  clean_names() %>% 
  select(year, real_gdp_growth) %>% 
  #changing eyar to calendar as with pop growth
  mutate(year = paste0("20", substr(year, 6, 7)),
         year = as.numeric(year)) %>% 
  #we want to set 2019 as the baseline for this data and make it cumulative 
  filter(year >= 2019)


gdp_coefficient <- gdp_growth %>% 
  mutate(real_gdp_growth = case_when(
    year == 2019 ~ 0,
    year != 2019 ~ real_gdp_growth),
    #assuming a coefficient of 0.35 for the central scenario (this is essential the 
    # per capita freight component)
    gdp_central = cumprod((real_gdp_growth * 0.35 * 0.01 + 1)),
    #in the upper assume gdp has effect of 60%
    gdp_upper = cumprod((real_gdp_growth * 0.6 * 0.01 + 1)),
    #in the lower scenario assume gdp has 10%
    gdp_lower = cumprod((real_gdp_growth * 0.1 * 0.01 + 1))) %>% 
  select(-real_gdp_growth) %>% 
  rename("fleet_year" = year)


all_fleets <- left_join(all_fleets, gdp_coefficient) %>% 
  mutate(vkt_central = vkt * gdp_central,
         vkt_upper = vkt * gdp_upper,
         vkt_lower = vkt * gdp_lower) %>% 
  select(-vkt) %>% 
  pivot_longer(cols = 11:13,
               names_to = "vkt_scenario",
               values_to = "vkt") %>% 
  select(-gdp_upper, -gdp_central, -gdp_lower)


# Adjusting Rigid trucks ------------------------------

# Given that we're overestimating the rigid trucks, we're also going to scale
# category to make consistent with historical data (it's a 3.6% diff)

all_fleets <- all_fleets %>% 
  mutate(vkt = case_when(
    fuel_class == "Rigid trucks" ~ vkt * (1 - 0.036),
    fuel_class != "Rigid trucks" ~ vkt
  ))



# Calculting totals -----------------------------------

#Preparing the emissions factors for consistency
#fin_emissions_factors <- emissions_factors %>% 
#  rename("pollutant_class" = vehicle_type) %>% 
#  select(pollutant_class, year_start, year_end, nox, pm10)



#Joining the emissions factors to vehicles
#all_fleets <- full_join(all_fleets, 
                        #fin_emissions_factors) %>% 
  
  #Ensuring that the emissions factors are properly assigned 
  #mutate(match = (sales_year >= year_start & sales_year <= year_end)) %>% 
  #filter(match == TRUE) %>% 
  #select(-match, -pollutant_class, -year_start, -year_end) %>% 
  #mutate(co2 = diesel_fuel_to_co2(diesel_rate_100 ) * vkt) %>% 
  #mutate(fuel_used = (vkt / 100) * diesel_rate_100)

# Adding rural/urabn split ------------------------------------

source("R/06-vkt-urban-rural-split.R")

all_fleets <- left_join(all_fleets, urban_rural %>% 
                          rename("fuel_class" = vehicle_type)) %>% 
  mutate(vkt = vkt * share) 





# Electric/ZEV estimates --------------------------------------------------
# Joining electric estimates to dataset from previous script

electric_uptake <- read_rds("data/electric_uptake.rds") %>% 
  mutate(electric_share = electric_share / 100)

all_fleets <- left_join(all_fleets,
          electric_uptake) 



# Energy intensity of the grid -----------------------------------------

# So we can estimate the CO2 produced by powering the electric vehicles, we will
# include an estimte of grid itnensity (AEMO step change scenario)

energy_intensity <- read_xlsx("data/emissions-intensity-grid.xlsx") %>% 
  clean_names() %>% 
  rename("fleet_year" = year)

# This only includes data from 2021 onwards, but this is not a problem given that
# the electric share < 2021 is assumed 0 in all classes. 
# we will just set the value to 0 to avoid problems with NAs

all_fleets <- left_join(
  all_fleets,
  energy_intensity) %>% 
  mutate(ei_g_wh = if_else(is.na(ei_g_wh), 0, ei_g_wh))





# Pollutant emissions -------------------------------------------------


source("R/00-setup.R")
source("R/07-fleet-turnover.R")


#' To get the pollutant estimates, first we're going to assign pollutant emissions 
#' values to vehicles based on the vehicle type and year of manufacture. The values used 
#' for exhaust emissions are unpublished BITRE estimates (passed on by David Cosgrove), 
#' as determined from a combination of models (include the COPERT suite for example). 
#' 
#' Non-exhuast emissions, including secondary pollutants, tyre wear, brake wear,
#' and re-entrained road dust, are from various other sources. Secondary pollutants
#' are calulcated later, using an offset ratio to estimate the conversion of NOx to 
#' PM2.5 nitrate fraction, and SOx to sulphates. The offset ratios used are 100:1 for NOx to nitrate
#' (PM2.5), and 40:1 for SOx to sulphates (PM2.5). These ratios are from US estimates (west coast),
#' https://www.4cleanair.org/wp-content/uploads/2021/01/01072011-NACAAPM2.5ModelingWorkgroupReport-FINAL_3.pdf 
#' (page 18)
#' 
#' Tyre wear and brake wear figures are US estimates from the MOVES model:
#' https://cfpub.epa.gov/si/si_public_file_download.cfm?p_download_id=525701 (pages 16 and 24)
#' 
#' Estimates of exhuast emissions are based on per litre of fuel consumed, while 
#' tyre and brake wear etc. are per kilometre travelled
#' 
#' Re-entrained road dust is from.... FILL IN 


#all emissions factors are reported as g/L of fuel used
emissions_factors <- read_xlsx("data/bitre-fuel-factors-updated.xlsx",
                               sheet = "Sheet1") %>% 
  rename("pollutant_class" = type) %>% 
  select(fuel_type, pollutant_class, year, NOx, PM10, NMVOC, SOx,
         tyre_pm25_km, tyre_pm10_km, brake_pm25_km, brake_pm10_km, road_wear_pm10_km,
         road_wear_pm25_km) %>% 
  rename("ex_nox_l" = NOx,
         "ex_pm10_l" = PM10,
         "ex_sox_l" = SOx,
         "ex_voc_l" = NMVOC,
         "pollutant_year" = year) 


all_fleets <- all_fleets %>% 
  mutate(pollutant_class = case_when(
    type == "light_rigid" ~ "medium_truck",
    type == "heavy_rigid" ~ "medium_truck",
    type == "articulated" ~ "heavy_truck",
    type == "buses" ~ "buses",
    type == "non_freight" ~ "medium_truck"),
    #and adding in the pollutant ages
    pollutant_year = case_when(
      sales_year > 2015 ~ "post 2015",
      sales_year %in% (2009:2015) ~ "2009-2015",
      sales_year %in% (2003:2008) ~ "2003-2008",
      sales_year %in% (1996:2002) ~ "1996-2002",
      sales_year < 1996 ~ "pre 1996"))



#Joining the emissions factors to vehicles
all_fleets <- left_join(all_fleets, emissions_factors) 


write_rds(all_fleets, "data/all_fleets.rds")


# Determining fuel use, electricity use, and Co2 -------------------------------------------  

#all_fleets <- all_fleets %>% 
#  mutate(co2 = (1 - electric_share) * diesel_fuel_to_co2(diesel_rate_100 ) * vkt +
#           electric_share * vkt * ev_consumption * 1000 * ei_g_wh) %>% 
#  mutate(fuel_used = (vkt / 100) * diesel_rate_100,
#         electricity_used_k_wh = electric_share * vkt * ev_consumption)

  
  
  

  
  
  
  
  
  
  
  
  
  

