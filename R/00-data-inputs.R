
# Dummy datasets

source("R/00-setup.R")

#' Yearly sales-----------------------------------------------------
#' making an assumption that sales grow at 1.7% each year across all 
#' categories equally and that in 2020 the sales were around about:
  #' 17,000 for light ridig trucks
  #' 22,000 for heavy rigid trucks
  #' 9,000 for articulated trucks
  #' 8,000 for buses

sales <- read_rds("data/sales.rds")


#' Survival curves ----------------------------------------
#' From ABS data we have average ages by type. We know that:
  #' Light rigid: ~11 years
  #' Heavy rigid ~16 years
  #' Articulated: ~12 year
  #' Buses: ~ 12 years 
#' played around in excel to make the assumptions work around these values to get
#' the following dataset. 

survival_curves <- read_rds("data/attrition.rds") %>% 
  rename("proportion_surviving" = cumulative_attr,
         "type" = vehicle_type) %>% 
  mutate(type = case_when(
    type == "Buses > 9 seats" ~ "buses",
    type == "Heavy Rigid Trucks > 4.5t GVM" ~ "heavy_rigid",
    type == "Light Rigid Trucks up to 4.5t GVM" ~ "light_rigid",
    type == "Prime Movers" ~ "articulated",
    type == "Non-freight carrying trucks" ~ "non_freight")) 
  

#' Fuel consumption and VKT's ------------------------------------
#' These are from ABS motor vehicle use survey 
fuel_vkt <- read_rds("data/fuel_consumption.rds")




#' Estimated VKTs by vehicle age 
vkt <- read_rds("data/vkt.rds")
  



#' Emissions to diesel relationship -----------------------------
#' From fleetEffSim
#' This probably does hold I would assume

diesel_co2_to_fuel <- function(.co2) {
  .diesel_consumption <- -0.001201 + 0.037436 * .co2
  return(.diesel_consumption)
}

#we actually want this rearranged so we put in diesel consumption and get out 
#co2 values. So rearranging:

# This converts diesel consumption in L/100km to carbon dioxide production in 
# g/km travelled. 

diesel_fuel_to_co2 <- function(.diesel_consumption) {
  .co2 <- (.diesel_consumption + 0.001201) / 0.037436 
  return(.co2)
}







