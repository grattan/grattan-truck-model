

# Electric trucks --------------------------------------------------

#' Although electric trucks and zero emission trucks are not prominent in the Australian
#' fleet at the moment, this simulation runs out to 2040 or beyond. In this time period
#' they are likely to play a non-negligible role, and can't be ignored. 
#' 
#' To estimate ZEV truck uptake, we will use data from McKinsey on electric truck uptake 
#' (https://www.mckinsey.com/industries/automotive-and-assembly/our-insights/whats-sparking-electric-vehicle-adoption-in-the-truck-industry)
#' forecasts in Europe - however, given the more challenging profile of Australia's
#' fleet, and our role as a technology taker, we assume that uptake in Australia is lagged
#' by 5 years (in the BAU without policy action) for rigid and articulated trucks. 
#' Non-freight vehicle are assumed to follow the articulated segment. Buses are 
#' treated separately. Data beyond 2035 is extrapolated from the McKinsey estimates


electric_uptake <- read_xlsx("data-raw/electric-uptake.xlsx") %>% 
  select(-value) %>% 
  #lagging by 5 years
  mutate(sales_year = sales_year + 5)

#' Assuming non-freight follows trajectory of articulated
electric_uptake <- bind_rows(
  
  electric_uptake %>% 
    filter(fuel_class == "Articulated trucks") %>% 
    mutate(fuel_class = "Non-freight carrying trucks"),
  
  electric_uptake)


#' Buses -----------------------------------------------------------------
#' 
#'  For buses, a trajectory is assumed whereby 25% of buses are electric by 2030;
#'  60% by 2035, and 100% by 2040 (for new sales). This is based of international
#'  forecasts and considering government announcements, such as NSW pledge to go 
#'  electric. It is likely to be a conservative timeline - actual progress may be faster. 

bus_uptake <- read_xlsx("data-raw/electric-uptake.xlsx",
                        sheet = "buses") %>% 
  select(fuel_class, sales_year, electric_share) 

electric_uptake <- bind_rows(
  bus_uptake,
  electric_uptake)



# Energy consumption estimates ---------------------------------------------

#' Unlike ICE vehicles which we have estimated fuel consumption for, we need to 
#' estimate the energy consumption for electric HDVs. These estimates are predominantly
#' based on ICCT estimates from Europe (https://theicct.org/sites/default/files/publications/EU-logistics-electrification-fv-202011.pdf)
#' However, given Australian vehicles tend to be larger and carry heavier loads,
#' we assume upper bound values for our estimates (this is conservative)

#' Values below are reported in kWh/km travelled
rigid_ev <- 1.2
articulated_ev <- 1.9

#' From our fuel consumption estimate:
#' all_fuel_consumption %>% filter(age == 0, sales_year == 2020) %>% view()
#' We can see that rigid buses are approximately 15% less energy intensive than 
#' rigid trucks, and non-freight carrying vehicles are 18% less intensive.We will apply this scaling
#' to the rigid values to get a non freight carrying and bus value

bus_ev <- 1.2 * 0.85
non_freight_ev <- 1.2 * 0.82


#' Adding this data to our dataset
electric_uptake <- electric_uptake %>% 
  mutate(ev_consumption = case_when(
    fuel_class == "Articulated trucks" ~ articulated_ev,
    fuel_class == "Rigid trucks" ~ rigid_ev,
    fuel_class == "Non-freight carrying trucks" ~ non_freight_ev,
    fuel_class == "Buses" ~ bus_ev)) %>% 
  #as a proportion / 1 
  mutate(electric_share = electric_share / 100)

#' We will also assume this is true regardless of the age of the vehicle. (i.e. we 
#' won't add an age category), and over time (assuming that any gains made to energy consumption
#' are used up by added features etc. instead of better efficiency)


write_rds(electric_uptake, "data/electric_uptake.rds")

