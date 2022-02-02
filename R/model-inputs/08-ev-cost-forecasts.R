
# This script pulls together estimates for the upfront cost of electric hevay vehicles based on segment
# which will be used for cost estimates


# Upfront vehicle cost estimates for trucks ---------------------------------

# Estimates are taken from CSIRO: 
#(https://aemo.com.au/-/media/files/electricity/nem/planning_and_forecasting/inputs-assumptions-methodologies/2021/csiro-ev-forecast-report.pdf)
# (page 24 - 33 in pdf)

# Manually pulling out the data because it's in a pdf...

ev_price <- tribble(~year,   ~range,    ~`Rigid trucks`,    ~`Articulated trucks`,     ~Buses,
                     2020,   "ev_short",        104,                   NA,                 269,
                     2025,   "ev_short",        92,                    NA,                 246,
                     2030,   "ev_short",        80,                    NA,                 223,
                     2035,   "ev_short",        70,                    NA,                 200,
                     2040,   "ev_short",        61,                    NA,                 180,
                     2045,   "ev_short",        61,                    NA,                 180,
                     2050,   "ev_short",        61,                    NA,                 180,
                     2020,   "ev_long",         143,                   901,                310,
                     2025,   "ev_long",         125,                   694,                279,
                     2030,   "ev_long",         109,                   535,                252,
                     2035,   "ev_long",         95,                    468,                227,
                     2040,   "ev_long",         83,                    420,                204,
                     2045,   "ev_long",         82,                    404,                203,
                     2050,   "ev_long",         81,                    400,                202) %>% 
  
  pivot_longer(cols = 3:5,
               values_to = "cost",
               names_to = "fuel_class") %>% 
  filter(!is.na(cost))

# The prices for diesel vehicles are set as constants:
  # we will add these later
articulated_diesel <- 300
rigid_diesel <- 61
bus_diesel <- 180


# Ploting the prices to see the trend  -----------------------------

ev_price %>% 
  ggplot(aes(x = year, 
             y = cost, 
             colour = fuel_class)) +
  geom_point() +
  geom_smooth() +

  theme_grattan(chart_type = "scatter",
                legend =  "top") +
  scale_y_continuous_grattan(labels = scales::label_dollar(),
                             limits = c(0, 1000)) +
  grattan_colour_manual(4, rev = TRUE) +
  labs(x = NULL, 
       y = "Upfront cost ($ '000")

# They all essentially seem like exponential decay fits 
# so first we will decide our ratios between long/short range vehicles, and 
# then fit decay models to each to interpolate between the datapoints given. 



# Long to short vehicle range ratios -----------------------------------

# To be conservative we are currently going to assume a long range price for 
# 80\% of rigid trucks, and 90% of buses. 
# But this is purely an assumption (CSIRO don't say wht a 'long' and 'short' range vehicles 
# are to estimated more precisely)

ev_price <- ev_price %>% 
  mutate(proportion = case_when(
    fuel_class == "Rigid trucks" & range == "ev_short" ~ 0.2,
    fuel_class == "Rigid trucks" & range == "ev_long" ~ 0.8,
    fuel_class == "Buses" & range == "ev_short" ~ 0.2,
    fuel_class == "Buses" & range == "ev_long" ~ 0.8,
    fuel_class == "Articulated trucks" ~ 1 )) %>% 
  mutate(cost = proportion * cost) %>% 
  group_by(year, fuel_class) %>% 
  summarise(cost = sum(cost))



# Fitting curves to interpolate ----------------------------------------

art <- ev_price %>% 
  filter(fuel_class == "Articulated trucks")
bus <- ev_price %>% 
  filter(fuel_class == "Buses")
rig <- ev_price %>% 
  filter(fuel_class == "Rigid trucks")

#We're just going to linearly interpolate between points. Given curves are probably 
#exponential decaying this is a conservative estimate (won't make a big difference though)

linear_fit <- bind_rows(
  approx(art$year, 
         art$cost,
         n = 31) %>% 
    as.tibble() %>% 
    mutate(fuel_class = "Articulated trucks"),
  
  approx(bus$year, 
         bus$cost,
         n = 31) %>% 
    as.tibble() %>% 
    mutate(fuel_class = "Buses"),
  
  approx(rig$year, 
         rig$cost,
         n = 31) %>% 
    as.tibble() %>% 
    mutate(fuel_class = "Rigid trucks")) %>% 
  
  rename("year" = x,
         "cost" = y) %>% 
  relocate(fuel_class)



# Checking that the fit is in line with the data 

ggplot() +
  geom_point(data = linear_fit,
            aes(x = year, 
                y = cost,
                colour = fuel_class,
                fill = fuel_class)) +
  
  geom_point(data = ev_price,
             aes(x = year, 
                 y = cost,
                 colour = fuel_class))


# Fit seems fine 


# Adding diesel costs  ------------------------------------

linear_fit <- bind_rows(

  linear_fit %>% 
    mutate(fuel = "diesel",
           cost = case_when(
             fuel_class == "Articulated trucks" ~ articulated_diesel,
             fuel_class == "Rigid trucks" ~ rigid_diesel,
             fuel_class == "Buses" ~ bus_diesel)),
  
  linear_fit %>% 
    mutate(fuel = "electric"))



# Assuming non-freight carrying vehicles face the same incremental cost gap as 
# rigid trucks:

non_freight <- linear_fit %>% 
  filter(fuel_class == "Rigid trucks") %>% 
  mutate(fuel_class = "Non-freight carrying trucks")

linear_fit <- bind_rows(linear_fit,
                        non_freight)


# Saving data -----------------------

#Convert to thousands
upfront_costs <- linear_fit %>% 
  mutate(cost = cost * 1000)


# Adding years to 2060 assume constant costs beyond 2050

upfront_costs <- bind_rows(
  upfront_costs %>% 
    filter(year > 2040) %>% 
    mutate(year = year + 10) %>% 
    group_by(fuel_class, fuel) %>% 
    mutate(cost = max(cost)),
  
  upfront_costs)


# Saving data
write_rds(upfront_costs, "data/upfront_costs.rds")


