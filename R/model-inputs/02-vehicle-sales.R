# Vehicle sales 

source("R/00-setup.R")
source("R/01-vehicle-attrition.R")

# This script aims to determine an estimate of vehicle sales rates of the past 10 years
# from ABS data, and then from that to make some estimate of longer term sales trends

# there also might be a way to do this back calculating from the attrition rates?


# These estimates are primarily going to be used to create forecasts, as well as 
# being used to test the model against the existing data to see if it matches up reasonably
# or if something has gone horribly wrong 

#If we filter for vehicles in their second year on register, given attrition is 
#likely to be very low for this category this should give us an idea of initial 
#sales numbers
new_rego_data <- rego_data %>% 
  filter(year_of_manufacture %nin% c("Total", "Not stated"),
         year_of_manufacture == series - 1) 


new_rego_data %>%   
  ggplot(aes(x = year_of_manufacture, 
             y = count, 
             colour = vehicle_type)) +
  scale_y_continuous_grattan(limits = c(0, 15000)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~vehicle_type)

# this suggests that rigid trucks have grown a bit (but slowed considerably in 2019/20)
# and that apart from that sales levels tend to be relatively steady
# BUT we do only have ~8 years of data, which isn't a lot to back out trends from these
# types of things


# Using attrition to back calculate historical sales --------------------------

# Checking what the cumulative attrition looks like 
attrition %>% 
  ggplot(aes(x = age, y = cumulative_attr, colour = vehicle_type)) +
  geom_point()


# We now want to bind our 2020 dataset with these cumulative attrition rates, and
# from that back out estimates of what the sales have been like over time 

new_sales <- left_join(

  rego_data %>% 
    filter(series != 2021,
           year_of_manufacture %nin% c("Total", "Not stated", 2021)) %>% 
    mutate(age = series - as.numeric(year_of_manufacture)),
  
  attrition) %>% 
  
  # and now multiplying through by the cumulative attrition to get an estimate of
  # original sales 
  
  mutate(new_sales = count * (1/ cumulative_attr))



#and seeing what this look like

new_sales %>% 
  #to get rid of recent years where the census data doesn't have values 
  filter(new_sales > 500) %>% 
  ggplot(aes(x = as.numeric(year_of_manufacture), 
             y = new_sales, 
             colour = vehicle_type)) +
  scale_x_continuous_grattan(limit = c(1980, 2020)) +
  geom_point() +
  geom_smooth(span = 1.5) +
  facet_wrap(~vehicle_type)


# This data evidently isn't particularly perfect, but it is relatively consistent across
# the different census year datasets 

# Fitting models for smoothed historical estimates -----------------------------

# Create model that will do the same thing as under the hood in ggplot2 for each group 
prime_movers <- new_sales %>% 
  filter(new_sales > 500,
         year_of_manufacture %in% 1980:2019,
         vehicle_type == "Prime Movers")

buses <- new_sales %>% 
  filter(new_sales > 500,
         year_of_manufacture %in% 1980:2019,
         vehicle_type == "Buses > 9 seats")

light_rigid <- new_sales %>% 
  filter(new_sales > 500,
         year_of_manufacture %in% 1980:2019,
         vehicle_type == "Light Rigid Trucks up to 4.5t GVM")

heavy_rigid <- new_sales %>% 
  filter(new_sales > 500,
         year_of_manufacture %in% 1980:2019,
         vehicle_type == "Heavy Rigid Trucks > 4.5t GVM")

non_freight <- new_sales %>% 
  #need to go a bit lower on the numbers because sometimes non-freight dips under 500 sales/year
  filter(new_sales > 200,
         year_of_manufacture %in% 1980:2019,
         vehicle_type == "Non-freight carrying trucks")



model_pm <- loess(new_sales ~ year_of_manufacture, 
               data = prime_movers, 
               span = 1.25)
model_bus <- loess(new_sales ~ year_of_manufacture, 
               data = buses, 
               span = 1.25)
model_lr <- loess(new_sales ~ year_of_manufacture, 
               data = light_rigid, 
               span = 1.25)
model_hr <- loess(new_sales ~ year_of_manufacture, 
               data = heavy_rigid, 
               span = 1.25)
model_nf <- loess(new_sales ~ year_of_manufacture, 
                  data = non_freight, 
                  span = 1.25)

# Add predicted values from model to original dataset using broom library

all_fitted <- bind_rows(
  augment(model_pm, prime_movers),
  augment(model_bus, buses),
  augment(model_lr, light_rigid),
  augment(model_hr, heavy_rigid),
  augment(model_nf, non_freight)) %>% 
  rename("est_historical_sales" = .fitted) %>% 
  mutate(year_of_manufacture = as.numeric(year_of_manufacture))

# Plotting each model
ggplot(data = all_fitted, 
       aes(x = year_of_manufacture, 
           y = est_historical_sales,
           colour = vehicle_type)) + 
  scale_y_continuous_grattan(limits = c(0, 20000)) +
  geom_point() +
  facet_wrap(~vehicle_type)


#For the purpose of the actual model, we'll just the actual census 2021 data so we 
# don't have to deal with these historical estimates. however, we'll used these smoothed
# lines as 'historical sales numbers' to check whether our attrition rates and assumptions
# lead to similar numbers as the MVC data

# We will probably also use these numbers as a basis for some of the future sales
# forecasts in the model 



# Creating sales forecasts -------------------------------------

# First summarising our sales based on historical data 
all_fitted <- all_fitted %>% 
  filter(series == 2020) %>% 
  rename("proportion_surviving" = cumulative_attr,
         "type" = vehicle_type) %>% 
  mutate(type = case_when(
    type == "Buses > 9 seats" ~ "buses",
    type == "Heavy Rigid Trucks > 4.5t GVM" ~ "heavy_rigid",
    type == "Light Rigid Trucks up to 4.5t GVM" ~ "light_rigid",
    type == "Prime Movers" ~ "articulated",
    type == "Non-freight carrying trucks" ~ "non_freight"),
    sales_year = year_of_manufacture) %>%
  select(type, sales_year, new_sales) %>% 
  unique() %>% 
  rename("sales" = new_sales)





# these estimates go from 1980 to 2019. Beyond 2019, we will forecast sales to grow in line with
#population projections as included in the 2021 intergenerational report (ABS scenarios
# are pre-covid)
# https://treasury.gov.au/sites/default/files/2021-07/p2021_182464-cd.zip

pop_growth <- read_xlsx("data/IG-report-data.xlsx",
                        sheet = "pop-growth") %>% 
  clean_names() %>% 
  #and because it's in financial year just assuming the earlier year applies
  mutate(year = substr(year, 1, 4)) %>% 
  mutate(year = as.numeric(year)) %>% 
  #converting to a proportion change 
  mutate(population_growth = (population_growth / 100) + 1) %>% 
  filter(year >= 2020) %>% 
  rename("sales_year" = year)



# we'll use this population data to assume the growth rate of freight sales

future_sales <- all_fitted %>% 
  filter(sales_year >= 2010) %>% 
  group_by(type) %>% 
  summarise(sales = mean(sales)) %>% 
  mutate(sales_year = 2019) %>% 
  group_by(type) %>% 
  complete(sales_year = 2020:2050) %>% 
  arrange(type, sales_year) %>% 
  na.locf() 

future_sales <- left_join(future_sales, pop_growth) %>% 
  arrange(type, sales_year) %>% 
  mutate(population_growth = case_when(
    sales_year == 2019 ~ 1,
    sales_year != 2019 ~ population_growth)) %>% 
  group_by(type) %>% 
  #converting population growth to cumulative rate
  mutate(population_growth_cum = cumprod(population_growth)) %>% 
  mutate(sales = sales * population_growth_cum) %>% 
  filter(sales_year != 2019)


#Binding these results with historical
sales <- bind_rows(all_fitted, future_sales) 


sales %>% 
  mutate(estimate = case_when(
    sales_year <= 2019 ~ "historical",
    sales_year > 2019 ~ "forecast")) %>%
  ggplot(aes(x = sales_year, y = sales, colour = estimate)) +
  geom_line() +
  theme_grattan() +
  grattan_colour_manual(2) +
  scale_y_continuous() +
  facet_wrap(~type)


write_rds(sales, "data/sales.rds")




