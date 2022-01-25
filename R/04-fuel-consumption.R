source("R/00-setup.R")

#'Relationship between vehicle age, year of manufacture and fuel consumption
#'
#'There's two effects going on - the first is vehicle manufacture year -- this determines
#'the baseline efficiency (improvements over time are likely)
#'
#'But age also matters because of different driving conditions - older vehicles are
#'like to use less fuel (for articulated trucks), because they carry less when they get 
#'older
#'
#'We'll pick apart these relationships using the MVUS data and historical releases

#' Loading data  --------------------------------------

fuel_data <- read_xlsx("data-raw/year-vs-fuel.xlsx",
                       sheet = "all") %>% 
  #filter(type %in% c("Articulated trucks")) %>% 
  clean_names() %>% 
  select(series, age, type, diesel_rate_100) 



#' Plotting relationships -----------------------------

#plotting year of manufactur against diesel rate
fuel_data %>% 
  filter(age != "All years") %>% 
  mutate(age = factor(age, levels = c("Under 5", "5 to 15", "Over 15"))) %>% 
  ggplot(aes(x = age, y = diesel_rate_100, colour = factor(series))) +
  #geom_smooth(method = "lm") +
  geom_point() +
  scale_y_continuous(limits = c(0, 60)) +
  facet_wrap(~type, scales = "free_y")

# plotting age vs diesel rate
fuel_data %>% 
  filter(age != "All years") %>% 
  mutate(age = case_when(
    age == "Over 15" ~ 20,
    age == "5 to 15" ~ 10,
    #we're assuming 2 here (as opposed to 2.5 or 3), because we know that the 
    #km travelled will skew this to be more representative of younger vehicles
    age == "Under 5" ~ 2)) %>% 
  #mutate(age = factor(age)) %>% 
  ggplot(aes(x = age, y = diesel_rate_100, colour = factor(series))) +
  geom_smooth(method = "lm") +
  geom_point() +
  facet_wrap(~type, scales = "free_y")


#' Looking at this data it's clear age and year of manufacture both have a relatively 
#' significant effect for articulated trucks; but for the other sectors we are worried about there's no clear
#' trend in either variable (except for buses there is a trend  with age - but there's 
#' no clear reason why this would be the case, and it's very possible it's just a factor of 
#' change over time in terms of bus routes and other things, so we are also assuming buses are flat)
#' 
#' As a result we're going to assume that age and year of manufacture before 2021 has no 
#' effect for all categories except articulated trucks. 
#' 


#' Articulated truck relationships ---------------------------------

articulated_changes <-  fuel_data %>% 
  filter(age != "All years",
         type == "Articulated trucks") %>% 
  mutate(age = case_when(
    age == "Over 15" ~ 20,
    age == "5 to 15" ~ 10,
    #we're assuming 2 here (as opposed to 2.5 or 3), because we know that the 
    #km travelled will skew this to be more representative of younger vehicles
    age == "Under 5" ~ 2))
  


# the data is too sparse to properly let us seperate out the effects of age and year of
# manufacture - attempting to do so led to very high skewed values (as the trends
# for year of manufacture are likely to be very inconsistent over time, particularly 
# in the longer term.)

# Instead, we are going to make an assumption (based on the data), that as vehicle age, 
# their fuel efficiency improves by approx 0.125L/100km per year. 

#we are also going to assume that for vehicle under 5 years of age, they 

# We're also going to make the conservative assumption (conservative as likely udnerestimates the 
# fuel use of older vehicles, and thus pollution), that vehicles sold pre 2012 have a new fuel
# consumption figure (0-5 years) of 53L/100km. This is also based on past data. 
#' https://www.ausstats.abs.gov.au/ausstats/subscriber.nsf/0/331AFCE3EFECE3EECA257378007BC0F2/$File/92080_12%20months_ended_31_October_2006.pdf)
#'
#' For vehicles sold in 2012 and newer, we will follow ABS data - fitting a polynomial to the 
#' 0-5 age category data. The relationship is: -0.0554*(2020-year_of_manufacture)^2 + 1.2779*(2020-year_of_manufacture) + 53.097))
#' the fit is very good, at 0.9996 (this was done in excel using the data from ABS in the 
#' `articulated_changes` table)

#first for the 'new' vehicles 

#series <- c(1970:2020) %>% 
#  tibble() %>% 
##  mutate(age = 0) %>% 
#  clean_names() %>% 
#  rename("year_of_manufacture" = x) %>% 
  # adding fuel data #
 # mutate(diesel_rate_100 = 54 + 0.125*2) #case_when(
    #we're also adding a 2*0.125 term to 'scale' the vehicle estimated from age 2 (our assumed age of the 0-5 category)
    #to be an age of a 0 age vehicle
    #year_of_manufacture < 2012 ~ 53 + 0.125*2,
    #year_of_manufacture >=  
      
      #2012 ~ -0.0554*(2020-year_of_manufacture)^2 + 1.2779*(2020-year_of_manufacture) + 53.097) + 1*0.125) 

#now adding the different ages, with the assumption that there is a 0.125L/100km decrease per age increase/year

  
year_of_manufacture <- (1970:2020)
age <- c(0:70)

fuel_consumption <- bind_rows(crossing(year_of_manufacture, age)) %>% 
            #just filling this to bind
            mutate(diesel_rate_100 = case_when(
              age <= 5 ~ 53.1,
              age <= 15 ~ 53.6,
              age > 15 ~ 50.5))



#'Forecasting back and forwards outside of period ---------------------
#' Getting the numbers for pre 2012 (assuming 2012 levels constant)

#' Now forecasting for post 2021 with expected improvements to efficiency of 0.5%

post_2021 <- crossing(
  
  fuel_consumption %>% 
    filter(year_of_manufacture == 2020),

  (2021:2040)) %>% 
  
  select(-year_of_manufacture) %>% 
  
  rename("year_of_manufacture" = `(2021:2040)`) %>% 
  mutate(diesel_rate_100 = case_when(
    year_of_manufacture == 2021 ~ diesel_rate_100,
    year_of_manufacture != 2021 ~ 0.995^(year_of_manufacture-2021) * diesel_rate_100)) 


#' Binding all data together for articulated trucks 

fuel_consumption_articulated <- bind_rows(
  fuel_consumption,
  post_2021) %>% 
  mutate(type = "Articulated trucks") %>% 
  rename("sales_year" = year_of_manufacture) %>% 
  select(type, sales_year, age, diesel_rate_100)



#' Buses -------------------------------------------------------
#' 
#' From the data, it's quite clear that buses have improved in efficiency over
#' time - on average the difference between the under 5 and over 15 categories is 22% 
#' in terms of efficiency. This essentially amounts to a 1% improvement per year, from
#' a baseline figure of ~ 32L/100km 20 years ago. 
#' 
#' We will assume that buses sold in 2000 or earlier operated at 32L/100km, and that since
#' then the improvement has been 1% per year form that baseline (so 0.33L/year)
#' 
#' The average value we're aiming for across the fleet is 28.6L/100km
#' Consistent with the greenhouse accounts, we'll assume that the improvement 
#' beyond 2020 is 0.5% YoY

buses <- fuel_consumption_articulated %>% 
  filter(sales_year == 2000) %>% 
  mutate(type = "Buses",
         diesel_rate_100 = 32)

buses_fuel <- crossing(
  buses,
  (1970:2021)) %>% 
  select(-sales_year) %>% 
  rename("sales_year" = `(1970:2021)`) %>% 
  mutate(diesel_rate_100 = case_when(
    sales_year < 2005 ~ 32,
    sales_year >= 2005 ~ 32 - (sales_year - 2000) * 0.4))

#now adding the post_2021 data for buses
 
buses_p_2021 <- buses_fuel %>% 
  filter(sales_year == 2021,
         age == 0) %>% 
  group_by(type) %>% 
  complete(sales_year = (2022:2050)) %>% 
  arrange(sales_year) %>% 
  na.locf() %>% 
  mutate(diesel_rate_100 = case_when(
    sales_year == 2021 ~ diesel_rate_100,
    sales_year != 2021 ~ 0.995^(sales_year-2021) * diesel_rate_100)) %>% 
  mutate(diesel_rate_100 = case_when(
    diesel_rate_100 <= 0 ~ 0,
    diesel_rate_100 > 0 ~ diesel_rate_100)) %>% 
  group_by(type, sales_year) %>% 
  complete(age = (1:70)) %>% 
  arrange(sales_year, age) %>% 
  na.locf()

buses_fuel <- bind_rows(
  buses_p_2021 %>% 
    filter(sales_year != 2021),
  buses_fuel)



#Rigid trucks ---------------------------------

# Although rigid trucks are broadly flat, they have some imrpvoments over time- 
#lookinga the 2020 data, the relationship between year of manufacture and fuel is
#roughly: y = -0.1143x + 258.56 (where x is year, y is fuel/100km and we assume 
#15+ vehicles are 20 years old on average, 5-15 are 10, and 0-5 are 2)

#because there's uncertainty before 2000 we will truncate the values there and assume 
#prior values are ~29.7L/100km based on that category in MVUS

rigid_historical <- buses_fuel %>% 
  filter(sales_year <= 2020) %>% 
  mutate(type = "Rigid trucks",
         diesel_rate_100 = case_when(
           sales_year < 2000 ~ 29.7,
           sales_year >= 2000 ~ -0.1143 * sales_year + 258.56)) 
  

rigid_p_2020 <- rigid_historical %>% 
  filter(sales_year == 2020) %>% 
  group_by(type, age) %>% 
  complete(sales_year = 2021:2050) %>% 
  arrange(age, sales_year) %>% 
  na.locf() %>% 
  mutate(diesel_rate_100 = case_when(
    sales_year == 2020 ~ diesel_rate_100,
    sales_year != 2020 ~ 0.995^(sales_year-2020) * diesel_rate_100)) %>% 
  filter(sales_year != 2020)
  
rigid_fuel <- bind_rows(
  rigid_historical,
  rigid_p_2020
)


#' Non-freight vehicles ------------------------
#' 
#' For onn-articulated trucks, we are goig to assume that there has been no change
#' to fuel cosnumption over time or with age up until 2021 in the relevant period. 
#' Looking ABS historical data, this is a veyr reasonable assumption to make.
#' 
#' For the values used, the 2020 MVUS average fuel consumption values are used. 

non_freight <- fuel_data %>% 
  filter(series == 2020,
         age == "All years",
         type == "Non-freight carrying trucks") %>% 
  select(-age, -series)

combinations <- crossing((1980:2021), 
                         (0:70), 
                         c("Non-freight carrying trucks")) %>% 
  clean_names() %>% 
  rename("sales_year" = x1980_2021,
         "age" = x0_70,
         "type" = c_non_freight_carrying_trucks)


non_freight <- left_join(combinations, non_freight) %>% 
  select(type, sales_year, age, diesel_rate_100)


# Now we need to forecast foreward - going to do this again assuming a 0.5% 
# improvment to fuel consumption per year form a baseline

other_post_2021 <- crossing(
  non_freight %>% 
    filter(sales_year == 2021),
  (2022:2040)) %>% 
  
  select(-sales_year) %>% 
  rename("sales_year" = `(2022:2040)`) %>% 
  #assuming a 0.5% improvement per year from baseline
  mutate(diesel_rate_100 = case_when(
    sales_year == 2021 ~ diesel_rate_100,
    sales_year != 2021 ~ 0.995^(sales_year-2021) * diesel_rate_100)) 



#Binding all these values together now:

all_fuel_consumption <- bind_rows(
  fuel_consumption_articulated,
  buses_fuel,
  rigid_fuel,
  non_freight,
  other_post_2021) %>% 
  #and small changes for compatability with model
  rename("fuel_class" = type) %>% 
  mutate(age = as.numeric(age))



write_rds(all_fuel_consumption,
          "data/fuel_consumption.rds")


all_fuel_consumption %>% 
  ggplot(aes(x = sales_year, y = diesel_rate_100, colour = fuel_class)) +
  geom_point()

