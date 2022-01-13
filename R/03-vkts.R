# VKT estimates 

source("R/00-setup.R")
source("R/00-data-inputs.R")

# Data are taken from the ABS MVUS

# Two main things we need to look at - the VKT by age (total), and the rural/urban
# split of those kms. 

# First trying to get an estimate of the km travelled by age 

# note that the units for this sheet are in millions initially (but we convert them)
vkt_total <- read_csv("data/tablebuilder/mvus-2018-vkt-age-totals.csv",
         skip = 6 ) %>% 
  clean_names() %>% 
  filter(!is.na(count)) %>% 
  #multiplying to make the total in correct units
  mutate(count = count * 1000000) %>% 
  rename("km_total" = count) %>% 
  select(vehicle_type,
         year_of_manufacture_groups,
         km_total)
  

# and the units for this are individual vehicles 
vech_no <- read_csv("data/tablebuilder/mvus-2018-vech-number-age.csv",
         skip = 6 ) %>% 
  clean_names() %>% 
  rename("no_vehicles" = count) %>% 
  filter(!is.na(no_vehicles)) %>% 
  select(vehicle_type, 
         year_of_manufacture_groups,
         no_vehicles)


vkts <- left_join(vkt_total, vech_no) %>% 
  mutate(av_vkt = km_total / no_vehicles,
         year_of_manufacture_groups = case_when(
           year_of_manufacture_groups == "2002 and earlier" ~ "Over 15",
           year_of_manufacture_groups == "2003 to 2012" ~ "5 to 15",
           year_of_manufacture_groups == "2013 and after" ~ "Under 5"),
         age_point_est = case_when(
           year_of_manufacture_groups == "Over 15" ~ 18,
           year_of_manufacture_groups == "5 to 15" ~ 10,
           year_of_manufacture_groups == "Under 5" ~ 3)) %>% 
  filter(vehicle_type != "Total")


#Plotting to see what this looks like 
#vkts %>% 
#  ggplot(aes(x = age_point_est, y = av_vkt / 1000, colour = vehicle_type)) +
#  geom_point() 
  #geom_smooth(span = 2,
  #            degree = 1)


# Models used in the MOVE model -------------------------------

# stuff on page 28 of here: 
# https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.360.3323&rep=rep1&type=pdf
# pull the equations in and see how they go - they are relative equations (as a proportion) so
# should be pretty easy to scale up really. 


# pulling out each of these equations into a tibble 

moves_curves <- seq(0, 50) %>% 
  tibble() %>% 
  clean_names() %>% 
  rename("age" = x) %>% 
  # adding each of the relationships 
  mutate(single_short_haul = 0.4289*exp(-0.0990*age),
         single_long_haul = 0.3339 * exp(-0.0762*age),
         combination_short_haul = 0.0016 * age^2 -0.0762 * age +0.9655,
         combination_long_haul = 0.0021 * age^2 -0.0887 * age + 1.0496)
  #because they are all relative, we want to standardise them to the baseline figure
  #(at age 2 because that's what we have data for)

single_sh <- moves_curves %>% 
  filter(age == 3) %>% 
  select(single_short_haul) %>% 
  pull()

single_lh <- moves_curves %>% 
  filter(age == 3) %>% 
  select(single_long_haul) %>% 
  pull()

combo_sh <- moves_curves %>% 
  filter(age == 3) %>% 
  select(combination_short_haul) %>% 
  pull()

combo_lh <- moves_curves %>% 
  filter(age == 3) %>% 
  select(combination_long_haul) %>% 
  pull()

#using these values to standardise all of them
moves_curves_st <- moves_curves %>% 
  mutate(single_short_haul = single_short_haul / single_sh,
         single_long_haul = single_long_haul / single_lh,
         combination_long_haul = combination_long_haul / combo_lh,
         combination_short_haul = combination_short_haul / combo_sh)




# Testing MOVES curves against ABS data --------------------------------------


# Annoyingly the polynomial functions are a bit of a pain really - because beyond 20 years
# we're already past the turning point...Ignoring that for a moment

# Now we want to plot these against the data we have, to try and get a rough sense 
# for how they track against one another 


est_vs_moves <- full_join(

moves_curves_st %>% 
  #multiplying by the km at age two for each respective category
  mutate(single_short_haul = single_short_haul * 2.78e4,
         single_long_haul = single_long_haul * 34902,
         combination_short_haul = combination_short_haul * 141918,
         combination_long_haul = combination_long_haul * 141918) %>% 
  pivot_longer(cols = 2:5,
               names_to = "vehicle_type",
               values_to = "av_vkt"),

vkts %>% 
  select(age_point_est, av_vkt, vehicle_type) %>% 
  rename("age"= age_point_est))


#est_vs_moves %>% 
#  ggplot(aes(x = age, y = av_vkt / 1000, colour = vehicle_type)) +
#  geom_point()


# Adjusting MOVES curves -------------------------------------------------------

#for articulated trucks this relationship actually seems to fit quite well - it's a little 
#high n the middle years but apart from that seems reasonable. For this category, we will take
#this estimate and (the combination short haul data) and truncate it at 16 years year. 
#We'll assume linear beyond this.

#the single long haul estimate also seems somewhat reasonable to use for the rigid truck
#category - it's by no means perfect, but is probably a better estimate than we would otherwise 
#generate. Main problem is that it's a bit low in the middle age ranges - we can come back
#to this if our total vkts in the segment seem unreaslistic. 

#the single short haul also looks reasonable for the non freight carrying truck category

#buses is trickier - there's no particularly good fit there (but as they're small we
#can probably just adopt a piece wise function based on linear estimates)

vkt_curves_adj <- moves_curves_st %>% 
  select(age, combination_short_haul, 
         single_long_haul, single_short_haul) %>% 
  mutate(#for articulated
         combination_short_haul = combination_short_haul * 141918,
         #for rigid
         single_long_haul = single_long_haul * 34902,
         #for non-freight carrying
         single_short_haul = single_short_haul * 2.78e4) %>% 
  rename("Articulated trucks" = combination_short_haul,
         "Rigid trucks" = single_long_haul,
         "Non-freight carrying trucks" = single_short_haul)
  
  #for 16 years and beyond (over 15), we're going to assume a linear relationships where km travelled
  # falls by 400per year. This is purely an assumption that fits the data nicely
  # to the MVUS - there is next to no data beyond this to compare to for trucks of 
  # this age, even from the MOVES curves. 


vkt_curves_adj <- vkt_curves_adj %>% 
  mutate(`Articulated trucks`  = case_when(
    #first boosting young vehicle vkt's - based on David (BITRE) advice that the 
    #MVUS doesn't accurately capture this data
    age %in% c(0, 1, 2) ~ `Articulated trucks` * 1.05,
    age %in% (3:15) ~ `Articulated trucks`,
    age >= 16 ~ (29450 - (age-16) * 400)))  %>% 
  
  #other version manually done
  #mutate(`Articulated trucks` = case_when(
  #  age >= 16 ~ (29450 - (age-16) * 400),
  #  age %in% c(0, 1) ~ 149000,
  #  age == 2 ~ 147000,
  #  age %in% (3:16) ~ `Articulated trucks`)) %>% 
    
    #After running the model, it also becomes clear that this overstates the km driven by
    #the 0-5 band of articulated trucks. As such we're going to assume that the very young vehicle (0-2)
    #are flatter at the top of the curve.
  pivot_longer(cols = 2:4,
               names_to = "vehicle_type",
               values_to = "vkt")



  

#also need to assume that light and rigid trucks follow the same path (as there's a lack
#of data here) and to add them as seperate categories

#plotting 
vkt_curves_adj %>% 
  ggplot(aes(x = age, y = vkt, colour = vehicle_type)) +
  geom_point()




# Adjustments to Rigid trucks ------------------------------------------

# Initially, the curves were left as is from the above code, and this was tested using the fleet 
# model against the ABS data. However, the VKT estimates were not as accurate as they could
# be, and need adjusting. Without adjustment, the results were:

# Total VKTs
# Rigid       Our model       ABS
# Over 15       1624m         1853m
# 5-15          4414m         4706m
# under 5       3597m         4417m

#So we need to adjust our model upwards for VKTs in all age groups, but there is a 
#bigger difference in the new age brackets. 

# we're going to dilate the model fit tslightly for a better fity

# our new formula for the fit is below, based on the single_long_haul category

#  y = (0.3339 * exp(-0.06 * (age))
# as opposed to original
# single_long_haul = 0.3339 * exp(-0.0762*age),

# this is standardised based on age 3 and scaled like before

# we multiple by 34902 at the end to scale for the base km as we did above 
#the results are a fraction high for older vehicles but overall all error is under ~10%
#given the magnitude of error of both the data we're working with and the model generally
#this is probably acceptable (particularly as this will be used for the BAU and test cases)

vkt_curves_adj <- bind_rows(

  vkt_curves_adj %>% 
    filter(vehicle_type == "Rigid trucks") %>% 
    mutate(vkt = 0.3339 * exp(-0.060*(age + 0.5))) %>% 
    #standardising to 0 again and then multiplying by the base figure we have 
    mutate(vkt = (vkt / single_lh) * 34902) %>%  
    # other version
    #and like with articulated trucks, bending the curve for very new trucks
    mutate(vkt = case_when(
      age == 0 ~ 35000,
      age == 1 ~ 34250,
      age == 2 ~ 34000,
      age == 3 ~ 33750,
      age == 4 ~ 33000,
      age %nin% c(1, 2, 3, 4) ~ vkt)),

 # 
  vkt_curves_adj %>% 
    filter(vehicle_type != "Rigid trucks"))



# Making the curves for buses -----------------------

#for buses it sort of looks like there are 2-3 phases - a steady vkt for the first ~10 years,
#then a relatively quick drop off to low levels. It's unclear what is used beyond that, 
#but we are going to assume low level use (this likely depends on the bus type etc.);
#this is also partly inferred from the attrition rates where if they topped being used 
#completely you would expect a rapid drop off at this point in the curve, which isnt
#visible.

#we will check these values against the final VKT estimates with the real data
#vkts %>% 
#  filter(vehicle_type == "Buses") %>% 
#  ggplot(aes(x = age_point_est, y = av_vkt)) +
#  geom_point() +
#  scale_y_continuous_grattan(limits = c(0, 40000)) +
#  scale_x_continuous_grattan(limits = c(0, 30)) 
  #geom_smooth(span = 1.1)

# We have very little to go off - so we will make some reasonable assumptions of a piecewise 
# function

# We assume that km in the first 10 years are steady, as this is likely a full use period
# Beyond this we assume that km declines at a steady rate (shallowing out a bit beyond 18 years)
# until it reaches no further use at age 30

# These assumption map to the ABS data km travelled breakdowns by age, and are based
#off those estimates. 

  
bus_vkt <- seq(0, 50) %>% 
  tibble() %>% 
  clean_names() %>% 
  rename("age" = x) %>% 
  mutate(vkt = case_when(
    age <= 10 ~ 32000,
    age < 30 ~ -1550 * (age - 10) + 32000,
    #age < 30  ~ 14750 - 1200 * (age - 17),
    age >= 30 ~ 0)) %>% 
  mutate(vehicle_type = "Buses")

bus_vkt %>% 
  ggplot(aes(x = age, y = vkt)) +
  geom_point() +
  scale_y_continuous_grattan(limits = c(0, 40000)) +
  scale_x_continuous_grattan(limits = c(0, 40)) 


  
# Adjusting 0 age vehicles ----------------------------------

#the last adjustment we are going to make is to 0 age vehicles - because these have
#been sold during the year, they probably won't do a full years worth of driving. 

#we will assume they drive 1/2 as much to account for the variability in when they 
#are likely to have been sold. 

vkt <- bind_rows(
  bus_vkt,
  vkt_curves_adj) %>% 
  rename("type" = vehicle_type) %>% 
  mutate(vkt = case_when(
    age == 0 ~ vkt/2,
    age != 0 ~ vkt))


vkt %>% 
  ggplot(aes(x = age, y = vkt, colour = type)) +
  geom_point()
  

write_rds(vkt, "data/vkt.rds")























