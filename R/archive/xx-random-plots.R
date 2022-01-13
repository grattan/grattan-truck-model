




all_fleets %>% 
  filter(fleet_year == 2020,
         vkt_scenario == "vkt_central") %>% 
  mutate(age_group = case_when(
    age <= 5 ~ "05 under",
    age <= 15 ~ "15 under",
    age > 15 ~ "15 over")) %>%
  group_by(fuel_class, age_group) %>% 
  summarise(av_fuel = weighted.mean(x = diesel_rate_100, w = total),
            fuel_total = (sum(total * vkt * diesel_rate_100 / 100) / 1000000),
            vkt_total = sum(total * vkt) / 1000000,
            vkt_av = weighted.mean(w = total, x = vkt), 
            total_vech = sum(total)) %>% view()


all_fleets %>% 
  filter(fleet_year == 2020,
         vkt_scenario == "vkt_central") %>% 
  ggplot(aes(x = sales_year, 
             y = total, 
             colour = type)) +
  geom_point() +
  geom_line() +
  facet_wrap(~type, scales = "free_y")




fleet_2020 <- read_csv("data/tablebuilder/final/MVUS-2021-type-no-registered.csv",
         skip = 6) %>% 
  clean_names() %>% 
  select(-annotations) %>% 
  #filter(counting %in% c("Vehicles", "Vehicle Level")) %>%
  filter(!is.na(count),
         year_of_manufacture < 2020) %>% 
  mutate(count = as.numeric(count),
         year_of_manufacture = as.numeric(year_of_manufacture),
         vehicle_type = case_when(
    vehicle_type %in% c("Fire Appliances", "Trucks with Machinery Mounted", 
                        "Other Non Freight Carrying Trucks", "Tow Trucks") ~ "Non-freight carrying trucks",
    vehicle_type %nin% c("Fire Appliances", "Trucks with Machinery Mounted", 
                         "Other Non Freight Carrying Trucks", "Tow Trucks") ~ vehicle_type)) %>% 
  filter(!is.na(year_of_manufacture)) %>% 
  group_by(vehicle_type, year_of_manufacture) %>% 
  summarise(count = sum(count))


fleet_2020 %>% 
  ggplot(aes(x = year_of_manufacture, y = count, colour = vehicle_type)) +
  geom_point() +
  geom_line() +
  facet_wrap(~vehicle_type, 
             scales = "free_y")


estimates <- left_join(
  all_fleets %>% 
    filter(fleet_year == 2020,
           vkt_scenario == "vkt_central") %>% 
    rename("estimated_sales" = total),
  
  fleet_2020 %>% 
    mutate(type = case_when(
      vehicle_type == "Buses > 9 seats" ~ "buses",
      vehicle_type == "Heavy Rigid Trucks > 4.5t GVM" ~ "heavy_rigid",
      vehicle_type == "Light Rigid Trucks up to 4.5t GVM" ~ "light_rigid",
      vehicle_type == "Prime Movers" ~ "articulated",
      vehicle_type == "Non-freight carrying trucks" ~ "non_freight")) %>%
    select(-vehicle_type) %>% 
    rename("sales_year" = year_of_manufacture,
           "abs_sales" = count)) %>% 

  mutate(resid = abs_sales - estimated_sales) %>% 
  select(type, sales_year, abs_sales, estimated_sales, resid) %>% 
  pivot_longer(cols = (3:4),
               names_to = "estimate",
               values_to = "value")



estimates %>% 
  ggplot(aes(x = sales_year, y = value, colour = estimate)) +
  geom_line() +
  facet_wrap(~type, scales = "free_y")



sales %>% 
  ggplot(aes(x = sales_year, 
             y = sales, 
             colour = type)) +
  geom_point() +
  geom_line() +
  facet_wrap(~type, scales = "free_y")




# Testing the carbon predictions ---------------------------------------------


co2 <- all_fleets %>% 
  group_by(fleet_year, fuel_class, vkt_scenario) %>% 
  summarise(total_co2_mt = sum(co2 * total / 1000000000000)) 

co2 %>% 
  filter(fuel_class != "Non-freight carrying trucks",
         vkt_scenario == "vkt_central") %>% 
  ggplot(aes(x = fleet_year, y = total_co2_mt, colour = fuel_class)) +
  geom_line() +
  theme_grattan() + 
  scale_y_continuous_grattan(limits = c(0, 20)) +
  scale_x_continuous_grattan(limits = c(2020, 2030), 
                             breaks = c(2020, 2025, 2030)) +
  grattan_colour_manual() +
  facet_wrap(~fuel_class)

#something funky s obviously going on with non-freight carrying trucks
#Our rigid truck numbers are very high compared to the greenhouse account estimates
#Our articulated trucks grow a fair bit more than the GHG account but are in the right ballpark


all_fleets %>% 
  group_by(fleet_year, vkt_scenario) %>% 
  summarise(total_vkt = sum(vkt)) %>% 
  mutate(growth_rate_perc = (total_vkt / lag(total_vkt) - 1) * 100)



write_rds(co2, "data/co2.rds")

  