
# This script produces a chart which describes the age distribution of the Australian heavy vehicle fleet


# Loading data from ABS motor vehicle survey

fleet_distribution <- read_csv("data-raw/tablebuilder/final/MVUS-2021-type-no-registered.csv",
         skip = 6) %>% 
  clean_names() %>% 
  mutate(series = 2021) %>% 
  select(-annotations) %>% 
  #filter(counting %in% c("Vehicles", "Vehicle Level")) %>%
  
  mutate(vehicle_type = case_when(
    vehicle_type %in% c("Fire Appliances", "Trucks with Machinery Mounted", 
                        "Other Non Freight Carrying Trucks", "Tow Trucks") ~ "Non-freight carrying trucks",
    vehicle_type %nin% c("Fire Appliances", "Trucks with Machinery Mounted", 
                         "Other Non Freight Carrying Trucks", "Tow Trucks") ~ vehicle_type)) %>% 
  
  filter(year_of_manufacture != "Total") %>% 
  mutate(year_of_manufacture = as.numeric(year_of_manufacture)) 



# Plotting ------------------------------


# Calulting how many vehicles are over 20 yearsat time (so pre 2000)

fleet_distribution %>% 
  filter(year_of_manufacture <= 2020) %>% 
  mutate(age_cat = if_else(
    year_of_manufacture <= 2000, 
    "Vehicle > 20 years old",
    "Vehicle < 20 years old")) %>%   
  group_by(age_cat, series) %>% 
  summarise(count = sum(count))

# Gives 604302 less than 20 years, and 172909 over 20
# therefore 22% older, 

  
  
  
# Plotting full chart 
  
fleet_distribution %>% 
  
  group_by(year_of_manufacture, series) %>% 
  summarise(count = sum(count)) %>% 
  filter(year_of_manufacture <= 2020) %>% 
  mutate(age_cat = if_else(
    year_of_manufacture <= 2000, 
    "Vehicle > 20 years old",
    "Vehicle < 20 years old")) %>%   
    
  ggplot(aes(x = year_of_manufacture,
             y = count,
             fill = age_cat)) +
  
  geom_col(position = "stack",
           alpha = 0.9) +
  
  theme_grattan() +
  scale_fill_manual(values = c(grattan_yellow, grattan_red)) +
  scale_colour_manual(values = c(grattan_yellow, grattan_red)) +
  scale_y_continuous_grattan(limits = c(0, 50000)) +
  
  # Title labels
  grattan_label(aes(x = 1965,
                    y = 32000,
                    label = "Vehicle older than 20 years"),
                hjust = "left",
                colour = grattan_red,
                fontface = "bold") +
  
  grattan_label(aes(x = 1965,
                    y = 36000,
                    label = "Vehicle younger than 20 years"),
                hjust = "left",
                colour = grattan_yellow,
                fontface = "bold") +
  
  # Percentage labels 
  grattan_label(aes(x = 2011.5,
                    y = 37000,
                    label = "78%"),
                hjust = "middle",
                colour = grattan_yellow,
                fontface = "bold") +
  grattan_label(aes(x = 1980.5,
                    y = 7000,
                    label = "22%"),
                hjust = "middle",
                colour = grattan_red,
                fontface = "bold") +
  
  
  labs(title = "Almost a quarter of the heavy vehicle fleet is over 20 years old",
       subtitle = "Number of heavy vehicles on register, by year of manufacture",
       x = "Year of manufacture",
       caption = "Vehicle are classified based on their age at the time of the survey.")


#grattan_save(filename = "atlas/lots-of-old-trucks.pdf",
#             save_pptx = TRUE,
#             type = "wholecolumn")






