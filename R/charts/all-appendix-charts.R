
# This script compiles the relevant charts from the analysis for the appendix

library(Cairo)


# Attrition chart  ---------------------------------------------

source("R/model-inputs/01-vehicle-attrition.R")

p1_wc <- attrition %>% 
  mutate(vehicle_type = case_when(
    vehicle_type == "Prime Movers" ~ "Articulated",
    vehicle_type == "Buses > 9 seats" ~ "Buses",
    vehicle_type == "Heavy Rigid Trucks > 4.5t GVM" ~ "Heavy rigid \ntrucks",
    vehicle_type == "Light Rigid Trucks up to 4.5t GVM" ~ "Light rigid \ntrucks",
    vehicle_type == "Non-freight carrying trucks" ~ "Non-freight \ncarrying trucks")) %>% 
  filter(vehicle_type != "Non-freight \ncarrying trucks") %>% 
  ggplot(aes(x = age, 
             y = cumulative_attr, 
             colour = vehicle_type)) +
  geom_line() +
  #geom_smooth() +
  scale_x_continuous_grattan(limits = c(0, 50),
                             breaks = c(0, 25, 50)) +
  scale_y_continuous_grattan(limits = c(0, 1),
                             breaks = c(0, 0.5, 1),
                             label = scales::label_percent()) +
  theme_grattan() +
  grattan_colour_manual() +
  
  facet_wrap(~vehicle_type) +
  
  labs(title= "Vehicle attrition rates by segment",
       subtitle = "Proportion of vehicles surviving by age",
       x = "Vehicle age")

p1_wc


grattan_save(filename = "atlas/appendix/p1_wc.pdf",
             object = p1_wc,
             save_ppt = FALSE,
             type = "wholecolumn",
             device = cairo_pdf)



# Sales estimates -----------------------------------------------

source("R/model-inputs/02-vehicle-sales.R")

p2_wc <- sales %>% 
  filter(sales_year <= 2040) %>% 
  mutate(type = case_when(
    type == "articulated" ~ "Articulated",
    type == "buses" ~ "Buses",
    type == "heavy_rigid" ~ "Heavy rigid \ntrucks",
    type == "light_rigid" ~ "Light rigid \ntrucks",
    type == "non_freight" ~ "Non-freight \ncarrying trucks"
  )) %>% 
  filter(type != "Non-freight \ncarrying trucks") %>% 
  mutate(estimate = case_when(
    sales_year <= 2019 ~ "historical",
    sales_year > 2019 ~ "forecast")) %>%
  ggplot(aes(x = sales_year, y = sales, colour = estimate)) +
  geom_line() +
  theme_grattan() +
  scale_y_continuous_grattan(breaks = c(0, 10000, 20000),
                             limits = c(0, 23000),
                             labels = scales::label_number_si()) +
  scale_x_continuous_grattan(breaks = c(1980, 2010, 2040),
                             limits = c(1980, 2047)) +
  grattan_colour_manual(2, reverse = TRUE) +
  facet_wrap(~type) + 
  labs(title = "Sales of heavy vehicles are forecast to grow in the future",
       subtitle = "Estimated historical and forecast heavy vehicle sales",
       x = NULL)

p2_wc

grattan_save(filename = "atlas/appendix/p2_wc.pdf",
             object = p2_wc,
             save_ppt = FALSE,
             type = "wholecolumn",
             device = cairo_pdf)


# VKTs ---------------------------------------------------------

source("R/model-inputs/03-vkts.R")

p3_wc <- vkt %>% 
  mutate(fuel_class = if_else(fuel_class == "Non-freight carrying trucks",
                 "Non-freight \ncarrying trucks",
                 fuel_class)) %>% 
  filter(age > 0) %>% 
  ggplot(aes(x = age, y = vkt, colour = fuel_class)) +
  geom_line() +
  theme_grattan() +
  scale_y_continuous_grattan(labels = scales::label_number_si(),
                             limits = c(0, NA)) +
  scale_x_continuous_grattan(limits = c(0, 30),
                             breaks = c(0, 15, 30)) +
  grattan_colour_manual(4) +
  facet_wrap(~fuel_class, scales = "free_y") + 
  labs(title = "Newer vehicles travel significantly further than older vehicles",
       subtitle = "Estimated km travelled per year by vehicle type",
       caption = "Note: Age 0 vehicles have been excluded from this chart",
       x = "Vehicle age")

p3_wc


grattan_save(filename = "atlas/appendix/p3_wc.pdf",
             object = p3_wc,
             save_ppt = FALSE,
             type = "wholecolumn",
             device = cairo_pdf)




# Freight growth -------------------------------------------------

all_fleets <- read_rds("data/all_fleets.rds")
library(patchwork)

freight_growth <- all_fleets %>% 
  group_by(fleet_year, vkt_scenario) %>% 
  summarise(total_vkts = sum(vkt * total)) %>% 
  group_by(vkt_scenario) %>% 
  mutate(perc_change = total_vkts[fleet_year == 2020],
         perc_change = (total_vkts / perc_change) - 1) %>% 
  select(-total_vkts) %>% 
  pivot_wider(names_from = vkt_scenario,
              values_from = perc_change)


cumulative <- freight_growth %>% 
  filter(fleet_year <= 2040) %>% 
  ggplot() +
  geom_ribbon(aes(
    x = fleet_year,
    ymin = vkt_lower,
    ymax = vkt_upper),
    fill = grattan_orange,
    colour = NA,
    alpha = 0.2) +
  geom_line(aes(x = fleet_year,
                y = vkt_central)) +
  theme_grattan() +
  scale_y_continuous_grattan(labels = scales::label_percent(
    accuracy = 1,
    suffix = "%",
    scale = 100),
    limits = c(0, 0.7)) +
  scale_x_continuous_grattan(limits = c(2020, 2041.5),
                             breaks = c(2020, 2030, 2040)) +
  grattan_colour_manual() +
  labs(subtitle = "Cumulative growth",
       x = NULL) 

freight_growth_yoy <- all_fleets %>% 
  group_by(fleet_year, vkt_scenario) %>% 
  summarise(total_vkts = sum(vkt * total)) %>% 
  group_by(vkt_scenario) %>% 
  arrange(vkt_scenario, fleet_year) %>% 
  mutate(perc_change = total_vkts / lag(total_vkts) - 1) %>% 
  select(-total_vkts) %>% 
  pivot_wider(names_from = vkt_scenario,
              values_from = perc_change)

yoy <- freight_growth_yoy %>% 
  ggplot() +
  geom_ribbon(aes(
    x = fleet_year,
    ymin = vkt_lower,
    ymax = vkt_upper),
    fill = grattan_orange,
    colour = NA,
    alpha = 0.2) +
  geom_line(aes(x = fleet_year,
                y = vkt_central)) +
  theme_grattan() +
  scale_y_continuous_grattan(
    limits = c(0, 0.035),
    breaks = c(0, 0.01, 0.02, 0.03),
    labels = scales::label_percent(
      accuracy = 1.0,
      suffix = "%",
      scale = 100)) +
  scale_x_continuous_grattan(limits = c(2020, 2040),
                             breaks = c(2020, 2030, 2040)) +
  grattan_colour_manual() +
  labs(subtitle = "Year on year growth",
       y = " ",
       x = NULL)


p4 <- cumulative + yoy #+
#  plot_annotation(
#    title = "Freight activity is estimated to grow",
#    subtitle = "Cumulative and year on year growth in freigth activity from 2020 levels")


p4


grattan_save(filename = "atlas/appendix/p4.pdf",
             object = p4,
             save_ppt = FALSE,
             force_labs = TRUE,
             type = "normal",
             device = cairo_pdf)


# ZE-HDV sales ------------------------------------------------------

source("R/model-inputs/07-electric-hdvs.R")

p5_wc <- electric_uptake %>% 
  mutate(fuel_class = if_else(fuel_class == "Non-freight carrying trucks",
                              "Non-freight \ncarrying trucks",
                              fuel_class)) %>% 
  filter(sales_year <= 2040) %>% 
  ggplot(aes(x = sales_year,
             y = electric_share,
             colour = fuel_class)) +
  geom_line() +
  theme_grattan() +
  grattan_colour_manual() +
  scale_x_continuous_grattan(limits = c(2020, 2042),
                             breaks = c(2020, 2030, 2040)) +
  scale_y_continuous_grattan(breaks = c(0, 0.5, 1),
                             labels = scales::label_percent()) +
  labs(title = "Zero emissions HDV estimates",
       subtitle = "Proportion of new sales estimated to be zero emissions heavy vehicles",
       x = NULL) +
  facet_wrap(~fuel_class)


p5_wc


grattan_save(filename = "atlas/appendix/p5_wc.pdf",
             object = p5_wc,
             save_ppt = FALSE,
             type = "wholecolumn")



# Forecast CO2 emissions ----------------------------------------------


all_fleets <- read_rds("data/all_fleets.rds")


co2 <- all_fleets %>% 
  filter(pollutant == "ex_nox_l") %>% 
  mutate(fuel_consumption = diesel_rate_100 * vkt / 100 * (1 - electric_share) * total,
         co2_ice = total * vkt * diesel_fuel_to_co2(diesel_rate_100) * (1 - electric_share),
         co2_ev = total * vkt * electric_share * ev_consumption * 1000 * ei_g_wh,
         co2 = co2_ice + co2_ev) %>% 
  group_by(fleet_year, fuel_class, vkt_scenario) %>% 
  #Summarise and convert from g to Mt
  summarise(total_co2_mt = sum(co2) / 1000000000000)



#Importing DISER data + getting only trucks + buses

co2_forecasts <- read_xlsx("data-raw/diser-co2-forecasts.xlsx",
                           sheet = "Sheet1") %>% 
  pivot_longer(cols = (2:11),
               values_to = "emissions_mt",
               names_to = "sector") %>% 
  clean_names() %>% 
  mutate(forecast_flag = case_when(
    year < 2021 ~ "historical",
    year >= 2021 ~ "forecast"))


hdv_co2 <- co2_forecasts %>%   
  filter(sector %in% c("Rigid trucks", "Articulated trucks", "Buses")) %>% 
  mutate(sector = factor(sector, 
                         levels = c("Articulated trucks", "Rigid trucks", "Buses"))) 


hdv_co2 <- bind_rows(
  hdv_co2,
  
  hdv_co2 %>% 
    filter(year == 2021) %>% 
    mutate(forecast_flag = "historical")
)


co2_ours <- co2 %>% 
  mutate(forecast_flag = "Grattan forecast") %>% 
  rename("emissions_mt" = total_co2_mt,
         "year" = fleet_year,
         "sector" = fuel_class) %>% 
  pivot_wider(names_from = vkt_scenario,
              values_from = emissions_mt) %>% 
  mutate("emissions_mt"= vkt_central)


co2_forecasts <- bind_rows(
  co2_ours,
  hdv_co2
)



hdv_co2 <- co2_forecasts %>%   
  filter(sector %in% c("Rigid trucks", "Articulated trucks", "Buses")) %>% 
  mutate(sector = factor(sector, 
                         levels = c("Articulated trucks", "Rigid trucks", "Buses"))) 


#just plotting grattan/historical

hdv_co2 <- hdv_co2 %>% 
  mutate(include = case_when(
    (forecast_flag == "Grattan forecast" & year > 2021) ~ TRUE,
    (forecast_flag == "historical" & year <= 2021) ~ TRUE,
    (forecast_flag == "forecast" & year > 2020 ~ TRUE))) %>% 
  filter(include ==  TRUE)

hdv_co2 <- bind_rows(
  hdv_co2,
  
  hdv_co2 %>% 
    filter(year == 2021) %>% 
    mutate(forecast_flag = "Grattan forecast")) %>% 
  filter(forecast_flag != "forecast")



p6 <- hdv_co2 %>% 
  
  filter(year <= 2030,
         year >= 2005) %>% 
  
  ggplot() +
  
  #the confidence band
  geom_ribbon(data = co2_forecasts %>% 
                filter(sector %in% c("Rigid trucks", 
                                     "Articulated trucks", "Buses"),
                       forecast_flag == "Grattan forecast",
                       year <= 2031) %>% 
                mutate(sector = factor(sector, 
                                       levels = c("Articulated trucks", 
                                                  "Rigid trucks", "Buses"))),
              aes(
                x = year,
                ymin = vkt_lower,
                ymax = vkt_upper,
                group = sector),
              fill = grattan_red,
              colour = NA,
              alpha = 0.18) +
  
  geom_line(aes(x = year, 
                y = emissions_mt,
                colour = forecast_flag)) +
  
  theme_grattan() + 
  grattan_colour_manual(2, 
                        reverse = TRUE) +
  scale_x_continuous_grattan(breaks = c(2010, 2021, 2030),
                             limits = c(2004, 2033)) +
  scale_y_continuous_grattan(breaks = c(0, 5, 10, 15),
                             limits = c(0, 19)) +
  
  geom_point(data = hdv_co2 %>% 
               filter(year %in% c(2005, 2021, 2030)),
             aes(x = year, 
                 y = emissions_mt,
                 colour = forecast_flag)) +
  
  geom_text(data = hdv_co2 %>% 
              filter(year %in% c(2005, 2021, 2030)),
            aes(x = year + 1,
                y = emissions_mt - 1,
                label = round(emissions_mt, 1),
                colour = forecast_flag),
            check_overlap = TRUE,
            size = 5,
            fontface = "bold") +
  
  geom_text(data = hdv_co2 %>% 
              filter(sector == "Articulated trucks"),
            aes(
              x = 2005.7,
              y = 13.9,
              label = "Historical \nemissions",
              hjust = 0,
              size = 11),
            colour = grattan_orange,
            fontface = "bold",
            check_overlap = TRUE) +
  
  geom_text(data = hdv_co2 %>% 
              filter(sector == "Articulated trucks"),
            aes(
              x = 2018.7,
              y = 16,
              label = "Forecast \nemissions",
              hjust = 0,
              size = 11),
            colour = grattan_red,
            fontface = "bold",
            check_overlap = TRUE) +
  
  
  facet_wrap(~sector) +
  
  labs(title = "Emissions from heavy vehicles are forecast to rise",
       subtitle = "Annual greenhouse gas emissions (Mt CO2e) from HDVs",
       x = NULL)


p6 



grattan_save(filename = "atlas/appendix/p6.pdf",
             object = p6,
             save_ppt = FALSE,
             type = "normal",
             device = cairo_pdf)




# Pollutant emissions - NOx and PM ---------------------------------

policy_outcomes <- read_rds("data/policy_outcomes.rds")


nox_pm25 <- policy_outcomes %>% 
  filter(pollutant_cat2 %in% c("nox", "pm25")) %>% 
  group_by(fleet_year, scenario, vkt_scenario, pollutant_cat2) %>% 
  summarise(pollutant_total = sum(pollutant_total)) %>% 
  filter(scenario == "baseline")

nox <- nox_pm25 %>% 
  filter(pollutant_cat2 == "nox") %>% 
  mutate(pollutant_cat2 = case_when(
    pollutant_cat2 == "nox" ~ "NOx",
    pollutant_cat2 == "pm25" ~ "PM2.5")) %>% 
  filter(fleet_year <= 2040) %>% 
  pivot_wider(names_from = vkt_scenario,
              values_from = pollutant_total) %>% 
  
  ggplot(aes(colour = pollutant_cat2,
             fill = pollutant_cat2)) +
  geom_ribbon(aes(x = fleet_year,
                  ymin = vkt_lower / 1000,
                  ymax = vkt_upper / 1000),
              alpha = 0.2,
              colour = NA,
              fill = grattan_orange) +
  geom_line(aes(x = fleet_year,
                y = vkt_central / 1000),
            colour = grattan_orange) +
  scale_y_continuous_grattan(limits = c(0, 150)) +
  scale_x_continuous_grattan(breaks = c(2020, 2030, 2040),
                             limits = c(2020, 2042)) +
  theme_grattan() +
  labs(#title = "NOx and PM2.5 emissions are are likely to remain persistent without action",
       subtitle = "NOx emissions",
       x = NULL) 

pm25 <- nox_pm25 %>% 
  filter(pollutant_cat2 == "pm25") %>% 
  mutate(pollutant_cat2 = case_when(
    pollutant_cat2 == "nox" ~ "NOx",
    pollutant_cat2 == "pm25" ~ "PM2.5")) %>% 
  filter(fleet_year <= 2040) %>% 
  pivot_wider(names_from = vkt_scenario,
              values_from = pollutant_total) %>% 
  
  ggplot(aes(colour = pollutant_cat2,
             fill = pollutant_cat2)) +
  geom_ribbon(aes(x = fleet_year,
                  ymin = vkt_lower / 1000,
                  ymax = vkt_upper / 1000),
              alpha = 0.2,
              colour = NA,
              fill = grattan_red) +
  geom_line(aes(x = fleet_year,
                y = vkt_central / 1000),
            colour = grattan_red) +
  scale_y_continuous_grattan(limits = c(0, 6)) +
  scale_x_continuous_grattan(breaks = c(2020, 2030, 2040),
                             limits = c(2020, 2042)) +
  theme_grattan() +
  grattan_colour_manual(2) +
  grattan_fill_manual(2) +
  labs(#title = "NOx and PM2.5 emissions are are likely to remain persistent without action",
       subtitle = "PM2.5 emissions",
       x = NULL) 


p7 <- nox + pm25

p7


grattan_save(filename = "atlas/appendix/p7.pdf",
             object = p7,
             save_ppt = FALSE,
             type = "normal",
             force_labs = TRUE,
             device = cairo_pdf)



# Pollutant health costs -------------------------------------------

policy_outcomes <- read_rds("data/policy_outcomes.rds")


poll_health_costs <- policy_outcomes %>% 
  group_by(fleet_year, scenario, vkt_scenario) %>% 
  summarise(health_cost_total = sum(health_cost_total)) %>% 
  filter(scenario == "baseline")

p8 <- poll_health_costs %>% 
  pivot_wider(names_from = vkt_scenario,
              values_from = health_cost_total) %>% 
  
  ggplot() +
  
  geom_ribbon(aes(x = fleet_year,
                  ymin = vkt_lower / 1000000000,
                  ymax = vkt_upper / 1000000000),
              alpha = 0.2,
              colour = NA) +
  geom_line(aes(x = fleet_year,
                y = vkt_central / 1000000000)) +
  scale_y_continuous_grattan(labels = scales::label_dollar(suffix = "b"),
                             limits = c(0, 4),
                             breaks = c(0, 2, 4)) +
  scale_x_continuous_grattan(breaks = c(2020, 2030, 2040),
                             limits = c(2020, 2040)) +
  theme_grattan() +
  grattan_colour_manual(4) +
  grattan_fill_manual(4) +
  labs(title = "Without intervention, health costs from heavy vehicles will rise",
       subtitle = "Estimated business as usual annual health cost, ($ millions, undiscounted)",
       caption = "Confidence intervals reflect uncertainty in estimated freight growth only.",
       x = NULL)


p8


grattan_save(filename = "atlas/appendix/p8.pdf",
             object = p8,
             save_ppt = FALSE,
             type = "normal",
             device = cairo_pdf)


# Euro VI scenario NOx/PM -------------------------------------------

nox2 <- policy_outcomes %>% 
  group_by(scenario, vkt_scenario, pollutant_cat2, fleet_year) %>% 
  summarise(pollutant_total = sum(pollutant_total)) %>% 
  filter(scenario %in% c("Euro 6 (2024)", "Euro 6 (2027)", "baseline"),
         fleet_year <= 2040) %>% 
  mutate(scenario = factor(scenario, levels = c("Euro 6 (2024)", "Euro 6 (2027)", "baseline"))) %>% 
  pivot_wider(names_from = vkt_scenario,
              values_from = pollutant_total) %>% 
  filter(pollutant_cat2 %in% c("nox")) %>% 
  
  ggplot() + 
  
  geom_ribbon(aes(x = fleet_year,
                  ymin = vkt_lower / 1000,
                  ymax = vkt_upper / 1000,
                  fill = scenario),
              colour = NA,
              alpha = 0.1) +
  
  geom_line(aes(x = fleet_year,
                y = vkt_central / 1000,
                colour = scenario)) +
  
  theme_grattan() +
  grattan_colour_manual(3) +
  grattan_fill_manual(3) +
  scale_y_continuous_grattan(limits = c(0, 150)) +
  scale_x_continuous_grattan(limits = c(2020, 2042),
                             breaks = c(2020, 2030, 2040)) +
  
  labs(subtitle = "NOx emissions",
       x = NULL)



pm2 <- policy_outcomes %>% 
  group_by(scenario, vkt_scenario, pollutant_cat2, fleet_year) %>% 
  summarise(pollutant_total = sum(pollutant_total)) %>% 
  filter(scenario %in% c("Euro 6 (2024)", "Euro 6 (2027)", "baseline"),
         fleet_year <= 2040) %>% 
  mutate(scenario = factor(scenario, levels = c("Euro 6 (2024)", "Euro 6 (2027)", "baseline"))) %>% 
  pivot_wider(names_from = vkt_scenario,
              values_from = pollutant_total) %>% 
  filter(pollutant_cat2 %in% c("pm25")) %>% 
  
  ggplot() + 
  
  geom_ribbon(aes(x = fleet_year,
                  ymin = vkt_lower / 1000,
                  ymax = vkt_upper / 1000,
                  fill = scenario),
              colour = NA,
              alpha = 0.1) +
  
  geom_line(aes(x = fleet_year,
                y = vkt_central / 1000,
                colour = scenario)) +
  
  geom_text(aes(x = 2040,
                y = 5.8,
                label = "Baseline"),
            colour = grattan_red,
            fontface = "bold",
            hjust = "right",
            check_overlap = TRUE) +
  
  geom_text(aes(x = 2040,
                y = 5.4,
                label = "Euro VI (2027)"),
            colour = grattan_orange,
            fontface = "bold",
            hjust = "right",
            check_overlap = TRUE) +
  
  geom_text(aes(x = 2040,
                 y = 5.0,
                 label = "Euro VI (2024)"),
             colour = grattan_yellow,
             fontface = "bold",
             hjust = "right",
            check_overlap = TRUE) +
  
  theme_grattan() +
  grattan_colour_manual(3) +
  grattan_fill_manual(3) +
  scale_y_continuous_grattan(limits = c(0, 6)) +
  scale_x_continuous_grattan(limits = c(2020, 2040),
                             breaks = c(2020, 2030, 2040)) +
  
  labs(subtitle = "PM2.5 emissions",
       x = NULL)


p9 <- nox2 + pm2

p9


grattan_save(filename = "atlas/appendix/p9.pdf",
             object = p9,
             save_ppt = FALSE,
             force_labs = TRUE,
             type = "normal",
             device = cairo_pdf)




# Euro VI scenario costs ---------------------------------------------


p10 <- policy_outcomes %>% 
  group_by(scenario, vkt_scenario, fleet_year) %>% 
  summarise(health_cost_total = sum(health_cost_total)) %>% 
  filter(scenario %in% c("Euro 6 (2024)", "Euro 6 (2027)", "baseline")) %>% 
  mutate(scenario = factor(scenario, levels = c("Euro 6 (2024)", "Euro 6 (2027)", "baseline"))) %>% 
  pivot_wider(names_from = vkt_scenario,
              values_from = health_cost_total) %>% 
  filter(fleet_year <= 2040) %>% 
  
  ggplot() + 
  
  geom_ribbon(aes(x = fleet_year,
                  ymin = vkt_lower / 1000000000,
                  ymax = vkt_upper / 1000000000,
                  fill = scenario),
              colour = NA,
              alpha = 0.15) +
  
  geom_line(aes(x = fleet_year,
                y = vkt_central / 1000000000,
                colour = scenario)) +
  
  theme_grattan() +
  grattan_colour_manual(3) +
  grattan_fill_manual(3) +
  scale_y_continuous_grattan(label = scales::label_dollar(suffix = "b"),
                             limits = c(0, 4.5)) +
  scale_x_continuous_grattan(limits = c(2020, 2041),
                             breaks = c(2020, 2030, 2040)) +
  
  geom_text(aes(x = 2040,
                y = 2.7,
                label = "Baseline"),
            colour = grattan_red,
            fontface = "bold",
            hjust = "right",
            check_overlap = TRUE) +
  
  geom_text(aes(x = 2040,
                y = 2.25,
                label = "Euro VI (2027)"),
            colour = grattan_orange,
            fontface = "bold",
            hjust = "right",
            check_overlap = TRUE) +
  
  geom_text(aes(x = 2040,
                y = 1.45,
                label = "Euro VI (2024)"),
            colour = grattan_yellow,
            fontface = "bold",
            hjust = "right",
            check_overlap = TRUE) +
  
  labs(title = "Euro 6 substantially decreases the health costs from pollution emitted from heavy vehicles",
       subtitle = "NOx and PM2.5 emissions from heavy vehicles ($ billions)",
       x = NULL)


p10


grattan_save(filename = "atlas/appendix/p10.pdf",
             object = p10,
             save_ppt = FALSE,
             type = "normal",
             device = cairo_pdf)



# tech target/zev target CO2 emissions ---------------------------------


source("R/model/11-policy-outcomes.R")


co2_colours <- c("Electric and engine and tyre standards" = grattan_yellow,
                 "Electric targets" = grattan_red,
                 "engine and tyre standards" = grattan_orange,
                 "baseline" = grattan_darkred,
                 "historical" = grattan_black)


p11_wc <- historical_projection_comb %>% 
  group_by(scenario, fleet_year, vkt_scenario) %>% 
  summarise(total_co2_mt = sum(total_co2_mt)) %>% 
  pivot_wider(names_from = vkt_scenario,
              values_from = total_co2_mt) %>% 
  filter(scenario %in% c("historical",
                         "baseline",
                         "Electric targets",
                         "engine and tyre standards",
                         "Electric and engine and tyre standards")) %>% 
  
  mutate(scenario = factor(scenario, levels = names(co2_colours)),
         dash = case_when(scenario == "historical" ~ "1",
                          scenario != "historical" ~ "2")) %>% 
  
  ggplot(aes(x = fleet_year, 
             y = vkt_central,
             colour = scenario)) +
  
  geom_ribbon(aes(
    ymin = vkt_lower,
    ymax = vkt_upper,
    fill = scenario),
    alpha = 0.075,
    colour = NA) +
  
  geom_line(aes(linetype = dash)) +
  
  theme_grattan() +
  scale_y_continuous_grattan(limits = c(0, 32)) +
  scale_x_continuous_grattan(limits = c(2000, 2040),
                             breaks = c(2000, 2020, 2040)) +
  scale_colour_manual(values = co2_colours) +
  scale_fill_manual(values = co2_colours) +
  labs(title = "A combination of vehicle standards and ZEV mandates can bring down HDV emissions",
       subtitle = "Annual carbon emissions from HDVs (Mt CO2)",
       caption = "Historical emissions are from DISER. Forecasts from Grattan analysis. Does not include scope 2 emissions
       from diesel vehicles.",
       x = NULL) +
  
  geom_text(aes(x = 2015,
                y = 17,
                label = "Forecast emissions",
                hjust = 0),
            colour = grattan_darkred,
            fontface = "bold",
            check_overlap = TRUE) +
  
  geom_text(aes(x = 2011,
                y = 23.5,
                label = "Historical",
                hjust = 0),
            colour = grattan_black,
            fontface = "bold",
            check_overlap = TRUE) +
  
  geom_text(aes(x = 2015,
                y = 15,
                label = "ZEV targets",
                hjust = 0),
            colour = grattan_red,
            fontface = "bold",
            check_overlap = TRUE)  +
  
  geom_text(aes(x = 2015,
                y = 13,
                label = "Technology standards",
                hjust = 0),
            colour = grattan_orange,
            fontface = "bold",
            check_overlap = TRUE) +
  
  geom_text(aes(x = 2015,
                y = 11,
                label = "Technology standards + ZEV targets",
                hjust = 0),
            colour = grattan_yellow,
            fontface = "bold",
            check_overlap = TRUE)


p11_wc


grattan_save(filename = "atlas/appendix/p11_wc.pdf",
             object = p11_wc,
             save_ppt = FALSE,
             type = "wholecolumn",
             device = cairo_pdf)



# CBA results -------------------------------------------------

source("R/costs-modelling/CBA.R")


colour_vals <- c("Infrast-\nructure costs" = grattan_grey5,
                 "Vehicle costs" = grattan_darkred,
                 "Time & weight penalty" = grattan_red,
                 "Avoided\nHealth costs" = grattan_lightorange,
                 "Avoided\nCO2" = grattan_yellow,
                 "Reduced\nnoise" = grattan_lightyellow,
                 "Reduced\nMainte-\nnance costs" = grattan_lightblue,
                 "Avoided\nfuel costs" = grattan_darkblue)


cba_benefit <- cba_summary_e_6$avoided_cost_b %>% sum()

# Euro 6
p12_wc <- cba_summary_e_6 %>% 
  mutate(cost_type = case_when(
    cost_type == "infrastructure_cost" ~ "Infrast-\nructure costs",
    cost_type == "purchase_price" ~ "Vehicle costs",
    cost_type == "time_weight_penalty" ~ "Time & weight penalty",
    cost_type == "health_cost_total" ~ "Avoided\nHealth costs",
    cost_type == "co2_social_cost" ~ "Avoided\nCO2",
    cost_type == "noise_cost" ~ "Reduced\nnoise",
    cost_type == "maintenance_cost_total" ~ "Reduced\nMainte-\nnance costs",
    cost_type == "fuel_cost" ~ "Avoided\nfuel costs"),
    cost_type = factor(cost_type, levels = names(colour_vals))) %>% 
  
  arrange(cost_type) %>% 
  mutate(cost_start = lag(cumsum(avoided_cost_b),
                          default = 0),
         cost_end = cumsum(avoided_cost_b),
         xmin = 1,
         xmin = cumsum(xmin),
         xmax = xmin + 1) %>%  
  
  ggplot() +
  
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = grattan_grey4) +
  
  geom_rect(aes(ymin = cost_start,
                ymax = cost_end,
                xmin = xmin - 0.4,
                xmax = xmax - 0.6,
                x = cost_type,
                fill = cost_type,
                colour = cost_type),
            alpha = 0.95) +
  
  geom_segment(data = . %>% 
                 filter(cost_type != "Avoided\nfuel costs"),
               aes(x = xmin - 0.4,
                   xend = xmax + 0.4,
                   yend = cost_end,
                   y = cost_end)) +
  
  theme_grattan() +
  theme(axis.text.x = element_text(size = 14)) +
  scale_x_discrete(labels = label_wrap(8)) +
  scale_y_continuous_grattan(labels = scales::label_dollar(suffix = "b"),
                             limits = c(-15, 12)) +
  scale_fill_manual(values = colour_vals) +
  scale_colour_manual(values = colour_vals) +
  
  
  geom_segment(aes(y = 0, yend = cba_benefit, x = 7, xend = 7), colour = grattan_grey3) +
  geom_segment(aes(y = cba_benefit, yend = cba_benefit, x = 7, xend = 7.1), colour = grattan_grey3) +
  geom_segment(aes(y = 0, yend = 0 , x = 7, xend = 7.1), colour = grattan_grey3) +
  
  grattan_label(aes(x = 7 - 0.1,
                    y = 5,
                    label = paste0("Estimated net \nbenefit of $", round(cba_benefit, 1), "b")),
                hjust = "right",
                fontface = "bold",
                colour = grattan_grey4) +
  
  grattan_label(data = . %>% 
                  filter(avoided_cost_b < 0),
                aes(x = cost_type,
                    y = cost_end - 1.4,
                    label = paste0("-$", round(abs(avoided_cost_b), digits = 1), "b"),
                    colour = cost_type)) +
  grattan_label(data = . %>% 
                  filter(avoided_cost_b > 0),
                aes(x = cost_type,
                    y = cost_end + 1.4,
                    label = paste0("$", round(abs(avoided_cost_b), digits = 1), "b"),
                    colour = cost_type)) +
  
  
  labs(title = "The benefits of accelerating zero emission truck uptake outweighs the costs",
       subtitle = "Estimated costs and benefits of zero emissions targets for heavy vehicles",
       x = NULL,
       caption = "Details of CBA metholody are included in appendix XX. Calculated using a 7% discount rate.")



p12_wc


grattan_save(filename = "atlas/appendix/p12_wc.pdf",
             object = p12_wc,
             save_ppt = FALSE,
             type = "wholecolumn",
             device = cairo_pdf)




# ZEV target trajectories ------------------------------------------


zev_targets <- read_xlsx("data-raw/electric-uptake.xlsx",
                         sheet = "electric-targets")


p13_wc <- zev_targets %>% 
  filter(sales_year <= 2040) %>% 
  ggplot(aes(x = sales_year,
             y = electric_target / 100,
             colour = fuel_class)) +
  geom_point() +
  geom_line()  +
  scale_y_continuous_grattan(label = scales::label_percent(),
                             limits = c(0, 1),
                             breaks = c(0, 0.5, 1)) +
  scale_x_continuous_grattan(limits = c(2023.5, 2041)) +
  
  geom_text(aes(x = 2033,
                y = 0.65,
                label = "Rigid trucks"),
            fontface = "bold",
            colour = grattan_red,
            check_overlap = TRUE) +
  
  geom_text(aes(x = 2036,
                y = 0.3,
                label = "Articulated trucks"),
            colour = grattan_orange,
            fontface = "bold",
            check_overlap = TRUE) +
  
  theme_grattan() +
  grattan_colour_manual(2) +
  labs(title = "Electric vehicle targets",
       subtitle = "Proportion of new sales required to be ZE-HDVs",
       x = NULL)


p13_wc


grattan_save(filename = "atlas/appendix/p13_wc.pdf",
             object = p13_wc,
             save_ppt = FALSE,
             type = "wholecolumn",
             device = cairo_pdf)




# Merging all PDFs ------------------------------------------------------------

library(pdftools)

# Getting all of the paths 
paths <- tibble(paths = c("p1_wc", "p2_wc", "p3_wc", "p4", "p5_wc", "p6", "p7", "p8", "p9", "p10", "p11_wc", "p12_wc", "p13_wc")) %>% 
  mutate(paths = if_else( 
    str_detect(paths, "wc") == TRUE,
    paste0("atlas/appendix/", paths, "/", paths, "_wholecolumn.pdf"),
    paste0("atlas/appendix/", paths, "/", paths, "_normal.pdf")))



# Using the paths to merge all the pdfs 

pdf_combine(as_vector(paths), output = "atlas/appendix/joined.pdf")



