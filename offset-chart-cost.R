

# Abatement cost chart 

source("R/08-fleet-turnover.R")
source("R/09-policy-scenarios.R")
source("R/11-policy-outcomes.R")





trucks_and_buses <- historical_projection_comb %>% 
  ungroup() %>% 
  filter(vkt_scenario == "vkt_central",
         scenario %in% c("historical", "baseline")) %>% 
  mutate(forecast_flag = case_when(
    scenario == "historical" ~ "historical",
    scenario != "historical" ~ "forecast")) %>% 
  select(fleet_year, fuel_class, total_co2_mt, forecast_flag) %>% 
  rename("year" = fleet_year,
         "emissions_mt" = total_co2_mt) %>% 
  group_by(year, forecast_flag) %>% 
  summarise(emissions_mt = sum(emissions_mt)) %>% 
  mutate(sector = "Trucks and buses") %>% 
  filter(year <= 2030)




# Creating data on our illustrative paths -----------------------------------

illustrative_forecast <- tribble(
  ~year,   ~forecast_flag,   ~emissions_mt,   ~sector,
  2050,    "illustrative1",      0,       "Trucks and buses",
  2050,    "illustrative2",      20,       "Trucks and buses",
  2050,    "illustrative3",      30,       "Trucks and buses")

#Creating the dataset

trucks_and_buses_all <- bind_rows(
  illustrative_forecast,
  
  trucks_and_buses %>% 
    filter(year == 2030) %>% 
    mutate(forecast_flag = "illustrative1"),
  
  trucks_and_buses %>% 
    filter(year == 2030) %>% 
    mutate(forecast_flag = "illustrative2"),
  
  trucks_and_buses %>% 
    filter(year == 2030) %>% 
    mutate(forecast_flag = "illustrative3"),
  
  trucks_and_buses
)


#Plotting ---------------------------------------------------------------

#The basic trajectory and forecasts -----
trucks_and_buses_all %>% 
  ggplot() +
  geom_line(aes(x = year,
                y = emissions_mt,
                colour = forecast_flag,
                linetype = forecast_flag)) + 
  theme_grattan() +
  scale_colour_manual(values = c(
    grattan_red, grattan_orange,
    grattan_grey3, grattan_grey3, grattan_grey3)) +
  
  
  scale_linetype_manual(values = c(
    "solid", "solid", "dashed", "dashed", "dashed")) +
  
  #grattan_colour_manual(n = 6, reverse = TRUE) +
  grattan_y_continuous(limits = c(-1, 36)) +
  scale_x_continuous_grattan(limits = c(2005, 2070),
                             breaks = c(2010, 2030, 2050)) +
  coord_cartesian(ylim = c(0, 35)) +
  
  #Adding some aesthetic parts/labels --------

annotate("rect", xmin = 2030, xmax = 2050, ymin = -1, ymax = 36,
         alpha = .1, fill = grattan_lightgrey, colour = grattan_grey5) +
  
  geom_point(data = trucks_and_buses_all %>% 
               filter(year == 2050),
             aes(x = year, y = emissions_mt),
             colour = grattan_red,
             size = 3.5) +
  
  
  geom_text(aes(
    x = 2050.8,
    y = 32.5,
    label = "The expected annual \ncost of offsets:",
    hjust = 0,
    size = 11),
    colour = grattan_grey4,
    fontface = "bold",
    check_overlap = TRUE) +
  
  geom_text(aes(
    x = 2005,
    y = 23.4,
    label = "Historical \nemissions",
    hjust = 0,
    size = 11),
    colour = grattan_orange,
    fontface = "bold",
    check_overlap = TRUE) +
  
  geom_text(aes(
    x = 2019,
    y = 26,
    label = "Forecast \nemissions",
    hjust = 0,
    size = 11),
    colour = grattan_red,
    fontface = "bold",
    check_overlap = TRUE) +
  
  geom_text(aes(
    x = 2037,
    y = 31.5,
    label = "Possible \ntrajectories",
    hjust = 0,
    size = 11),
    colour = grattan_grey5,
    fontface = "bold",
    check_overlap = TRUE) +
  
  #labels for individual offsets
  geom_text(data = trucks_and_buses_all %>% 
              filter(year == 2050),
            aes(x = year + 0.8,
                y = emissions_mt + 0.5,
                label = paste0("$", emissions_mt * 25, "m"),
                hjust = 0,
                size = 11),
            colour = grattan_red,
            fontface = "bold",
            check_overlap = TRUE) +
  
  #geom_vline(xintercept = 2030,
  #           linetype = "solid",
  #           size = 0.5) +
  
  labs(title = "Any emissions not abated by 2050 must be offset",
       subtitle = "Annual greenhouse gas emissions (Mt CO2e) from heavy duty vehicles",
       x = NULL,
       caption = "Includes emissions from rigid truck, articulated truck, and bus categories.
       Forecasts to 2030 from GHG accounts. Forecasts beyond 2030 are illustrative only.
       Assumes an offset cost of $25/tonne in 2050, as consistent with `The Plan'.")


grattan_save(type = "normal",
             save_pptx = TRUE,
             filename = "atlas/cost-of-offsets-chart.pdf")







# Plotting by vehicle type --------------------------------------

sectors <- historical_projection_comb %>% 
  ungroup() %>% 
  filter(vkt_scenario == "vkt_central",
         scenario %in% c("historical", "baseline")) %>% 
  mutate(forecast_flag = case_when(
    scenario == "historical" ~ "historical",
    scenario != "historical" ~ "forecast")) %>% 
  select(fleet_year, fuel_class, total_co2_mt, forecast_flag) %>% 
  rename("year" = fleet_year,
         "emissions_mt" = total_co2_mt,
         "sector" = fuel_class) %>% 
  group_by(year, forecast_flag, sector) %>% 
  summarise(emissions_mt = sum(emissions_mt)) %>% 
  filter(year <= 2030) %>% 
  filter(sector != "Non-freight carrying trucks") %>% 
  mutate(sector = factor(sector,
                         levels = c("Articulated trucks",
                                    "Rigid trucks",
                                    "Buses")))



sectors %>% 
  ggplot() +
  geom_line(aes(x = year, 
                y = emissions_mt,
                colour = forecast_flag)) +
  
  theme_grattan() + 
  grattan_colour_manual(2, 
                        reverse = TRUE) +
  scale_x_continuous_grattan(breaks = c(1990, 2010, 2030),
                             limits = c(1990, 2035)) +
  scale_y_continuous_grattan(breaks = c(0, 5, 10, 15),
                             limits = c(0, 15)) +
  
  geom_point(data = sectors %>% 
               filter(year %in% c(2005, 2021, 2030)),
             aes(x = year, 
                 y = emissions_mt,
                 colour = forecast_flag)) +
  
  geom_text(data = sectors %>% 
              filter(year %in% c(2005, 2030)),
            aes(x = year + 1,
                y = emissions_mt - 0.5,
                label = round(emissions_mt,1),
                colour = forecast_flag),
            check_overlap = TRUE,
            size = 5,
            fontface = "bold") +
  
  geom_text(data = sectors %>% 
              filter(sector == "Articulated trucks"),
            aes(
              x = 1998,
              y = 12.3,
              label = "Historical \nemissions",
              hjust = 0,
              size = 11),
            colour = grattan_orange,
            fontface = "bold",
            check_overlap = TRUE) +
  
  geom_text(data = sectors %>% 
              filter(sector == "Articulated trucks"),
            aes(
              x = 2012,
              y = 14,
              label = "Forecast \nemissions",
              hjust = 0,
              size = 11),
            colour = grattan_red,
            fontface = "bold",
            check_overlap = TRUE) +
  
  
  facet_wrap(~sector) +
  
  labs(title = "Emissions from heavy vehicles are forecast to rise between now and 2030",
       subtitle = "Annual greenhouse gas emissions (Mt CO2e) from heavy duty vehicles",
       x = NULL)




grattan_save(type = "normal",
             save_pptx = TRUE,
             filename = "atlas/sector-projections.pdf")






