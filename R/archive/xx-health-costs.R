
# Health costs 

source("R/06-vkt-urban-rural-split.R")
source("R/07-fleet-turnover.R")
source("R/08-pollutant-emissions.R")
source("R/09-scenario-euro-6.R")

#' This script uses the analysis from previous scripts (08/09) to pull together the 
#' health costs associated with the estimated pollutant emissions. 
#' 



# Importing health cost data ----------------------------------

#' Health cost data (damage costs) were provided by BITRE, and are based on estimates
#' derived from Jacbon Marsden Assc consulting work into fuel quality and emissions over
#' the 2016-2018 period. 
#' 

# All damage costs are in $ / tonne pollutant in 2016 Aud

damage_costs <- read_xlsx("data/BITRE-damage-costs.xlsx",
         sheet = "r-input") %>% 
  clean_names() %>% 
  pivot_longer(cols = (2:15),
               values_to = "cost_2016_au",
               names_to = "pollutant") %>% 
  mutate(region = case_when(
      str_detect(pollutant, "_metro") == TRUE ~ "metro",
      str_detect(pollutant, "nonmet") == TRUE ~ "non_metro"),
    pollutant = case_when(
      str_detect(pollutant, "_metro") == TRUE ~ substr(pollutant, start = 1, stop = nchar(pollutant) - 6),
      str_detect(pollutant, "nonmet") == TRUE ~ substr(pollutant, start = 1, stop = nchar(pollutant) - 7))) %>% 
  #and just reordering and inflating to 2020 values
  select(pollutant, region, year, cost_2016_au) %>% 
  mutate(cost_2016_au = cost_2016_au * 1.0646) %>% 
  rename("damage_cost_t" = cost_2016_au,
         "fleet_year" = year)


# Combining with model forecasts ---------------------------------------

#These costs currently are only pm2.5 and NOx, from exhaust emissions only 

fleet_costs_combined <- left_join(euro_scenarios,
          damage_costs %>%
            mutate(pollutant = str_c(pollutant, "_cost")) %>% 
            pivot_wider(names_from = pollutant,
                        values_from = damage_cost_t))

fleet_costs <- fleet_costs_combined %>%
  group_by(vkt_scenario, fleet_year, fuel_class, age, region, scenario) %>% 
  #assuming 95% of pm10 is pm2.5 for the moment
  summarise(total_pm25_cost = sum(pm10 * 0.95 * fuel_used * total * pm2_5_combustion_cost / 1000000),
            #note that the remaining 5% of pm10-2.5 emissions are assigned to the non-exhuast and coarse combustion
            #category (the name is just simplified to non-exhuast, but it also includes coarse combustion)
            total_pm10_primary_cost = sum(pm10 * 0.05 * fuel_used * total * pm10_nonexhaust_cost / 1000000),
            total_sox_cost = sum(sox * fuel_used * total * sox_cost / 1000000),
            total_voc_cost = sum(voc * fuel_used * total * hc_voc_cost / 1000000),
            total_nox_cost = sum(nox * fuel_used * total * nox_cost / 1000000)) 


# Currently this does not include secondary pollutants. 
# This paper: https://www.researchgate.net/publication/261404252_Evaluation_of_the_SO2_and_NOX_offset_ratio_method_to_account_for_secondary_PM25_formation
# outlines a basic method for estimating secondary pollutants but it gives crazy numbers
# and probably isn't really applicable in this situation anyway. 


# Previous studies (ie.e https://www.epa.nsw.gov.au/~/media/EPA/Corporate%20Site/resources/air/HealthPartEmiss.ashx)
# have excluded secondary costs because of this uncertainty - that is probably the approach we should take here 
# because we have no real way of establishing this (and it' sporbbaly unlikely to significantly affect results.)
# This makes our estimates more conservative. 


# Fleet cost plots -----------------------------------

# First just plotting the headline results - currently this is only including PM2.5
# and NOx values (more work to be done with PM10/secondary and all that)

fleet_costs %>% 
  pivot_longer(cols = 7:11,
               values_to = "cost",
               names_to = "pollutant") %>% 
  group_by(scenario, vkt_scenario, fleet_year) %>% 
  summarise(cost = sum(cost)) %>% 
  pivot_wider(values_from = cost,
              names_from = vkt_scenario) %>% 
  
  ggplot(aes(x = fleet_year)) +
  geom_ribbon(aes(
    ymin = vkt_lower / 1000000,
    ymax = vkt_upper / 1000000,
    fill = scenario),
    alpha= 0.2,
    colour = NA) +
  geom_line(aes(y = vkt_central / 1000000,
            colour = scenario)) +
  grattan_colour_manual() + 
  grattan_fill_manual() +
  scale_y_continuous_grattan(labels = scales::label_dollar(),
                             limits = c(0, 3000)) +
  theme_grattan(legend = "top") +
  labs(title = "Without intervention, health costs from heavy vehicles will rise",
       subtitle = "Estimated annual health cost, ($ millions, undiscounted)",
       x = NULL)
  
# Encouragingly, these costs re pretty much in line with BITRE estimates from much earlier work
# (https://www.bitre.gov.au/sites/default/files/wp_063.pdf) when it comes to order of magnitude.
# They have total costs around ~$1.5-$4 billion/year. But they also They do also exclude road dust 
# similarly to our estimate. Our estimate is on the lower side, which is unsurprising because
# their study was done in 2005, and pollution from motor vehicle has dropped quite considerably since then. 



# Looking at what's causing it more directly:
# We're going to break it down to see which vehicle types are causing the highest costs
# etc. to figure out where the problem really lies

#Vehicle type

fleet_costs %>% 
  pivot_longer(cols = 7:11,
               values_to = "cost",
               names_to = "pollutant") %>% 
  group_by(scenario, vkt_scenario, fleet_year, fuel_class) %>% 
  summarise(cost = sum(cost)) %>% 
  pivot_wider(values_from = cost,
              names_from = vkt_scenario) %>% 
  filter(fuel_class %in% c("Articulated trucks", "Rigid trucks", "Buses")) %>% 
  
  ggplot(aes(x = fleet_year)) +
  geom_ribbon(aes(
    ymin = vkt_lower / 1000000,
    ymax = vkt_upper / 1000000,
    fill = scenario),
    alpha= 0.2,
    colour = NA) +
  geom_line(aes(y = vkt_central / 1000000,
                colour = scenario)) +
  grattan_colour_manual() + 
  grattan_fill_manual() +
  scale_y_continuous_grattan(labels = scales::label_dollar()) +
  theme_grattan(legend = "top") +
  labs(title = "Without intervention, health costs from heavy vehicles will rise",
       subtitle = "Estimated annual health cost, ($ millions, undiscounted)",
       x = NULL) +
  facet_wrap(~fuel_class)


# Vehicle age  ---------------------------------------

fleet_costs %>% 
  filter(fleet_year == 2021) %>% 
  pivot_longer(cols = 7:11,
               values_to = "cost",
               names_to = "pollutant") %>% 
  group_by(age, scenario, vkt_scenario, fleet_year, fuel_class) %>% 
  summarise(cost = sum(cost)) %>% 
  pivot_wider(values_from = cost,
              names_from = vkt_scenario) %>% 
  filter(fuel_class %in% c("Articulated trucks", "Rigid trucks", "Buses")) %>% 
  
  ggplot(aes(x = age)) +
  geom_ribbon(aes(
    ymin = vkt_lower / 1000000,
    ymax = vkt_upper / 1000000,
    fill = scenario),
    alpha= 0.2,
    colour = NA) +
  geom_smooth(aes(y = vkt_central / 1000000,
                colour = scenario),
              span = 0.5,
              se = TRUE,
              alpha = 0.1) +
  grattan_colour_manual() + 
  grattan_fill_manual() +
  scale_y_continuous_grattan(labels = scales::label_dollar(),
                             limits = c(0, 40)) +
  theme_grattan(legend = "top") +
  labs(title = "Without intervention, health costs from heavy vehicles will rise",
       subtitle = "Estimated annual health cost, ($ millions, undiscounted)",
       x = NULL) +
  facet_wrap(~fuel_class)


#' Interesting to see that age doesn't really seem to be a big problem for articulated trucks
#' or buses, but that mid range age (~20 year or so) is a really big contributor for Rigid
#' trucks. Probably important to focus any age based policies there really 


# And by pollutant type -------------------------------

fleet_costs %>% 
  pivot_longer(cols = 7:11,
               values_to = "cost",
               names_to = "pollutant") %>% 
  group_by(scenario, vkt_scenario, fleet_year, pollutant) %>% 
  summarise(cost = sum(cost)) %>% 
  pivot_wider(values_from = cost,
              names_from = vkt_scenario) %>% 
  
  ggplot(aes(x = fleet_year)) +
  geom_ribbon(aes(
    ymin = vkt_lower / 1000000,
    ymax = vkt_upper / 1000000,
    fill = scenario),
    alpha= 0.2,
    colour = NA) +
  geom_line(aes(y = vkt_central / 1000000,
                colour = scenario)) +
  grattan_colour_manual() + 
  grattan_fill_manual() +
  scale_y_continuous_grattan(labels = scales::label_dollar()) +
  theme_grattan(legend = "top") +
  labs(title = "NOx is like to be the main driver of health costs in the future",
       subtitle = "Estimated annual health cost, ($ millions, undiscounted)",
       x = NULL) +
  facet_wrap(~pollutant,
             scales = "free_y")

#' Also interesting to see that the vast majority of the cost (particularly in the
#' further future) is driven by NOx, not by PM2.5 (which is probably a bit unexpected really)
#' 
#' Might be worth trying to compare this to past research to make sure it's consistent. 
#' It also probably comes from some secondary pollutants in the work being baked into the 
#' NOx costs (like O3 in particular)









  