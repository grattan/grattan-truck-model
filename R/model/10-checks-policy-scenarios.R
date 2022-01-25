

#' Policy scenario checks 

#' We're going to chart a series of things and do a series of checks to make sure
#' that all the policy scenarios make sense as they stand

policy_scenarios <- read_rds("data/policy-scenarios.rds")


# First checking the average NOx/PM emissions in each scenario (av g/L equiv)------
#' 
#' 
pm_nox <- bind_rows(
    policy_scenarios %>% 
    filter(vkt_scenario == "vkt_central",
           fleet_year == 2040,
           pollutant == "ex_nox_l") %>% 
    group_by(scenario) %>% 
    summarise(av_rate = mean((1 - electric_share) * pollutant_rate * total, na.rm = TRUE)) %>% 
    mutate(pollutant = "nox"),
    
    policy_scenarios %>% 
      filter(vkt_scenario == "vkt_central",
             fleet_year == 2040,
             pollutant == "ex_pm25_l") %>%
      group_by(scenario) %>% 
      summarise(av_rate = mean((1 - electric_share) * pollutant_rate * total, na.rm = TRUE)) %>% 
      mutate(pollutant = "pm"))
    


pm_nox %>% 
  mutate(scenario = factor(scenario,
                    levels = pm_nox %>% 
                      filter(pollutant == "pm") %>% 
                      arrange(av_rate) %>% 
                      pull(scenario))) %>% 
  
  ggplot(aes(y = scenario,
             x = av_rate,
             fill = scenario,
             colour = scenario)) +
  geom_col() +
  theme_grattan() + 
  grattan_fill_manual(10) +
  grattan_colour_manual(10) +
  scale_x_continuous_grattan() +
  facet_wrap(~pollutant,
             scales = "free")


#' Looks good!



# Checking electric vehicle share --------------------

ev_share <- policy_scenarios %>% 
  filter(vkt_scenario == "vkt_central") %>% 
  group_by(scenario, sales_year, fuel_class) %>% 
  summarise(ev_share = mean((electric_share)))

ev_share %>% 
  ggplot(aes(x = sales_year, y = ev_share, 
             colour = scenario, alpha = 0.3)) +
  geom_jitter() +
  #theme_grattan() + 
  grattan_fill_manual(10) +
  grattan_colour_manual(10) +
  facet_wrap(~fuel_class)

# Also looks all good! 

