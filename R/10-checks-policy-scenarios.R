

#' Policy scenario checks 

#' We're going to chart a series of things and do a series of checks to make sure
#' that all the policy scenarios make sense as they stand

policy_scenarios <- read_rds("data/policy-scenarios.rds")


# First checking the average NOx/PM emissions in each scenario (av g/L equiv)------
#' 
#' 

pm_nox <- policy_scenarios %>% 
  filter(vkt_scenario == "vkt_central",
         fleet_year == 2030) %>% 
  group_by(scenario) %>% 
  summarise(av_pm = mean((1 - electric_share) * ex_nox_l * total, na.rm = TRUE),
            av_nox = mean((1 - electric_share) * ex_pm10_l * total, na.rm = TRUE)) 

pm_nox %>% 
  mutate(scenario = factor(scenario,
                    levels = pm_nox %>% 
                      arrange(av_pm) %>% 
                      pull(scenario))) %>% 
  
  ggplot(aes(y = scenario,
             x = av_pm,
             fill = scenario,
             colour = scenario)) +
  geom_col() +
  theme_grattan() + 
  grattan_fill_manual(10) +
  grattan_colour_manual(10)


pm_nox %>% 
  mutate(scenario = factor(scenario,
                           levels = pm_nox %>% 
                             arrange(av_nox) %>% 
                             pull(scenario))) %>% 
  
  ggplot(aes(y = scenario,
             x = av_nox,
             fill = scenario,
             colour = scenario)) +
  geom_col() +
  theme_grattan() + 
  grattan_fill_manual(10) +
  grattan_colour_manual(10)


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

