

#' This script produces charts which illustrate the effects on implementing 
#' Euro VI regulations for the Australian heavy vehicle fleet, in terms of pollution reduction
#' and health costs avoided
#' 
#' The first two charts are those used in the report - extra charts beyond this 


source("R/00-setup.R")
policy_outcomes <- read_rds("data/policy_outcomes.rds")
library(tidyverse)
library(glue)
library(knitr)
library(pdftools)
library(Cairo)


# Charts for report: pollution and costs =============================

colour_levs <- c("Euro 6 (2024)" = grattan_yellow,
                 "Euro 6 (2027)" = grattan_orange,
                 "baseline" = grattan_red)
colour_names <- names(colour_levs)




# Euro VI pollution --------------------------------------------------

c2_euro_vi_pollution <- policy_outcomes %>% 
  filter(pollutant_cat2 %in% c("nox", "pm25")) %>% 
  group_by(scenario, vkt_scenario, fleet_year, pollutant_cat2) %>% 
  summarise(pollutant_total = sum(pollutant_total) / 1000) %>% 
  filter(scenario %in% c("Euro 6 (2024)", "Euro 6 (2027)", "baseline"),
         fleet_year <= 2040) %>% 
  mutate(scenario = factor(scenario, levels = colour_names)) %>% 
  pivot_wider(names_from = vkt_scenario,
              values_from = pollutant_total) %>% 
  
  ggplot() + 
  geom_ribbon(aes(x = fleet_year,
                  ymin = vkt_lower,
                  ymax = vkt_upper,
                  fill = scenario),
              colour = NA,
              alpha = 0.1) +
  
  geom_line(aes(x = fleet_year,
                y = vkt_central,
                colour = scenario)) +
  
  #Points to end of lines
  geom_point(data = . %>% filter(fleet_year == 2040),
             aes(x = fleet_year, y = vkt_central,
                 colour = scenario)) +
  
  theme_grattan() +
  scale_colour_manual(values = colour_levs) +
  scale_fill_manual(values = colour_levs) +
  scale_y_continuous_grattan(limits = c(0,NA)) +
  scale_x_continuous_grattan(limits = c(2020, 2041),
                             breaks = c(2020, 2030, 2040)) +
  
  
  geom_text(data = . %>% filter(pollutant_cat2 == "nox") %>% slice(1),
            aes(x = 2030,
                y = 170,
                label = "NOx emissions"),
            check_overlap = TRUE) +
  geom_text(data = . %>% filter(pollutant_cat2 == "pm25") %>% slice(1),
            aes(x = 2030,
                y = 6.3,
                label = "PM2.5 emissions"),
            check_overlap = TRUE) +
  
  
  theme(plot.subtitle = element_markdown(),
        strip.text.x = element_blank()) +
  labs(#title = "Implementing Euro VI will reduce health costs from pollution",
    #subtitle = "NOx and PM2.5 emissions from heavy vehicles ($ billions)",
    subtitle = glue("<span style='font-size:17pt'>Annual pollution from heavy vehicles ('000 tonnes) if Euro VI is:<br> ",
                    "<span style='font-size:17pt; color:{colour_levs['baseline']}'>**Not introduced**, ",
                    "<span style='font-size:17pt; color:{colour_levs['Euro 6 (2027)']}'>**introduced in 2027**, ",
                    "<span style='font-size:17pt; color:{colour_levs['Euro 6 (2024)']}'>**introduced in 2024**"),
    x = NULL) +
  
  facet_wrap(~pollutant_cat2, scales = "free_y")






# Euro VI scenario costs ---------------------------------------------

c2_euro_vi_cost <- policy_outcomes %>% 
  group_by(scenario, vkt_scenario, fleet_year) %>% 
  summarise(health_cost_total = sum(health_cost_total) / 1000000000) %>% 
  filter(scenario %in% c("Euro 6 (2024)", "Euro 6 (2027)", "baseline"),
         fleet_year <= 2040) %>% 
  mutate(scenario = factor(scenario, levels = colour_names)) %>% 
  pivot_wider(names_from = vkt_scenario,
              values_from = health_cost_total) %>% 
  
  ggplot() + 
  geom_ribbon(aes(x = fleet_year,
                  ymin = vkt_lower,
                  ymax = vkt_upper,
                  fill = scenario),
              colour = NA,
              alpha = 0.1) +
  
  geom_line(aes(x = fleet_year,
                y = vkt_central,
                colour = scenario)) +
  
  theme_grattan() +
  scale_colour_manual(values = colour_levs) +
  scale_fill_manual(values = colour_levs) +
  scale_y_continuous_grattan(label = scales::label_dollar(suffix = "b"),
                             limits = c(0, 4),
                             breaks = c(0, 2, 4)) +
  scale_x_continuous_grattan(limits = c(2020, 2049),
                             breaks = c(2020, 2030, 2040)) +
  
  # Adding segment to show gap between no introduction/2024
  geom_segment(data = . %>% filter(fleet_year == 2040, scenario %in% c("baseline", "Euro 6 (2024)")),
               aes(x = 2040.5, xend = 2040.5,
                   y = min(vkt_central), yend = max(vkt_central)),
               colour = grattan_grey3, linetype = "dashed",
               lineend = "round") +
  
  #And adding caps to the segment
  geom_segment(data = . %>% 
                 filter(fleet_year == 2040, scenario %in% c("baseline", "Euro 6 (2024)")),
               aes(x = c(2040.3, 2040.3), xend = c(2040.7, 2040.7),
                   y = c(min(vkt_central), max(vkt_central)), yend = c(min(vkt_central), max(vkt_central))),
               colour = c(grattan_grey3, grattan_grey3)) +
  
  geom_point(data = . %>% filter(fleet_year == 2040),
             aes(x = fleet_year, y = vkt_central,
                 colour = scenario)) +
  
  geom_richtext(data = . %>% slice(1),
                x = 2040.7, y = 2.5,
                label = glue("<span style= 'font-size:15pt; color:{grattan_grey3}'>If Euro VI is introduced<br>",
                             "<span style='font-size:15pt; color:{colour_levs['Euro 6 (2024)']}'>**in 2024**, ",
                             "<span style='font-size:15pt; color:{grattan_grey3}'>yearly health <br>costs from pollution will <br>be about ",
                             "<span style='font-size:15pt; color:{colour_levs['Euro 6 (2024)']}'>**$1.8b**",
                             "<span style='font-size:15pt; color:{grattan_grey3}'> lower <br>by 2040"),
                hjust = "left",
                lineheight = 1.5,
                fill = NA, label.color = NA) + 
  
  theme(plot.subtitle = element_markdown()) +
  labs(#title = "Implementing Euro VI will reduce health costs from pollution",
       #subtitle = "NOx and PM2.5 emissions from heavy vehicles ($ billions)",
       subtitle = glue("<span style='font-size:17pt'>Health costs from pollution ($ billions) if Euro VI is: ",
                       "<span style='font-size:17pt; color:{colour_levs['baseline']}'>**Not introduced**, <br>",
                       "<span style='font-size:17pt; color:{colour_levs['Euro 6 (2027)']}'>**introduced in 2027**, ",
                       "<span style='font-size:17pt; color:{colour_levs['Euro 6 (2024)']}'>**introduced in 2024**"),
       x = NULL) 


#grattan_save(filename = "atlas/euro-vi-avoided-costs.pdf",
#             type = "wholecolumn",
#             ignore_long_title = TRUE,
#             save_ppt = FALSE,
#             force_labs = TRUE,
#             device = cairo_pdf)





# ---------














# Alternative chart versions (Unused) ---------------------------------------------



# Not used in report: Cumulative health costs:


policy_outcomes %>% 
  group_by(scenario, vkt_scenario, fleet_year) %>% 
  summarise(health_cost_total = sum(health_cost_total) / 1000000000) %>% 
  mutate(health_cost_total = health_cost_total * (1 / ((1 + 0.07) ^ (fleet_year - 2022)))) %>% 
  filter(scenario %in% c("Euro 6 (2024)", "Euro 6 (2027)", "baseline"),
         fleet_year <= 2040 & fleet_year >= 2024) %>% 
  mutate(health_cost_cum = cumsum(health_cost_total)) %>% 
  select(-health_cost_total) %>% 
  mutate(scenario = factor(scenario, levels = colour_names)) %>% 
  pivot_wider(names_from = vkt_scenario,
              values_from = health_cost_cum) %>% 
  
  ggplot() + 
  geom_ribbon(aes(x = fleet_year,
                  ymin = vkt_lower,
                  ymax = vkt_upper,
                  fill = scenario),
              colour = NA,
              alpha = 0.1) +
  
  geom_line(aes(x = fleet_year,
                y = vkt_central,
                colour = scenario)) +
  
  theme_grattan() +
  scale_colour_manual(values = colour_levs) +
  scale_fill_manual(values = colour_levs) +
  scale_y_continuous_grattan(label = scales::label_dollar(suffix = "b"),
                             limits = c(0, 30),
                             breaks = c(0, 10, 20, 30)) +
  scale_x_continuous_grattan(limits = c(2024, 2049),
                             breaks = c(2024, 2028, 2032, 2036, 2040)) +
  
  # Adding segment to show gap between no introduction/2024
  geom_segment(data = . %>% filter(fleet_year == 2040, scenario %in% c("baseline", "Euro 6 (2024)")),
               aes(x = 2040.5, xend = 2040.5,
                   y = min(vkt_central), yend = max(vkt_central)),
               colour = grattan_grey3, linetype = "dashed",
               lineend = "round") +
  
  #And adding caps to the segment
  geom_segment(data = . %>% 
                 filter(fleet_year == 2040, scenario %in% c("baseline", "Euro 6 (2024)")),
               aes(x = c(2040.3, 2040.3), xend = c(2040.7, 2040.7),
                   y = c(min(vkt_central), max(vkt_central)), yend = c(min(vkt_central), max(vkt_central))),
               colour = c(grattan_grey3, grattan_grey3)) +
  
  geom_point(data = . %>% filter(fleet_year == 2040),
             aes(x = fleet_year, y = vkt_central,
                 colour = scenario)) +
  
  geom_richtext(data = . %>% slice(1),
                x = 2040.7, y = 22,
                label = glue("<span style= 'font-size:15pt; color:{grattan_grey3}'>If Euro VI is introduced<br>",
                             "<span style='font-size:15pt; color:{colour_levs['Euro 6 (2024)']}'>**in 2024**, ",
                             "<span style='font-size:15pt; color:{grattan_grey3}'>cumulative health <br>costs from pollution will <br>be about ",
                             "<span style='font-size:15pt; color:{colour_levs['Euro 6 (2024)']}'>**$6b**",
                             "<span style='font-size:15pt; color:{grattan_grey3}'> lower <br>by 2040"),
                hjust = "left",
                lineheight = 1.5,
                fill = NA, label.color = NA) + 
  
  theme(plot.subtitle = element_markdown()) +
  labs(#title = "Implementing Euro VI will reduce health costs from pollution",
    #subtitle = "NOx and PM2.5 emissions from heavy vehicles ($ billions)",
    subtitle = glue("<span style='font-size:17pt'>Cumulative health costs from pollution ($ billions) from 2024 if Euro VI is: ",
                    "<span style='font-size:17pt; color:{colour_levs['baseline']}'><br>**Not introduced**, ",
                    "<span style='font-size:17pt; color:{colour_levs['Euro 6 (2027)']}'>**introduced in 2027**, ",
                    "<span style='font-size:17pt; color:{colour_levs['Euro 6 (2024)']}'>**introduced in 2024**"),
    x = NULL,
    caption = "Note: Calculated with a discount rate of 7%. Does not include residual values.") 



# Further options ----------------------------------------

poll_colour <- c("nox" = grattan_red,
                 "pm25" = grattan_orange)
colour_names2 <- names(colour_levs)


cumulative_pol_saved <- policy_outcomes %>% 
  group_by(scenario, vkt_scenario, pollutant_cat2, fleet_year) %>% 
  summarise(pollutant_total = sum(pollutant_total)) %>% 
  filter(scenario %in% c("Euro 6 (2024)", "Euro 6 (2027)", "baseline"),
         fleet_year <= 2040, 
         vkt_scenario == "vkt_central",
         pollutant_cat2 %in% c("nox", "pm25")) %>% 
  mutate(scenario = factor(scenario, levels = colour_names2)) %>% 
  # Converting to 'avoided emissions' 
  pivot_wider(names_from = scenario, values_from = pollutant_total) %>% 
  mutate(`Euro 6 (2024)` = `Euro 6 (2027)` - `Euro 6 (2024)`,
         `Euro 6 (2027)` =  baseline - `Euro 6 (2027)`) %>% 
  select(-baseline) %>% 
  pivot_longer(cols = 4:5,
               names_to = "scenario",
               values_to = "pollutant_total") %>% 
  #Adding over time
  group_by(scenario, pollutant_cat2) %>% 
  mutate(pollutant_total_sum = cumsum(pollutant_total))

# Plotting cumulative emissions -------------------
cumulative_pol_saved %>% 
  ggplot() + 
  geom_col(aes(y = pollutant_total_sum / 1000,
               x = fleet_year,
               fill = pollutant_cat2,
               alpha = scenario)) +
  
  theme_grattan() +
  scale_fill_manual(values = poll_colour) +
  scale_colour_manual(values = poll_colour) +
  scale_alpha_discrete(range = c(0.7, 1)) +
  grattan_x_continuous(limits = c(2022, 2040)) +
  grattan_y_continuous(label = label_comma()) +
  
  geom_text(data = . %>% group_by(pollutant_cat2) %>%  slice(1),
            aes(x = c(2031, 2031),
                y = c(1060, 17),
                label = c("NOx", "PM2.5"),
                colour = pollutant_cat2),
            fontface = "bold",
            check_overlap = TRUE) +
  
  facet_wrap(~pollutant_cat2,
             scales = "free") +
  
  theme(plot.subtitle = element_markdown(), strip.text.x = element_blank()) +
  labs(title = "Cumulative difference in pollution",
       subtitle = glue("<span style='font-size:17pt'>Thousands of tonnes of pollutant emissions avoided, if Euro VI is: <br>",
                       "<span style='font-size:17pt; color:{grattan_red3}'>**Introduced in 2024**, ",
                       "<span style='font-size:17pt; color:{grattan_red}'>**introduced in 2027**"),
       x = NULL) 



# Plotting yearly emissions
cumulative_pol_saved %>% 
  ggplot() + 
  geom_col(aes(y = pollutant_total / 1000,
               x = fleet_year,
               fill = pollutant_cat2,
               alpha = scenario)) +
  
  theme_grattan() +
  scale_fill_manual(values = poll_colour) +
  scale_colour_manual(values = poll_colour) +
  scale_alpha_discrete(range = c(0.7, 1)) +
  grattan_x_continuous(limits = c(2022, 2040)) +
  grattan_y_continuous(label = label_comma()) +
  
  geom_text(data = . %>% group_by(pollutant_cat2) %>%  slice(1),
            aes(x = c(2031, 2031),
                y = c(94, 1.72),
                label = c("NOx", "PM2.5"),
                colour = pollutant_cat2),
            fontface = "bold",
            check_overlap = TRUE) +
  
  facet_wrap(~pollutant_cat2,
             scales = "free") +
  
  theme(plot.subtitle = element_markdown(), strip.text.x = element_blank()) +
  labs(title = "Annual difference in pollution",
       subtitle = glue("<span style='font-size:17pt'>Thousands of tonnes of pollutant emissions avoided, if Euro VI is: <br>",
                       "<span style='font-size:17pt; color:{grattan_red3}'>**Introduced in 2024**, ",
                       "<span style='font-size:17pt; color:{grattan_red}'>**introduced in 2027**"),
       x = NULL) 





















