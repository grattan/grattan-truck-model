
source("R/00-setup.R")
library(patchwork)
#Technology cost estimates ------------------------------

#' This script pulls together ICCT cost estimates from the EU and US for heavy duty vehicles

#' US estimates
#' US estimates are neatly contained here:
#' https://theicct.org/sites/default/files/publications/ICCT_position-brief_HDVenginetech-India_jun2015.pdf (page 1)
#' They are from a baseline 2010 vehicle for a 2025 period (page 2), and are 
#' reported as $2014 US
#' 
#' In the dataset below hte incr_cost is reported as $/% and the perc_potential
#' refers to the total stated gains that can be acheieved

#' Inflation/exchange factors

us_to_aus <- 1.41
us_2014_2021 <- 1.17
eu_2016_2021 <- 1.052
eu_to_aus <- 1.6

us_costs <- tribble(
  ~technology,          ~cost_type,    ~incr_cost,  ~perc_potential,
  "Aero",                  "US",           894,             0.16,
  "Tyres",                 "US",           251,             0.08,
  "Weight<br>reduction",    "US",           9666,            0.02,
  "Transmission",          "US",           2184,            0.02,
  "Engine",                "US",           334,             0.24) %>% 
  #and converting to 2021 Aus dollars
  mutate(incr_cost = incr_cost * us_to_aus * us_2014_2021)


#' EU estimates
#' The EU estimates are from two papers - one paper dedicated to the efficiency gains
#' that can be achieved through individual technology, and the other paper to the costs
#' It seems very far to link the two, given the ICCT describes them as 'companion pieces'. 
#' Effectiveness: https://theicct.org/publications/fuel-efficiency-technology-european-heavy-duty-vehicles-baseline-and-potential-2020
#' Costs: https://theicct.org/sites/default/files/publications/ICCT_EU-HDV-tech-2025-30_20180116.pdf
#' 
#' By collating the improvement expected of different tech packages, and the costs, we
#' can calculate the incremental costs for each technology. 
#' 
#' We are only taking esimtates of long haul trucks, to be (relatively) consistent
#' with the US data (which is for sleeper cab tractor-trucks)
#' All EU costs are in euros (2016)
#' 
#' For each dataset, all the relevant tech packages that are estimated are included - 
#' we'll work out a way to pick which ones to include later!
#' 
#' For the package effectiveness and costs, 'long haul' and 2025 year values
#' were used for all. Packages for Transmission are not included in the EU data

eu_engine <- tribble(
  ~technology,    ~package,                 ~cost_type,       ~cost,  ~perc_potential,
  "Engine",       "2017 best in class",         "EU",           284,           0.033,
  "Engine",       "2020+",                      "EU",           2065,          0.095,
  "Engine",       "2020+ with WHR",             "EU",           5763,          0.117,
  "Engine",       "Engine long-term",           "EU",           7081,          0.181) 


#' For estimates involving both tractors and trailers, the costs are assumed with a 
#' 1.4 trailer:tractor ratio (consistent with ICCT assumptions), and includes the per vehicle maintenance 
#' costs listed for tires
eu_aero <- tribble(
  ~technology,    ~package,                 ~cost_type,          ~cost,      ~perc_potential,
  "Aero",       "Incremental",                 "EU",           335 + 1005*1.4,      0.053,
  "Aero",       "Moderate",                    "EU",           446 + 1194*1.4,      0.074,
  "Aero",       "Advanced",                    "EU",           669 + 1194*1.4,      0.085,
  "Aero",       "Long-term",                   "EU",           1673 + 1194*1.4,     0.132) 

eu_tyres <- tribble(
  ~technology,    ~package,                 ~cost_type,           ~cost,                       ~perc_potential,
  "Tyres",       "Incremental",                 "EU",           113 + 137*1.4 + 219 + 267,         0.028,
  "Tyres",       "Moderate",                    "EU",           113 + 172*1.4 +219 + 334,          0.058,
  "Tyres",       "Advanced",                    "EU",           122 + 172*1.4 + 238 + 334,         0.070,
  "Tyres",       "Long-term",                   "EU",           141 + 172*1.4 + 275 + 334,         0.084) 

eu_weight <- tribble(
  ~technology,                   ~package,      ~cost_type,         ~cost,      ~perc_potential,
  "Weight<br>reduction",       "Incremental",     "EU",           81 + 39*1.4,          0.003,
  "Weight<br>reduction",       "Moderate",        "EU",           255 + 168*1.4,        0.06,
  "Weight<br>reduction",       "Advanced",        "EU",           1044 + 1023*1.4,      0.016,
  "Weight<br>reduction",       "Long-term",       "EU",           3755 + 5285*1.4,      0.034) 



#' Binding the EU and US data and calculting incremental costs


eu_all <- bind_rows(
  eu_engine,
  eu_aero,
  eu_tyres,
  eu_weight) %>% 
  mutate(cost = cost * eu_2016_2021 * eu_to_aus,
         incr_cost = cost / (perc_potential*100)) %>% 
  filter(package %in% c("Engine long-term", "Advanced")) %>% 
  select(-package, -cost)


all_tech <- bind_rows(
  us_costs,
  eu_all)




# Plotting ------------------------------------------
col_values <- c("Weight<br>reduction" = grattan_red,
                "Transmission" = grattan_darkorange,
                "Aero" = grattan_orange,
                "Engine" = grattan_darkblue,
                "Tyres" = grattan_lightblue)


# Making a column for labels of how much is possible to be upgraded
all_tech <- all_tech %>% 
  mutate(technology = factor(technology, levels = names(col_values))) %>% 
  group_by(technology) %>% 
  #Creating axis labels 
  mutate(tech_potential_min = min(perc_potential),
         max = max(incr_cost), min = min(incr_cost),
         axis_label = glue("<span style='font-size:17pt;  color:{grattan_black}'> {technology} <br>",
                           "<span style='font-size:13pt;  color:{col_values[technology]}'> Cost applicable<br>for up to **{round(tech_potential_min * 100, 0)}%**<br>improvement") %>% 
           fct_reorder(min))



# Plot included in report
# Plot is "the available improvements (%) for $1000 dollars. upfront" 

c3_technology_costs <- all_tech %>% 
  mutate(perc_for_1000 = 1000 / incr_cost) %>% 
  ggplot() +
  geom_linerange(aes(ymin = 1000 / min, ymax = 1000 / max,
                     x = axis_label,
                     colour = technology),
                 alpha = 0.2,
                 size = 10) +
  
  geom_point(aes(axis_label, perc_for_1000, colour = technology, shape = cost_type),
             position = "dodge",
             size = 4, 
             stroke = 1.5) +
  
  geom_point(aes(x = 0.5, y = 5.5), colour = grattan_grey3, fill = "white", size = 4, stroke = 1.5, shape = 21) +
  geom_point(aes(x = 0.5, y = 5.2), colour = grattan_grey3,  size = 6, stroke = 1.5, shape = 20) +
  geom_text(data = . %>% slice(1), 
            aes(x = 0.65, y = 5.5, label = "EU estimate"), 
            colour = grattan_grey3, check_overlap = TRUE, hjust = "left", size = 6) +
  geom_text(data = . %>% slice(1), 
            aes(x = 0.65, y = 5.2, label = "US estimate"), 
            colour = grattan_grey3, check_overlap = TRUE, hjust = "left", size = 6) +
  
  theme_grattan() +
  theme(axis.text.x = element_markdown()) +
  scale_color_manual(values = col_values) +
  scale_fill_manual(values = col_values) +
  scale_shape_manual(values = c(21, 20)) + 
  grattan_y_continuous(labels = label_percent(scale = 1, 
                                              accuracy =1),
                      limits = c(0, 6)) +
  
  labs(x = NULL) 
  
  #labs(x = NULL,
  #     title = "Engine and tyres provide the most cost effective co2 reduction",
  #     subtitle = "Per cent CO2 reduction available for $1,000, by technology")





# Old plot - as incremental cost ($/% improvement). Not included in report

all_tech %>% 
  ggplot() +
  
  geom_linerange(aes(ymin = min, ymax = max,
                     x = axis_label,
                     colour = technology,
                     fill = technology),
                 alpha = 0.2,
                 size = 10) +
  
  geom_point(aes(axis_label, incr_cost, colour = technology, shape = cost_type),
             position = "dodge",
             size = 4, 
             stroke = 1.5) +
  
  theme_grattan() +
  theme(axis.text.x = element_markdown()) +
  scale_color_manual(values = col_values) +
  scale_fill_manual(values = col_values) +
  scale_shape_manual(values = c(21, 20)) + 
  grattan_y_continuous(labels = label_dollar(),
                       limits = c(0, 16000)) +
  
  labs(x = NULL)

#labs(x = NULL,
#     title = "Engine and tyres provide the most cost effective co2 reduction",
#     subtitle = "Cost per per centage improvement to fuel efficiency")






