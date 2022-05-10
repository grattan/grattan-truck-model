

# ZEV target trajectories ------------------------------------------
source("R/00-setup.R")

zev_targets <- read_xlsx("data-raw/electric-uptake.xlsx",
                         sheet = "electric-targets")


c3_zev_targets <- zev_targets %>% 
  filter(sales_year %in% c(2024, 2027, 2030, 2035, 2040)) %>% 
  ggplot(aes(x = sales_year,
             y = electric_target / 100,
             colour = fuel_class)) +
  
  geom_line(linetype = "dashed",
            alpha = 0.8)  +
  geom_point(shape = 21,
             fill = "white",
             stroke = 2) +

  scale_y_continuous_grattan(label = scales::label_percent(),
                             limits = c(0, 1.089),
                             breaks = c(0, 0.5, 1)) +
  scale_x_continuous_grattan(limits = c(2023.5, 2041)) +
  
  # Articulated truck labels
  grattan_label(data = . %>%  filter(fuel_class == "Articulated trucks",
                                     sales_year >= 2025),
                aes(x = sales_year + 0.8,
                    y = electric_target / 100 - 0.035,
                    label = paste0(round(electric_target, 1), "%")),
                fontface = "bold",
                fill = NA,
                size = 16) +
  
  #Rigid truck labels
  grattan_label(data = . %>%  filter(fuel_class == "Rigid trucks",
                                     sales_year >= 2025),
                aes(x = sales_year - 0.25,
                    y = electric_target / 100 + 0.06,
                    label = paste0(round(electric_target, 1), "%")),
                fontface = "bold",
                fill = NA,
                size = 16) +
  
  theme_grattan() +
  grattan_colour_manual(2) +
  theme(plot.subtitle = element_markdown()) +

  labs(#title = "Zero-emissions sales targets should be set for heavy vehicles",
       subtitle = glue("<span style='font-size:18pt; color:{grattan_grey3}'>Proportion of new ",
                       "<span style='font-size:18pt; color:{grattan_red}'>**rigid**",
                       "<span style='font-size:18pt; color:{grattan_grey3}'> and ",
                       "<span style='font-size:18pt; color:{grattan_orange}'>**articulated** ",
                       "<span style='font-size:18pt; color:{grattan_grey3}'>truck sales required<br>to be ZE-HDVs under proposed sales targets"),
    x = NULL) 



