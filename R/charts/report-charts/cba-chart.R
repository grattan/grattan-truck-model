
# This script produced a plot of the CBA results ------------
# Running the original CBA code 
source("R/costs-modelling/CBA.R")


#Summarising data ----------------------------

colour_vals <- c("Infrast-\nructure costs" = grattan_grey5,
                 "Vehicle costs" = grattan_darkred,
                 "Time & weight penalty" = grattan_red,
                 "Avoided\nHealth costs" = grattan_lightorange,
                 "Avoided\nCO2" = grattan_yellow,
                 "Reduced\nnoise" = grattan_lightyellow,
                 "Reduced\nMainte-\nnance costs" = grattan_lightblue,
                 "Avoided\nfuel costs" = grattan_darkblue)


cba_euro_6_scenario <- cba_summary_e_6 %>% 
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
         xmax = xmin + 1) 



# Plotting ------------------------------
#It's called Euro 6 because also assumes euro 6 gets implemented


# The total net benefit for use on chart 
cba_benefit <- cba_euro_6_scenario$avoided_cost_b %>% sum()


c3_cba_chart <- cba_euro_6_scenario %>%  
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
  
  labs(x = NULL)

  #labs(title = "The benefits of accelerating zero emission truck uptake outweighs the costs",
  #     subtitle = "Estimated costs and benefits of zero emissions targets for heavy vehicles",
  #     x = NULL,
  #     caption = "Details of CBA metholody are included in appendix XX. Calculated using a 7% discount rate.")


#grattan_save(filename = "",
#             object = p12_wc,
#             save_ppt = FALSE,
#             type = "wholecolumn",
#             device = cairo_pdf)

