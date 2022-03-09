

# This script produces an illustrative chart on ZEV uptake rates, and how the curve 
# can be shifted forwards


# First producing a dummy dataset using some S curve functions ------

curves <- seq(from = -10,
              to = 10,
              by = 0.3) %>% 
  tibble() %>% 
  clean_names() %>% 
  mutate(y = 1 / (1 + 10 * exp(-x)),
         y2 = 1 / (1 + 10 * exp(-x - 3))) %>% 
  pivot_longer(cols = 2:3,
               names_to = "curve")



# Plotting the data ----------------------------------------------

curves %>% 
  ggplot() +

  geom_ribbon(aes(x = x, 
                  ymax = value,
                  fill = curve),
              colour = NA,
              ymin = 0, 
              alpha = 0.2) +
  
  geom_line(aes(x = x,
                y = value,
                colour = curve)) +
  
  theme_grattan() +
  grattan_colour_manual(2, rev = TRUE) +
  grattan_fill_manual(2, rev = TRUE) +
  scale_y_continuous_grattan(label = scales::label_percent(),
                             limits = c(0, 1.2),
                             breaks = c(0, 0.5, 1)) +
  scale_x_continuous_grattan(limits = c(-6, 7)) +
  
  geom_vline(xintercept = -1.5,
             colour = grattan_grey4,
             linetype = "dashed",
             size = 1) +
  
  
  grattan_label(aes(label = "Total cost of \nownership parity",
                    x = -1.3,
                    y = 1.1,
                    hjust = "left"),
                colour = grattan_grey4) +
  
  geom_text(aes(label = "No action \nscenario",
                    x = 4.5,
                    y = 0.8,
                    hjust = "left",
                    fontface = "bold"),
            check_overlap = TRUE,
            fill = NA,
            colour = grattan_red,
            linehight = 0.8,
            padding = 0.1) +
  
  grattan_label(aes(label = "Policy intervention \nscenario",
                    x = 3.5,
                    y = 1.1,
                    hjust = "left",
                    fontface = "bold"),
                colour = grattan_orange) +
  
  labs(title = "Shifting the EV uptake curve forward is well worth it",
       subtitle = "Illustrative ZE-HDV uptake rates, per cent of new sales",
       x = NULL)
  
  
 # grattan_save(filename = "atlas/illustrative-uptake-curves.pdf",
#               type = "normal",
#               save_ppt = TRUE)
