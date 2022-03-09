
#' Plotting ICCT data on ZEV prices
#' https://theicct.org/wp-content/uploads/2022/02/purchase-cost-ze-trucks-feb22-1.pdf


zev_prices <- read_xlsx("data-raw/icct-zev-prices.xlsx",
          sheet = "Sheet1") %>% 
  clean_names()




zev_prices %>% 
  ggplot(aes(x = range_km, 
             y = price_us * 1.39,
             colour = year)) +
  geom_point(size = 7,
             alpha = 0.8,
             ) +
  
  theme_grattan(chart_type = "scatter") +
  scale_x_continuous_grattan(labels = scales::label_comma(),
                             limits = c(0, 1400)) +
  scale_y_continuous_grattan(labels = scales::label_dollar(),
                             limits = c(0, 1500000)) +
  
  scale_colour_manual(values = c(grattan_red, 
                                 grattan_lightblue,
                                 grattan_darkblue)) +
  
  labs(title = "Zero emission heavy vehicle prices are expected to fall",
       subtitle = "Forecast range and price of zero emissions heavy vehicles",
       y = "Price",
       x = "Range (km)",
       caption = "Includes estimated for both battery electric and fuel cell vehicles") +
  
  geom_text(aes(x = 1015,
                y = 1142000,
                label = "2018-2020 estimates"),
            colour = grattan_red,
            fontface = "bold",
            check_overlap = TRUE) +
  
  geom_text(aes(x = 1060,
                y = 592000,
                label = "2025 estimates"),
            colour = grattan_lightblue,
            fontface = "bold",
            check_overlap = TRUE) +
  
  geom_text(aes(x = 960,
                y = 255000,
                label = "2030 estimates"),
            colour = grattan_darkblue,
            fontface = "bold",
            check_overlap = TRUE) 
  
  

