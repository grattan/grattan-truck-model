
# This script plots some charts that illustrate that old vehicle are damaging in 
# terms of health pollution

source("R/00-setup.R")
policy_outcomes <- read_rds("data/policy_outcomes.rds")


# Collating data ------------------------------------------

data_2020 <- policy_outcomes %>% 
  filter(fleet_year == 2022,
         vkt_scenario == "vkt_central",
         scenario == "baseline") 


data_2020_sum <- data_2020 %>% 
  group_by(sales_year, fuel_class, vkt, total) %>% 
  summarise(health_cost_total = sum(health_cost_total)) %>% 
  
  group_by(sales_year) %>% 
  summarise(vkt = sum(vkt * total),
            health_cost_total = sum(health_cost_total)) %>% 
  #summarising the values as a proportion 
  ungroup() %>% 
  mutate(vkt_prop = vkt / sum(vkt),
         health_cost_total_prop = health_cost_total / sum(health_cost_total))



# Histogram for just PM25 --------------------------------------

# Wrangling data -----
data_2020_pm25 <- data_2020 %>% 
  group_by(sales_year, fuel_class, vkt, total, pollutant_cat2) %>% 
  summarise(pollutant_total = sum(pollutant_total)) %>% 
  filter(pollutant_cat2 == "pm25") %>% 
  
  group_by(sales_year) %>% 
  summarise(vkt = sum(vkt * total),
            pollutant_total = sum(pollutant_total)) %>% 
  #summarising the values as a proportion 
  ungroup() %>% 
  mutate(vkt_prop = vkt / sum(vkt),
         pollutant_total_prop = pollutant_total / sum(pollutant_total))


# Labels -----
labs_1 <- tibble(lab = c("Share of total kilometres travelled", "Share of total PM2.5 emissions"),
                 x = c(1980, 1980),
                 y = c(165, 165),
                 name = c("vkt_prop", "pollutant_total_prop")) %>% 
  mutate(name = factor(name, levels = c("vkt_prop", "pollutant_total_prop")))


segment_label <- data_2020_pm25 %>% 
  mutate(age_cat = case_when(sales_year <= 2000 ~ "pre-2020",
                             sales_year > 2000 ~ "post-2020")) %>% 
  group_by(age_cat) %>% 
  summarise(vkt_prop = sum(vkt_prop),
            pollutant_total_prop = sum(pollutant_total_prop)) %>% 
  pivot_longer(cols = 2:3) %>% 
  filter(age_cat == "pre-2020") %>% 
  mutate(x = 1990, 
         y = 87) %>% 
  select(-age_cat) %>% 
  mutate(name = factor(name, levels = c("vkt_prop", "pollutant_total_prop")),
         value =  paste0(round(value * 100), "%")) %>% 
  rename("labs" = value)


# Plot -----------
c2_old_trucks_vkt_pmc25 <- data_2020_pm25 %>% 
  select(-pollutant_total, -vkt) %>% 
  pivot_longer(cols = 2:3) %>%
  #adjusting so we can use a density plot
  mutate(value = round(value * 1000)) %>% 
  uncount(value) %>%
  mutate(name = factor(name, levels = c("vkt_prop", "pollutant_total_prop")),
         age_cat = case_when(sales_year <= 2000 ~ "pre-2020",
                             sales_year > 2000 ~ "post-2020")) %>%
  
  filter(sales_year <= 2020) %>% 
  
  ggplot(aes(fill = name)) +
  
  geom_histogram(aes(x = sales_year,
                     alpha = age_cat)) +
  scale_alpha_discrete(range = c(0.5, 0.9)) +
  
  geom_text(data = labs_1,
            aes(x = x,
                y = y,
                label = lab,
                colour = name),
            fontface = "bold",
            hjust = "left") +
  
  #Creating callout areas
  geom_segment(aes(x = 1980, xend = 2000, y = 75, yend = 75)) +
  geom_segment(aes(x = 1980, xend = 1980, y = 75, yend = 70)) +
  geom_segment(aes(x = 2000, xend = 2000, y = 75, yend = 70)) +
  
  # And labeling them
  geom_text(data = segment_label,
            aes(x = x,
                y = y,
                label = labs,
                colour = name),
            fontface = "bold",
            hjust = "middle") +
  
  theme_grattan() +
  theme(strip.text.y = element_blank()) +
  scale_fill_manual(values = c(grattan_yellow, grattan_red, grattan_grey3)) +
  scale_colour_manual(values = c(grattan_yellow, grattan_red, grattan_grey3)) +
  scale_y_continuous_grattan(label = scales::label_percent(scale = 0.1,
                                                           accuracy = 1),
                             limits = c(0, 180),
                             breaks = c(0, 50, 100, 150)) +
  
  facet_grid(rows = vars(name)) +
  
  labs(x = "Year of manufacture")
  
  #labs(title = "Older vehicles are responsible for an outsized share of PM2.5 emissions",
  #     subtitle = "Portion of total health costs and kilometres travelled in 2022",
  #     x = "Year of manufacture")


#grattan_save(filename = "atlas/old-trucks-disproportionate-damage.pdf",
#             save_pptx = TRUE,
#             type = "wholecolumn")




