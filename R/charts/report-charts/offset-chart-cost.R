

# Abatement cost chart 
source("R/00-setup.R")
source("R/model/11-policy-outcomes.R")


# Historical data form DISER
diser <- read_excel("data-raw/diser-co2-forecasts.xlsx") %>% 
  select(Year, `Articulated trucks`, `Rigid trucks`, `Buses`) %>% 
  pivot_longer(cols = 2:4,
               names_to = "fuel_class",
               values_to = "emissions") %>% 
  clean_names() %>% 
  group_by(year) %>% 
  summarise(emissions = sum(emissions)) %>% 
  filter( year <= 2021)

# Creating the different trjaectories
proj <- diser %>% 
  filter(year %in% 2020:2021) %>% 
  mutate(year = case_when(
    year == 2020 ~ 2050,
    year == 2021 ~ 2021))

diser_proj <- bind_rows(
  diser %>% 
    mutate(scenario = "historical"),
  proj %>% 
    mutate(emissions = if_else(
      year == 2050, 0, emissions),
      scenario = "zero"),
  proj %>% 
    mutate(emissions = if_else(
      year == 2050, 28, emissions),
      scenario = "high"),
  proj %>% 
    mutate(emissions = if_else(
      year == 2050, 15, emissions),
      scenario = "med")) %>% 
  mutate(line = case_when(
    scenario == "historical" ~ "a",
    scenario != "historical" ~ "b"))


# Plotting -------------------------------------------------- 
library(glue)
library(knitr)
library(pdftools)
library(Cairo)


c3_offset_costs <- diser_proj %>% 
  ggplot(aes(x = year,
              y = emissions,
              colour = scenario)) +
  geom_line() +
  theme_grattan() +
  scale_colour_manual(values = c(grattan_darkorange, grattan_red, grattan_orange, grattan_yellow)) +
  scale_y_continuous_grattan(limits = c(0, 32)) +
  scale_x_continuous_grattan(limits = c(1990, 2060),
                             breaks = c(1990, 2010, 2030, 2050)) +
  
  geom_point(data = . %>% 
               filter(year == 2050),
             size = 4) +
  geom_vline(xintercept = 2021,
             linetype = "dashed") +
  geom_text(data = . %>% 
              filter(year == 2050, emissions > 0),
            aes(x = year + 1,
                y = emissions,
                label = paste0("$", emissions * 25, "m")), # used a carbon cost of $25 here
            fontface = "bold",
            hjust = "left") +
  geom_text(data = . %>% 
              filter(year == 2050, emissions == 0),
            aes(x = year + 1,
                y = emissions + 1,
                label = paste0("$", emissions * 25, "m")), # used a carbon cost of $25 here
            fontface = "bold",
            hjust = "left") +
  #adding captions
  geom_richtext(data = . %>% ungroup() %>% slice(1),
                x = 1992, y = 22,
                label = glue("<span style='font-size:13pt; color:{grattan_red}'>**Actual emissions<br>**",
                             "<span style='font-size:13pt; color:{grattan_grey3}'>doubled between<br>1990 and 2021"),
                hjust = "left",
                lineheight = 1.5,
                fill = NA, label.colour = NA) + 
  geom_richtext(data = . %>% ungroup() %>% slice(1),
                x = 2025, y = 30,
                label = glue("<span style='font-size:13pt; color:{grattan_darkorange}'>**If emissions keep<br>rising** ",
                             "<span style='font-size:13pt; color:{grattan_grey3}'>offsets could<br>cost $700m per year"),
                hjust = "left",
                lineheight = 1.5,
                fill = "white", label.colour = NA) + 
  geom_richtext(data = . %>% ungroup() %>% slice(1),
                x = 2044, y = 19.5,
                label = glue("<span style='font-size:13pt; color:{grattan_orange}'>**If emissions fall**<br>",
                             "<span style='font-size:13pt; color:{grattan_grey3}'>offsets would cost<br>far less"),
                hjust = "left",
                lineheight = 1.5,
                fill = "white", label.colour = NA) + 
  
  labs(x = NULL)
      # title = "Offset costs in 2050 could be substantial without action",
      # subtitle = "Annual emissions from heavy vehicles (Mt)")

#grattan_save(type = "wholecolumn",
##             filename = "atlas/offset-chart.pdf",
#             save_ppt = TRUE)


