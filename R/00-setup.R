#Setup file

# by Lachlan Fox, Grattan Institute
# Packages ---------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(scales)
library(purrr)
library(glue)
library(fst)
library(janitor)
library(grattantheme)
library(spatstat)
library(ggtext)
library(zoo)
library(readxl)
library(devtools)
library(broom)
library(patchwork)


# Project functions ------------------------------------------------------------
`%nin%` <- Negate(`%in%`)

#' Emissions to diesel relationship -----------------------------
#' From fleetEffSim

diesel_co2_to_fuel <- function(.co2) {
  .diesel_consumption <- -0.001201 + 0.037436 * .co2
  return(.diesel_consumption)
}

#We actually want this rearranged so we put in diesel consumption and get out 
#co2 values. So rearranging:

# This converts diesel consumption in L/100km to carbon dioxide production in 
# g/km travelled. 

diesel_fuel_to_co2 <- function(.diesel_consumption) {
  .co2 <- (.diesel_consumption + 0.001201) / 0.037436 
  return(.co2)
}

