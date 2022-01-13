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


# Project functions ------------------------------------------------------------
`%nin%` <- Negate(`%in%`)
# READ DATA  ===================================================================
