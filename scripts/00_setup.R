
# load libraries 
library(here)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(glue)
library(ggplot2)
library(forcats)
library(knitr)
library(paletteer)
library(ggtext)
library(geomtextpath)

# define project paths to key directories 
RAW     <- here("data", "raw")
CLEAN   <- here("data", "clean")
MARTS   <- here("data", "marts")
REPORTS <- here("reports")
FIGURES <- here("figures")

# define global variables for visualisations and outputs

BASE_FAMILY     <- "Palatino Linotype"      # base font for plots 
BASE_SIZE       <- 11                       # base font size

COLOUR_PALETTE  <- paletteer_d("MoMAColors::Abbott")
COLOUR_GREEN    <- COLOUR_PALETTE[[5]]    # Green Taxi
COLOUR_UBER     <- COLOUR_PALETTE[[1]]    # Uber 
COLOUR_OTHER    <- COLOUR_PALETTE[[7]]    # Other

FIG_CAPTION     <- "Source: NYC Taxi & Limousine Commission (TLC) Trip Record Data (2021)" 