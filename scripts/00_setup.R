
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