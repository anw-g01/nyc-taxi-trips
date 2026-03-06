
# load libraries 
library(here)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(glue)

# define project paths to key directories 
RAW     <- here("data", "raw")
CLEAN   <- here("data", "clean")
REPORTS <- here("reports")