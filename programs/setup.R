
library(readr)
library(lubridate)
library(dplyr)
library(stringr)
library(tidyr)
library(glue)

ROOT        <- getwd()                              # file path to project root directory

RAW         <- file.path(ROOT, "data/raw")          # source input data 
CLEAN       <- file.path(ROOT, "data/clean")        # destination for cleaned data
REPORTS     <- file.path(ROOT, "reports")           # destination for reports
