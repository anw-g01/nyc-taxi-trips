
library(readr)
library(lubridate)
library(dplyr)

ROOT <- r"(C:\Users\anwarat.gurung\OneDrive - Katalyze Data\Documents\R\Anwarat_Gurung_R_Case_Study)"

DATA <- file.path(ROOT, "data/raw")   # path to source input data CSVs

# 1. TAXI TRIPS CSV

trips <- read_csv(
    file = file.path(DATA, "taxi_trips.csv"),
    
    # explicitly specify column types
    col_types = cols(
        VendorID            = col_factor(),
        store_and_fwd_flag  = col_factor(),
        RatecodeID          = col_factor(),
        passenger_count     = col_integer(),
        PaymentType         = col_factor(),
        trip_type           = col_factor()
    ),

    # read empty strings and "NA" entries as null values
    na = c("", "NA"),
)

# 2. TAXI TIME & LOCATION CSV

times <- read_csv(
    file = file.path(DATA, "taxi_time_location.csv"),
    col_types = cols(
        trip_type       = col_factor(),
        time            = col_datetime(format = "%Y-%m-%dT%H:%M:%SZ"),    # datetimes parsed ISO 8601 string format
        location_codeID = col_character()
    )
)

# 3. TAXI ZONES LOOKUP CSV

locations <- read_csv(
    file = file.path(DATA, "taxi_zone_lookup.csv"),
    col_types = cols(
        LocationID      = col_character(), 
        Borough         = col_factor(),         # 7 unique boroughs
        service_zone    = col_factor()          # 5 unique service zones
    )
)

# view samples of each data set

dfs <- list(trips, times, locations)

for (df in dfs) {
  
    cat("\n")     # blank line between samples
    print(slice_sample(df, n = 6))
  
}