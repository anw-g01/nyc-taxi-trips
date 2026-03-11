
# ------------------------------------- #
# ---------- IMPORT RAW DATA ---------- #
# ------------------------------------- #

# 1. TRIPS: import trip-level data (vendors, payments, etc.) from CSV

TRIPS <- read_csv(
    file = file.path(RAW, "taxi_trips.csv"),
    
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

# 2. TIMES: import trip-level data (pickup/dropoff times and location IDs) from CSV

TIMES <- read_csv(
    file = file.path(RAW, "taxi_time_location.csv"),
    col_types = cols(
        trip_type       = col_factor(),
        time            = col_datetime(format = "%Y-%m-%dT%H:%M:%SZ"),    # datetimes parsed ISO 8601 string format
        location_codeID = col_factor()
    )
)

# 3. LOCATIONS: import location-level lookup data (boroughs, zones, etc.) from CSV

LOCATIONS <- read_csv(
    file = file.path(RAW, "taxi_zone_lookup.csv"),
    col_types = cols(
        LocationID      = col_factor(),         # primary key for location lookup
        Borough         = col_factor(),         # 7 unique boroughs
        service_zone    = col_factor(),         # 5 unique service zones
        Zone            = col_factor()        
    ),
    na = c("", "NA", "Unknown", "N/A", "NV")
)

# ----------------------------------------------------- #
# ---------- EXPLORATORY DATA ANALYSIS (EDA) ---------- #
# ----------------------------------------------------- #

# view samples and summary statistics of each data set

df_list <- list(
    "TRIPS" = TRIPS,
    "TIMES" = TIMES,
    "LOCATIONS" = LOCATIONS
)
 
for (df_name in names(df_list)) {

    df <- df_list[[df_name]]    # get data frame by name from list of data frames

    # select n random samples from each data frame
    cat(glue("\n\nTABLE SAMPLE: {df_name}\n\n\n"))          # blank line for cleaner output
    print(slice_sample(df, n = 6))

    # view summary statistics of each data frame
    cat(glue("\n\nSUMMARY STATISTICS: {df_name}\n\n\n"))
    print(summary(df))                  

    # view missing value counts for each column in each data frame
    cat(glue("\n\nMISSING VALUE COUNTS: {df_name}\n\n\n"))
    print(
        summarise(
            df, across(everything(), ~ sum(is.na(.)))   
        )   
    )
  
    # view duplicate "UniqueID" value counts for each colum in each data frame
    cat(glue("\n\nDUPLICATE UniqueID COUNTS: {df_name}\n\n\n"))
    print(
        df %>% 
        select(contains("uniqueid", ignore.case = TRUE)) %>% 
        summarise(
            across(everything(), ~ sum(duplicated(.)))
        )
    )
}

# summarise the min/max/mean of all numerical columns
NUM_VARS_SUMMARY <- TRIPS %>% 
    summarise(
        across(
            where(is.numeric),
            list(
                min  = ~ round(min(., na.rm = TRUE), 2),
                mean = ~ round(mean(., na.rm = TRUE), 2),
                max  = ~ round(max(., na.rm = TRUE), 2)
            )
        )
    )  %>%
    # transpose columns into long format
    pivot_longer(
        everything(),
        names_to = c("variable", ".value"),
        names_pattern = "(.+)_(min|max|mean)"
    )

write_csv(NUM_VARS_SUMMARY, file = file.path(REPORTS, "num_vars_summary.csv"))

# summarise distinct values of all character columns
CHAR_VARS_SUMMARY <- TRIPS %>% 
    summarise(
        across(
            where(~ is.character(.) | is.factor(.)),
            ~ n_distinct(., na.rm = TRUE)
        )
    ) %>%
    pivot_longer(
        everything(),
        names_to = "column",
        values_to = "n_distinct"
    )

write_csv(CHAR_VARS_SUMMARY, file = file.path(REPORTS, "char_vars_summary.csv"))

# optional: remove temporary objects from environment variables
rm(df_name, df)

# ------------------------------------------- #
# ---------- DATA INSPECTION (EDA) ---------- #
# ------------------------------------------- #

# create a new environment contrainer to store EDA datasets (for inspection only)
EDA <- new.env()

# inspect duplicate "UniqueID" observations only
EDA$duplicate_ids <- TRIPS %>% 
    group_by(UniqueID) %>% 
    summarise(n_dups = n()) %>% 
    filter(n_dups > 1) %>% 
    left_join(TRIPS, by ="UniqueID") %>% 
    arrange(desc(n_dups)) %>% 
    select(-n_dups)

# check for entire duplicated rows
EDA$duplicate_rows <- TRIPS %>% 
    filter(
        duplicated(
            across(everything())
        )
    )

# inspect trips with zero (or less) trip distance
EDA$zero_distances <- TRIPS %>% 
    filter(trip_distance <= 0) %>% 
    arrange(trip_distance)

# inspect trips with negative fare amount
EDA$negative_fares <- TRIPS %>% 
    filter(fare_amount <= 0) %>% 
    arrange(fare_amount)

# inspect trips with zero (or less) passenger count 
EDA$zero_passengers <- TRIPS %>% 
    filter(passenger_count <= 0) %>% 
    arrange(passenger_count)

# inspect negative trip distances and fare amounts together (potentially cancelled trips that were logged)
EDA$cancelled_trips <- TRIPS %>% 
    filter(trip_distance <= 0 & fare_amount <= 0)

# inspect negative "extras" (miscellaneous & surcharges)
EDA$negative_extras <- TRIPS %>% 
    filter(extra < 0) %>% 
    arrange(extra)

# inspect largest trip distances 
EDA$longest_trips <- TRIPS %>% 
    filter(trip_distance > 50) %>% 
    arrange(desc(trip_distance)) 
# NOTE: all trips with unrealisitically high trip distances (trip_distance > 500 miles) have PaymentType = "Voided Trip" 

# inspect trips with "Voided Trip" payment type (7) 
EDA$voided_trips <- TRIPS %>% 
    filter(PaymentType == 7) %>% 
    arrange(desc(trip_distance))