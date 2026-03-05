
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

dfs_list <- list(
    "TRIPS" = TRIPS,
    "TIMES" = TIMES,
    "LOCATIONS" = LOCATIONS
)
 
for (df_name in names(dfs_list)) {

    df <- dfs_list[[df_name]]    # get data frame by name from list of data frames

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

# inspect duplicate "UniqueID" observations only
duplicate_ids <- TRIPS %>% 
    group_by(UniqueID) %>% 
    summarise(n_dups = n()) %>% 
    filter(n_dups > 1) %>% 
    left_join(TRIPS, by ="UniqueID") %>% 
    arrange(desc(n_dups)) %>% 
    select(-n_dups)

# check for entire duplicated rows
duplicate_rows <- TRIPS %>% 
    filter(
        duplicated(
            across(everything())
        )
    )

# inspect trips with zero (or less) trip distance
zero_distances <- TRIPS %>% 
    filter(trip_distance <= 0) %>% 
    arrange(trip_distance)

# inspect trips with negative fare amount
negative_fares <- TRIPS %>% 
    filter(fare_amount <= 0) %>% 
    arrange(fare_amount)

# inspect trips with zero (or less) passenger count 
zero_passengers <- TRIPS %>% 
    filter(passenger_count <= 0) %>% 
    arrange(passenger_count)

# inspect negative trip distances and fare amounts together (potentially cancelled trips that were logged)
cancelled_trips <- TRIPS %>% 
    filter(trip_distance <= 0 & fare_amount <= 0)

# inspect negative "extras" (miscellaneous & surcharges)
negative_extras <- TRIPS %>% 
    filter(extra < 0) %>% 
    arrange(extra)

# inspect largest trip distances 
longest_trips <- TRIPS %>% 
    filter(trip_distance > 100) %>% 
    arrange(desc(trip_distance)) 

# all trips with unrealisitically high trip distances (trip_distance > 500 miles) have PaymentType = "Voided Trip" 

# inspect trips with "Voided Trip" payment type (7) 
voided_trips <- TRIPS %>% 
    filter(PaymentType == 7) %>% 
    arrange(desc(trip_distance))