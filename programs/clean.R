
# ------------------------------------------- #
# ---------- MISSING VALUES REPORT ---------- #
# ------------------------------------------- #

# create empty data frame to store missing value counts for each column in each table
missing = data.frame(
    "table" = character(),
    "column" = character(),
    "n_missing" = integer()
)

for (df_name in names(dfs_list)) {
  
    df <- dfs_list[[df_name]]    # access the data frame object
    
    # create temporary data frame to concatenate to main "missing" data frame
    counts <- df %>% 
      
        # count no. of missing values in each column
        summarise_all(~ sum(is.na(.))) %>% 
      
        # transpose all columns into a single row
        pivot_longer(
            cols = everything(),
            names_to = "column",        # same consistent colum name from "missing" base data frame
            values_to = "n_missing"
        ) %>% 
      
        # add column for table name to identify which table each column belongs to
        mutate(table = df_name)
      
    missing <- missing %>% 
      
        # stack the counts data frame onto the main "missing" data frame
        bind_rows(counts) %>% 
  
        # sort by largest missing value counts first
        arrange(desc(n_missing))
}

write_csv(
    missing, 
    file = file.path(REPORTS, "missing_values.csv")
)

# ----------------------------------------------- #
# ---------- DATA CLEANING & WRANGLING ---------- #
# ----------------------------------------------- #

TRIPS <- TRIPS %>% 

    # standardise all column names to lower case
    rename_with(~ str_to_lower(.)) %>% 
  
    # remove entire duplicate rows (ensure all columns remain)
    distinct(uniqueid, .keep_all = TRUE) %>%      # keep all columns
  
    # remove rows with negative trip distances (accounts for only ~5% of total observations of the data)
    filter(
        trip_distance > 0 & trip_distance < 500,    # remove unrealistic trip_distances greater than 1000 miles
        fare_amount > 0, # remove negative fare amounts
    ) %>%    
    # all trip distances > 500 miles are Voided Trips (the reset are kept as they form a large portion of the data)
    # there is one trip ~600 that could be legitimate but was removed as the next lowest is 100 miles and thus considered an anomaly for analysis
    
    # add levels and labels to factor columns (for categorical variables)
    mutate(
        vendorid = factor(
            vendorid,
            levels = c("1", "2", "3"),
            labels = c("Green Taxi", "Uber", "Other")
        ),
        ratecodeid = factor(
            ratecodeid,
            levels = c("1", "2", "3", "4", "5", "8"),
            labels = c(
                "Standard Rate",
                "JFK",
                "Newark",
                "Nassau or Westchester",
                "Negotiated Fare",
                "Group Ride"
            )
        ),
        paymenttype = factor(
            paymenttype,
            levels = c("1", "2", "3", "4", "5", "7"),
            labels = c(
                "Credit Card",
                "Cash",
                "No Charge",
                "Dispute",
                "Unknown",
                "Voided Trip"
            )
        ),
        trip_type = factor(
            trip_type,
            levels = c("1", "2", "3"),
            labels = c("Inner City", "Outer City", "Other")
        ),

        # convert passenger counts of 0 (or less) to NA (assumed to be data entry errors)
        passenger_count = if_else(passenger_count <= 0, NA_integer_, passenger_count)
    ) 

summary(TRIPS)

TIMES <- TIMES %>% 

    # standardise all column names to lower case
    rename_with(~ str_to_lower(.)) %>% 

    # optional: replace all "drop off" entries to "dropoff"
    mutate(trip_type = str_replace(trip_type, "drop off", "dropoff"))
    
LOCATIONS <- LOCATIONS %>% 

    # standardise all column names to lower case
    rename_with(~ str_to_lower(.))