
# --------------------------------------- #
# ---------- DATA  PREPARATION ---------- #
# --------------------------------------- #

# define date intervals for each season (winter will be the default case)
spring_interval <- interval(as_date("2021-03-20"), as_date("2021-06-19")) 
summer_interval <- interval(as_date("2021-06-20"), as_date("2021-09-22"))
autumn_interval <- interval(as_date("2021-09-22"), as_date("2021-12-20"))

TIMES_WIDE <- TIMES_CLEAN %>%

    # tranpose taxi times table to have pickup and dropoff times as separate columns
    pivot_wider(                                    # long to wide format
        id_cols = uniqueid,
        names_from = trip_type,                     # create separate columns from trip_type: "pickup" and "drop off"
        values_from = c(time, location_codeid),     # create a time and location_codeid column for EACH pickup and dropoff
        # optional: name new columns as <trip_type>_<time/locatino_codeid>                                        
        names_glue = "{trip_type}_{.value}"
    ) %>% 
    
    # create new date-related columns from pickup_time
    mutate(

        # extract date and time components from pickup_time
        pickup_year = as.integer(year(pickup_time)),
        pickup_date = date(pickup_time),
        pickup_hour = hour(pickup_time),
        pickup_weekday = wday(
            pickup_time, 
            label = TRUE,   # display weekday names (not numbers)
            abbr = FALSE    # show full weekday names
        ),

        # optional: re-order weekday ordering
        pickup_weekday = factor(
            pickup_weekday,
            levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
            ordered = TRUE
        ),

        # calculate trip duration (convert to seconds without extra characters)
        duration_secs = as.integer(as.duration(dropoff_time - pickup_time)),
        duration_mins = duration_secs / 60,     

        # create a new column for trip season
        trip_season = case_when(
            pickup_time %within% spring_interval ~ "Spring",
            pickup_time %within% summer_interval ~ "Summer",
            pickup_time %within% autumn_interval ~ "Autumn",
            TRUE ~ "Winter"
        ),

        # convert trip_season to an ordered factor
        trip_season = factor(
            trip_season,
            levels = c("Spring", "Summer", "Autumn", "Winter"),
            ordered = TRUE
        )
    )  %>% 
    
    # remove unrealistic trip durations 
    filter_out(
        duration_secs <= 0         # zero or less (most likely errors)
        | duration_mins > 90       # longer than threshold: 2 hours (conservative) or 1.5 hours (strict)
    )    

rm(spring_interval, summer_interval, autumn_interval)
  
# merge TRIPS and TIMES_WIDE tables
TAXI_TRIPS_NYC <- TRIPS_CLEAN %>% 

    # LEFT JOIN: keep all rows from TRIPS, add time and location IDs from TIMES_WIDE
    left_join(
        TIMES_WIDE,
        by = "uniqueid"     # add matching columns from TIMES based on UNIQUEID key
    ) %>% 
    
    # only keep trips in 2021 (only 5 trips not in 2021)
    filter(pickup_year == 2021) %>%     # filtered AFTER joining due to LEFT JOIN

    # LEFT JOIN: keep all current rows, add pickup_borough from LOCATIONS
    left_join(
        LOCATIONS_CLEAN %>% 
            select(locationid, borough) %>%                     # primary key and borough detail of only interest 
            rename(pickup_borough = borough),                   # new_name = old_name
        by = join_by(pickup_location_codeid == locationid)     # different column key names
    ) %>% 
  
    # LEFT JOIN: keep all current rows, add dropoff_borough from LOCATIONS
    left_join(
        LOCATIONS_CLEAN %>% 
            select(locationid, borough) %>% 
            rename(dropoff_borough = borough),
        by = join_by(dropoff_location_codeid == locationid)     # different column key names
    ) %>% 
    
    mutate(
        # flag if pickup and dropoff boroughs were the same
        borough_flag = if_else(pickup_borough == dropoff_borough, TRUE, FALSE),

        # enforce mandatory $0.30 surcharge after 2015
        improvement_surcharge = if_else(pickup_year >= 2015, 0.3, 0)
    ) %>% 

    # drop columns
    select(
        -contains("location_codeid"),   # not required after merging Boroughs
        -ehail_fee,                     # all values are NA
        -store_and_fwd_flag             # not required for analysis
    )

# export TAXI_TRIPS_NYC dataset to CSV
write_csv(
    TAXI_TRIPS_NYC,
    file = file.path(CLEAN, "taxi_trips_nyc.csv")
)

# export TAXI_TRIPS_NYC dataset to RDS (preserves all column data types)
saveRDS(
    TAXI_TRIPS_NYC,
    file = file.path(CLEAN, "taxi_trips_nyc.rds")
)