
library(dplyr)
library(tidyr)

# define date intervals for each season (winter will be the default case)
spring_interval <- interval(as_date("2021-03-20"), as_date("2021-06-19")) 
summer_interval <- interval(as_date("2021-06-20"), as_date("2021-09-22"))
autumn_interval <- interval(as_date("2021-09-22"), as_date("2021-12-20"))

times_wide <- times %>%

    # tranpose taxi times table to have pickup and dropoff times as separate columns
    pivot_wider(                                    # long to wide format
        id_cols = uniqueid,
        names_from = trip_type,                     # create separate columns from trip_type: "pickup" and "drop off"
        values_from = c(time, location_codeid),     # create a time and location_codeid column for EACH pickup and dropoff
        # optional: name new columns as <trip_type>_<time/locatino_codeid>                                        
        names_glue = "{trip_type}_{.value}"
    ) %>% 
    
    # replace spaces in column names (mainly for "drop off") with underscores
    rename_all(~ str_replace(., " ", "_"))  %>% 

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

        # calculate trip duration 
        trip_duration = as.duration(dropoff_time - pickup_time),
        trip_duration_secs = as.integer(trip_duration),

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
    )
  
  
# merge TRIPS and TIMES tables
taxi_trips_nyc <- trips %>% 

    # RIGHT JOIN: keep all rows from TRIPS, add time and location details from TIMES_WIDE
    left_join(
        times_wide,
        by = "uniqueid"     # add matching columns from TIMES based on UNIQUEID key
    ) %>% 
    
    # only keep trips in 2021 (only 5 trips not in 2021)
    filter(pickup_year == 2021) %>% 

    # LEFT JOIN: keep all current rows, add pickup_borough location from LOCATIONS
    left_join(
        locations %>% 
            select(locationid, borough) %>%                     # primary key and borough detail of only interest 
            rename(pickup_borough = borough),                   # new_name = old_name
        by = c("pickup_location_codeid" = "locationid")         # add matching columns from LOCATIONS based on location identifier keys
    ) %>% 
  
    # LEFT JOIN: keep all current rows, add dropoff_borough location from LOCATIONS
    left_join(
        locations %>% 
            select(locationid, borough) %>% 
            rename(dropoff_borough = borough),
        by = c("dropoff_location_codeid" = "locationid")
    ) %>% 

    # drop location code ID columns
    select(-contains("location")) %>% 

    # flag if pickup and dropoff boroughs were the same
    mutate(borough_flag = if_else(pickup_borough == dropoff_borough, TRUE, FALSE))


locations %>%
    rename(pickup_borough = borough) %>% 
    View()
