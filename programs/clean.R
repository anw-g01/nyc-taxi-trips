
library(stringr)
library(dplyr)


trips <- trips %>% 

    mutate(

        # convert categorical columns to factors with labels
        VendorID = factor(
            VendorID,
            levels = c(1, 2, 3),
            labels = c("Green Taxi", "Uber", "Other")
        ),
        RatecodeID = factor(
            RatecodeID,
            levels = c(1, 2, 3, 4, 5, 6),
            labels = c(
                "Standard Rate",
                "JFK",
                "Newark",
                "Nassau or Westchester",
                "Negotiated Fare",
                "Group Ride"
            )
        ),
        PyamentType = factor(
            PaymentType,
            levels = c(1, 2, 3, 4, 5, 6),
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
            levels = c(1, 2, 3),
            labels = c("Inner City", "Outer City", "Other")
        )
    ) %>% 
    
    # standardise all column names to lower case
    rename_with(~ str_to_lower(.))
    

times <- times %>% 

    # standardise all column names to lower case
    rename_with(~ str_to_lower(.)) %>% 

    # optional: replace all "drop off" entries to "dropoff"
    mutate(trip_type = str_replace(trip_type, "drop off", "dropoff"))
    

locations <- locations %>% 

    # standardise all column names to lower case
    rename_with(~ str_to_lower(.))
    
