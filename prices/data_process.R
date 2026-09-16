rm(list = ls())

library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(here)

#### ISO-NE Electricity Price ####

setwd(paste0(here(), "/prices/NE"))

# Import and merge all CSV files into one data frame
price_NE <- list.files(pattern = "\\.csv$", full.names = TRUE) %>%
  lapply(function(file) {
    
    # Get column names from line 5
    col_names <- names(
      read_csv(file, skip = 4, n_max = 0, show_col_types = FALSE)
    )
    
    # Read data starting from line 7
    price <- read_csv(
      file,
      skip = 6,
      col_names = col_names,
      col_types = cols(
        `Hour Ending` = col_character(),
        .default = col_guess()
        ),
      show_col_types = FALSE
    ) %>% select(-H)
    
    # Create datetime column
    price <- price %>%
      # Identify the dates that have a "02X" entry, indicating the DST switch date
      group_by(Date) %>% mutate(dst_end = any(`Hour Ending` == "02X")) %>% ungroup() %>%
      # Covert time to UTC
      mutate(
        time = as.POSIXct(
          paste(Date, `Hour Ending`), 
          format = "%m/%d/%Y %H", 
          tz = "America/New_York"),
        time_EDT = case_when(
          # Fall transition: first 02 is EDT (UTC-4)
          `Hour Ending` == "02" & dst_end ~ 
            force_tz(as.POSIXct(
              paste(Date, `Hour Ending`), 
              format = "%m/%d/%Y %H"), 
              tzone = "Etc/GMT+4"),
          # Fall transition: repeated 02X is EST (UTC-5)
          `Hour Ending` == "02X" ~ 
            force_tz(as.POSIXct(
              paste(Date, "02"), 
              format = "%m/%d/%Y %H"), 
              tzone = "Etc/GMT+5"),
          # Spring transition: HE 02 is interpreted as EST (UTC-5)
          `Hour Ending` == "02" & is.na(time) ~
            # Spring transition: HE 02 is interpreted as EST (UTC-5)
            force_tz(as.POSIXct(
              paste(Date, "03"),
              format = "%m/%d/%Y %H"),
              tz = "Etc/GMT+4"),
          TRUE ~ time),
        time_utc = with_tz(time_EDT, tzone = "UTC"))
    
    # Check for time conversion problems
    problem_time <- price %>%
      filter( !is.na(`Hour Ending`) & is.na(time_utc) )
    
    if (nrow(problem_time) > 0) {
      cat("\nTime conversion problem in file:", file, "\n")
      print(problem_time %>%
          select(Date, `Hour Ending`, dst_end,
                 time, time_EDT, time_utc)
      )
      browser()
    }
    
    # Average energy prices by date-hour across all locations
    price <- price %>%
      group_by(time_utc) %>%
      summarise(
        ENGY_price = mean(`Energy Component`, na.rm = TRUE),
        .groups = "drop"
      )
    
    cat("Done for:", file, "\n")
    
    return(price)
    
  }) %>%
  bind_rows() %>%
  arrange(time_utc)

#### Electricity Prices in other regions provided by IESO ####

setwd(paste0(here(), "/prices/Others"))

# Import and merge all CSV files into one data frame
price_others <- list.files(pattern = "\\.csv$", full.names = TRUE) %>%
  lapply(function(file) {
    
    # Read lines 4 and 5 as raw text
    header <- readLines(file, n = 5)
    
    # Region names from line 4
    region <- strsplit(header[4], ",", fixed = TRUE)[[1]]
    
    # Variable names from line 5
    variable <- strsplit(header[5], ",", fixed = TRUE)[[1]]
    
    # Construct column names
    col_names <- variable
    
    col_names[4:length(col_names)] <- paste(
      region[4:length(region)],
      variable[4:length(variable)],
      sep = "__"
    )
    
    # Read actual observations starting from line 6
    price <- read_csv(
      file,
      skip = 5,
      col_names = col_names,
      show_col_types = FALSE) %>%
      filter(!is.na(DELIVERY_DATE)) %>%
      # Convert region-variable columns to long format
      pivot_longer(
        cols = -c(DELIVERY_DATE, DELIVERY_HOUR, INTERVAL),
        names_to = c("region", "variable"),
        names_sep = "__",
        values_to = "value") %>%
      # Put 10N, 10S, 30R, and ENGY back into columns
      pivot_wider(
        names_from = variable,
        values_from = value)
    
    # Format date column - Eastern Standard Time is used year round
    price <- price %>% mutate(DELIVERY_DATE = ymd(DELIVERY_DATE)) %>%
      mutate(time = ymd_h(paste(DELIVERY_DATE, DELIVERY_HOUR), tz = "Etc/GMT+5")) %>%
      mutate(time_utc = with_tz(time, tzone = "UTC"))
    
    # Average energy prices by date-hour across all locations by region
    price <- price %>%
      # Combine all Quebec regions into one
      mutate( region = if_else(grepl("^Quebec", region), "Quebec", region) ) %>%
      # Average across intervals and, for Quebec, across all Quebec regions
      group_by(time_utc, region) %>%
      summarise( ENGY_price = mean(ENGY, na.rm = TRUE), .groups = "drop")
    
    cat("Done for:", file, "\n")
    
    return(price)

    }) %>%
  bind_rows() %>%
  arrange(time_utc)

#### Combine all price data together ####
price_all <- price_others %>%
  bind_rows(
    price_NE %>%
      mutate(region = "New England") %>%
      select(time_utc, region, ENGY_price) 
    ) %>%
  arrange(time_utc, region)

#### Save the dataframes as RData files ####
setwd(paste0(here(), "/prices"))
save(price_NE, file = "price_NE.RData")
save(price_others, file = "price_Others.RData")
save(price_all, file = "price_all.RData")
