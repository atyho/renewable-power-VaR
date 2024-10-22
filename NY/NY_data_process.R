rm(list = ls())

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(lubridate)
library(here)

#### Electricity Demand ####

setwd(paste0(here(), "/load"))

# Import and merge all CSV files into one data frame
load_NY <- list.files() %>%
  lapply(read_csv, show_col_types = FALSE) %>% 
  bind_rows

# Rename columns and convert time to UTC 
load_NY <- load_NY %>%
  rename(time = "Time Stamp", timezone = "Time Zone") %>%
  mutate(
    time_EDT = case_when(
      timezone == "EDT" ~ force_tz(as.POSIXct(time, format = "%m/%d/%Y %H:%M:%S"), tzone = "Etc/GMT+4"),
      timezone == "EST" ~ force_tz(as.POSIXct(time, format = "%m/%d/%Y %H:%M:%S"), tzone = "Etc/GMT+5")),
    # Convert to UTC using the correctly adjusted time zones
    time_utc = with_tz(time_EDT, tzone = "UTC"))

# Aggregate electricity load to hourly data by zone
load_NY <- load_NY %>%
  # Offset time and represent the time as hour ending
  mutate(hour = ceiling_date(time_utc, unit = "hour")) %>%
  # Group by the hour ending, fuel type, and timezone
  group_by(hour, Name) %>%
  # Summarize the data to get total generation for each combination
  summarize(load = mean(Load, na.rm = TRUE), .groups = "drop") %>%
  rename(time_utc = "hour") %>%
  # Reshape data to wide format by fuel type
  pivot_wider(names_from = Name, values_from = load) %>%
  # Sum up all columns (after pivot) row-wise
  rowwise() %>%
  mutate(load_NY = sum(c_across(where(is.numeric)), na.rm = TRUE)) %>%
  ungroup()

#### Electricity Supply ####

setwd(paste0(here(), "/fuel_mix"))

# Import and merge all CSV files into one data frame
fuelmix_NY <- list.files() %>%
  lapply(read_csv, show_col_types = FALSE) %>% 
  bind_rows

# Rename columns and convert time to UTC 
fuelmix_NY <- fuelmix_NY %>%
  rename(time = "Time Stamp", timezone = "Time Zone",
         fuel = "Fuel Category", gen_mw = "Gen MW") %>%
  mutate(
    time_EDT = case_when(
      timezone == "EDT" ~ force_tz(as.POSIXct(time, format = "%m/%d/%Y %H:%M:%S"), tzone = "Etc/GMT+4"),
      timezone == "EST" ~ force_tz(as.POSIXct(time, format = "%m/%d/%Y %H:%M:%S"), tzone = "Etc/GMT+5")),
    time_utc = with_tz(time_EDT, tzone = "UTC"))
    
# Aggregate electricity generation to hourly data by fuel type
fuelmix_NY <- fuelmix_NY %>%
  # Offset time and represent the time as hour ending
  mutate(hour = ceiling_date(time_utc, unit = "hour")) %>%
  # Group by the hour ending, fuel type, and timezone
  group_by(hour, fuel) %>%
  # Summarize the data to get total generation for each combination
  summarize(gen_mw = mean(gen_mw, na.rm = TRUE), .groups = "drop") %>%
  rename(time_utc = "hour") %>%
  # Reshape data to wide format by fuel type
  pivot_wider(names_from = fuel, values_from = gen_mw)

NYISO <- inner_join(load_NY %>% select(time_utc, load_NY), 
                    fuelmix_NY, by = "time_utc") %>%
  mutate(time = with_tz(time_utc, tzone = "America/New_York"))

#### Save the dataframes as RData files ####
save(load_NY, file = "../load_NYISO.RData")
save(fuelmix_NY, file = "../fuelmix_NYISO.RData")
save(NYISO, file = "../NYISO.RData")