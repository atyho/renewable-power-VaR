rm(list = ls())

library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(here)

#### Electricity Demand ####

setwd(paste0(here(), "/load"))

# Import and merge all CSV files into one data frame
load_NE <- list.files(pattern = "\\.csv$", full.names = TRUE) %>%
  lapply(read_csv, show_col_types = FALSE, skip = 5) %>% 
  bind_rows %>% select(-H)

# Rename columns
names(load_NE) <- c("date","hour","load_NE")

# Remove 2 duplicated entries in the raw data
load_NE <- load_NE %>% distinct()

# Create datetime column
load_NE <- load_NE %>%
  # Identify the dates that have a "02X" entry, indicating the DST switch date
  group_by(date) %>% mutate(dst_end = any(hour == "02X")) %>% ungroup() %>%
  # Covert time to UTC
  mutate(
    time = as.POSIXct(paste(date, hour), format = "%m/%d/%Y %H", tz = "America/New_York"),
    time_EDT = case_when(
      hour == "02" & dst_end ~ 
        force_tz(as.POSIXct(paste(date, hour), format = "%m/%d/%Y %H"), tzone = "Etc/GMT+4"),
      hour == "02X" ~ 
        force_tz(as.POSIXct(paste(date, "2"), format = "%m/%d/%Y %H"), tzone = "Etc/GMT+5"),
      TRUE ~ time),
    time_utc = with_tz(time_EDT, tzone = "UTC")) %>%
  select(time_utc, time, load_NE)

#### Electricity Supply ####

setwd(paste0(here(), "/fuel_mix"))

# Import and merge all CSV files into one data frame
fuelmix_NE <- list.files(pattern = "\\.csv$", full.names = TRUE) %>%
  lapply(function(file) {
    read_csv(file, skip = 6, show_col_types = FALSE, 
             col_names = c("data_type", "date", "time", "fuel_type", 
                           "fuel_cat", "gen_mw", "marginal")
             )}) %>% 
  bind_rows() %>%
  select(-data_type)

# Combine date and time into a datetime (UTC) column
fuelmix_NE <- fuelmix_NE %>%
  mutate(
    row_num = row_number(),
    time_local = as.POSIXct(paste(date, time), format = "%m/%d/%Y %H:%M:%S", tz = "America/New_York")
    ) %>%
  # Classify the EST switched datetime
  group_by(fuel_type) %>%
  mutate(dst_ended_hr = ifelse(is.na(time_local < lag(time_local)), FALSE,
                               time_local < lag(time_local) & row_num > lag(row_num))) %>% 
  ungroup() %>%
  # Re-classify the remaining entries within the EST switching hour (1am)
  group_by(fuel_type, date = date(time_local), hour = hour(time_local)) %>%
  mutate(dst_ended_hr = if_else(cumsum(dst_ended_hr) > 0, TRUE, dst_ended_hr)) %>%
  ungroup() %>%
  # Identify the DST ending day by checking for time rollback
  group_by(date) %>%
  mutate(dst_ending_day = any(dst_ended_hr)) %>%
  ungroup() %>%
  # Adjust the time according to the DST ending day and hour
  mutate(
    time_EST = case_when(
      dst_ending_day & dst_ended_hr ~ force_tz(time_local, tzone = "Etc/GMT+5"),
      dst_ending_day & !dst_ended_hr ~ force_tz(time_local, tzone = "Etc/GMT+4"),
      TRUE ~ time_local)
    ) %>%
  # Convert the adjusted time to UTC
  mutate(time_utc = with_tz(time_EST, tzone = "UTC"))

# Convert to hourly data by fuel type using averages
fuelmix_NE <- fuelmix_NE %>%
  # Offset time and represent the time as hour ending
  mutate(hour_ending = ceiling_date(time_utc, unit = "hour")) %>%
  group_by(hour_ending, fuel_type) %>%
  summarize(gen_mw = mean(gen_mw, na.rm = TRUE), .groups = "drop") %>% 
  pivot_wider(names_from = fuel_type, values_from = gen_mw, values_fill = list(gen_mw = 0)) %>%
  ungroup() %>%
  rename(time_utc = hour_ending)

ISONE <- inner_join(load_NE, fuelmix_NE, by = "time_utc") %>%
  mutate(time = with_tz(time_utc, tzone = "America/New_York"))

#### Save the dataframes as RData files ####
save(load_NE, file = "../load_NE.RData")
save(fuelmix_NE, file = "../fuelmix_NE.RData")
save(ISONE, file = "../ISONE.RData")
