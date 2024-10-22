rm(list = ls())

library(dplyr)
library(readxl)
library(stringr)
library(lubridate)
library(here)

#### Electricity Demand ####

setwd(paste0(here(), "/load"))

# Import and merge all CSV files into one data frame
load_QC <- list.files() %>%
  lapply(read_excel) %>% 
  bind_rows

# Function to shift the duplicated times due to daylight saving
shift_dst_duplicates <- function(df) {
  df %>%
    mutate(
      row_num = row_number(),
      dst_duplicated = duplicated(Date) & hour(Date) == 1,
      dst_end_flag = if_else(row_num %in% (row_num[dst_duplicated] - 1), TRUE, FALSE),
      # For hourly data, shift by 59 minutes to avoid funny function default settings
      time = if_else(dst_end_flag, Date - minutes(59), Date)
    )
}

# Revert the minute shift
revert_dst_shift <- function(df) {
  df %>%
    mutate(
      time_utc = if_else(dst_end_flag, time_utc + minutes(59), time_utc)
    )
}

# Apply the function to shift duplicated times due to daylight saving
load_QC <- shift_dst_duplicates(load_QC)

# Convert the shifted times to UTC
load_QC <- load_QC %>%
  mutate(time = force_tz(time, tzone = "America/Montreal"),
         time_utc = with_tz(time, tzone = "UTC"))

# Apply the function to revert the shift in UTC
load_QC <- revert_dst_shift(load_QC)

# Drop unnecessary columns
load_QC <- load_QC %>% 
  mutate(time = with_tz(time_utc, tzone = "America/Montreal"),
         load_QC = load_QC$'Moyenne (MW)') %>% 
  select(-c(Date, 'Moyenne (MW)', row_num, dst_duplicated, dst_end_flag))

#### Electricity Supply ####

setwd(paste0(here(), "/fuel_mix"))

# Import and merge all CSV files into one data frame
fuelmix_QC <- list.files() %>%
  lapply(read_excel) %>% 
  bind_rows

# Split the data into two parts: 2019-2022 and 2023
fuelmix_QC_2019_2022 <- fuelmix_QC %>% filter(year(Date) < 2023)
fuelmix_QC_2023 <- fuelmix_QC %>% filter(year(Date) == 2023)

# Adjust time for 2019-2022 data to "America/Montreal"
# Apply the function to shift duplicated times due to daylight saving
fuelmix_QC_2019_2022 <- shift_dst_duplicates(fuelmix_QC_2019_2022)

# Convert the shifted times to UTC
fuelmix_QC_2019_2022 <- fuelmix_QC_2019_2022 %>%
  mutate(time = force_tz(time, tzone = "America/Montreal"),
         time_utc = with_tz(time, tzone = "UTC"))

# Apply the function to revert the shift in UTC
fuelmix_QC_2019_2022 <- revert_dst_shift(fuelmix_QC_2019_2022)

# Drop unnecessary columns
fuelmix_QC_2019_2022 <- fuelmix_QC_2019_2022 %>% 
  mutate(time = with_tz(time_utc, tzone = "America/Montreal")) %>% 
  select(-c(Date, row_num, dst_duplicated, dst_end_flag))

# Adjust time for 2023 data to "Etc/GMT+5" (EST)
fuelmix_QC_2023 <- fuelmix_QC_2023 %>%
  mutate(time = force_tz(Date, tzone = "Etc/GMT+5")) %>%
  mutate(time_utc = with_tz(time, tzone = "UTC")) %>%
  select(-Date)

# Combine the data back together, adjust timing convention
fuelmix_QC <- bind_rows(fuelmix_QC_2019_2022, fuelmix_QC_2023) %>%
  mutate(time_utc = time_utc + minutes(30)) %>%
  mutate(time = with_tz(time_utc, tzone = "America/Montreal")) %>%
  arrange(time_utc)

HydroQC <- inner_join(load_QC, fuelmix_QC, by = c("time","time_utc")) %>%
  arrange(time_utc)

#### Save the dataframes as RData files ####
save(load_QC, file = "../load_HydroQC.RData")
save(fuelmix_QC, file = "../fuelmix_HydroQC.RData")
save(HydroQC, file = "../HydroQC.RData")
