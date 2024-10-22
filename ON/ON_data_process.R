rm(list = ls())

library(dplyr)
library(readr)
library(stringr)
library(here)
library(lubridate)

#### Electricity Demand ####

setwd(paste0(here(), "/demand"))

# Import and merge all CSV files into one data frame, skipping the first 3 lines
load_ON <- list.files() %>%
  lapply(function(file) read_csv(file, skip = 3, show_col_types = FALSE)) %>%
  bind_rows()

# Rename columns
names(load_ON) <- c("Date","Hour","demand_mkt","demand_ON")

# Format date column
# Eastern Standard Time is used year round
load_ON <- load_ON %>% mutate(Date = ymd(Date)) %>%
  mutate(time = ymd_h(paste(Date, Hour), tz = "Etc/GMT+5")) %>%
  mutate(time_utc = with_tz(time, tzone = "UTC")) %>%
  select(-Date, -Hour)

#### Electricity Supply ####

setwd(paste0(here(), "/fuel_mix"))

# Import and merge all CSV files into one data frame
fuelmix_ON <- list.files() %>%
  lapply(read_csv, show_col_types = FALSE) %>% 
  bind_rows

# Format date column
# Eastern Standard Time is used year round
fuelmix_ON <- fuelmix_ON %>% mutate(Date = mdy(Date)) %>%
  mutate(time = ymd_h(paste(Date, Hour), tz = "Etc/GMT+5")) %>%
  mutate(time_utc = with_tz(time, tzone = "UTC")) %>%
  select(-Date, -Hour)

IESO <- inner_join(load_ON, fuelmix_ON, by = c("time","time_utc"))

#### Hydro Capacity ####

setwd(paste0(here(), "/capacity"))

# Read and preprocess the CSV files to remove trailing commas
capacity_ON <- list.files(pattern = "*.csv") %>%
  lapply(function(file) {
    # Read lines and remove trailing commas
    lines <- read_lines(file)
    cleaned_lines <- str_remove(lines, ",+$")
    
    # Parse cleaned lines into a data frame
    read_csv(paste(cleaned_lines, collapse = "\n"), skip = 3, col_select = 1:28, show_col_types = FALSE)
  }) %>% 
  bind_rows()

names(capacity_ON) <- c("Date", "Generator", "Fuel_Type",	"Measurement",	
                        "Hour1", "Hour2", "Hour3", "Hour4", "Hour5", "Hour6",
                        "Hour7", "Hour8",	"Hour9", "Hour10", "Hour11", "Hour12",
                        "Hour13", "Hour14", "Hour15", "Hour16", "Hour17", "Hour18",
                        "Hour19", "Hour20", "Hour21", "Hour22",	"Hour23",	"Hour24")

# Filter for fuel type
capacity_ON <- capacity_ON %>% filter(Fuel_Type == "HYDRO")

# Reshape to long format for Hour
capacity_ON <- capacity_ON %>%
  pivot_longer(cols = starts_with("Hour"), 
               names_to = "Hour", values_to = "Value") %>%
  mutate(Hour = parse_number(Hour))

# Summarize the data by Date, Hour, and Measurement
capacity_ON <- capacity_ON %>%
  group_by(Fuel_Type, Date, Hour, Measurement) %>%
  summarise(Total_Value = sum(Value, na.rm = TRUE), .groups = 'drop')

# Pivot to wide format with each Measurement as a column
capacity_ON <- capacity_ON %>%
  pivot_wider(names_from = Measurement, values_from = Total_Value) %>%
  mutate(time = ymd_h(paste(Date, Hour), tz = "Etc/GMT+5")) %>%
  mutate(time_utc = with_tz(time, tzone = "UTC")) %>%
  arrange(time) %>% select(-Date, -Hour, -time)

##### Save the dataframes as RData files ####

save(load_ON, file = "../load_IESO.RData")
save(fuelmix_ON, file = "../fuelmix_IESO.RData")
save(capacity_ON, file = "../capacity_IESO.RData")
save(IESO, file = "../IESO.RData")
