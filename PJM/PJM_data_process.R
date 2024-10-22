rm(list = ls())

library(dplyr)
library(readr)
library(tidyr)
library(here)

######################
# Electricity Demand #
######################

setwd(here())
setwd("load")

# Import and merge all CSV files into one data frame
load_PJM <- list.files() %>%
  lapply(read_csv, show_col_types = FALSE) %>% 
  bind_rows

# Only keep data for regional transmission organization (RTO)
load_PJM <- load_PJM %>% filter(nerc_region == "RTO")

load_PJM <- load_PJM %>% 
  mutate(time = as.POSIXct(datetime_beginning_ept,
                           format="%m/%d/%Y %I:%M:%S %p", tz="US/Eastern")) %>%
  select(time, mw) %>% rename(load_mw = mw) %>%
  arrange(time)

######################
# Electricity Supply #
######################

setwd(here())
setwd("fuel_mix")

# Import and merge all CSV files into one data frame
fuelmix_PJM <- list.files() %>%
  lapply(read_csv, show_col_types = FALSE) %>% 
  bind_rows

fuelmix_PJM <- fuelmix_PJM %>% 
  mutate(time = as.POSIXct(fuelmix_PJM$datetime_beginning_ept,
                           format="%m/%d/%Y %I:%M:%S %p", tz="US/Eastern")) %>%
  select(time, fuel_type, mw) %>% arrange(fuel_type, time)

# Need to take care of day-time saving
fuelmix_PJM <- fuelmix_PJM %>% 
  aggregate(mw ~ time + fuel_type, FUN = mean) %>% 
  pivot_wider(names_from = fuel_type, values_from = mw)

PJM <- inner_join(load_PJM, fuelmix_PJM, by = "time")

# Save the dataframe as an RData file
save(load_PJM, file = "../load_PJM.RData")
save(fuelmix_PJM, file = "../fuelmix_PJM.RData")
save(PJM, file = "../PJM.RData")
