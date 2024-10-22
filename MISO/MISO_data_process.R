rm(list = ls())

library(dplyr)
library(readxl)
library(readr)
library(tidyr)
library(stringr)
library(here)

######################
# Electricity Demand #
######################

setwd(here())
setwd("load")

# Import and merge all CSV files into one data frame
load_MISO <- list.files() %>%
  lapply(read_excel) %>% 
  bind_rows %>% select(-Footnote)

load_MISO <- load_MISO %>% filter(Region=="Central")

load_MISO <- load_MISO %>%
  mutate(time = as.POSIXct(paste(load_MISO$'Market Day', HourEnding -1), 
                           format="%m/%d/%Y %H", tz="EST")) %>%
  rename(load_mw = 'Actual Load (MWh)') %>%
  select(time, load_mw) %>% 
  arrange(time)

load_MISO <- na.omit(load_MISO)

######################
# Electricity Supply #
######################

setwd(here())
setwd("fuel_mix")

# Import and merge all CSV files into one data frame
fuelmix_MISO <- list.files() %>%
  lapply(read_csv, show_col_types = FALSE) %>% 
  bind_rows

fuelmix_MISO <- fuelmix_MISO %>% 
  mutate(time = as.POSIXct(paste(`Market Date`, HourEnding -1), 
                           format="%m/%d/%Y %H", tz="EST")) %>%
  filter(Region=="Central") %>% 
  select(time, `Fuel Type`, `[RT Generation State Estimator`) %>%
  pivot_wider(names_from = `Fuel Type`, values_from = `[RT Generation State Estimator`)

MISO <- inner_join(load_MISO, fuelmix_MISO, by = "time")

# Save the dataframe as an RData file
save(load_MISO, file = "../load_MISO.RData")
save(fuelmix_MISO, file = "../fuelmix_MISO.RData")
save(MISO, file = "../MISO.RData")
