rm(list = ls())

# Load the required packages
library(xml2)
library(tidyr)
library(dplyr)
library(lubridate)

# Read the XML file
xml_file <- read_xml("PUB_GenOutputbyFuelHourly_2023.xml")

# Define the namespace (replace 'd' with any prefix, e.g., 'd')
ns <- c(d = "http://www.ieso.ca/schema")

# Extract the daily data with namespace
daily_data <- xml_find_all(xml_file, ".//d:DailyData", ns)

# Initialize an empty list to store the results
results <- list()

# Loop through each daily entry
for (day_node in daily_data) {
  
  # Extract the date
  day <- xml_text(xml_find_first(day_node, ".//d:Day", ns))
  
  # Extract the hourly data for each day
  hourly_data <- xml_find_all(day_node, ".//d:HourlyData", ns)
  
  # Loop through each hour
  for (hour_node in hourly_data) {
    
    # Extract the hour
    hour <- xml_text(xml_find_first(hour_node, ".//d:Hour", ns))
    
    # Extract the fuel types and their outputs
    fuels <- xml_find_all(hour_node, ".//d:FuelTotal", ns)
    
    # Loop through each fuel type
    for (fuel_node in fuels) {
      
      # Extract fuel type and output
      fuel <- xml_text(xml_find_first(fuel_node, ".//d:Fuel", ns))
      output <- xml_text(xml_find_first(fuel_node, ".//d:Output", ns))
      
      # Append to the results list
      results <- append(results, list(data.frame(
        Day = day,
        Hour = hour,
        Fuel = fuel,
        Output = as.numeric(output),
        stringsAsFactors = FALSE
      )))
    }
  }
}

# Combine the list into a single data frame
final_df <- do.call(rbind, results)

# Reshape the data to wide format with Day and Hour
wide_df <- final_df %>%
  pivot_wider(names_from = Fuel, values_from = Output, values_fill = 0) %>% # Fill missing values with 0
  mutate("Total Output" = rowSums(select(., NUCLEAR, GAS, HYDRO, WIND, SOLAR, BIOFUEL))) %>% # Calculate Total Output
  select(Day, Hour, NUCLEAR, GAS, HYDRO, WIND, SOLAR, BIOFUEL, Total_Output) %>% # Reorder columns
  rename(Date = Day) # Rename the column 'Day' to 'Date'

# Save the wide data frame to a CSV file
write.csv(wide_df, "PUB_GenOutputbyFuelHourly_2023.csv", row.names = FALSE)
