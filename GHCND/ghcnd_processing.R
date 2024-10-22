# Remove all objects from the environment
rm(list = ls())

#### Load required libraries ####
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)

library(ggplot2)
library(ggthemes)
theme_set(theme_bw())
library(Cairo)

#### Create a list of GHCN stations ####

# Read the GHCN station file
# Define the column positions based on the fixed-width specification
ghcn_station_all <- read_fwf("ghcnd-stations.txt",
                             fwf_positions(
                               start = c(1, 13, 22, 32, 39, 42, 73, 77, 81),
                               end = c(11, 20, 30, 37, 40, 71, 75, 79, 85),
                               col_names = c(
                                 "ID","Latitude","Longitude","Elevation",
                                 "State","Name","GSN_FLAG","HCN_CRN_FLAG",
                                 "WMO_ID")),
                             show_col_types = FALSE)

# Filter the selected stations to include only those with a non-missing WMO_ID
ghcn_station_all <- ghcn_station_all %>% filter(!is.na(WMO_ID) & WMO_ID != "")

# Define region mappings by State/Province code
IESO_states <- c("ON")                                # Ontario for IESO
HYDROQC_states <- c("QC")                             # Quebec for Hydro Quebec
NYISO_states <- c("NY")                               # New York for NY ISO
ISONE_states <- c("ME", "VT", "NH", "MA", "RI", "CT") # New England states for ISO-NE

# Combine all filtered stations into one data frame
ghcn_station <- bind_rows(
  ghcn_station_all %>% filter(State %in% IESO_states) %>% mutate(Region = "IESO"),
  ghcn_station_all %>% filter(State %in% HYDROQC_states) %>% mutate(Region = "Hydro Quebec"),
  ghcn_station_all %>% filter(State %in% NYISO_states) %>% mutate(Region = "NYISO"),
  ghcn_station_all %>% filter(State %in% ISONE_states) %>% mutate(Region = "ISO-NE"))

# Find duplicated coordinates
repeated_coords <- ghcn_station %>%
  group_by(Latitude, Longitude) %>%
  filter(n() > 1) %>%  # Only keep stations with the same coordinates
  arrange(Latitude, Longitude)

# Display the stations with repeated coordinates
print(repeated_coords)

#### Select station data file for required stations ####

# Get the list of station IDs from the selected GHCN stations
ghcn_station_ids <- ghcn_station$ID

# List the contents of the tar.gz file
file_list <- untar("ghcnd_all.tar.gz", list = TRUE)

# Function to find station files in batches
find_files <- function(station_ids, file_list, batch_size = 100) {
  
  # Initialize an empty vector to store the matched file names
  matched_files <- character(0)
  
  # Split the station IDs into batches
  station_batches <- split(station_ids, ceiling(seq_along(station_ids) / batch_size))
  
  # Loop through each batch of station IDs
  for (batch in station_batches) {
    # Create the regex pattern for the current batch
    batch_pattern <- paste0(batch, collapse = "|")
    
    # Find files that match the pattern
    matched_batch <- file_list[grepl(batch_pattern, file_list)]
    
    # Combine with the matched files from previous batches
    matched_files <- c(matched_files, matched_batch)
  }
  
  return(matched_files)
}

# Find the matching CSV files in batches (adjust batch_size if needed)
station_files <- find_files(ghcn_station_ids, file_list, batch_size = 50)

# Extract only the relevant CSV files
untar("ghcnd_all.tar.gz", files = station_files)

#### Load selected station data files ####

# Define the fixed-width column positions for GHCN-Daily with flags for each day
col_positions <- fwf_positions(
  start = c(1, 12, 16, 18, seq(22, 266, by = 8), seq(27, 271, by = 8), seq(28, 272, by = 8), seq(29, 273, by = 8)),
  end = c(11, 15, 17, 21, seq(26, 270, by = 8), seq(27, 271, by = 8), seq(28, 272, by = 8), seq(29, 273, by = 8)),
  col_names = c("ID", "Year", "Month", "Element", 
                paste0("Day", 1:31), 
                paste0("MFLAG", 1:31), 
                paste0("QFLAG", 1:31), 
                paste0("SFLAG", 1:31)))

# Set the default column type to character
col_types <- cols(
  .default = col_character()
)

# Define a function for reshaping the GHCN-Daily station data
reshape_ghcn_data <- function(station_data, element, start_year, end_year) {
  
  # Step 1: Filter to retain only the specified element and date
  station_data <- station_data %>% 
    filter(Element == element) %>% 
    filter(Year >= start_year & Year <= end_year)
  
  # Force all Day columns to numeric, replacing non-numeric values with NA
  station_data <- station_data %>%
    mutate(across(starts_with("Day"), ~ as.numeric(.)))
  
  # Step 2: Create a long format for the element values and include the date
  value_long <- station_data %>%
    pivot_longer(
      cols = starts_with("Day"),    # All day columns (Day1, Day2, ..., Day31)
      names_to = "Day",             # New column for day number
      names_prefix = "Day",         # Remove "Day" prefix
      values_to = element,          # Use the element name as the value column name
    ) %>%
    mutate(Date = suppressWarnings(ymd(paste(Year, Month, Day, sep = "-")))) %>%
    filter(!is.na(Date)) %>%  # Filter out invalid dates
    select(ID, Date, all_of(element))  # Select relevant columns for element
  
  # Step 3: Create long format for MFLAG and include the date
  mflag_long <- station_data %>%
    pivot_longer(
      cols = starts_with("MFLAG"),  # All MFLAG columns
      names_to = "Day",             # New column for day number
      names_prefix = "MFLAG",       # Remove "MFLAG" prefix
      values_to = "MFLAG",          # Name for MFLAG values
    ) %>%
    mutate(Date = suppressWarnings(ymd(paste(Year, Month, Day, sep = "-")))) %>%
    filter(!is.na(Date)) %>%  # Filter out invalid dates
    select(ID, Date, MFLAG)  # Select relevant columns for MFLAG
  
  # Step 4: Create long format for QFLAG and include the date
  qflag_long <- station_data %>%
    pivot_longer(
      cols = starts_with("QFLAG"),  # All QFLAG columns
      names_to = "Day",             # New column for day number
      names_prefix = "QFLAG",       # Remove "QFLAG" prefix
      values_to = "QFLAG",          # Name for QFLAG values
    ) %>%
    mutate(Date = suppressWarnings(ymd(paste(Year, Month, Day, sep = "-")))) %>%
    filter(!is.na(Date)) %>%  # Filter out invalid dates
    select(ID, Date, QFLAG)  # Select relevant columns for QFLAG
  
  # Step 5: Create long format for SFLAG and include the date
  sflag_long <- station_data %>%
    pivot_longer(
      cols = starts_with("SFLAG"),  # All SFLAG columns
      names_to = "Day",             # New column for day number
      names_prefix = "SFLAG",       # Remove "SFLAG" prefix
      values_to = "SFLAG",          # Name for SFLAG values
    ) %>%
    mutate(Date = suppressWarnings(ymd(paste(Year, Month, Day, sep = "-")))) %>%
    filter(!is.na(Date)) %>%  # Filter out invalid dates
    select(ID, Date, SFLAG)  # Select relevant columns for SFLAG
  
  # Step 6: Join all the long formats together by ID and Date
  formatted_data <- value_long %>%
    left_join(mflag_long, by = c("ID", "Date")) %>%
    left_join(qflag_long, by = c("ID", "Date")) %>%
    left_join(sflag_long, by = c("ID", "Date"))
  
  # Step 7: Replace missing values (-9999) with NA
  formatted_data <- formatted_data %>%
    mutate(across(c(all_of(element), MFLAG, QFLAG, SFLAG), ~ ifelse(. == -9999, NA, .))) %>%
    mutate(across(c(MFLAG, QFLAG, SFLAG), as.character))  # Ensure flags are always character
  
  return(formatted_data)
}

# Loop over all station data files
TMAX_data_by_station <- list() # Initialize an empty list to store data
TMIN_data_by_station <- list() # Initialize an empty list to store data
start_year <- 1980
end_year <- 2023
for (filename in station_files) {
  
  # Load the station data for a specific station
  station_data <- read_fwf(filename, col_positions, 
                           col_types = col_types, show_col_types = FALSE)

  # Use the reshape function to process the station data
  station_data_TMAX <- reshape_ghcn_data(station_data, "TMAX", start_year, end_year)
  station_data_TMIN <- reshape_ghcn_data(station_data, "TMIN", start_year, end_year)
  
  # Append the reshaped station data to the list
  TMAX_data_by_station[[filename]] <- station_data_TMAX
  TMIN_data_by_station[[filename]] <- station_data_TMIN

}

# Combine all the station data into a single dataframe
TMAX_data <- bind_rows(TMAX_data_by_station)
TMIN_data <- bind_rows(TMIN_data_by_station)

# Join the combined station data with station information
TMAX_data <- TMAX_data %>% left_join(ghcn_station, by = "ID")
TMIN_data <- TMIN_data %>% left_join(ghcn_station, by = "ID")

save(TMAX_data, file = "TMAX_data.RData")
save(TMIN_data, file = "TMIN_data.RData")

#### Identify regional extreme dates ####

# Function to identify extreme days and events (heat/cold)
station_extreme <- function(station_data, element, prctile, direction) {
  
  # Filter the data for the relevant months based on the element
  if (element == "TMAX") {
    # For TMAX, use only data from April to September (months 4 to 9)
    data_threshold <- station_data %>%
      filter(month(Date) >= 4 & month(Date) <= 9)
  } else if (element == "TMIN") {
    # For TMIN, use only data from October to March (months 10 to 12 and 1 to 3)
    data_threshold <- station_data %>%
      filter(month(Date) >= 10 | month(Date) <= 3)
  } else {
    stop("Element must be either 'TMAX' or 'TMIN'")
  }
  
  # Calculate the specified percentile for an element at each station (based on filtered data)
  prctile_data <- data_threshold %>% group_by(ID) %>%
    summarize(prctile_val = quantile(!!sym(element), prctile, na.rm = TRUE))
  
  # Identify days of extreme weather based on both the percentile threshold and hard limits
  station_data <- station_data %>%
    left_join(prctile_data, by = "ID") %>%
    mutate(extreme_day = ifelse(
      (direction == "above" & !!sym(element) > prctile_val & !!sym(element) > 320) | 
        (direction == "below" & !!sym(element) < prctile_val & !!sym(element) < -200), 1, 0)
      )
  
  # Drop rows where the element value is missing
  station_data <- station_data %>% filter(!is.na(!!sym(element)))

  # Identify station extreme events (>=3 consecutive days exceeding the threshold)
  station_extreme_data <- station_data %>% 
    group_by(ID) %>% arrange(Date) %>%
    mutate(event_group = cumsum(extreme_day != lag(extreme_day, default = 0))) %>% # Group consecutive extreme days
    group_by(ID, event_group) %>%
    mutate(extreme_event = ifelse(extreme_day == 1 & n() >= 3, 1, 0)) %>% # Identify extreme events
    ungroup() %>% select(-event_group) %>% # Clean up the intermediate grouping column
    arrange(ID, Date) # Sort by Station ID and Date before returning the result
  
  return(station_extreme_data)
}

# Function to define regional extreme days based on extreme events
regional_extreme <- function(station_extreme_data) {
  
  # Group by region and date, and check if any station in the region has an extreme event
  regional_extreme_data <- station_extreme_data %>%
    group_by(Region, Date) %>%
    summarize(regional_extreme = any(extreme_event == 1), .groups = "drop")
  
  return(regional_extreme_data)
}

# For station heat days (TMAX above 95th percentile)
station_heat <- station_extreme(TMAX_data, "TMAX", 0.95, direction = "above") %>%
  filter(year(Date) >= 2019 & year(Date) <= 2023)

# For station cold days (TMIN below 5th percentile)
station_cold <- station_extreme(TMIN_data, "TMIN", 0.05, direction = "below") %>%
  filter(year(Date) >= 2019 & year(Date) <= 2023)

# Identify regional heat wave days (>=1 station under a heat wave)
regional_heat <- regional_extreme(station_heat)

# Identify regional heat wave days (>=1 station under a heat wave)
regional_cold <- regional_extreme(station_cold)

save(regional_heat, file = "regional_heat.RData")
save(regional_cold, file = "regional_cold.RData")

#### Save the workspace ####
save.image(file = "GHCND_workspace.RData")
#### Correlation of heat wave dates between stations within the same region ####

station_heat_cor <- station_heat %>%
  filter(extreme_event == 1) %>%
  select(Date, ID, Region, extreme_event) %>%
  group_by(Region) %>%
  do({
    cor_matrix <- pivot_wider(., names_from = ID, values_from = extreme_event,
                              values_fill = list(extreme_event = 0)) %>%
      arrange(Region, Date) %>% select(-Date, -Region) %>%
      cor(., use = "pairwise.complete.obs")
    
    # Extract the upper triangular part of the correlation matrix (excluding diagonal)
    cor_values <- cor_matrix[upper.tri(cor_matrix, diag = FALSE)]
    
    # Calculate summary statistics for the correlation values
    tibble(cor_min = min(cor_values, na.rm = TRUE),
           cor_max = max(cor_values, na.rm = TRUE),
           cor_median = median(cor_values, na.rm = TRUE),
           cor_mean = mean(cor_values, na.rm = TRUE))
    }) %>% ungroup()

# View the correlation results
cat("Correlation of heat wave dates between stations within the same region")
print(station_heat_cor)

#### Correlation of cold spell dates between stations within the same region ####

station_cold_cor <- station_cold %>%
  filter(extreme_event == 1) %>%
  select(Date, ID, Region, extreme_event) %>%
  group_by(Region) %>%
  do({
    cor_matrix <- pivot_wider(., names_from = ID, values_from = extreme_event,
                              values_fill = list(extreme_event = 0)) %>%
      arrange(Region, Date) %>% select(-Date, -Region) %>%
      cor(., use = "pairwise.complete.obs")
    
    # Extract the upper triangular part of the correlation matrix (excluding diagonal)
    cor_values <- cor_matrix[upper.tri(cor_matrix, diag = FALSE)]
    
    # Calculate summary statistics for the correlation values
    tibble(cor_min = min(cor_values, na.rm = TRUE),
           cor_max = max(cor_values, na.rm = TRUE),
           cor_median = median(cor_values, na.rm = TRUE),
           cor_mean = mean(cor_values, na.rm = TRUE))
  }) %>% ungroup()

# View the correlation results
cat("Correlation of cold spell dates between stations within the same region")
print(station_cold_cor)
  
#### Correlation of heat wave dates between regions ####

region_heat_cor <- regional_heat %>%
  filter(regional_extreme == 1) %>%
  pivot_wider(names_from = Region, values_from = regional_extreme, 
              values_fill = list(regional_extreme = 0)) %>%
  arrange(Date) %>% select(-Date) %>%
  cor(., use = "pairwise.complete.obs")

# View the correlation matrix
cat("Correlation of heat wave dates between regions")
print(region_heat_cor)
write.csv(region_heat_cor, file = "region_heat_cor_inter.csv", row.names = FALSE)

#### Correlation of cold spell dates between regions ####

region_cold_cor <- regional_cold %>%
  filter(regional_extreme == 1) %>%
  pivot_wider(names_from = Region, values_from = regional_extreme, 
              values_fill = list(regional_extreme = 0)) %>%
  arrange(Date) %>% select(-Date) %>%
  cor(., use = "pairwise.complete.obs")

# View the correlation matrix
cat("Correlation of cold spell dates between regions")
print(region_cold_cor)
write.csv(region_cold_cor, file = "region_cold_cor_inter.csv", row.names = FALSE)

#### Create average heat wave day counts within a month for each region ####
regional_heat_freq <- regional_heat %>%
  mutate(month = month(Date), year = year(Date)) %>%
  group_by(Region, month, year) %>%
  summarize(days = sum(regional_extreme, na.rm = TRUE), .groups = "drop") %>%
  group_by(Region, month) %>%
  summarize(days_per_year = mean(days), .groups = "drop")

# Keep months with non-zero heat wave days
non_zero_months <- regional_heat_freq %>% group_by(month) %>%
  filter(days_per_year > 0) %>% pull(month)

regional_heat_freq <- regional_heat_freq %>% filter(month %in% non_zero_months)

#### Function to create extreme weather frequency plot ####

plot_extreme_freq <- function(data) {
  
  # Rename the Region levels to desired abbreviations
  data$Region <- recode(data$Region,
                        "IESO" = "ON", "Hydro Quebec" = "QC",
                        "NYISO" = "NY", "ISO-NE" = "NE")
  
  # Ensure correct factor levels and ordering
  data$Region <- factor(data$Region, levels = c("ON", "QC", "NY", "NE"))
  
  # Create the bar plot
  plot <- 
    ggplot(data, 
           aes(x = factor(month, levels = 1:12, labels = month.abb),
               y = days_per_year, fill = Region)) +
    geom_bar(stat = "identity", position = "dodge", color = "black", size = 0.25) +  # Bar graph with reduced line width
    labs(title = "", x = "", y = "", fill = "") +                     # Label for y-axis
    theme_minimal() +                                                              # Minimal theme
    theme(axis.line = element_blank(),                # Black axis lines
          panel.grid.major.y = element_line(color = "gray90", linewidth = 0.25),   # Show y-axis major grid lines
          panel.grid.minor = element_blank(),                                      # No minor grid lines
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),# Reduced border line width
          legend.position = "bottom",                                              # Legend at the bottom
          legend.direction = "horizontal",                                         # Horizontal legend
          legend.margin = margin(t = -20),                                         # Reduce space between the graph and the legend
          legend.title = element_blank(),                                          # No legend title
          legend.box.background = element_blank()) +                               # No legend background box
    scale_y_continuous(breaks = seq(0, max(data$days_per_year), by = 1)) +         # Add y-axis ticks
    scale_fill_manual(values = c("black", "gray50", "gray70", "gray90"),           # Custom fill colors for each region
                      labels = c("ON", "QC", "NY", "NE"))                          # Correct legend labels
  
  return(plot)
}

#### Create heat wave frequency plot ####

plot_heat_wave_freq <- plot_extreme_freq(regional_heat_freq)


cairo_ps(filename = "heat_waves.eps", 
         width=3, height=4, pointsize=12, fallback_resolution=300)
print(plot_heat_wave_freq)
dev.off()

#### Create average cold spell day counts within a month for each region ####
regional_cold_freq <- regional_cold %>%
  mutate(month = month(Date), year = year(Date)) %>%
  group_by(Region, month, year) %>%
  summarize(days = sum(regional_extreme, na.rm = TRUE), .groups = "drop") %>%
  group_by(Region, month) %>%
  summarize(days_per_year = mean(days), .groups = "drop")

# Keep months with non-zero cold spell days
non_zero_months <- regional_cold_freq %>% group_by(month) %>%
  filter(days_per_year > 0) %>% pull(month)

regional_cold_freq <- regional_cold_freq %>% filter(month %in% non_zero_months)

#### Create cold spell frequency plot ####

plot_cold_wave_freq <- plot_extreme_freq(regional_cold_freq)

cairo_ps(filename = "cold_waves.eps", 
         width=3, height=4, pointsize=12, fallback_resolution=300)
print(plot_cold_wave_freq)
dev.off()

#### Check if there are times with all regions having heat waves ####
dates_all_regions_heat <- regional_heat %>%
  group_by(Date) %>%
  summarize(all_regions_heat = all(regional_heat == 1)) %>%  # Check if all regions have regional_heat == 1 for the date
  filter(all_regions_heat == TRUE)  # Keep only the dates where all regions have heat

print(dates_all_regions_heat)


