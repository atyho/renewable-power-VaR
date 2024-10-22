rm(list = ls())

library(dplyr)
library(lubridate)
library(zoo)

library(ggplot2)
library(ggthemes)
theme_set(theme_bw())
library(Cairo)

load(file="VaR.RData")

# Standardize the load column name to load_mw for all ISOs
IESO <- IESO %>% rename(load_mw = demand_ON)
HydroQC <- HydroQC %>% rename(load_mw = load_QC)
NYISO <- NYISO %>% rename(load_mw = load_NY)
ISONE <- ISONE %>% rename(load_mw = load_NE)

#### Table on fuel mix ####

# Function to join ISO datasets for a specific variable of interest by time_utc
join_iso_data <- function(iso_list, var_name) {
  
  # Start with the first ISO dataset and select the variable
  joined_data <- iso_list[[1]] %>% select(time_utc, !!sym(var_name))
  
  # Loop through the remaining ISO datasets
  for (i in 2:length(iso_list)) {
    joined_data <- joined_data %>%
      inner_join(iso_list[[i]] %>% select(time_utc, !!sym(var_name)), by = "time_utc")
  }
  
  colnames(joined_data) <- c("time_utc", names(iso_list))
  return(joined_data)
}

# Define list of ISOs
iso_list <- list(IESO = IESO, HydroQC = HydroQC, NYISO = NYISO, ISONE = ISONE)

# Create a named list of your data frames
fuelmix_stat <- list(
  load_mw = join_iso_data(iso_list, "load_mw") %>% summarise(across(-time_utc, ~ mean(.x, na.rm = TRUE))),
  gen_mw = join_iso_data(iso_list, "total") %>% summarise(across(-time_utc, ~ mean(.x, na.rm = TRUE))),
  renewable_mw = join_iso_data(iso_list, "renewable_mw") %>% summarise(across(-time_utc, ~ mean(.x, na.rm = TRUE))),
  variable_mw = join_iso_data(iso_list, "variable_mw") %>% summarise(across(-time_utc, ~ mean(.x, na.rm = TRUE))),
  fossil_mw = join_iso_data(iso_list, "fossil_mw") %>% summarise(across(-time_utc, ~ mean(.x, na.rm = TRUE)))
)

# Use bind_rows to flatten and include row names from the list
fuelmix_stat <- bind_rows(fuelmix_stat, .id = "Statistic")
write.csv(fuelmix_stat, "fuelmix_stat.csv", row.names = FALSE)

#### Table on correlation among ISOs ####

# Define list of ISOs
iso_list <- list(IESO = IESO, HydroQC = HydroQC, NYISO = NYISO, ISONE = ISONE)

# Create a named list of your data frames
ISO_load_mw <- join_iso_data(iso_list, "load_mw")
cor_load <- cor(na.omit(ISO_load_mw) %>% select(-time_utc))
write.csv(cor_load, "cor_load.csv", row.names = FALSE)

ISO_surplus_mw = join_iso_data(iso_list, "surplus_mw")
cor_surplus <- cor(na.omit(ISO_surplus_mw) %>% select(-time_utc))
write.csv(cor_surplus, "cor_surplus.csv", row.names = FALSE)

#### Functions for plotting seasonality patterns ####

# Function to calculate mean values by different time dimensions
find_iso_mean <- function(data, var_name) {
  list(
    mean_by_hour = data %>%
      group_by(hour = hour(with_tz(time_utc, tzone = "America/New_York"))) %>%
      summarise(mean_mw = mean(!!sym(var_name), na.rm = TRUE)),
    
    mean_by_weekday = data %>%
      group_by(weekday = wday(with_tz(time_utc, tzone = "America/New_York"))) %>%
      summarise(mean_mw = mean(!!sym(var_name), na.rm = TRUE)),
    
    mean_by_month = data %>%
      group_by(month = month(with_tz(time_utc, tzone = "America/New_York"))) %>%
      summarise(mean_mw = mean(!!sym(var_name), na.rm = TRUE)),
    
    mean_by_year = data %>% 
      group_by(year = year(with_tz(time_utc, tzone = "America/New_York"))) %>%
      summarise(mean_mw = mean(!!sym(var_name), na.rm = TRUE)) %>% 
      filter(year <= 2023) # Drop the last hour of the data set
  )
}

# Function to calculate means for each ISO
find_mean_mw <- function(iso_list, var_name) {

  # Create data set for variable of interest
  joined_data <- join_iso_data(iso_list, var_name)
  
  # Calculate mean for each ISO and store in a list
  list(
    IESO = find_iso_mean(joined_data, "IESO"),
    HydroQC = find_iso_mean(joined_data, "HydroQC"),
    NYISO = find_iso_mean(joined_data, "NYISO"),
    ISONE = find_iso_mean(joined_data, "ISONE")
    )
}

# Create plotting data and generate the plot
create_plot <- function(mean_data_list, aggre_var, x_var, show_legend = TRUE, 
                            x_tick_interval = 1, label_every_nth_x = 1, 
                            y_tick_interval = 2.5, label_every_nth_y = 1) {
  
  # Create the plotting data and convert MW to GW
  plot_data <- bind_rows(
    mean_data_list$IESO[[aggre_var]] %>% mutate(ISO = "ON", mean_gw = mean_mw / 1000),
    mean_data_list$HydroQC[[aggre_var]] %>% mutate(ISO = "QC", mean_gw = mean_mw / 1000),
    mean_data_list$NYISO[[aggre_var]] %>% mutate(ISO = "NY", mean_gw = mean_mw / 1000),
    mean_data_list$ISONE[[aggre_var]] %>% mutate(ISO = "NE", mean_gw = mean_mw / 1000)
  )
  
  # Change the order of ISO factor levels for the legend
  plot_data$ISO <- factor(plot_data$ISO, levels = c("ON", "QC", "NY", "NE"))
  
  # Calculate the min and max values for the x and y axis
  x_min <- min(plot_data[[x_var]], na.rm = TRUE)
  x_max <- max(plot_data[[x_var]], na.rm = TRUE)
  y_min <- floor(min(plot_data$mean_gw, na.rm = TRUE))
  y_max <- ceiling(max(plot_data$mean_gw, na.rm = TRUE))
  
  # Generate labels: Display only every nth x-axis label
  x_labels <- seq(floor(x_min), ceiling(x_max), by = x_tick_interval)
  x_labels_to_show <- ifelse(x_labels %% label_every_nth_x == 0, x_labels, "")
  
  # Generate labels: Display only every nth y-axis label
  y_labels <- seq(floor(y_min / y_tick_interval) * y_tick_interval, 
                  ceiling(y_max / y_tick_interval) * y_tick_interval, 
                  by = y_tick_interval)
  y_labels_to_show <- ifelse(y_labels %% (y_tick_interval * label_every_nth_y) == 0, y_labels, "")
  
  # Create the base plot
  plot <- ggplot(plot_data, aes(x = .data[[x_var]], y = mean_gw, linetype = ISO, group = ISO)) +
    geom_line(color = "black", linewidth = 1.2) +  # Black lines with different linetypes
    labs(x = "", y = "") +  # Empty x and y labels
    theme_minimal() +
    theme(
      legend.position = if (show_legend) "bottom" else "none",  # Conditional legend display
      legend.title = element_blank(),  # Remove the legend title
      legend.text = element_text(size = 10),  # Adjust font size of legend text
      legend.key.width = unit(3, "line"),  # Extend the lines in the legend
      legend.margin = margin(t = -20),  # Reduce space between the graph and the legend
      axis.title = element_text(size = 12),  # Adjust axis title size
      axis.text = element_text(size = 10),  # Adjust axis text size
      axis.ticks = element_line(color = "black"),  # Add ticks on the axis
      plot.title = element_text(size = 14, hjust = 0.5),  # Center the plot title
      panel.grid = element_blank(),  # Remove all grid lines
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1)  # Add black border
    ) +
    scale_linetype_manual(values = c("ON" = "solid", "QC" = "dotdash", "NY" = "dashed", "NE" = "dotted")) +  # Different line types
    scale_x_continuous(breaks = x_labels, labels = x_labels_to_show, limits = c(x_min, x_max)) +  # Show only every nth x label
    scale_y_continuous(breaks = y_labels, labels = y_labels_to_show, limits = c(y_min, y_max)) +  # Show only every nth y label
    guides(linetype = guide_legend(nrow = 2, byrow = TRUE))
  
  return(plot)
}

#### Seasonality patterns for surplus_mw ####

# Calculate means for surplus_mw
iso_list <- list(IESO = IESO, HydroQC = HydroQC, NYISO = NYISO, ISONE = ISONE)
mean_surplus_mw <- find_mean_mw(iso_list, "surplus_mw")

plot_hour <- create_plot(mean_surplus_mw, "mean_by_hour", "hour", 
                         show_legend = FALSE, 
                         x_tick_interval = 1, label_every_nth_x = 6, 
                         y_tick_interval = 2.5, label_every_nth_y = 2)

print(plot_hour)

cairo_ps(filename = "mean_surplus_hour.eps", 
         width = 3, height = 2.5, pointsize = 12, fallback_resolution = 300)
print(plot_hour)
dev.off()

plot_weekday <- create_plot(mean_surplus_mw, "mean_by_weekday", "weekday", 
                            show_legend = FALSE, 
                            x_tick_interval = 1, label_every_nth_x = 1, 
                            y_tick_interval = 2.5, label_every_nth_y = 2)

print(plot_weekday)

cairo_ps(filename = "mean_surplus_weekday.eps", 
         width = 3, height = 2.5, pointsize = 12, fallback_resolution = 300)
print(plot_weekday)
dev.off()

plot_month <- create_plot(mean_surplus_mw, "mean_by_month", "month", 
                          show_legend = FALSE, 
                          x_tick_interval = 1, label_every_nth_x = 2, 
                          y_tick_interval = 2.5, label_every_nth_y = 2)

print(plot_month)

cairo_ps(filename = "mean_surplus_month.eps", 
         width = 3, height = 2.5, pointsize = 12, fallback_resolution = 300)
print(plot_month)
dev.off()

plot_year <- create_plot(mean_surplus_mw, "mean_by_year", "year", 
                         show_legend = TRUE, 
                         x_tick_interval = 1, label_every_nth_x = 1, 
                         y_tick_interval = 2.5, label_every_nth_y = 2)

print(plot_year)

cairo_ps(filename = "mean_surplus_year.eps", 
         width = 3, height = 2.9, pointsize = 12, fallback_resolution = 300)
print(plot_year)
dev.off()

#### Seasonality patterns for load_mw ####

# Calculate means for load_mw
iso_list <- list(IESO = IESO, HydroQC = HydroQC, NYISO = NYISO, ISONE = ISONE)
mean_load_mw <- find_mean_mw(iso_list, "load_mw")

plot_hour <- create_plot(mean_load_mw, "mean_by_hour", "hour", 
                         show_legend = FALSE, 
                         x_tick_interval = 1, label_every_nth_x = 6, 
                         y_tick_interval = 2.5, label_every_nth_y = 2)

print(plot_hour)

cairo_ps(filename = "mean_load_hour.eps", 
         width = 3, height = 2.5, pointsize = 12, fallback_resolution = 300)
print(plot_hour)
dev.off()

plot_weekday <- create_plot(mean_load_mw, "mean_by_weekday", "weekday", 
                            show_legend = FALSE, 
                            x_tick_interval = 1, label_every_nth_x = 1, 
                            y_tick_interval = 2.5, label_every_nth_y = 2)

print(plot_weekday)

cairo_ps(filename = "mean_load_weekday.eps", 
         width = 3, height = 2.5, pointsize = 12, fallback_resolution = 300)
print(plot_weekday)
dev.off()

plot_month <- create_plot(mean_load_mw, "mean_by_month", "month", 
                          show_legend = FALSE, 
                          x_tick_interval = 1, label_every_nth_x = 2, 
                          y_tick_interval = 2.5, label_every_nth_y = 2)

print(plot_month)

cairo_ps(filename = "mean_load_month.eps", 
         width = 3, height = 2.5, pointsize = 12, fallback_resolution = 300)
print(plot_month)
dev.off()

plot_year <- create_plot(mean_load_mw, "mean_by_year", "year", 
                         show_legend = TRUE, 
                         x_tick_interval = 1, label_every_nth_x = 1, 
                         y_tick_interval = 2.5, label_every_nth_y = 2)

print(plot_year)

cairo_ps(filename = "mean_load_year.eps", 
         width = 3, height = 2.9, pointsize = 12, fallback_resolution = 300)
print(plot_year)
dev.off()
