rm(list = ls())

library(dplyr)
library(tidyr)
library(lubridate)

library(ggplot2)
library(ggthemes)
theme_set(theme_bw())
library(Cairo)

load(file="../VaR_estimation/VaR.RData")
load(file="../GHCND/regional_heat.RData")
load(file="../GHCND/regional_cold.RData")

#### Prepare the data set for analysis ####

# Replace Region labels
regional_heat <- regional_heat %>% 
  mutate(Region = case_when(
    Region == "Hydro Quebec" ~ "HydroQC",
    Region == "ISO-NE" ~ "ISONE",
    TRUE ~ Region))

regional_cold <- regional_cold %>% 
  mutate(Region = case_when(
    Region == "Hydro Quebec" ~ "HydroQC",
    Region == "ISO-NE" ~ "ISONE",
    TRUE ~ Region))

# Convert to wide shape
regional_heat <- regional_heat %>%
  pivot_wider(names_from = Region,
              values_from = regional_extreme)

regional_cold <- regional_cold %>%
  pivot_wider(names_from = Region,
              values_from = regional_extreme)

# Join regional extreme weather data sets
regional_extreme <- regional_heat %>%
  left_join(regional_cold, by = "Date", suffix = c(".heat", ".cold"))

regional_extreme <- regional_extreme %>%
  mutate(heat_count = rowSums(across(ends_with(".heat")), na.rm = TRUE)) %>%
  mutate(cold_count = rowSums(across(ends_with(".cold")), na.rm = TRUE))

data.estimate <- data.estimate %>%
  mutate(
    time = with_tz(time_utc, tzone = "America/New_York"),
    # subtract a minute to ensure the date is consistent with hour-ending
    Date = as.Date(format(time %m-% minutes(1), usetz = TRUE))
    ) %>%
  left_join(regional_extreme, by = "Date")


#### Summarize extreme renewable surplus (deficit) as percentage of renewable generation ####

# Join estimated renewable surplus and risk with actual renewable generation for each ISO
data.estimate <- data.estimate %>%
  left_join(IESO %>% select(time_utc, renewable_mw), by = "time_utc") %>%
  rename(renewable_IESO = renewable_mw) %>%
  left_join(HydroQC %>% select(time_utc, renewable_mw), by = "time_utc") %>%
  rename(renewable_HydroQC = renewable_mw) %>%
  left_join(NYISO %>% select(time_utc, renewable_mw), by = "time_utc") %>%
  rename(renewable_NYISO = renewable_mw) %>%
  left_join(ISONE %>% select(time_utc, renewable_mw), by = "time_utc") %>%
  rename(renewable_ISONE = renewable_mw) %>%
  mutate(renewable_total = renewable_IESO + renewable_HydroQC + renewable_NYISO + renewable_ISONE)

# Function to summarize risk in percentage of renewable generation
find_risk_prct <- function(data.estimate) {
  
  # Calculate renewable surplus (deficit) as a percentage of renewable generation
  data.estimate <- data.estimate %>%
    mutate(
      # VaR and ES percentages for IESO
      VaR_p5_prct_IESO = (VaR_p5_IESO + fitted.IESO) / renewable_IESO,
      VaR_p10_prct_IESO = (VaR_p10_IESO + fitted.IESO) / renewable_IESO,
      ES_p5_prct_IESO = (ES_p5_IESO + fitted.IESO) / renewable_IESO,
      ES_p10_prct_IESO = (ES_p10_IESO + fitted.IESO) / renewable_IESO,
      
      # VaR and ES percentages for HydroQC
      VaR_p5_prct_HydroQC = (VaR_p5_HydroQC + fitted.HydroQC) / renewable_HydroQC,
      VaR_p10_prct_HydroQC = (VaR_p10_HydroQC + fitted.HydroQC) / renewable_HydroQC,
      ES_p5_prct_HydroQC = (ES_p5_HydroQC + fitted.HydroQC) / renewable_HydroQC,
      ES_p10_prct_HydroQC = (ES_p10_HydroQC + fitted.HydroQC) / renewable_HydroQC,
      
      # VaR and ES percentages for NYISO
      VaR_p5_prct_NYISO = (VaR_p5_NYISO + fitted.NYISO) / renewable_NYISO,
      VaR_p10_prct_NYISO = (VaR_p10_NYISO + fitted.NYISO) / renewable_NYISO,
      ES_p5_prct_NYISO = (ES_p5_NYISO + fitted.NYISO) / renewable_NYISO,
      ES_p10_prct_NYISO = (ES_p10_NYISO + fitted.NYISO) / renewable_NYISO,
      
      # VaR and ES percentages for ISONE
      VaR_p5_prct_ISONE = (VaR_p5_ISONE + fitted.ISONE) / renewable_ISONE,
      VaR_p10_prct_ISONE = (VaR_p10_ISONE + fitted.ISONE) / renewable_ISONE,
      ES_p5_prct_ISONE = (ES_p5_ISONE + fitted.ISONE) / renewable_ISONE,
      ES_p10_prct_ISONE = (ES_p10_ISONE + fitted.ISONE) / renewable_ISONE,
      
      # Updated VaR and ES percentages for total, considering all ISOs
      VaR_p5_prct_total = VaR_p5_total / renewable_total,
      VaR_p10_prct_total = VaR_p10_total / renewable_total,
      ES_p5_prct_total = ES_p5_total / renewable_total,
      ES_p10_prct_total = ES_p10_total / renewable_total
    )
  
  # Summarize extreme renewable surplus (VaR and ES) in percentage of renewable generation
  risk_prct_summary <- list(
    # VaR Summaries
    VaR_p10_prct_IESO = summary(data.estimate$VaR_p10_prct_IESO),
    VaR_p10_prct_HydroQC = summary(data.estimate$VaR_p10_prct_HydroQC),
    VaR_p10_prct_NYISO = summary(data.estimate$VaR_p10_prct_NYISO),
    VaR_p10_prct_ISONE = summary(data.estimate$VaR_p10_prct_ISONE),
    VaR_p10_prct_total = summary(data.estimate$VaR_p10_prct_total),
    
    VaR_p5_prct_IESO = summary(data.estimate$VaR_p5_prct_IESO),
    VaR_p5_prct_HydroQC = summary(data.estimate$VaR_p5_prct_HydroQC),
    VaR_p5_prct_NYISO = summary(data.estimate$VaR_p5_prct_NYISO),
    VaR_p5_prct_ISONE = summary(data.estimate$VaR_p5_prct_ISONE),
    VaR_p5_prct_total = summary(data.estimate$VaR_p5_prct_total),
    
    # ES Summaries
    ES_p10_prct_IESO = summary(data.estimate$ES_p10_prct_IESO),
    ES_p10_prct_HydroQC = summary(data.estimate$ES_p10_prct_HydroQC),
    ES_p10_prct_NYISO = summary(data.estimate$ES_p10_prct_NYISO),
    ES_p10_prct_ISONE = summary(data.estimate$ES_p10_prct_ISONE),
    ES_p10_prct_total = summary(data.estimate$ES_p10_prct_total),
    
    ES_p5_prct_IESO = summary(data.estimate$ES_p5_prct_IESO),
    ES_p5_prct_HydroQC = summary(data.estimate$ES_p5_prct_HydroQC),
    ES_p5_prct_NYISO = summary(data.estimate$ES_p5_prct_NYISO),
    ES_p5_prct_ISONE = summary(data.estimate$ES_p5_prct_ISONE),
    ES_p5_prct_total = summary(data.estimate$ES_p5_prct_total)
  )
  
  # Convert the summary list into a data frame
  risk_prct_summary <- do.call(rbind, risk_prct_summary)
  
  return(list(data_estimate = data.estimate, 
              risk_prct_summary = risk_prct_summary)
         )
}

#### Summarize extreme renewable surplus (VaR and ES) in levels (MW) of renewable generation ####

# Function to summarize risk in levels
find_risk_lvl <- function(data.estimate) {
  risk_lvl_summary <- list(
    # VaR Summaries for p10
    VaR_p10_lvl_IESO = summary(data.estimate$VaR_p10_IESO + data.estimate$fitted.IESO),
    VaR_p10_lvl_HydroQC = summary(data.estimate$VaR_p10_HydroQC + data.estimate$fitted.HydroQC),
    VaR_p10_lvl_NYISO = summary(data.estimate$VaR_p10_NYISO + data.estimate$fitted.NYISO),
    VaR_p10_lvl_ISONE = summary(data.estimate$VaR_p10_ISONE + data.estimate$fitted.ISONE),
    VaR_p10_lvl_total = summary(data.estimate$VaR_p10_total 
                                + data.estimate$fitted.IESO + data.estimate$fitted.HydroQC
                                + data.estimate$fitted.NYISO + data.estimate$fitted.ISONE),
    
    # VaR Summaries for p5
    VaR_p5_lvl_IESO = summary(data.estimate$VaR_p5_IESO + data.estimate$fitted.IESO),
    VaR_p5_lvl_HydroQC = summary(data.estimate$VaR_p5_HydroQC + data.estimate$fitted.HydroQC),
    VaR_p5_lvl_NYISO = summary(data.estimate$VaR_p5_NYISO + data.estimate$fitted.NYISO),
    VaR_p5_lvl_ISONE = summary(data.estimate$VaR_p5_ISONE + data.estimate$fitted.ISONE),
    VaR_p5_lvl_total = summary(data.estimate$VaR_p5_total
                               + data.estimate$fitted.IESO + data.estimate$fitted.HydroQC
                               + data.estimate$fitted.NYISO + data.estimate$fitted.ISONE),
    
    # ES Summaries for p10
    ES_p10_lvl_IESO = summary(data.estimate$ES_p10_IESO + data.estimate$fitted.IESO),
    ES_p10_lvl_HydroQC = summary(data.estimate$ES_p10_HydroQC + data.estimate$fitted.HydroQC),
    ES_p10_lvl_NYISO = summary(data.estimate$ES_p10_NYISO + data.estimate$fitted.NYISO),
    ES_p10_lvl_ISONE = summary(data.estimate$ES_p10_ISONE + data.estimate$fitted.ISONE),
    ES_p10_lvl_total = summary(data.estimate$ES_p10_total 
                               + data.estimate$fitted.IESO + data.estimate$fitted.HydroQC
                               + data.estimate$fitted.NYISO + data.estimate$fitted.ISONE),
    
    # ES Summaries for p5
    ES_p5_lvl_IESO = summary(data.estimate$ES_p5_IESO + data.estimate$fitted.IESO),
    ES_p5_lvl_HydroQC = summary(data.estimate$ES_p5_HydroQC + data.estimate$fitted.HydroQC),
    ES_p5_lvl_NYISO = summary(data.estimate$ES_p5_NYISO + data.estimate$fitted.NYISO),
    ES_p5_lvl_ISONE = summary(data.estimate$ES_p5_ISONE + data.estimate$fitted.ISONE),
    ES_p5_lvl_total = summary(data.estimate$ES_p5_total 
                              + data.estimate$fitted.IESO + data.estimate$fitted.HydroQC
                              + data.estimate$fitted.NYISO + data.estimate$fitted.ISONE)
  )
  
  # Convert the summary list into a data frame
  risk_lvl_summary <- do.call(rbind, risk_lvl_summary)
  
  return(list(data_estimate = data.estimate, 
              risk_lvl_summary = risk_lvl_summary)
  )
}

#### Overall risk analysis ####

risk_prct_results <- find_risk_prct(data.estimate)
data.estimate.risk <- risk_prct_results$data_estimate
risk_prct_summary <- risk_prct_results$risk_prct_summary

# Save the results to a CSV file
write.csv(risk_prct_summary, file = "risk_prct_summary.csv", row.names = TRUE)

risk_lvl_results <- find_risk_lvl(data.estimate.risk)
data.estimate.risk <- risk_lvl_results$data_estimate
risk_lvl_summary <- risk_lvl_results$risk_lvl_summary

# Save the results to a CSV file
write.csv(risk_lvl_summary, file = "risk_lvl_summary.csv", row.names = TRUE)

#### Risk analysis for heat wave days ####

data_heat <- data.estimate %>% filter(heat_count > 0)

risk_prct_results <- find_risk_prct(data_heat)
data_heat.risk <- risk_prct_results$data_estimate
risk_prct_summary <- risk_prct_results$risk_prct_summary

# Save the results to a CSV file
write.csv(risk_prct_summary, file = "risk_prct_summary_heat.csv", row.names = TRUE)

risk_lvl_results <- find_risk_lvl(data_heat.risk)
data_heat.risk <- risk_lvl_results$data_estimate
risk_lvl_summary <- risk_lvl_results$risk_lvl_summary

# Save the results to a CSV file
write.csv(risk_lvl_summary, file = "risk_lvl_summary_heat.csv", row.names = TRUE)

#### Risk analysis for cold wave days ####

data_cold <- data.estimate %>% filter(cold_count > 0)

risk_prct_results <- find_risk_prct(data_cold)
data_cold.risk <- risk_prct_results$data_estimate
risk_prct_summary <- risk_prct_results$risk_prct_summary

# Save the results to a CSV file
write.csv(risk_prct_summary, file = "risk_prct_summary_cold.csv", row.names = TRUE)

risk_lvl_results <- find_risk_lvl(data_cold.risk)
data_cold.risk <- risk_lvl_results$data_estimate
risk_lvl_summary <- risk_lvl_results$risk_lvl_summary

# Save the results to a CSV file
write.csv(risk_lvl_summary, file = "risk_lvl_summary_cold.csv", row.names = TRUE)

#### Risk analysis for extreme weather days ####

data_extreme <- data.estimate %>% 
  mutate(extreme_count = heat_count + cold_count) %>%
  filter(extreme_count > 0)

risk_prct_results <- find_risk_prct(data_extreme)
data_extreme.risk <- risk_prct_results$data_estimate
risk_prct_summary <- risk_prct_results$risk_prct_summary

# Save the results to a CSV file
write.csv(risk_prct_summary, file = "risk_prct_summary_extreme.csv", row.names = TRUE)

risk_lvl_results <- find_risk_lvl(data_extreme.risk)
data_extreme.risk <- risk_lvl_results$data_estimate
risk_lvl_summary <- risk_lvl_results$risk_lvl_summary

# Save the results to a CSV file
write.csv(risk_lvl_summary, file = "risk_lvl_summary_extreme.csv", row.names = TRUE)

#### Plots for ES ####

# Calculate the 0.1% and 99.9% quantiles for trimming the x-axis for each dataset
x_min_overall <- quantile(data.estimate$ES_p5_total / 1000, probs = 0.001, na.rm = TRUE)
x_max_overall <- quantile(data.estimate$ES_p5_total / 1000, probs = 0.999, na.rm = TRUE)

x_min_heat <- quantile(data_heat.risk$ES_p5_total / 1000, probs = 0.001, na.rm = TRUE)
x_max_heat <- quantile(data_heat.risk$ES_p5_total / 1000, probs = 0.999, na.rm = TRUE)

x_min_cold <- quantile(data_cold.risk$ES_p5_total / 1000, probs = 0.001, na.rm = TRUE)
x_max_cold <- quantile(data_cold.risk$ES_p5_total / 1000, probs = 0.999, na.rm = TRUE)

x_min_extreme <- quantile(data_extreme.risk$ES_p5_total / 1000, probs = 0.001, na.rm = TRUE)
x_max_extreme <- quantile(data_extreme.risk$ES_p5_total / 1000, probs = 0.999, na.rm = TRUE)

# Find the overall minimum and maximum for consistent trimming across datasets
x_min_trim <- min(x_min_overall, x_min_heat, x_min_cold, x_min_extreme)
x_max_trim <- max(x_max_overall, x_max_heat, x_max_cold, x_max_extreme)

# Plot the density plots with different linetypes
ES_dist <- ggplot() +
  # Overall estimate
  geom_density(data = data.estimate, aes(x = ES_p5_total / 1000, color = "Overall", fill = "Overall", linetype = "Overall"), alpha = 0.5) +
  
  # Heat-wave data
  geom_density(data = data_heat.risk, aes(x = ES_p5_total / 1000, color = "Heat Wave", fill = "Heat Wave", linetype = "Heat Wave"), alpha = 0.5) +
  
  # Cold-wave data
  geom_density(data = data_cold.risk, aes(x = ES_p5_total / 1000, color = "Cold Wave", fill = "Cold Wave", linetype = "Cold Wave"), alpha = 0.5) +
  
  # Extreme risk data
  geom_density(data = data_extreme.risk, aes(x = ES_p5_total / 1000, color = "Extreme Risk", fill = "Extreme Risk", linetype = "Extreme Risk"), alpha = 0.5) +
  
  # Labels and axis setup
  labs(title = "", x = "Expected Shortfall (GW)", y = "Density") + 
  
  # Trim x-axis based on the calculated minimum and maximum
  coord_cartesian(xlim = c(x_min_trim, x_max_trim)) + 
  scale_x_continuous(breaks = seq(-3.0, 0, by = 0.5)) +
  
  # Customizing color, fill, and linetype
  scale_color_manual(name = "Scenario",
                     values = c("Overall" = "blue", "Heat Wave" = "red", "Cold Wave" = "green", "Extreme Risk" = "purple")) +
  scale_fill_manual(name = "Scenario",
                    values = c("Overall" = "blue", "Heat Wave" = "red", "Cold Wave" = "green", "Extreme Risk" = "purple")) +
  scale_linetype_manual(name = "Scenario",
                        values = c("Overall" = "solid", "Heat Wave" = "dashed", "Cold Wave" = "dotted", "Extreme Risk" = "dotdash")) +
  
  # Theme customization
  theme(axis.line = element_line(colour="black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "bottom", legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key.width = unit(2, "line"))

# Display the plot
print(ES_dist)

# Save the plot as EPS file
cairo_ps(filename = "ES_dist_plot.eps", width = 3, height = 3, pointsize = 12, fallback_resolution = 300)
print(ES_dist)
dev.off()

