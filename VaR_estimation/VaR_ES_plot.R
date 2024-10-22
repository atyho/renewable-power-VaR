rm(list = ls())

library(dplyr)
library(ggplot2)
library(ggthemes)
theme_set(theme_bw())
library(Cairo)

load(file="VaR.RData")

#### Summarize Risk in terms of shock values (MW) ####

# Capture summaries for VaR and ES in a list
shock_lvl_summary <- list(
  # VaR Summaries
  VaR_p10_IESO = summary(data.estimate$VaR_p10_IESO),
  VaR_p10_HydroQC = summary(data.estimate$VaR_p10_HydroQC),
  VaR_p10_NYISO = summary(data.estimate$VaR_p10_NYISO),
  VaR_p10_ISONE = summary(data.estimate$VaR_p10_ISONE),
  VaR_p10_total = summary(data.estimate$VaR_p10_total),
  
  VaR_p5_IESO = summary(data.estimate$VaR_p5_IESO),
  VaR_p5_HydroQC = summary(data.estimate$VaR_p5_HydroQC),
  VaR_p5_NYISO = summary(data.estimate$VaR_p5_NYISO),
  VaR_p5_ISONE = summary(data.estimate$VaR_p5_ISONE),
  VaR_p5_total = summary(data.estimate$VaR_p5_total),
  
  # ES Summaries
  ES_p10_IESO = summary(data.estimate$ES_p10_IESO),
  ES_p10_HydroQC = summary(data.estimate$ES_p10_HydroQC),
  ES_p10_NYISO = summary(data.estimate$ES_p10_NYISO),
  ES_p10_ISONE = summary(data.estimate$ES_p10_ISONE),
  ES_p10_total = summary(data.estimate$ES_p10_total),
  
  ES_p5_IESO = summary(data.estimate$ES_p5_IESO),
  ES_p5_HydroQC = summary(data.estimate$ES_p5_HydroQC),
  ES_p5_NYISO = summary(data.estimate$ES_p5_NYISO),
  ES_p5_ISONE = summary(data.estimate$ES_p5_ISONE),
  ES_p5_total = summary(data.estimate$ES_p5_total)
)

# Convert to a data frame and save it to CSV
shock_lvl_summary <- do.call(rbind, shock_lvl_summary)

# Save the results to a CSV file
write.csv(shock_lvl_summary, file = "shock_lvl_summary.csv", row.names = TRUE)

#### Summarize extreme renewable surplus (deficit) as percentage of renewable generation ####

# Joint estimated renewable surplus and risk with actual renewable generation for each ISO
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
    VaR_p5_prct_total = VaR_p5_total / (renewable_IESO + renewable_HydroQC + renewable_NYISO + renewable_ISONE),
    VaR_p10_prct_total = VaR_p10_total / (renewable_IESO + renewable_HydroQC + renewable_NYISO + renewable_ISONE),
    ES_p5_prct_total = ES_p5_total / (renewable_IESO + renewable_HydroQC + renewable_NYISO + renewable_ISONE),
    ES_p10_prct_total = ES_p10_total / (renewable_IESO + renewable_HydroQC + renewable_NYISO + renewable_ISONE)
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

# Save the results to a CSV file
write.csv(risk_prct_summary, file = "risk_prct_summary.csv", row.names = TRUE)

#### Summarize extreme renewable surplus (VaR and ES) in levels (MW) of renewable generation ####
risk_lvl_summary <- list(
  # VaR Summaries for p10
  VaR_p10_prct_IESO = summary(data.estimate$VaR_p10_IESO + data.estimate$fitted.IESO),
  VaR_p10_prct_HydroQC = summary(data.estimate$VaR_p10_HydroQC + data.estimate$fitted.HydroQC),
  VaR_p10_prct_NYISO = summary(data.estimate$VaR_p10_NYISO + data.estimate$fitted.NYISO),
  VaR_p10_prct_ISONE = summary(data.estimate$VaR_p10_ISONE + data.estimate$fitted.ISONE),
  VaR_p10_prct_total = summary(data.estimate$VaR_p10_total 
                               + data.estimate$fitted.IESO + data.estimate$fitted.HydroQC
                               + data.estimate$fitted.NYISO + data.estimate$fitted.ISONE),
  
  # VaR Summaries for p5
  VaR_p5_prct_IESO = summary(data.estimate$VaR_p5_IESO + data.estimate$fitted.IESO),
  VaR_p5_prct_HydroQC = summary(data.estimate$VaR_p5_HydroQC + data.estimate$fitted.HydroQC),
  VaR_p5_prct_NYISO = summary(data.estimate$VaR_p5_NYISO + data.estimate$fitted.NYISO),
  VaR_p5_prct_ISONE = summary(data.estimate$VaR_p5_ISONE + data.estimate$fitted.ISONE),
  VaR_p5_prct_total = summary(data.estimate$VaR_p5_total 
                              + data.estimate$fitted.IESO + data.estimate$fitted.HydroQC
                              + data.estimate$fitted.NYISO + data.estimate$fitted.ISONE),
  
  # ES Summaries for p10
  ES_p10_prct_IESO = summary(data.estimate$ES_p10_IESO + data.estimate$fitted.IESO),
  ES_p10_prct_HydroQC = summary(data.estimate$ES_p10_HydroQC + data.estimate$fitted.HydroQC),
  ES_p10_prct_NYISO = summary(data.estimate$ES_p10_NYISO + data.estimate$fitted.NYISO),
  ES_p10_prct_ISONE = summary(data.estimate$ES_p10_ISONE + data.estimate$fitted.ISONE),
  ES_p10_prct_total = summary(data.estimate$ES_p10_total 
                              + data.estimate$fitted.IESO + data.estimate$fitted.HydroQC
                              + data.estimate$fitted.NYISO + data.estimate$fitted.ISONE),
  
  # ES Summaries for p5
  ES_p5_prct_IESO = summary(data.estimate$ES_p5_IESO + data.estimate$fitted.IESO),
  ES_p5_prct_HydroQC = summary(data.estimate$ES_p5_HydroQC + data.estimate$fitted.HydroQC),
  ES_p5_prct_NYISO = summary(data.estimate$ES_p5_NYISO + data.estimate$fitted.NYISO),
  ES_p5_prct_ISONE = summary(data.estimate$ES_p5_ISONE + data.estimate$fitted.ISONE),
  ES_p5_prct_total = summary(data.estimate$ES_p5_total 
                             + data.estimate$fitted.IESO + data.estimate$fitted.HydroQC
                             + data.estimate$fitted.NYISO + data.estimate$fitted.ISONE)
  )

# Convert the summary list into a data frame
risk_lvl_summary <- do.call(rbind, risk_lvl_summary)

# Save the results to a CSV file
write.csv(risk_lvl_summary, file = "risk_lvl_summary.csv", row.names = TRUE)
