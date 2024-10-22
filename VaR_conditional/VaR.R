rm(list = ls())

library(dplyr)
library(lubridate)
library(zoo)

library(VineCopula)
library(rugarch)
library(sn)
library(forecast)

library(foreach)
library(doParallel)

library(ggplot2)

#### Program configuration ####

# Set random seed
set.seed(831114)

# Specify the number of CPU cores you want to use
num_cores <- 3 #detectCores() -1

# Register a parallel back-end
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Get the number of cores in the cluster
num_cores_in_cluster <- getDoParWorkers()
cat("Number of cores in the cluster:", num_cores_in_cluster, "\n")

# Record the start time
start_time <- Sys.time()

#### Load ISO data ####

load("../ON/IESO.RData")
load("../QC/HydroQC.RData")
load("../NY/NYISO.RData")
load("../NE/ISONE.RData")

#### Create new variables based on fuel type ####

IESO <- IESO %>%
  mutate(variable_mw = WIND + SOLAR,
         renewable_mw = variable_mw + HYDRO + NUCLEAR + BIOFUEL,
         fossil_mw = GAS,
         surplus_mw = renewable_mw - demand_ON,
         total_mw = renewable_mw + fossil_mw) %>%
  filter(year(time) > 2018) %>% select(-c(demand_mkt, `Total Output`))

HydroQC <- HydroQC %>%
  mutate(variable_mw = Eolien + Solaire,
         renewable_mw = variable_mw + Hydraulique + Autres,
         fossil_mw = Thermique,
         surplus_mw = renewable_mw - load_QC,
         total_mw = renewable_mw + fossil_mw) %>%
  filter(year(time) > 2018) %>% select(-c(Total))

NYISO <- NYISO %>%
  mutate(variable_mw = Wind,
         renewable_mw = variable_mw + Hydro + Nuclear + `Other Renewables`,
         fossil_mw = `Dual Fuel` + `Natural Gas` + `Other Fossil Fuels`,
         surplus_mw = renewable_mw - load_NY,
         total_mw = renewable_mw + fossil_mw) %>%
  filter(year(time) > 2018)

ISONE <- ISONE %>%
  mutate(variable_mw = Solar + Wind,
         renewable_mw = variable_mw + Hydro + Nuclear + `Landfill Gas` + Refuse + Wood,
         fossil_mw = `Coal` + `Natural Gas` + `Oil`,
         surplus_mw = renewable_mw - load_NE,
         total_mw = renewable_mw + fossil_mw) %>%
  filter(year(time) > 2018)

#### Merge ISO data ####

# Perform all inner joins in a single pipeline
renewable_surplus <- 
  IESO %>% select(time_utc, surplus_mw) %>%
  inner_join(NYISO %>% select(time_utc, surplus_mw), by = "time_utc", suffix = c(".IESO", ".NYISO")) %>%
  inner_join(ISONE %>% select(time_utc, surplus_mw), by = "time_utc") %>%
  inner_join(HydroQC %>% select(time_utc, surplus_mw), by = "time_utc", suffix = c(".ISONE", ".HydroQC")) %>%
  mutate(
    year = factor(year(time_utc)),
    month = factor(month(time_utc)),
    week = factor(week(time_utc)),
    wday = factor(wday(time_utc)),
    hour = factor(hour(time_utc))
  )

renewable_surplus <- na.omit(renewable_surplus)

# Data check: Find time_utc values in A that are missing in B
missing_NYISO <- anti_join(HydroQC, NYISO, by = "time_utc")
missing_ISONE <- anti_join(HydroQC, ISONE, by = "time_utc")

#### Demean trend and seasonality ####

mean_yr <- renewable_surplus %>%
  group_by(year) %>%
  summarize(
    mean_yr.IESO = mean(surplus_mw.IESO, na.rm = TRUE),
    mean_yr.HydroQC = mean(surplus_mw.HydroQC, na.rm = TRUE),
    mean_yr.NYISO = mean(surplus_mw.NYISO, na.rm = TRUE),
    mean_yr.ISONE = mean(surplus_mw.ISONE, na.rm = TRUE),
    .groups = 'drop')

renewable_surplus <- left_join(renewable_surplus, mean_yr, by = "year")

mean_mn <- renewable_surplus %>%
  group_by(month) %>%
  summarize(
    mean_mn.IESO = mean(surplus_mw.IESO, na.rm = TRUE),
    mean_mn.HydroQC = mean(surplus_mw.HydroQC, na.rm = TRUE),
    mean_mn.NYISO = mean(surplus_mw.NYISO, na.rm = TRUE),
    mean_mn.ISONE = mean(surplus_mw.ISONE, na.rm = TRUE),
    .groups = 'drop')

renewable_surplus <- left_join(renewable_surplus, mean_mn, by = "month")

mean_wdayhr <- renewable_surplus %>%
  group_by(wday, hour) %>%
  summarize(
    mean_wdayhr.IESO = mean(surplus_mw.IESO, na.rm = TRUE),
    mean_wdayhr.HydroQC = mean(surplus_mw.HydroQC, na.rm = TRUE),
    mean_wdayhr.NYISO = mean(surplus_mw.NYISO, na.rm = TRUE),
    mean_wdayhr.ISONE = mean(surplus_mw.ISONE, na.rm = TRUE),
    .groups = 'drop')

renewable_surplus <- left_join(renewable_surplus, mean_wdayhr, by = c("wday","hour"))

#### ARMA-GARCH ####

# Automatically find the best ARMA process for the mean equation
auto_arma_fit <- auto.arima(renewable_surplus$surplus_mw.IESO -
                              renewable_surplus$mean_yr.IESO -
                              renewable_surplus$mean_mn.IESO -
                              renewable_surplus$mean_wdayhr.IESO, 
                            seasonal = TRUE)

# Extract the AR and MA orders
arma_order <- arimaorder(auto_arma_fit)
p <- arma_order[1]  # AR order
q <- arma_order[3]  # MA order

# Model specification
margin.spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                          mean.model = list(armaOrder = c(2,5)),
                          distribution.model = "sstd")

# Estimate the model with de-meaned data
ar.garch.IESO <- ugarchfit(spec = margin.spec, solver = "hybrid",
                           data = renewable_surplus$surplus_mw.IESO -
                             renewable_surplus$mean_yr.IESO -
                             renewable_surplus$mean_mn.IESO -
                             renewable_surplus$mean_wdayhr.IESO)

ar.garch.HydroQC <- ugarchfit(spec = margin.spec, solver = "hybrid",
                              data = renewable_surplus$surplus_mw.HydroQC - 
                                renewable_surplus$mean_yr.HydroQC - 
                                renewable_surplus$mean_mn.HydroQC - 
                                renewable_surplus$mean_wdayhr.HydroQC)

ar.garch.NYISO <- ugarchfit(spec = margin.spec, solver = "hybrid",
                            data = renewable_surplus$surplus_mw.NYISO - 
                              renewable_surplus$mean_yr.NYISO - 
                              renewable_surplus$mean_mn.NYISO - 
                              renewable_surplus$mean_wdayhr.NYISO)

ar.garch.ISONE <- ugarchfit(spec = margin.spec, solver = "hybrid",
                            data = renewable_surplus$surplus_mw.ISONE - 
                             renewable_surplus$mean_yr.ISONE - 
                             renewable_surplus$mean_mn.ISONE - 
                             renewable_surplus$mean_wdayhr.ISONE)

# Standardize the residuals
res.IESO <- residuals(ar.garch.IESO)/sigma(ar.garch.IESO)
res.HydroQC <- residuals(ar.garch.HydroQC)/sigma(ar.garch.HydroQC)
res.NYISO <- residuals(ar.garch.NYISO)/sigma(ar.garch.NYISO)
res.ISONE <- residuals(ar.garch.ISONE)/sigma(ar.garch.ISONE)

# Combine residuals into a dataframe
data.res <- data.frame(res.IESO = res.IESO,
                       res.HydroQC = res.HydroQC,
                       res.NYISO = res.NYISO,
                       res.ISONE = res.ISONE)

#### Estimate copula model for the residuals ####

# Convert standardized residuals to pseudo-observations 
data.res.pobs <- pobs(data.res)

# Select the R-vine structure, families and parameters
# Allow for the copula families: Gauss, t, Clayton, Gumbel, Frank and Joe
RVM <- RVineStructureSelect(data.res.pobs, familyset = c(1:6), progress = TRUE)

# Show estimates of the selected R-vine structure
summary(RVM)
writeLines(capture.output(summary(RVM)), "RVM_summary.txt")
write.csv(summary(RVM), file = "RVM_summary.csv", row.names = FALSE)

# contour plots of all pair-copulas
contour(RVM,
        col = "black",  # Set the contour lines to black
        drawlabels = TRUE)  # Add labels to the contour lines

#### Value at Risk Simulation ####

# Define the number of periods and repetitions
n_period <- nrow(renewable_surplus)
n_rep <- 1

# Loop through each period of time
# foreach ensures the results are reassembled in the correct order 
par_packages = c("VineCopula","rugarch","sn","dplyr")
sim_risk <- foreach (period_i = 1:n_period, .packages = par_packages) %dopar% {

  # Create a list with empty matrices to store simulation results
  VaR_sim <- list(
    IESO = matrix(NA, nrow = n_rep, ncol = 2),
    HydroQC = matrix(NA, nrow = n_rep, ncol = 2),
    NYISO = matrix(NA, nrow = n_rep, ncol = 2),
    ISONE = matrix(NA, nrow = n_rep, ncol = 2),
    total = matrix(NA, nrow = n_rep, ncol = 2))

  ES_sim <- list(
    IESO = matrix(NA, nrow = n_rep, ncol = 2),
    HydroQC = matrix(NA, nrow = n_rep, ncol = 2),
    NYISO = matrix(NA, nrow = n_rep, ncol = 2),
    ISONE = matrix(NA, nrow = n_rep, ncol = 2),
    total = matrix(NA, nrow = n_rep, ncol = 2))
  
  # Loop through each repetition in this period
  for (rep in 1:n_rep) {
    
    # Simulate from the fitted vine copula
    simdata <- RVineSim(200, RVM)
    
    # Apply quantile function to retrieve shock values 
    sim.res.IESO <- sapply(simdata[,"res.IESO"], FUN = qst, 
                           df = ar.garch.IESO[shape], mean = 0, sd = 1, 
                           skew = ar.garch.IESO[skew])
    
    sim.res.IESO <- sim.res.IESO*as.numeric(sigma(ar.garch.IESO)[period_i])
    
    sim.res.HydroQC <- sapply(simdata[,"res.HydroQC"], FUN = qst, 
                              df = ar.garch.HydroQC[shape], mean = 0, sd = 1, 
                              skew = ar.garch.HydroQC[skew])
    
    sim.res.HydroQC <- sim.res.HydroQC*as.numeric(sigma(ar.garch.HydroQC)[period_i])
    
    sim.res.NYISO <- sapply(simdata[,"res.NYISO"], FUN = qst, 
                            df = ar.garch.NYISO[shape], mean = 0, sd = 1, 
                            skew = ar.garch.NYISO[skew])
    
    sim.res.NYISO <- sim.res.NYISO*as.numeric(sigma(ar.garch.NYISO)[period_i])
    
    sim.res.ISONE <- sapply(simdata[,"res.ISONE"], FUN = qst, 
                           df = ar.garch.ISONE[shape], mean = 0, sd = 1, 
                           skew = ar.garch.ISONE[skew])
    
    sim.res.ISONE <- sim.res.ISONE*as.numeric(sigma(ar.garch.ISONE)[period_i])
    
    sim.res <- data.frame(sim.res.IESO, 
                          sim.res.HydroQC, 
                          sim.res.NYISO, 
                          sim.res.ISONE) %>%
      mutate(sim.res.total = sim.res.IESO + sim.res.HydroQC + sim.res.NYISO + sim.res.ISONE)
    
    # Simulated value-at-risk value
    VaR_sim$IESO[rep,1]    <- quantile(sim.res$sim.res.IESO, probs = 0.05)
    VaR_sim$HydroQC[rep,1] <- quantile(sim.res$sim.res.HydroQC, probs = 0.05)
    VaR_sim$NYISO[rep,1]   <- quantile(sim.res$sim.res.NYISO, probs = 0.05)
    VaR_sim$ISONE[rep,1]   <- quantile(sim.res$sim.res.ISONE, probs = 0.05)
    VaR_sim$total[rep,1]   <- quantile(sim.res$sim.res.total, probs = 0.05)
    
    VaR_sim$IESO[rep,2]    <- quantile(sim.res$sim.res.IESO, probs = 0.1)
    VaR_sim$HydroQC[rep,2] <- quantile(sim.res$sim.res.HydroQC, probs = 0.1)
    VaR_sim$NYISO[rep,2]   <- quantile(sim.res$sim.res.NYISO, probs = 0.1)
    VaR_sim$ISONE[rep,2]   <- quantile(sim.res$sim.res.ISONE, probs = 0.1)
    VaR_sim$total[rep,2]   <- quantile(sim.res$sim.res.total, probs = 0.1)

    # Calculate Expected Shortfall (ES) by averaging losses beyond VaR
    ES_sim$IESO[rep,1]    <- mean(sim.res$sim.res.IESO[sim.res$sim.res.IESO < VaR_sim$IESO[rep,1]])
    ES_sim$HydroQC[rep,1] <- mean(sim.res$sim.res.HydroQC[sim.res$sim.res.HydroQC < VaR_sim$HydroQC[rep,1]])
    ES_sim$NYISO[rep,1]   <- mean(sim.res$sim.res.NYISO[sim.res$sim.res.NYISO < VaR_sim$NYISO[rep,1]])
    ES_sim$ISONE[rep,1]   <- mean(sim.res$sim.res.ISONE[sim.res$sim.res.ISONE < VaR_sim$ISONE[rep,1]])
    ES_sim$total[rep,1]   <- mean(sim.res$sim.res.total[sim.res$sim.res.total < VaR_sim$total[rep,1]])
    
    ES_sim$IESO[rep,2]    <- mean(sim.res$sim.res.IESO[sim.res$sim.res.IESO < VaR_sim$IESO[rep,2]])
    ES_sim$HydroQC[rep,2] <- mean(sim.res$sim.res.HydroQC[sim.res$sim.res.HydroQC < VaR_sim$HydroQC[rep,2]])
    ES_sim$NYISO[rep,2]   <- mean(sim.res$sim.res.NYISO[sim.res$sim.res.NYISO < VaR_sim$NYISO[rep,2]])
    ES_sim$ISONE[rep,2]   <- mean(sim.res$sim.res.ISONE[sim.res$sim.res.ISONE < VaR_sim$ISONE[rep,2]])
    ES_sim$total[rep,2]   <- mean(sim.res$sim.res.total[sim.res$sim.res.total < VaR_sim$total[rep,2]])
    
  }
  
  # Store the results for this period in the list
  sim_risk <- list(time_utc = renewable_surplus$time_utc[period_i],
                   VaR_p5_IESO = mean(VaR_sim$IESO[,1]),
                   VaR_p5_HydroQC = mean(VaR_sim$HydroQC[,1]),
                   VaR_p5_NYISO = mean(VaR_sim$NYISO[,1]),
                   VaR_p5_ISONE = mean(VaR_sim$ISONE[,1]),
                   VaR_p5_total = mean(VaR_sim$total[,1]),
                   
                   VaR_p10_IESO = mean(VaR_sim$IESO[,2]),
                   VaR_p10_HydroQC = mean(VaR_sim$HydroQC[,2]),
                   VaR_p10_NYISO = mean(VaR_sim$NYISO[,2]),
                   VaR_p10_ISONE = mean(VaR_sim$ISONE[,2]),
                   VaR_p10_total = mean(VaR_sim$total[,2]),
                   
                   ES_p5_IESO = mean(ES_sim$IESO[,1]),
                   ES_p5_HydroQC = mean(ES_sim$HydroQC[,1]),
                   ES_p5_NYISO = mean(ES_sim$NYISO[,1]),
                   ES_p5_ISONE = mean(ES_sim$ISONE[,1]),
                   ES_p5_total = mean(ES_sim$total[,1]),
                   
                   ES_p10_IESO = mean(ES_sim$IESO[,2]),
                   ES_p10_HydroQC = mean(ES_sim$HydroQC[,2]),
                   ES_p10_NYISO = mean(ES_sim$NYISO[,2]),
                   ES_p10_ISONE = mean(ES_sim$ISONE[,2]),
                   ES_p10_total = mean(ES_sim$total[,2])
                   )
  
}

# Convert simulation results into a dataframe
sim_risk_df <- do.call(rbind, lapply(sim_risk, as.data.frame))

#### Distribution of renewable surplus ####

# Compute fitted values from ARMA-GARCH
fitted.IESO <- fitted(ar.garch.IESO) + 
  renewable_surplus$mean_yr.IESO + 
  renewable_surplus$mean_mn.IESO + 
  renewable_surplus$mean_wdayhr.IESO

fitted.HydroQC <- fitted(ar.garch.HydroQC) + 
  renewable_surplus$mean_yr.HydroQC + 
  renewable_surplus$mean_mn.HydroQC + 
  renewable_surplus$mean_wdayhr.HydroQC

fitted.NYISO <- fitted(ar.garch.NYISO) +
  renewable_surplus$mean_yr.NYISO + 
  renewable_surplus$mean_mn.NYISO + 
  renewable_surplus$mean_wdayhr.NYISO

fitted.ISONE <- fitted(ar.garch.ISONE) + 
  renewable_surplus$mean_yr.ISONE + 
  renewable_surplus$mean_mn.ISONE + 
  renewable_surplus$mean_wdayhr.ISONE

# Combine with the shocks
data.estimate <- data.frame(fitted.IESO, fitted.HydroQC, 
                            fitted.NYISO, fitted.ISONE,
                            sim_risk_df)

save.image(file="VaR.RData")

#### Record the end time ####
end_time <- Sys.time()
elapsed_time <- end_time-start_time
print(paste("Program execution time:", round(elapsed_time,digits=1), units(elapsed_time)))

stopCluster(cl)
