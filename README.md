## Introduction

This repository contains the data files and the analytical programs used for the paper title "Walk Down to Electric Avenue: Risk Management of Interdependence of Renewable Energy in the North American Market."

## Content

This repository is structured in the following way:

### Data folders:

#### Folder: NE  
Description: Program for cleaning source data from ISO-NE. \
Execution: run *NE_data_process.R*

#### Folder: NY  
Description: Program for cleaning source data from NYISO. \
Execution: run *NY_data_process.R*  

#### Folder: ON  
Description: Program for cleaning source data from IESO. \
Execution: run *fuel_mix_xml_read.R* to read data in xml format and then run *ON_data_process.R* for data cleaning.

#### Folder: QC  
Description: Program for cleaning source data from Hydro Quebec. \
Execution: run *QC_data_process.R*

#### Folder: GHCND
Description: Program for cleaning source data from GHCNd, as well as identifying heat and cold waves. \
Execution: run *ghcnd_processing.R*

### Analytical Programs

#### Folder: VaR_estimation  
##### Description: 
Analytical program for computing the renewable energy surplus, estimating region-specific ARIMA processes, vine copula estimate, and simulate the value-at-risk (VaR) levels in Section 3.

#### Folder: VaR_conditional  
##### Description:




# Data sources
- [Independent Electricity System Operator (IESO)](https://www.ieso.ca/)
- [Hydro Quebec](https://www.hydroquebec.com/documents-data/open-data/electricity-generation-quebec/)
- [New York Independent System Operator (NYISO)](https://www.nyiso.com/)
- [Independent System Operator New England (ISO-NE)](https://www.iso-ne.com/])
- [Global Historical Climatology Network daily (GHCNd)](https://www.ncei.noaa.gov/products/land-based-station/global-historical-climatology-network-daily)
