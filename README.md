## Introduction

This repository contains the data files and the analytical programs used for the paper title "Walk Down to Electric Avenue: Risk Management of Interdependence of Renewable Energy in the North American Market."

## Content

This repository is structured in the following way: first execute data cleaning programs specified in the 4 data folders ([NE](#NE), NY, ON, QC) to process the source data. Also execute the data processing program in GHCND folder to identify extreme weather dates. Data folders are independent and they do not have to be executed in any specific order.

For the analytical programs, first execute the programs in the 

### Data folders:

#### Folder: [NE](/NE)
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

##### Execution:
stylized_facts.R
VaR.R
VaR_ES_plot.R

#### Folder: VaR_conditional  
##### Description:
Conduct the value-at-risk simulation (in Section 5) for dates categorized as with extreme weather in specific regions.

VaR.R
VaR_ES_plot.R

# Data sources
- [Independent Electricity System Operator (IESO)](https://www.ieso.ca/)
- [Hydro Quebec](https://www.hydroquebec.com/documents-data/open-data/electricity-generation-quebec/)
- [New York Independent System Operator (NYISO)](https://www.nyiso.com/)
- [Independent System Operator New England (ISO-NE)](https://www.iso-ne.com/])
- [Global Historical Climatology Network daily (GHCNd)](https://www.ncei.noaa.gov/products/land-based-station/global-historical-climatology-network-daily)
