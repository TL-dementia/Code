# Description of this data
[![DOI](https://zenodo.org/badge/798656941.svg)](https://doi.org/10.5281/zenodo.14624558)

 The codes and data are for the paper Improving accuracy in the estimation of probable dementia in racially and ethnically diverse groups with penalized regression and transfer learning (2025) by Jung Hyun Kim, M.Maria Glymour, Kenneth Langa, and Anja K. Leist. 

## Data
- Source data
  - The RAND HRS Longitudinal File 2018 (V1) [link](https://hrsdata.isr.umich.edu/data-products/rand-hrs-archived-data-products).
- Target data
  -  2016 HCAP Summary Cognitive and Functional Measures Data [link](https://hrsdata.isr.umich.edu/data-products/2016-hcap-summary-cognitive-and-functional) belongs to sensitive health data and requires access [link](https://hrsdata.isr.umich.edu/data-products/sensitive-health/order-form)
- Core files to calculate IQCODE for proxy respondents can be obtained from below. 
  - HRS 2014 Core Early Release (Version 2.0) [link](https://hrsdata.isr.umich.edu/data-products/2014-hrs-core).
  - HRS 2016 Core Final Release (Version 2.0) [link](https://hrsdata.isr.umich.edu/data-products/2016-hrs-core).
- Previous dementia estimation algorithms can be obtained from below.
  -  Gianattasio-Power Predicted Dementia Probability Scores and Dementia Classifications [link](https://hrsdata.isr.umich.edu/data-products/gianattasio-power-predicted-dementia-probability-scores-and-dementia-classifications?_ga=2.244704837.1182193271.1691073106-1414241986.1689842587&_gl=1*1k7ms74*_ga*MTQxNDI0MTk4Ni4xNjg5ODQyNTg3*_ga_FF28MW3MW2*MTY5MTA3MzEwNS41LjEuMTY5MTA3MzExMC4wLjAuMA.)
  -   Predicted Cognition and Dementia Measures [link](https://hrsdata.isr.umich.edu/data-products/predicted-cognition-and-dementia-measures?_gl=1*vdt11x*_ga*MTQxNDI0MTk4Ni4xNjg5ODQyNTg3*_ga_FF28MW3MW2*MTY5MTA3MzEwNS41LjEuMTY5MTA3MzEzNC4wLjAuMA..&_ga=2.148252151.1182193271.1691073106-1414241986.1689842587)

## Software
- STATA code is used to prepare the dataset and R code for the analysis.
- STATA version 17.0, R version 4.3.0 Glmtrans package version 2.0.0 are used.

# Steps to reproduce
## Data code (in STATA)
1. RAND_HRS.do subsets the variables of interest from the RAND HRS longitudinal data.
2. HCAP_diagnosis.do subsets the variable of interest from the HCAP data.
3. Core data code.do is to build cognitive assessment scores for the proxy respondents.

## Main Estimation code (in R)
1. Use 1. Final data (proxy+self).R to get the final analytical samples.
2. Use 2. Descriptive statistics.R to generate the descriptive statistics. 
3. Use 3-1. Self estimation.R to replicate the main results for the self-respondents.
4. Use 3-2. Proxy estimation.R to replicate the main results for the proxy-respondents.
5. Use 4. Source of bias.R to visualize the bias.

If you encounter any errors/problems reproducing the results, please send Jung Hyun an email.
