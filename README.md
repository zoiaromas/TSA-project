# Time Series Analysis - project in R

## Description

This project was completed as part of a Time Series Analysis course. The goal of the project was to analyze two time series datasets, I decidesd to use data from the Federal Reserve Economic Data (FRED) website as a main source.

1. **Non-Seasonal Time Series:** Monthly changes in the global price of olive oil (in USD per ton) from January 2000 to December 2023.
2. **Seasonal Time Series:** Monthly retail sales data in categories such as furniture, home furnishings, electronics, and household appliances (in million USD) for the same period.

For each time series, decomposition was performed, followed by fitting an ARIMA model (for the non-seasonal series) or a SARIMA model (for the seasonal series). Extrapolative forecasting models were also applied. The forecasts were evaluated both empirically and using appropriate statistical indicators.

The time series were split into in-sample and out-of-sample parts (3 months for the non-seasonal series and a full seasonal cycle for the seasonal series) to assess forecast accuracy. The analysis was performed using Demetra and RStudio, utilizing available libraries.

**Note:** The report and code comments are written in Polish.
