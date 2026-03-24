#!/usr/bin/env Rscript
# Example: ARIMA Forecast on S&P 500
# Demonstrates the finance module

library(RScriptsandStuff)

prices <- fetch_stock_prices("^GSPC", from = "2023-01-01", to = "2024-01-01")
adjusted <- prices[, grep("Adjusted", colnames(prices))]

fc <- fit_arima_forecast(adjusted, h = 30)
plot(fc, main = "S&P 500 — 30-Day ARIMA Forecast")

log_returns <- compute_log_returns(as.numeric(adjusted))
cat("Mean daily log return:", round(mean(log_returns), 6), "\n")
cat("Std  daily log return:", round(sd(log_returns), 6), "\n")
