#!/usr/bin/env Rscript
# Example: Random Portfolio vs Benchmark
# Demonstrates the portfolio module

library(RScriptsandStuff)
library(tidyquant)

tickers <- c("AAPL", "MSFT", "GOOG", "AMZN", "TSLA", "NVDA", "META", "JPM")
benchmark <- "SPY"

stock_data <- tickers %>%
  tq_get(get = "stock.prices", from = "2023-01-01", to = "2024-01-01") %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted, mutate_fun = periodReturn,
               period = "daily", type = "log", col_rename = "Ra")

bench_data <- benchmark %>%
  tq_get(get = "stock.prices", from = "2023-01-01", to = "2024-01-01") %>%
  tq_transmute(select = adjusted, mutate_fun = periodReturn,
               period = "daily", type = "log", col_rename = "Rb")

weights <- build_random_portfolio(tickers, zero_fraction = 0.25, seed = 42)
cat("Portfolio weights:\n")
print(weights)

portfolio_ret <- stock_data %>%
  tq_portfolio(assets_col = symbol, returns_col = Ra,
               weights = weights, col_rename = "Ra")

capm <- evaluate_portfolio_capm(portfolio_ret, bench_data)
cat("\nCAPM metrics:\n")
print(capm)

growth <- stock_data %>%
  tq_portfolio(assets_col = symbol, returns_col = Ra,
               weights = weights, col_rename = "investment.growth",
               wealth.index = TRUE) %>%
  mutate(investment.growth = investment.growth * 10000)

plot_portfolio_growth(growth, title = "Random Portfolio — $10K Growth")
