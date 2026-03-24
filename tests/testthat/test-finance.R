test_that("compute_log_returns produces correct length and values", {
  prices <- c(100, 110, 121, 133.1)
  lr <- compute_log_returns(prices)
  expect_length(lr, 3)
  expect_true(all(is.finite(lr)))
  expect_equal(lr[1], log(110 / 100), tolerance = 1e-10)
})

test_that("compute_log_returns rejects non-positive prices", {
  expect_error(compute_log_returns(c(100, -10, 50)),
               "all prices must be positive")
})

test_that("compute_log_returns rejects too-short input", {
  expect_error(compute_log_returns(c(100)),
               "prices must have > lag elements")
})

test_that("screen_stocks_by_return filters correctly", {
  df <- data.frame(
    ticker = c("A", "B", "C", "D"),
    MeanLogReturns = c(0.01, 0.05, 0.10, 0.20)
  )
  result <- screen_stocks_by_return(df, 0.75)
  expect_true(nrow(result) <= nrow(df))
  expect_true(all(result$MeanLogReturns > quantile(df$MeanLogReturns, 0.75)))
})

test_that("screen_stocks_by_return rejects missing columns", {
  df <- data.frame(ticker = "A", WrongCol = 0.1)
  expect_error(screen_stocks_by_return(df), "Missing required columns")
})

test_that("screen_stocks_by_return rejects non-data.frame", {
  expect_error(screen_stocks_by_return(list()), "Input must be a data.frame")
})

test_that("screen_stocks_by_risk filters correctly", {
  df <- data.frame(
    ticker = c("A", "B", "C", "D"),
    StdLogReturns = c(0.01, 0.02, 0.05, 0.10)
  )
  result <- screen_stocks_by_risk(df, 0.25)
  expect_true(nrow(result) >= 1)
})

test_that("screen_stocks_by_cv warns when all returns are negative", {
  df <- data.frame(
    ticker = c("A", "B"),
    MeanLogReturns = c(-0.1, -0.2),
    StdLogReturns = c(0.01, 0.02)
  )
  expect_warning(result <- screen_stocks_by_cv(df),
                 "No tickers with positive MeanLogReturns")
  expect_equal(nrow(result), 0)
})

test_that("fit_arima_forecast returns a forecast object", {
  ts_data <- ts(cumsum(rnorm(100)), frequency = 12)
  fc <- fit_arima_forecast(ts_data, h = 12)
  expect_s3_class(fc, "forecast")
  expect_length(fc$mean, 12)
})

test_that("fit_arima_forecast with explicit order works", {
  ts_data <- ts(cumsum(rnorm(100)), frequency = 12)
  fc <- fit_arima_forecast(ts_data, h = 5, order = c(1, 1, 0))
  expect_s3_class(fc, "forecast")
  expect_length(fc$mean, 5)
})

test_that("fit_arima_forecast rejects bad h", {
  ts_data <- ts(rnorm(50), frequency = 12)
  expect_error(fit_arima_forecast(ts_data, h = 0), "h must be a positive integer")
})

test_that("fit_arima_forecast rejects bad order length", {
  ts_data <- ts(rnorm(50), frequency = 12)
  expect_error(fit_arima_forecast(ts_data, order = c(1, 1)),
               "order must be length 3")
})

test_that("fetch_stock_prices rejects date range errors", {
  expect_error(fetch_stock_prices("AAPL", "2024-01-01", "2023-01-01"),
               "from must precede to")
})
