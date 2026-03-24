test_that("compute_log_returns produces correct length and values", {
  prices <- c(100, 110, 121, 133.1)
  lr <- compute_log_returns(prices)
  expect_length(lr, 3)
  expect_true(all(is.finite(lr)))
  expect_true(abs(lr[1] - log(110 / 100)) < 1e-10)
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

test_that("screen_stocks_by_risk filters correctly", {
  df <- data.frame(
    ticker = c("A", "B", "C", "D"),
    StdLogReturns = c(0.01, 0.02, 0.05, 0.10)
  )
  result <- screen_stocks_by_risk(df, 0.25)
  expect_true(nrow(result) >= 1)
})

test_that("fit_arima_forecast returns a forecast object", {
  ts_data <- ts(cumsum(rnorm(100)), frequency = 12)
  fc <- fit_arima_forecast(ts_data, h = 12)
  expect_s3_class(fc, "forecast")
  expect_length(fc$mean, 12)
})
