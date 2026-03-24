test_that("build_random_portfolio produces valid weights", {
  tickers <- c("AAPL", "GOOG", "MSFT", "TSLA", "AMZN")
  pf <- build_random_portfolio(tickers, zero_fraction = 0.4, seed = 123)
  expect_equal(nrow(pf), length(tickers))
  expect_equal(sum(pf$weights), 1.0, tolerance = 1e-10)
  expect_true(all(pf$weights >= 0))
})

test_that("plot_portfolio_growth returns a ggplot", {
  df <- data.frame(
    date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 12),
    investment.growth = 10000 * cumprod(1 + rnorm(12, 0.01, 0.03))
  )
  p <- plot_portfolio_growth(df)
  expect_s3_class(p, "ggplot")
})
