test_that("build_random_portfolio produces valid weights", {
  tickers <- c("AAPL", "GOOG", "MSFT", "TSLA", "AMZN")
  pf <- build_random_portfolio(tickers, zero_fraction = 0.4, seed = 123)
  expect_equal(nrow(pf), length(tickers))
  expect_equal(sum(pf$weights), 1.0, tolerance = 1e-10)
  expect_true(all(pf$weights >= 0))
  expect_equal(names(pf), c("symbols", "weights"))
})

test_that("build_random_portfolio guarantees at least one non-zero weight", {
  pf <- build_random_portfolio(c("A"), zero_fraction = 0, seed = 1)
  expect_true(sum(pf$weights) == 1)
})

test_that("build_random_portfolio rejects bad inputs", {
  expect_error(build_random_portfolio(character(0)),
               "tickers must be a non-empty character vector")
  expect_error(build_random_portfolio(c("A"), zero_fraction = 1.0),
               "zero_fraction must be in")
  expect_error(build_random_portfolio(123),
               "tickers must be a non-empty character vector")
})

test_that("plot_portfolio_growth returns a ggplot", {
  df <- data.frame(
    date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 12),
    investment.growth = 10000 * cumprod(1 + rnorm(12, 0.01, 0.03))
  )
  p <- plot_portfolio_growth(df)
  expect_s3_class(p, "ggplot")
})

test_that("plot_portfolio_growth rejects missing columns", {
  df <- data.frame(date = Sys.Date(), wrong_col = 1)
  expect_error(plot_portfolio_growth(df),
               "growth_df must have 'date' and 'investment.growth'")
})

test_that("evaluate_portfolio_capm rejects bad inputs", {
  expect_error(
    evaluate_portfolio_capm(data.frame(date = 1, wrong = 1),
                            data.frame(date = 1, Rb = 1)),
    "portfolio_returns must have 'date' and 'Ra' columns"
  )
})
