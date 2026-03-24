#' @title Portfolio Optimization Module
#' @description Random portfolio generation, CAPM evaluation, and growth plots
#'   using tidyquant.
#' @name portfolio
NULL

#' Build a random portfolio from a vector of tickers
#'
#' Generates random integer weights, zeros-out a fraction, then normalises to
#' sum to 1.
#'
#' @param tickers Character vector of ticker symbols.
#' @param zero_fraction Fraction of tickers to zero-out (for sparsity). Default `0.72`.
#' @param seed Random seed.
#' @return A tibble with columns `symbols` and `weights`.
#' @export
build_random_portfolio <- function(tickers, zero_fraction = 0.72, seed = 301L) {
  set.seed(seed)
  n <- length(tickers)
  w <- sample(1:100, n, replace = TRUE)
  zero_idx <- sample(seq_len(n), size = round(n * zero_fraction))
  w[zero_idx] <- 0
  if (sum(w) == 0) w[sample(seq_len(n), 1)] <- 1
  tibble::tibble(symbols = tickers, weights = w / sum(w))
}

#' Evaluate a portfolio against a benchmark using CAPM metrics
#'
#' @param portfolio_returns A data.frame with columns `date` and `Ra`
#'   (portfolio returns).
#' @param benchmark_returns A data.frame with columns `date` and `Rb`
#'   (benchmark returns).
#' @return A tibble of CAPM statistics from
#'   \code{PerformanceAnalytics::table.CAPM}.
#' @export
evaluate_portfolio_capm <- function(portfolio_returns, benchmark_returns) {
  merged <- dplyr::left_join(portfolio_returns, benchmark_returns, by = "date")
  merged %>%
    tidyquant::tq_performance(Ra = Ra, Rb = Rb,
                              performance_fun = PerformanceAnalytics::table.CAPM)
}

#' Plot portfolio investment growth over time
#'
#' @param growth_df A data.frame with columns `date` and `investment.growth`.
#' @param title Plot title.
#' @return A ggplot object.
#' @export
plot_portfolio_growth <- function(growth_df, title = "Portfolio Growth") {
  ggplot2::ggplot(growth_df, ggplot2::aes(x = date, y = investment.growth)) +
    ggplot2::geom_line(linewidth = 1.2, colour = "#2C3E50") +
    ggplot2::geom_smooth(method = "loess", se = FALSE, colour = "#E74C3C") +
    ggplot2::labs(title = title, x = NULL, y = "Portfolio Value ($)") +
    ggplot2::scale_y_continuous(labels = scales::dollar) +
    ggplot2::theme_minimal(base_size = 14)
}
