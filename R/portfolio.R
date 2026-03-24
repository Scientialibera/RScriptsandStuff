#' @title Portfolio Optimization Module
#' @description Random portfolio generation, CAPM evaluation, and investment
#'   growth visualisation using tidyquant.
#' @name portfolio
NULL

#' Build a random sparse portfolio from a vector of tickers
#'
#' Generates random integer weights, zeros out a configurable fraction for
#' sparsity, then normalises to sum to 1. Guarantees at least one non-zero
#' weight.
#'
#' @param tickers Character vector of ticker symbols.
#' @param zero_fraction Fraction of tickers to zero-out. Default \code{0.72}.
#'   Must be in \code{[0, 1)}.
#' @param seed Random seed.
#' @return A tibble with columns \code{symbols} and \code{weights} (sum to 1).
#' @export
#' @examples
#' build_random_portfolio(c("AAPL", "GOOG", "MSFT"), zero_fraction = 0.3)
build_random_portfolio <- function(tickers, zero_fraction = 0.72, seed = 301L) {
  stopifnot(
    "tickers must be a non-empty character vector" =
      is.character(tickers) && length(tickers) >= 1L,
    "zero_fraction must be in [0, 1)" = zero_fraction >= 0 && zero_fraction < 1
  )

  set.seed(seed)
  n <- length(tickers)
  w <- sample(1:100, n, replace = TRUE)

  n_zero <- round(n * zero_fraction)
  if (n_zero > 0L && n_zero < n) {
    zero_idx <- sample(seq_len(n), size = n_zero)
    w[zero_idx] <- 0
  }
  if (sum(w) == 0) w[sample(seq_len(n), 1)] <- 1

  tibble::tibble(symbols = tickers, weights = w / sum(w))
}

#' Evaluate a portfolio against a benchmark using CAPM metrics
#'
#' @param portfolio_returns Data.frame with columns \code{date} and \code{Ra}.
#' @param benchmark_returns Data.frame with columns \code{date} and \code{Rb}.
#' @return A tibble of CAPM statistics.
#' @export
evaluate_portfolio_capm <- function(portfolio_returns, benchmark_returns) {
  stopifnot(
    "portfolio_returns must have 'date' and 'Ra' columns" =
      all(c("date", "Ra") %in% names(portfolio_returns)),
    "benchmark_returns must have 'date' and 'Rb' columns" =
      all(c("date", "Rb") %in% names(benchmark_returns))
  )

  merged <- dplyr::left_join(portfolio_returns, benchmark_returns, by = "date")

  if (nrow(merged) == 0L) {
    stop("No overlapping dates between portfolio and benchmark", call. = FALSE)
  }

  merged %>%
    tidyquant::tq_performance(
      Ra = Ra, Rb = Rb,
      performance_fun = PerformanceAnalytics::table.CAPM
    )
}

#' Plot portfolio investment growth over time
#'
#' @param growth_df Data.frame with columns \code{date} and
#'   \code{investment.growth}.
#' @param title Plot title. Default \code{"Portfolio Growth"}.
#' @return A \code{ggplot} object.
#' @export
plot_portfolio_growth <- function(growth_df, title = "Portfolio Growth") {
  stopifnot(
    "growth_df must have 'date' and 'investment.growth'" =
      all(c("date", "investment.growth") %in% names(growth_df)),
    "growth_df must not be empty" = nrow(growth_df) >= 1L
  )

  ggplot2::ggplot(growth_df, ggplot2::aes(x = date, y = investment.growth)) +
    ggplot2::geom_line(linewidth = 1.2, colour = "#2C3E50") +
    ggplot2::geom_smooth(method = "loess", se = FALSE, colour = "#E74C3C") +
    ggplot2::labs(title = title, x = NULL, y = "Portfolio Value ($)") +
    ggplot2::scale_y_continuous(labels = scales::dollar) +
    ggplot2::theme_minimal(base_size = 14)
}
