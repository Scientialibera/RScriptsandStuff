#' @title Quantitative Finance Module
#' @description Stock data retrieval, log-return computation, screening filters,
#'   and ARIMA forecasting.
#' @name finance
NULL

#' Fetch adjusted stock prices from Yahoo Finance
#'
#' @param symbols Character vector of ticker symbols.
#' @param from Start date (character or Date).
#' @param to End date (character or Date).
#' @return An xts object (single symbol) or named list of xts objects.
#' @export
fetch_stock_prices <- function(symbols, from, to) {
  envir <- new.env(parent = emptyenv())
  quantmod::getSymbols(symbols, src = "yahoo", from = from, to = to, env = envir)
  result <- as.list(envir)
  if (length(result) == 1L) result[[1]] else result
}

#' Compute log returns for a price series
#'
#' @param prices Numeric vector or xts of adjusted prices.
#' @param lag Differencing lag (default 1).
#' @return Numeric vector of log returns (length = `length(prices) - lag`).
#' @export
compute_log_returns <- function(prices, lag = 1L) {
  lr <- diff(log(prices), lag = lag)
  na.omit(lr)
}

#' Screen stocks by mean log return (top quantile)
#'
#' @param ticker_table A data.frame with columns `ticker` and `MeanLogReturns`.
#' @param quantile_threshold Quantile cutoff (default 0.75).
#' @return Filtered data.frame of tickers above the threshold.
#' @export
screen_stocks_by_return <- function(ticker_table, quantile_threshold = 0.75) {
  cutoff <- quantile(ticker_table$MeanLogReturns, quantile_threshold)
  ticker_table[ticker_table$MeanLogReturns > cutoff, , drop = FALSE]
}

#' Screen stocks by standard deviation of log returns (bottom quantile = low risk)
#'
#' @param ticker_table A data.frame with columns `ticker` and `StdLogReturns`.
#' @param quantile_threshold Quantile cutoff (default 0.25).
#' @return Filtered data.frame.
#' @export
screen_stocks_by_risk <- function(ticker_table, quantile_threshold = 0.25) {
  cutoff <- quantile(abs(ticker_table$StdLogReturns), quantile_threshold)
  ticker_table[abs(ticker_table$StdLogReturns) < cutoff, , drop = FALSE]
}

#' Screen stocks by coefficient of variation (risk/reward)
#'
#' Keeps only stocks with positive mean returns and CV below the given quantile.
#'
#' @param ticker_table Data.frame with `MeanLogReturns`, `StdLogReturns`.
#' @param quantile_threshold Quantile cutoff for CV (default 0.30).
#' @return Filtered data.frame.
#' @export
screen_stocks_by_cv <- function(ticker_table, quantile_threshold = 0.30) {
  tt <- ticker_table[ticker_table$MeanLogReturns > 0, , drop = FALSE]
  tt$CV <- tt$StdLogReturns / tt$MeanLogReturns
  cutoff <- quantile(tt$CV, quantile_threshold)
  tt[tt$CV < cutoff, , drop = FALSE]
}

#' Fit an ARIMA model and produce a forecast
#'
#' Uses \code{forecast::auto.arima} when `order` is NULL, otherwise
#' \code{forecast::Arima} with the given order.
#'
#' @param ts_data A ts or xts time series object.
#' @param h Forecast horizon (number of periods ahead). Default `30`.
#' @param order Optional numeric vector of length 3 (p,d,q).
#' @param seasonal Optional numeric vector of length 3 for seasonal component.
#' @param xreg Optional external regressors.
#' @return A \code{forecast} object.
#' @export
fit_arima_forecast <- function(ts_data, h = 30L,
                               order = NULL, seasonal = NULL,
                               xreg = NULL) {
  if (is.null(order)) {
    model <- forecast::auto.arima(ts_data, xreg = xreg)
  } else {
    s <- if (!is.null(seasonal)) list(order = seasonal) else list()
    model <- forecast::Arima(ts_data, order = order, seasonal = s)
  }
  forecast::forecast(model, h = h, xreg = xreg)
}
