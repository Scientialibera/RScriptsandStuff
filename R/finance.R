#' @title Quantitative Finance Module
#' @description Stock data retrieval, log-return computation, quantile-based
#'   screening filters, and ARIMA forecasting with full input validation.
#' @name finance
#' @importFrom stats na.omit quantile sd var
NULL

#' Fetch adjusted stock prices from Yahoo Finance
#'
#' Downloads one or more symbols via \code{\link[quantmod]{getSymbols}} into a
#' clean return object.
#'
#' @param symbols Character vector of ticker symbols (e.g. \code{"AAPL"}).
#' @param from Start date (character \code{"YYYY-MM-DD"} or Date).
#' @param to End date (character \code{"YYYY-MM-DD"} or Date).
#' @return An xts object (single symbol) or named list of xts objects.
#' @export
#' @examples
#' \dontrun{
#' prices <- fetch_stock_prices("AAPL", "2023-01-01", "2024-01-01")
#' }
fetch_stock_prices <- function(symbols, from, to) {
  stopifnot(
    "symbols must be a character vector" = is.character(symbols) && length(symbols) >= 1L,
    "from must be coercible to Date" = !is.na(as.Date(from)),
    "to must be coercible to Date"   = !is.na(as.Date(to)),
    "from must precede to" = as.Date(from) < as.Date(to)
  )

  envir <- new.env(parent = emptyenv())
  tryCatch(
    quantmod::getSymbols(symbols, src = "yahoo", from = from, to = to,
                         env = envir, auto.assign = TRUE),
    error = function(e) {
      stop("Failed to fetch symbols [", paste(symbols, collapse = ", "),
           "]: ", conditionMessage(e), call. = FALSE)
    }
  )
  result <- as.list(envir)
  if (length(result) == 1L) result[[1]] else result
}

#' Compute log returns for a price series
#'
#' @param prices Numeric vector, ts, or xts of prices.
#' @param lag Differencing lag. Default \code{1}.
#' @return Numeric vector of log returns (length \code{length(prices) - lag}).
#' @export
#' @examples
#' compute_log_returns(c(100, 110, 121))
compute_log_returns <- function(prices, lag = 1L) {
  p <- as.numeric(prices)
  stopifnot(
    "prices must have > lag elements" = length(p) > lag,
    "all prices must be positive" = all(p > 0, na.rm = TRUE)
  )
  lr <- diff(log(p), lag = lag)
  stats::na.omit(lr)
}

#' Screen stocks by mean log return (top quantile)
#'
#' @param ticker_table Data.frame with at least columns \code{ticker} and
#'   \code{MeanLogReturns}.
#' @param quantile_threshold Quantile cutoff. Default \code{0.75}.
#' @return Filtered data.frame.
#' @export
screen_stocks_by_return <- function(ticker_table, quantile_threshold = 0.75) {
  .validate_screen_input(ticker_table, "MeanLogReturns")
  stopifnot("quantile_threshold must be in (0,1)" =
              quantile_threshold > 0 && quantile_threshold < 1)
  cutoff <- stats::quantile(ticker_table$MeanLogReturns, quantile_threshold)
  ticker_table[ticker_table$MeanLogReturns > cutoff, , drop = FALSE]
}

#' Screen stocks by standard deviation of log returns (low risk)
#'
#' Keeps tickers whose absolute \code{StdLogReturns} falls below the given
#' quantile.
#'
#' @param ticker_table Data.frame with \code{ticker} and \code{StdLogReturns}.
#' @param quantile_threshold Quantile cutoff. Default \code{0.25}.
#' @return Filtered data.frame.
#' @export
screen_stocks_by_risk <- function(ticker_table, quantile_threshold = 0.25) {
  .validate_screen_input(ticker_table, "StdLogReturns")
  cutoff <- stats::quantile(abs(ticker_table$StdLogReturns), quantile_threshold)
  ticker_table[abs(ticker_table$StdLogReturns) < cutoff, , drop = FALSE]
}

#' Screen stocks by coefficient of variation (risk / reward)
#'
#' Keeps only tickers with positive mean returns whose CV is below the
#' specified quantile.
#'
#' @param ticker_table Data.frame with \code{MeanLogReturns} and
#'   \code{StdLogReturns}.
#' @param quantile_threshold CV quantile cutoff. Default \code{0.30}.
#' @return Filtered data.frame with an added \code{CV} column.
#' @export
screen_stocks_by_cv <- function(ticker_table, quantile_threshold = 0.30) {
  .validate_screen_input(ticker_table, c("MeanLogReturns", "StdLogReturns"))
  tt <- ticker_table[ticker_table$MeanLogReturns > 0, , drop = FALSE]
  if (nrow(tt) == 0L) {
    warning("No tickers with positive MeanLogReturns; returning empty frame")
    tt$CV <- numeric(0)
    return(tt)
  }
  tt$CV <- tt$StdLogReturns / tt$MeanLogReturns
  cutoff <- stats::quantile(tt$CV, quantile_threshold)
  tt[tt$CV < cutoff, , drop = FALSE]
}

#' Fit an ARIMA model and produce a forecast
#'
#' Delegates to \code{\link[forecast]{auto.arima}} when \code{order} is
#' \code{NULL}, otherwise uses \code{\link[forecast]{Arima}}.
#'
#' @param ts_data A \code{ts} or \code{xts} time series.
#' @param h Forecast horizon. Default \code{30}.
#' @param order Optional integer vector of length 3 \code{(p, d, q)}.
#' @param seasonal Optional integer vector of length 3 for the seasonal part.
#' @param xreg Optional external regressors (matrix).
#' @return A \code{\link[forecast]{forecast}} object.
#' @export
#' @examples
#' fc <- fit_arima_forecast(ts(cumsum(rnorm(100)), frequency = 12), h = 12)
#' plot(fc)
fit_arima_forecast <- function(ts_data, h = 30L,
                               order = NULL, seasonal = NULL,
                               xreg = NULL) {
  stopifnot("h must be a positive integer" = h >= 1L)
  if (!is.null(order)) {
    stopifnot("order must be length 3" = length(order) == 3L)
  }
  if (!is.null(seasonal)) {
    stopifnot("seasonal must be length 3" = length(seasonal) == 3L)
  }

  tryCatch({
    if (is.null(order)) {
      model <- forecast::auto.arima(ts_data, xreg = xreg)
    } else {
      s <- if (!is.null(seasonal)) list(order = seasonal) else list()
      model <- forecast::Arima(ts_data, order = order, seasonal = s)
    }
    forecast::forecast(model, h = h, xreg = xreg)
  }, error = function(e) {
    stop("ARIMA fitting/forecasting failed: ", conditionMessage(e), call. = FALSE)
  })
}

# ---- internal helpers ----

.validate_screen_input <- function(df, required_cols) {
  if (!is.data.frame(df)) stop("Input must be a data.frame", call. = FALSE)
  missing <- setdiff(required_cols, names(df))
  if (length(missing) > 0L) {
    stop("Missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
}
