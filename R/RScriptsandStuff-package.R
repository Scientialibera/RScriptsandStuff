#' @keywords internal
#' @aliases RScriptsandStuff-package
"_PACKAGE"

#' RScriptsandStuff: ML, Quant Finance & NLP Toolkit
#'
#' Enterprise-grade R package consolidating machine learning classifiers
#' (KNN, XGBoost, CNN), quantitative finance tools (portfolio optimisation,
#' CAPM screening, ARIMA forecasting), and NLP utilities (spam detection,
#' sentiment analysis) into reusable, tested, documented functions.
#'
#' @section Modules:
#' \describe{
#'   \item{Classification}{\code{\link{train_knn_cv}}, \code{\link{train_xgboost_cv}},
#'     \code{\link{classification_metrics}}}
#'   \item{Deep Learning}{\code{\link{build_cnn_model}}}
#'   \item{Finance}{\code{\link{fetch_stock_prices}}, \code{\link{compute_log_returns}},
#'     \code{\link{fit_arima_forecast}}}
#'   \item{Portfolio}{\code{\link{build_random_portfolio}},
#'     \code{\link{evaluate_portfolio_capm}}, \code{\link{plot_portfolio_growth}}}
#'   \item{NLP}{\code{\link{build_spam_detector}}, \code{\link{predict_spam}},
#'     \code{\link{analyze_sentiment}}}
#'   \item{Regression}{\code{\link{run_multiple_regression}},
#'     \code{\link{plot_regression}}}
#' }
#'
#' @docType package
#' @name RScriptsandStuff-package
NULL
