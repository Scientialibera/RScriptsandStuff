# RScriptsandStuff 1.0.0

## Breaking Changes

* All original flat scripts removed from repository root. Functionality is now
  available as documented, tested, exported functions under `R/`.

## New Features

* **Classification module** (`R/classification.R`): `train_knn_cv()`,
  `predict_knn_cv()`, `train_xgboost_cv()`, `predict_xgboost_cv()`,
  `classification_metrics()`.
* **Deep Learning module** (`R/deep_learning.R`): `build_cnn_model()` with
  configurable architecture (conv blocks, dropout, dense layers).
* **Finance module** (`R/finance.R`): `fetch_stock_prices()`,
  `compute_log_returns()`, `screen_stocks_by_return()`,
  `screen_stocks_by_risk()`, `screen_stocks_by_cv()`,
  `fit_arima_forecast()`.
* **Portfolio module** (`R/portfolio.R`): `build_random_portfolio()`,
  `evaluate_portfolio_capm()`, `plot_portfolio_growth()`.
* **NLP module** (`R/nlp.R`): `build_spam_detector()`, `predict_spam()`,
  `analyze_sentiment()`.
* **Regression module** (`R/regression.R`): `run_multiple_regression()`,
  `plot_regression()`.

## Infrastructure

* Proper R package structure: DESCRIPTION, NAMESPACE, LICENSE (MIT).
* roxygen2 documentation for all 20 exported functions.
* testthat v3 unit tests with edge-case and error-path coverage.
* 7 runnable example scripts under `inst/examples/`.
* GitHub Actions CI on Ubuntu, Windows, macOS (R release + devel) + coverage.
* Input validation (`stopifnot`) and `tryCatch` error handling throughout.
* Internal utility helpers (`R/utils.R`) for shared validation patterns.
* `NEWS.md`, `CONTRIBUTING.md`, `CODE_OF_CONDUCT.md`.
