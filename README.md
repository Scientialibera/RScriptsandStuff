# RScriptsandStuff

[![R-CMD-check](https://github.com/Scientialibera/RScriptsandStuff/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Scientialibera/RScriptsandStuff/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)

Enterprise-grade R package consolidating machine learning classifiers, quantitative finance tools, and NLP utilities into reusable, tested, documented functions with full input validation and structured error handling.

## Installation

```r
# From GitHub (recommended)
remotes::install_github("Scientialibera/RScriptsandStuff")

# Or clone + install locally
git clone https://github.com/Scientialibera/RScriptsandStuff.git
R CMD INSTALL RScriptsandStuff
```

## Package Structure

```
RScriptsandStuff/
├── R/
│   ├── RScriptsandStuff-package.R   # Package-level documentation
│   ├── classification.R             # KNN & XGBoost with cross-validation
│   ├── deep_learning.R              # Configurable CNN builder (Keras)
│   ├── finance.R                    # Stock data, log returns, screening, ARIMA
│   ├── portfolio.R                  # Random portfolios, CAPM, growth plots
│   ├── nlp.R                        # Spam detection, sentiment analysis
│   ├── regression.R                 # Multiple regression & scatter plots
│   ├── utils.R                      # Internal validation helpers
│   └── zzz.R                        # .onAttach startup message
├── tests/testthat/                  # Unit tests (testthat v3)
│   ├── test-classification.R
│   ├── test-finance.R
│   ├── test-portfolio.R
│   ├── test-nlp.R
│   ├── test-regression.R
│   └── test-utils.R
├── inst/
│   ├── examples/                    # 7 runnable example scripts
│   └── extdata/                     # Sample datasets (Iris_Example.xlsx)
├── man/                             # Auto-generated roxygen2 docs
├── .github/workflows/               # CI: R-CMD-check (4 platforms) + coverage
├── DESCRIPTION                      # Package metadata & versioned dependencies
├── NAMESPACE                        # 20 exported functions
├── LICENSE                          # MIT
├── NEWS.md                          # Changelog
├── CONTRIBUTING.md                  # Contribution guidelines
├── CODE_OF_CONDUCT.md               # Contributor Covenant
├── .Rbuildignore                    # R CMD build exclusions
├── .lintr                           # Lint configuration
└── .gitignore
```

## Modules & API Reference

### Classification (`R/classification.R`)

| Function | Description |
|---|---|
| `train_knn_cv(data, formula, ...)` | KNN with repeated k-fold cross-validation |
| `predict_knn_cv(model, newdata, truth_col)` | Predict + confusion matrix + accuracy |
| `train_xgboost_cv(x, y, ...)` | XGBoost with configurable hyperparameter grid |
| `predict_xgboost_cv(model, newdata, threshold)` | Threshold-based binary prediction |
| `classification_metrics(predicted, actual)` | Accuracy, precision, sensitivity, specificity |

### Deep Learning (`R/deep_learning.R`)

| Function | Description |
|---|---|
| `build_cnn_model(input_shape, num_classes, ...)` | Configurable CNN (conv blocks, dropout, dense, softmax) |

### Quantitative Finance (`R/finance.R`)

| Function | Description |
|---|---|
| `fetch_stock_prices(symbols, from, to)` | Download from Yahoo Finance via quantmod |
| `compute_log_returns(prices, lag)` | Log-differenced returns with validation |
| `screen_stocks_by_return(ticker_table, quantile_threshold)` | Filter by top-quantile mean return |
| `screen_stocks_by_risk(ticker_table, quantile_threshold)` | Filter by bottom-quantile volatility |
| `screen_stocks_by_cv(ticker_table, quantile_threshold)` | Filter by coefficient of variation |
| `fit_arima_forecast(ts_data, h, order, seasonal, xreg)` | Auto or manual ARIMA + forecast |

### Portfolio Optimization (`R/portfolio.R`)

| Function | Description |
|---|---|
| `build_random_portfolio(tickers, zero_fraction, seed)` | Random sparse weight generation |
| `evaluate_portfolio_capm(portfolio_returns, benchmark_returns)` | CAPM table vs benchmark |
| `plot_portfolio_growth(growth_df, title)` | Investment growth ggplot |

### NLP (`R/nlp.R`)

| Function | Description |
|---|---|
| `build_spam_detector(texts, labels, ...)` | DTM + freq-filter + Naive Bayes training |
| `predict_spam(detector, newdtm)` | Predict on held-out or new documents |
| `analyze_sentiment(texts)` | Bing lexicon per-document sentiment scoring |

### Regression (`R/regression.R`)

| Function | Description |
|---|---|
| `run_multiple_regression(data, formula)` | lm + summary + correlation matrix |
| `plot_regression(data, x, y, colour, facet)` | Scatter + lm smooth, optional faceting |

## Quick Start

```r
library(RScriptsandStuff)

# --- Classification: KNN on Iris ---
model <- train_knn_cv(iris, Species ~ ., k_range = 1:15)
result <- predict_knn_cv(model, iris[1:10, ], "Species")
print(result$accuracy)

# --- Finance: ARIMA Forecast ---
prices <- fetch_stock_prices("AAPL", from = "2023-01-01", to = "2024-01-01")
fc <- fit_arima_forecast(prices[, "AAPL.Adjusted"], h = 30)
plot(fc)

# --- Finance: Stock Screening ---
log_rets <- compute_log_returns(c(100, 110, 121, 115, 130))

# --- Portfolio: Random Allocation ---
weights <- build_random_portfolio(c("AAPL", "MSFT", "GOOG"), zero_fraction = 0.3)
print(weights)

# --- NLP: Sentiment Analysis ---
sentiment <- analyze_sentiment(c("Great earnings report!", "Market crash fears."))
print(sentiment)

# --- Regression: Iris ---
result <- run_multiple_regression(iris, Petal.Width ~ Sepal.Length + Sepal.Width)
plot_regression(iris, Sepal.Length, Petal.Width, colour = Species, facet = TRUE)
```

## Running Examples

Full worked examples live in `inst/examples/`:

```r
source(system.file("examples", "knn_iris.R", package = "RScriptsandStuff"))
source(system.file("examples", "arima_forecast.R", package = "RScriptsandStuff"))
source(system.file("examples", "portfolio_optimization.R", package = "RScriptsandStuff"))
source(system.file("examples", "spam_detection.R", package = "RScriptsandStuff"))
source(system.file("examples", "sentiment_analysis.R", package = "RScriptsandStuff"))
source(system.file("examples", "regression_iris.R", package = "RScriptsandStuff"))
source(system.file("examples", "cnn_fashion_mnist.R", package = "RScriptsandStuff"))
```

## Testing

```r
# Run all tests
devtools::test()

# With coverage report
covr::package_coverage()
```

Tests cover all 6 modules + internal utilities with happy-path, edge-case, and error-path assertions. CI runs on Ubuntu, Windows, and macOS across R release and devel.

## Enterprise Features

- **Input validation**: Every exported function validates arguments with `stopifnot()` and informative error messages.
- **Error handling**: External API calls (Yahoo Finance, ARIMA fitting) wrapped in `tryCatch()` with descriptive re-thrown errors.
- **Soft dependencies**: `keras`/`tensorflow` and `tidytext` are optional — functions fail gracefully with install instructions.
- **Internal utilities**: Shared validation helpers in `R/utils.R` prevent code duplication.
- **CI/CD**: GitHub Actions on 4 OS/R-version combos + automated code coverage.
- **Lint configuration**: `.lintr` enforces 120-char lines and snake_case naming.
- **Documentation**: Full roxygen2 docs with `@param`, `@return`, `@export`, `@examples`.
- **Changelog**: `NEWS.md` tracks breaking changes and new features per release.

## Disclaimer

The quantitative finance modules are for **educational purposes only** and do not constitute investment advice.

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md).

## License

MIT — see [LICENSE](LICENSE).
