# RScriptsandStuff

[![R-CMD-check](https://github.com/Scientialibera/RScriptsandStuff/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Scientialibera/RScriptsandStuff/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)

Enterprise-grade R package that consolidates machine learning classifiers, quantitative finance tools, and NLP utilities into reusable, tested, documented functions.

## Package Structure

```
RScriptsandStuff/
├── R/                          # Modular function library
│   ├── classification.R        # KNN & XGBoost with cross-validation
│   ├── deep_learning.R         # Configurable CNN builder (Keras)
│   ├── finance.R               # Stock data, log returns, screening, ARIMA
│   ├── portfolio.R             # Random portfolios, CAPM, growth plots
│   ├── nlp.R                   # Spam detection, sentiment analysis
│   └── regression.R            # Multiple regression & scatter plots
├── tests/testthat/             # Unit tests (testthat v3)
├── inst/examples/              # Runnable example scripts
├── man/                        # Auto-generated roxygen2 docs
├── .github/workflows/          # CI: R-CMD-check on 4 platforms
├── DESCRIPTION                 # Package metadata & dependencies
├── NAMESPACE                   # Exported functions
└── LICENSE                     # MIT
```

## Modules

### Classification (`R/classification.R`)

| Function | Description |
|---|---|
| `train_knn_cv()` | KNN with repeated k-fold cross-validation |
| `predict_knn_cv()` | Predict + confusion matrix + accuracy |
| `train_xgboost_cv()` | XGBoost with configurable hyperparameters |
| `predict_xgboost_cv()` | Threshold-based binary prediction |
| `classification_metrics()` | Accuracy, precision, sensitivity, specificity |

### Deep Learning (`R/deep_learning.R`)

| Function | Description |
|---|---|
| `build_cnn_model()` | Configurable CNN (conv layers, dropout, dense) |

### Quantitative Finance (`R/finance.R`)

| Function | Description |
|---|---|
| `fetch_stock_prices()` | Download from Yahoo Finance via quantmod |
| `compute_log_returns()` | Log-differenced returns |
| `screen_stocks_by_return()` | Filter by top-quantile mean return |
| `screen_stocks_by_risk()` | Filter by bottom-quantile volatility |
| `screen_stocks_by_cv()` | Filter by coefficient of variation |
| `fit_arima_forecast()` | Auto or manual ARIMA + forecast |

### Portfolio Optimization (`R/portfolio.R`)

| Function | Description |
|---|---|
| `build_random_portfolio()` | Random sparse weight generation |
| `evaluate_portfolio_capm()` | CAPM table vs benchmark |
| `plot_portfolio_growth()` | Investment growth ggplot |

### NLP (`R/nlp.R`)

| Function | Description |
|---|---|
| `build_spam_detector()` | DTM + freq-filter + Naive Bayes training |
| `predict_spam()` | Predict on held-out or new documents |
| `analyze_sentiment()` | Bing lexicon sentiment scoring |

### Regression (`R/regression.R`)

| Function | Description |
|---|---|
| `run_multiple_regression()` | lm + summary + correlation matrix |
| `plot_regression()` | Scatter + lm smooth, optional faceting |

## Installation

```r
# Install from GitHub
remotes::install_github("Scientialibera/RScriptsandStuff")

# Or clone and install locally
# git clone https://github.com/Scientialibera/RScriptsandStuff.git
# R CMD INSTALL RScriptsandStuff
```

## Quick Start

```r
library(RScriptsandStuff)

# --- Classification ---
model <- train_knn_cv(iris, Species ~ ., k_range = 1:15)
result <- predict_knn_cv(model, iris[1:10, ], "Species")

# --- Finance ---
prices <- fetch_stock_prices("AAPL", from = "2023-01-01", to = "2024-01-01")
fc <- fit_arima_forecast(prices[, "AAPL.Adjusted"], h = 30)
plot(fc)

# --- NLP ---
sentiment <- analyze_sentiment(c("Great product!", "Terrible service."))

# --- Portfolio ---
weights <- build_random_portfolio(c("AAPL", "MSFT", "GOOG"), zero_fraction = 0.3)

# --- Regression ---
result <- run_multiple_regression(iris, Petal.Width ~ Sepal.Length + Sepal.Width)
plot_regression(iris, Sepal.Length, Petal.Width, colour = Species)
```

## Running Examples

Full worked examples are in `inst/examples/`:

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
devtools::test()
```

Tests cover all modules using `testthat` v3. CI runs on Ubuntu, Windows, and macOS across R release and devel.

## Original Scripts

The original flat scripts are preserved in the repo root for reference:

| Script | Now in module |
|---|---|
| `Cross_Validation_Knn.R` | `R/classification.R` |
| `Pima_Exercise_1.R` | `R/classification.R` |
| `Deep_Learning_Convolutional.R` | `R/deep_learning.R` |
| `Chaotic_Learning.R` | `R/finance.R` |
| `Time_Series.R` | `R/finance.R` |
| `Quantitative_Finance.R` | `R/finance.R` |
| `Financial_Feature_Engineering.R` | `R/finance.R` |
| `Portfolio_Optimization_TSX.R` | `R/portfolio.R` |
| `Spam_Detector.R` | `R/nlp.R` |
| `Single_Sentiment_Analysis.R` | `R/nlp.R` |
| `R_Iris_Multiple_Regression.R` | `R/regression.R` |

## Disclaimer

The quantitative finance modules are for **educational purposes only** and do not constitute investment advice.

## License

MIT -- see [LICENSE](LICENSE).
