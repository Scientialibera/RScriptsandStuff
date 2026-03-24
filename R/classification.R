#' @title Classification Module
#' @description Reusable KNN and XGBoost classification with cross-validation,
#'   input validation, and structured error handling.
#' @name classification
#' @importFrom stats predict quantile
NULL

#' Train a KNN classifier with repeated cross-validation
#'
#' Wraps \code{\link[caret]{train}} with sensible defaults for KNN,
#' including pre-processing, hyper-parameter grid, and repeated CV.
#'
#' @param data A data.frame with at least two columns (features + response).
#' @param formula A formula (e.g. \code{Species ~ .}).
#' @param k_range Integer vector of k values to search. Default \code{1:20}.
#' @param cv_folds Number of CV folds. Default \code{10}.
#' @param cv_repeats Number of CV repeats. Default \code{5}.
#' @param preprocess Character vector passed to \code{caret::train}.
#'   Default \code{c("zv", "center", "scale")}.
#' @param seed Random seed for reproducibility. Default \code{125}.
#' @return A \code{caret::train} object.
#' @export
#' @examples
#' \dontrun{
#' model <- train_knn_cv(iris, Species ~ ., k_range = c(3, 5, 7))
#' }
train_knn_cv <- function(data,
                         formula,
                         k_range = 1:20,
                         cv_folds = 10L,
                         cv_repeats = 5L,
                         preprocess = c("zv", "center", "scale"),
                         seed = 125L) {
  stopifnot(
    "data must be a data.frame" = is.data.frame(data),
    "formula must be a formula" = inherits(formula, "formula"),
    "k_range must be a positive integer vector" = is.numeric(k_range) && all(k_range > 0),
    "cv_folds must be >= 2" = cv_folds >= 2L,
    "cv_repeats must be >= 1" = cv_repeats >= 1L,
    "data must have at least 2 rows" = nrow(data) >= 2L
  )

  set.seed(seed)
  grid <- expand.grid(k = as.integer(k_range))
  ctrl <- caret::trainControl(
    method = "repeatedcv",
    number = cv_folds,
    repeats = cv_repeats,
    classProbs = TRUE
  )

  tryCatch(
    caret::train(
      formula,
      data = data,
      method = "knn",
      preProcess = preprocess,
      trControl = ctrl,
      tuneGrid = grid,
      metric = "Accuracy"
    ),
    error = function(e) {
      stop("KNN training failed: ", conditionMessage(e), call. = FALSE)
    }
  )
}

#' Predict and evaluate a trained KNN model
#'
#' @param model A trained \code{caret} model.
#' @param newdata Test data.frame.
#' @param truth_col Name of the response column (character scalar).
#' @return A list: \code{predictions}, \code{confusion_matrix}, \code{accuracy}.
#' @export
predict_knn_cv <- function(model, newdata, truth_col) {
  stopifnot(
    "model must be a caret train object" = inherits(model, "train"),
    "newdata must be a data.frame" = is.data.frame(newdata),
    "truth_col must be a single character string" = is.character(truth_col) && length(truth_col) == 1L,
    "truth_col not found in newdata" = truth_col %in% names(newdata)
  )

  preds <- predict(model, newdata)
  truth <- newdata[[truth_col]]
  cm <- caret::confusionMatrix(preds, truth)
  list(
    predictions = preds,
    confusion_matrix = cm,
    accuracy = cm$overall["Accuracy"]
  )
}

#' Train an XGBoost classifier with cross-validation
#'
#' @param x Numeric matrix of training features.
#' @param y Numeric vector of labels (0/1 for binary) or factor for multi-class.
#' @param params Named list of XGBoost hyperparameters. Sensible defaults
#'   provided.
#' @param cv_folds Number of CV folds. Default \code{10}.
#' @param preprocess Character vector for caret preprocessing.
#' @param seed Random seed.
#' @return A \code{caret::train} object.
#' @export
train_xgboost_cv <- function(x, y,
                             params = list(
                               eta = 0.08,
                               nrounds = 5000L,
                               max_depth = 30L,
                               min_child_weight = 1L,
                               subsample = 0.5,
                               colsample_bytree = 0.5,
                               gamma = 1
                             ),
                             cv_folds = 10L,
                             preprocess = c("center", "scale"),
                             seed = 125L) {
  stopifnot(
    "x must be a matrix" = is.matrix(x),
    "y must be numeric or factor" = is.numeric(y) || is.factor(y),
    "nrow(x) must equal length(y)" = nrow(x) == length(y),
    "cv_folds must be >= 2" = cv_folds >= 2L
  )

  set.seed(seed)
  grid <- expand.grid(params)
  ctrl <- caret::trainControl(
    method = "cv",
    number = cv_folds,
    classProbs = TRUE
  )

  tryCatch(
    caret::train(
      x = x,
      y = y,
      method = "xgbTree",
      preProcess = preprocess,
      trControl = ctrl,
      tuneGrid = grid,
      verbose = FALSE
    ),
    error = function(e) {
      stop("XGBoost training failed: ", conditionMessage(e), call. = FALSE)
    }
  )
}

#' Predict with a trained XGBoost model
#'
#' @param model A caret-trained XGBoost model.
#' @param newdata Numeric matrix of test features.
#' @param threshold Decision threshold for binary classification. Default \code{0.5}.
#' @return Numeric vector of predicted classes (0/1).
#' @export
predict_xgboost_cv <- function(model, newdata, threshold = 0.5) {
  stopifnot(
    "model must be a caret train object" = inherits(model, "train"),
    "newdata must be a matrix" = is.matrix(newdata),
    "threshold must be between 0 and 1" = threshold >= 0 && threshold <= 1
  )
  raw <- predict(model, newdata)
  as.numeric(raw > threshold)
}

#' Compute classification performance metrics
#'
#' Computes accuracy, precision, sensitivity (recall), and specificity from
#' predicted vs actual vectors. Handles binary and multi-class via the
#' confusion matrix diagonal.
#'
#' @param predicted Factor or numeric vector of predictions.
#' @param actual Factor or numeric vector of ground truth.
#' @return A named list: accuracy, precision, sensitivity, specificity.
#' @export
#' @examples
#' classification_metrics(factor(c("a","b","a")), factor(c("a","a","a")))
classification_metrics <- function(predicted, actual) {
  stopifnot(
    "predicted and actual must have the same length" = length(predicted) == length(actual)
  )
  if (length(predicted) == 0L) {
    warning("Empty input; returning NAs")
    return(list(accuracy = NA_real_, precision = NA_real_,
                sensitivity = NA_real_, specificity = NA_real_))
  }

  ct <- table(Pred = predicted, Actual = actual)
  tp <- ct[1, 1]
  fp <- if (nrow(ct) > 1) ct[2, 1] else 0
  fn <- if (ncol(ct) > 1) ct[1, 2] else 0
  tn <- if (nrow(ct) > 1 && ncol(ct) > 1) ct[2, 2] else 0

  list(
    accuracy    = (tp + tn) / max(tp + fp + fn + tn, 1),
    precision   = tp / max(tp + fp, 1),
    sensitivity = tp / max(tp + fn, 1),
    specificity = tn / max(tn + fp, 1)
  )
}
