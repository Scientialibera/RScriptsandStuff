#' @title Classification Module
#' @description Reusable KNN and XGBoost classification with cross-validation.
#' @name classification
NULL

#' Train a KNN classifier with repeated cross-validation
#'
#' @param data A data.frame with features and a response column.
#' @param formula A formula (e.g. `Species ~ .`).
#' @param k_range Integer vector of k values to search. Default `1:20`.
#' @param cv_folds Number of CV folds. Default `10`.
#' @param cv_repeats Number of CV repeats. Default `5`.
#' @param preprocess Character vector passed to \code{caret::train}. Default
#'   `c("zv", "center", "scale")`.
#' @param seed Random seed for reproducibility. Default `125`.
#' @return A \code{caret::train} object.
#' @export
train_knn_cv <- function(data,
                         formula,
                         k_range = 1:20,
                         cv_folds = 10L,
                         cv_repeats = 5L,
                         preprocess = c("zv", "center", "scale"),
                         seed = 125L) {
  set.seed(seed)
  grid <- expand.grid(k = k_range)
  ctrl <- caret::trainControl(
    method = "repeatedcv",
    number = cv_folds,
    repeats = cv_repeats,
    classProbs = TRUE
  )
  caret::train(
    formula,
    data = data,
    method = "knn",
    preProcess = preprocess,
    trControl = ctrl,
    tuneGrid = grid,
    metric = "Accuracy"
  )
}

#' Predict and evaluate a trained KNN model
#'
#' @param model A trained \code{caret} model.
#' @param newdata Test data.frame.
#' @param truth_col Name of the response column (character).
#' @return A list with `predictions`, `confusion_matrix`, and `accuracy`.
#' @export
predict_knn_cv <- function(model, newdata, truth_col) {
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
#' @param y Numeric vector of labels (0/1 for binary).
#' @param params Named list of XGBoost hyperparameters. Defaults provided.
#' @param cv_folds Number of CV folds. Default `10`.
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
  set.seed(seed)
  grid <- expand.grid(params)
  ctrl <- caret::trainControl(
    method = "cv",
    number = cv_folds,
    classProbs = TRUE
  )
  caret::train(
    x = x,
    y = y,
    method = "xgbTree",
    preProcess = preprocess,
    trControl = ctrl,
    tuneGrid = grid,
    verbose = FALSE
  )
}

#' Predict with a trained XGBoost model
#'
#' @param model A caret-trained XGBoost model.
#' @param newdata Numeric matrix of test features.
#' @param threshold Decision threshold for binary classification. Default `0.5`.
#' @return Numeric vector of predicted classes (0/1).
#' @export
predict_xgboost_cv <- function(model, newdata, threshold = 0.5) {
  raw <- predict(model, newdata)
  as.numeric(raw > threshold)
}

#' Compute classification performance metrics
#'
#' @param predicted Factor or numeric vector of predictions.
#' @param actual Factor or numeric vector of ground truth.
#' @return A named list: accuracy, precision, sensitivity, specificity.
#' @export
classification_metrics <- function(predicted, actual) {
  ct <- table(Pred = predicted, Actual = actual)
  tp <- ct[1, 1]
  fp <- if (nrow(ct) > 1) ct[2, 1] else 0
  fn <- if (ncol(ct) > 1) ct[1, 2] else 0
  tn <- if (nrow(ct) > 1 && ncol(ct) > 1) ct[2, 2] else 0

  list(
    accuracy    = (tp + tn) / (tp + fp + fn + tn),
    precision   = tp / max(tp + fp, 1),
    sensitivity = tp / max(tp + fn, 1),
    specificity = tn / max(tn + fp, 1)
  )
}
