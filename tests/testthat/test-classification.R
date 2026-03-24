test_that("train_knn_cv returns a caret model on iris", {
  data(iris)
  model <- train_knn_cv(
    data = iris,
    formula = Species ~ .,
    k_range = c(3, 5, 7),
    cv_folds = 3,
    cv_repeats = 1,
    seed = 42
  )
  expect_s3_class(model, "train")
  expect_equal(model$method, "knn")
})

test_that("train_knn_cv rejects bad inputs", {
  expect_error(train_knn_cv(data = "not_a_df", formula = y ~ x),
               "data must be a data.frame")
  expect_error(train_knn_cv(data = iris, formula = "not_a_formula"),
               "formula must be a formula")
  expect_error(train_knn_cv(data = iris, formula = Species ~ ., cv_folds = 1),
               "cv_folds must be >= 2")
  expect_error(train_knn_cv(data = iris[1, ], formula = Species ~ .),
               "data must have at least 2 rows")
})

test_that("predict_knn_cv returns accuracy between 0 and 1", {
  data(iris)
  set.seed(42)
  idx <- sample(nrow(iris), 120)
  model <- train_knn_cv(iris[idx, ], Species ~ ., k_range = 5,
                        cv_folds = 3, cv_repeats = 1)
  result <- predict_knn_cv(model, iris[-idx, ], "Species")
  expect_true(result$accuracy > 0.5)
  expect_true(result$accuracy <= 1.0)
  expect_s3_class(result$confusion_matrix, "confusionMatrix")
})

test_that("predict_knn_cv rejects missing truth_col", {
  data(iris)
  model <- train_knn_cv(iris, Species ~ ., k_range = 5,
                        cv_folds = 3, cv_repeats = 1)
  expect_error(predict_knn_cv(model, iris, "NonExistent"),
               "truth_col not found in newdata")
})

test_that("classification_metrics returns correct structure", {
  pred <- factor(c("a", "a", "b", "b"))
  act  <- factor(c("a", "b", "a", "b"))
  m <- classification_metrics(pred, act)
  expect_named(m, c("accuracy", "precision", "sensitivity", "specificity"))
  expect_true(all(sapply(m, is.numeric)))
})

test_that("classification_metrics computes perfect accuracy on trivial case", {
  pred <- factor(c("a", "a", "b", "b"))
  act  <- factor(c("a", "a", "b", "b"))
  m <- classification_metrics(pred, act)
  expect_equal(m$accuracy, 1.0)
  expect_equal(m$precision, 1.0)
  expect_equal(m$sensitivity, 1.0)
  expect_equal(m$specificity, 1.0)
})

test_that("classification_metrics rejects mismatched lengths", {
  expect_error(
    classification_metrics(factor(c("a")), factor(c("a", "b"))),
    "predicted and actual must have the same length"
  )
})

test_that("classification_metrics warns on empty input", {
  expect_warning(
    result <- classification_metrics(factor(character(0)), factor(character(0))),
    "Empty input"
  )
  expect_true(is.na(result$accuracy))
})
