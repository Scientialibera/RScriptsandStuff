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

test_that("predict_knn_cv returns accuracy between 0 and 1", {
  data(iris)
  set.seed(42)
  idx <- sample(nrow(iris), 120)
  model <- train_knn_cv(iris[idx, ], Species ~ ., k_range = 5, cv_folds = 3, cv_repeats = 1)
  result <- predict_knn_cv(model, iris[-idx, ], "Species")
  expect_true(result$accuracy > 0.5)
  expect_true(result$accuracy <= 1.0)
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
})
