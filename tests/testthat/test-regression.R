test_that("run_multiple_regression returns correct components", {
  data(iris)
  result <- run_multiple_regression(
    iris, Petal.Width ~ Petal.Length + Sepal.Length + Sepal.Width
  )
  expect_s3_class(result$model, "lm")
  expect_true(!is.null(result$summary))
  expect_true(is.matrix(result$correlation_matrix))
  expect_equal(ncol(result$correlation_matrix), 4)
})

test_that("run_multiple_regression rejects non-data.frame", {
  expect_error(run_multiple_regression("not_df", y ~ x),
               "data must be a data.frame")
})

test_that("run_multiple_regression rejects too few rows", {
  df <- data.frame(x = 1, y = 2)
  expect_error(run_multiple_regression(df, y ~ x),
               "data must have at least 3 rows")
})

test_that("run_multiple_regression rejects insufficient numeric cols", {
  df <- data.frame(a = letters[1:5], b = letters[6:10])
  expect_error(run_multiple_regression(df, a ~ b),
               "Need at least 2 numeric columns")
})

test_that("plot_regression returns a ggplot", {
  data(iris)
  p <- plot_regression(iris, Sepal.Length, Petal.Width,
                       colour = Species, title = "Test Plot")
  expect_s3_class(p, "ggplot")
})

test_that("plot_regression with facet returns a ggplot", {
  data(iris)
  p <- plot_regression(iris, Sepal.Length, Petal.Width,
                       colour = Species, facet = TRUE)
  expect_s3_class(p, "ggplot")
})

test_that("plot_regression rejects non-data.frame", {
  expect_error(plot_regression("bad", x, y), "data must be a data.frame")
})
