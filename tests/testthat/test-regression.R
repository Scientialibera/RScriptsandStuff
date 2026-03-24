test_that("run_multiple_regression returns correct components", {
  data(iris)
  result <- run_multiple_regression(iris, Petal.Width ~ Petal.Length + Sepal.Length + Sepal.Width)
  expect_s3_class(result$model, "lm")
  expect_true(!is.null(result$summary))
  expect_true(is.matrix(result$correlation_matrix))
})

test_that("plot_regression returns a ggplot", {
  data(iris)
  p <- plot_regression(iris, Sepal.Length, Petal.Width, colour = Species,
                       title = "Test Plot")
  expect_s3_class(p, "ggplot")
})
