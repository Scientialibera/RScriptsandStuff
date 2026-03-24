test_that("analyze_sentiment returns correct structure", {
  skip_if_not_installed("tidytext")
  texts <- c(
    "I love this amazing wonderful product",
    "This is terrible awful horrible experience"
  )
  result <- analyze_sentiment(texts)
  expect_true("sentiment" %in% names(result))
  expect_true(nrow(result) >= 1)
})

test_that("classification_metrics works for spam-like output", {
  pred <- factor(c("ham", "ham", "spam", "spam", "ham"))
  act  <- factor(c("ham", "spam", "spam", "spam", "ham"))
  m <- classification_metrics(pred, act)
  expect_equal(m$accuracy, 4 / 5)
})
