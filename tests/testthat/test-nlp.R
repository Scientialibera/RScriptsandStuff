test_that("analyze_sentiment returns correct structure", {
  skip_if_not_installed("tidytext")
  texts <- c(
    "I love this amazing wonderful product",
    "This is terrible awful horrible experience"
  )
  result <- analyze_sentiment(texts)
  expect_true("sentiment" %in% names(result))
  expect_true(nrow(result) >= 1)
  expect_true(is.numeric(result$sentiment))
})

test_that("analyze_sentiment rejects non-character input", {
  expect_error(analyze_sentiment(123), "texts must be a character vector")
})

test_that("analyze_sentiment rejects empty input", {
  expect_error(analyze_sentiment(character(0)), "texts must not be empty")
})

test_that("build_spam_detector rejects mismatched lengths", {
  expect_error(
    build_spam_detector(c("hello", "world"), factor(c("ham"))),
    "texts and labels must have equal length"
  )
})

test_that("build_spam_detector rejects too few documents", {
  expect_error(
    build_spam_detector(c("a", "b"), factor(c("ham", "spam"))),
    "need at least 10 documents"
  )
})

test_that("build_spam_detector rejects non-factor labels", {
  texts <- rep("hello world", 20)
  expect_error(
    build_spam_detector(texts, rep("ham", 20)),
    "labels must be a factor"
  )
})

test_that("predict_spam rejects invalid detector", {
  expect_error(
    predict_spam(list(model = NULL)),
    "detector must be a list from build_spam_detector"
  )
})

test_that("classification_metrics works for spam-like output", {
  pred <- factor(c("ham", "ham", "spam", "spam", "ham"))
  act  <- factor(c("ham", "spam", "spam", "spam", "ham"))
  m <- classification_metrics(pred, act)
  expect_equal(m$accuracy, 4 / 5)
})
