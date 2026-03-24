test_that(".assert_columns passes with correct columns", {
  df <- data.frame(a = 1, b = 2, c = 3)
  expect_invisible(RScriptsandStuff:::.assert_columns(df, c("a", "b")))
})

test_that(".assert_columns fails on missing columns", {
  df <- data.frame(a = 1)
  expect_error(
    RScriptsandStuff:::.assert_columns(df, c("a", "z"), caller = "test_fn"),
    "test_fn: Missing required columns: z"
  )
})

test_that(".assert_columns fails on non-data.frame", {
  expect_error(
    RScriptsandStuff:::.assert_columns(list(), "a"),
    "Expected a data.frame"
  )
})

test_that(".assert_in_range passes valid values", {
  expect_invisible(RScriptsandStuff:::.assert_in_range(0.5, "x", 0, 1))
})

test_that(".assert_in_range fails out-of-range", {
  expect_error(
    RScriptsandStuff:::.assert_in_range(1.5, "x", 0, 1),
    "x must be in"
  )
})

test_that(".assert_in_range fails on NA", {
  expect_error(
    RScriptsandStuff:::.assert_in_range(NA_real_, "x", 0, 1),
    "x must be a single numeric value"
  )
})

test_that(".require_pkg fails on missing package", {
  expect_error(
    RScriptsandStuff:::.require_pkg("nonexistent_pkg_12345"),
    "Package 'nonexistent_pkg_12345' is required"
  )
})
