#' @title Internal Utilities
#' @description Shared helper functions used across modules. Not exported.
#' @name utils
#' @keywords internal
NULL

#' Safely require a package, with an informative install message
#' @param pkg Package name (character).
#' @keywords internal
.require_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      "Package '", pkg, "' is required but not installed.\n",
      "Install with: install.packages('", pkg, "')",
      call. = FALSE
    )
  }
}

#' Validate that a data.frame contains required columns
#' @param df A data.frame.
#' @param required Character vector of required column names.
#' @param caller Optional caller name for clearer error messages.
#' @keywords internal
.assert_columns <- function(df, required, caller = NULL) {
  if (!is.data.frame(df)) {
    prefix <- if (!is.null(caller)) paste0(caller, ": ") else ""
    stop(prefix, "Expected a data.frame, got ", class(df)[1], call. = FALSE)
  }
  missing <- setdiff(required, names(df))
  if (length(missing) > 0L) {
    prefix <- if (!is.null(caller)) paste0(caller, ": ") else ""
    stop(prefix, "Missing required columns: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(df)
}

#' Validate a numeric argument falls within a range
#' @param x Numeric scalar.
#' @param name Argument name for error messages.
#' @param lower Lower bound (inclusive).
#' @param upper Upper bound (exclusive by default).
#' @keywords internal
.assert_in_range <- function(x, name, lower = -Inf, upper = Inf) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x)) {
    stop(name, " must be a single numeric value", call. = FALSE)
  }
  if (x < lower || x >= upper) {
    stop(name, " must be in [", lower, ", ", upper, ")", call. = FALSE)
  }
  invisible(x)
}
