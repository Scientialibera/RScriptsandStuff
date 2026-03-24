#' @title Regression Module
#' @description Multiple linear regression with diagnostics and ggplot2
#'   visualisation helpers.
#' @name regression
#' @importFrom stats lm cor
NULL

#' Run a multiple linear regression
#'
#' Fits a linear model via \code{\link[stats]{lm}}, returns the model,
#' summary, and a correlation matrix of all numeric columns.
#'
#' @param data A data.frame.
#' @param formula A formula (e.g. \code{Petal.Width ~ .}).
#' @return A list:
#'   \describe{
#'     \item{model}{\code{lm} object.}
#'     \item{summary}{\code{summary.lm} object.}
#'     \item{correlation_matrix}{Numeric correlation matrix.}
#'   }
#' @export
#' @examples
#' result <- run_multiple_regression(iris, Petal.Width ~ Sepal.Length + Sepal.Width)
#' print(result$summary)
run_multiple_regression <- function(data, formula) {
  stopifnot(
    "data must be a data.frame" = is.data.frame(data),
    "formula must be a formula" = inherits(formula, "formula"),
    "data must have at least 3 rows" = nrow(data) >= 3L
  )

  numerics <- data[, vapply(data, is.numeric, logical(1)), drop = FALSE]
  if (ncol(numerics) < 2L) {
    stop("Need at least 2 numeric columns for a correlation matrix", call. = FALSE)
  }

  model <- stats::lm(formula, data = data)
  list(
    model = model,
    summary = summary(model),
    correlation_matrix = stats::cor(numerics)
  )
}

#' Scatter plot with linear regression trend by group
#'
#' @param data A data.frame.
#' @param x Unquoted column name for x axis.
#' @param y Unquoted column name for y axis.
#' @param colour Unquoted column name for colour grouping (optional).
#' @param facet Logical; if \code{TRUE}, facet by colour variable.
#' @param title Plot title.
#' @return A \code{ggplot} object.
#' @export
#' @examples
#' plot_regression(iris, Sepal.Length, Petal.Width, colour = Species, facet = TRUE)
plot_regression <- function(data, x, y, colour = NULL,
                            facet = FALSE, title = "Scatter Plot") {
  stopifnot("data must be a data.frame" = is.data.frame(data))

  p <- ggplot2::ggplot(data, ggplot2::aes(
    x = {{ x }}, y = {{ y }}, colour = {{ colour }}
  )) +
    ggplot2::geom_point(alpha = 0.6, size = 2) +
    ggplot2::geom_smooth(method = "lm", se = TRUE, linewidth = 1) +
    ggplot2::labs(title = title) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      legend.position = "bottom"
    )

  if (facet) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars({{ colour }}))
  }
  p
}
