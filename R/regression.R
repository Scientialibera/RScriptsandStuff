#' @title Regression Module
#' @description Multiple linear regression and visualisation helpers.
#' @name regression
NULL

#' Run a multiple linear regression
#'
#' @param data A data.frame.
#' @param formula A formula (e.g. `Petal.Width ~ .`).
#' @return A list with `model` (lm object), `summary`, and `correlation_matrix`.
#' @export
run_multiple_regression <- function(data, formula) {
  model <- lm(formula, data = data)
  numerics <- data[, sapply(data, is.numeric), drop = FALSE]
  list(
    model = model,
    summary = summary(model),
    correlation_matrix = cor(numerics)
  )
}

#' Scatter plot with linear regression by group
#'
#' @param data A data.frame.
#' @param x Unquoted column name for x axis.
#' @param y Unquoted column name for y axis.
#' @param colour Unquoted column name for colour grouping (optional).
#' @param facet Logical; if TRUE, facet by colour variable.
#' @param title Plot title.
#' @return A ggplot object.
#' @export
plot_regression <- function(data, x, y, colour = NULL,
                            facet = FALSE, title = "Scatter Plot") {
  p <- ggplot2::ggplot(data, ggplot2::aes(
    x = {{ x }}, y = {{ y }}, colour = {{ colour }}
  )) +
    ggplot2::geom_point(alpha = 0.6) +
    ggplot2::geom_smooth(method = "lm", se = TRUE) +
    ggplot2::labs(title = title) +
    ggplot2::theme_minimal(base_size = 14)

  if (facet) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars({{ colour }}))
  }
  p
}
