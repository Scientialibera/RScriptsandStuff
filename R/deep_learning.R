#' @title Deep Learning Module
#' @description Configurable CNN model builder for image classification.
#'   Requires \pkg{keras} and \pkg{tensorflow} (soft dependency).
#' @name deep_learning
NULL

#' Build a Convolutional Neural Network for image classification
#'
#' Constructs a sequential Keras model with a configurable number of
#' convolutional blocks (conv -> conv -> pool -> dropout), dense layers,
#' and a softmax output head.
#'
#' @param input_shape Numeric vector of length 3, e.g. \code{c(28, 28, 1)}.
#' @param num_classes Positive integer. Number of output classes.
#' @param conv_filters Integer vector of filter counts per conv layer.
#'   Default \code{c(56, 56, 128)}.
#' @param kernel_size Two-element integer vector. Default \code{c(3, 3)}.
#' @param pool_size Two-element integer vector. Default \code{c(2, 2)}.
#' @param dense_units Integer vector of dense-layer widths.
#'   Default \code{c(128, 128)}.
#' @param dropout_conv Dropout rate after conv/pool blocks. Default \code{0.25}.
#' @param dropout_dense Numeric vector of dropout rates for dense layers.
#'   Default \code{c(0.35, 0.5)}.
#' @param optimizer A Keras optimizer. If \code{NULL} (default), uses
#'   \code{keras::optimizer_adadelta()}.
#' @param compile Logical; compile the model before returning? Default \code{TRUE}.
#' @return A (compiled or uncompiled) Keras sequential model.
#' @export
#' @examples
#' \dontrun{
#' model <- build_cnn_model(c(28, 28, 1), num_classes = 10)
#' summary(model)
#' }
build_cnn_model <- function(input_shape = c(28, 28, 1),
                            num_classes = 10L,
                            conv_filters = c(56L, 56L, 128L),
                            kernel_size = c(3L, 3L),
                            pool_size = c(2L, 2L),
                            dense_units = c(128L, 128L),
                            dropout_conv = 0.25,
                            dropout_dense = c(0.35, 0.5),
                            optimizer = NULL,
                            compile = TRUE) {
  if (!requireNamespace("keras", quietly = TRUE)) {
    stop("Package 'keras' is required but not installed.\n",
         "Install with: install.packages('keras')", call. = FALSE)
  }
  stopifnot(
    "input_shape must be length 3" = length(input_shape) == 3L,
    "num_classes must be >= 2"     = num_classes >= 2L,
    "conv_filters must have >= 1 element" = length(conv_filters) >= 1L,
    "dropout_conv must be in [0,1)"       = dropout_conv >= 0 && dropout_conv < 1,
    "all dropout_dense values must be in [0,1)" =
      all(dropout_dense >= 0) && all(dropout_dense < 1)
  )

  model <- keras::keras_model_sequential()

  model <- model %>%
    keras::layer_conv_2d(
      filters = conv_filters[1],
      kernel_size = kernel_size,
      activation = "relu",
      padding = "same",
      kernel_initializer = "he_normal",
      input_shape = input_shape
    )

  for (i in seq_along(conv_filters)[-1]) {
    model <- model %>%
      keras::layer_conv_2d(
        filters = conv_filters[i],
        kernel_size = kernel_size,
        activation = "relu",
        padding = "same"
      )
    if (i %% 2 == 0 || i == length(conv_filters)) {
      model <- model %>%
        keras::layer_max_pooling_2d(pool_size = pool_size) %>%
        keras::layer_dropout(rate = dropout_conv)
    }
  }

  model <- model %>% keras::layer_flatten()

  for (j in seq_along(dense_units)) {
    dr <- if (j <= length(dropout_dense)) dropout_dense[j] else utils::tail(dropout_dense, 1)
    model <- model %>%
      keras::layer_dense(units = dense_units[j], activation = "relu") %>%
      keras::layer_dropout(rate = dr)
  }

  model <- model %>%
    keras::layer_dense(units = num_classes, activation = "softmax")

  if (compile) {
    if (is.null(optimizer)) optimizer <- keras::optimizer_adadelta()
    model %>% keras::compile(
      loss = "categorical_crossentropy",
      optimizer = optimizer,
      metrics = c("accuracy")
    )
  }

  model
}
