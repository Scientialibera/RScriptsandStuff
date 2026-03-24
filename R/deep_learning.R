#' @title Deep Learning Module
#' @description CNN model builder for image classification (requires keras/tensorflow).
#' @name deep_learning
NULL

#' Build a Convolutional Neural Network for image classification
#'
#' Constructs a sequential Keras model with configurable conv layers,
#' dropout, and a softmax output. Requires the \pkg{keras} and
#' \pkg{tensorflow} packages.
#'
#' @param input_shape Numeric vector, e.g. `c(28, 28, 1)`.
#' @param num_classes Number of output classes. Default `10`.
#' @param conv_filters Integer vector of filter counts per conv block.
#'   Default `c(56, 56, 128)`.
#' @param kernel_size Two-element integer vector. Default `c(3, 3)`.
#' @param pool_size Two-element integer vector. Default `c(2, 2)`.
#' @param dense_units Integer vector of dense-layer widths. Default `c(128, 128)`.
#' @param dropout_conv Dropout rate after conv blocks. Default `0.25`.
#' @param dropout_dense Numeric vector of dropout rates for dense layers.
#'   Default `c(0.35, 0.5)`.
#' @param optimizer Keras optimizer. Default \code{keras::optimizer_adadelta()}.
#' @param compile Logical; if TRUE (default), compiles the model.
#' @return A compiled (or uncompiled) Keras sequential model.
#' @export
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
    stop("Package 'keras' is required. Install with: install.packages('keras')")
  }

  model <- keras::keras_model_sequential()

  # First conv block

  model <- model %>%
    keras::layer_conv_2d(
      filters = conv_filters[1],
      kernel_size = kernel_size,
      activation = "relu",
      padding = "same",
      kernel_initializer = "he_normal",
      input_shape = input_shape
    )

  # Additional conv layers
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

  # Flatten + Dense layers

  model <- model %>% keras::layer_flatten()
  for (j in seq_along(dense_units)) {
    dr <- if (j <= length(dropout_dense)) dropout_dense[j] else tail(dropout_dense, 1)
    model <- model %>%
      keras::layer_dense(units = dense_units[j], activation = "relu") %>%
      keras::layer_dropout(rate = dr)
  }

  # Output

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
