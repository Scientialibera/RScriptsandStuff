#!/usr/bin/env Rscript
# Example: CNN on Fashion MNIST
# Demonstrates the deep_learning module
# Requires keras + tensorflow (GPU recommended)

library(RScriptsandStuff)
library(keras)

fashion_mnist <- dataset_fashion_mnist()
c(train_img, train_lbl) %<-% fashion_mnist$train
c(test_img, test_lbl) %<-% fashion_mnist$test

train_img <- array_reshape(train_img, c(nrow(train_img), 28, 28, 1)) / 255
test_img  <- array_reshape(test_img,  c(nrow(test_img),  28, 28, 1)) / 255

train_lbl <- to_categorical(train_lbl, 10)
test_lbl  <- to_categorical(test_lbl, 10)

model <- build_cnn_model(
  input_shape  = c(28, 28, 1),
  num_classes  = 10,
  conv_filters = c(56, 56, 128),
  dense_units  = c(128, 128),
  dropout_conv = 0.25,
  dropout_dense = c(0.35, 0.5)
)

summary(model)

model %>% fit(
  train_img, train_lbl,
  batch_size = 64,
  epochs = 10,
  validation_split = 0.2
)

score <- model %>% evaluate(test_img, test_lbl, verbose = 0)
cat("Test loss:    ", score[[1]], "\n")
cat("Test accuracy:", score[[2]], "\n")
