### Author: EA
### Description:
# In this program, we work on what many call the "Hello World" of machine learning, the mnist image dataset. In this case we will be exploring the
# fashion version of the set and not the number one. However, the set is of the same size and just as mnist, has luckily already been 
# cleaned and organized in such a way that the main objective of this exercise is to tweak our Convultional Neural Network to produce the
# best results. As is common with image classification, we use convultional layer, followed by a pooling layer and finally fed to regular, 
# 1-d hidden neuron layer.

library(tensorflow)
library(keras)
library(caret)

# Download the dataset we will be working with
fashion_mnist <- dataset_fashion_mnist()

# Create 4 new variables. Test and train image set and its respective response variable. Again, everything has already been structured in a tidy way
c(train_imag, train_labels) %<-% fashion_mnist$train
c(test_imag, test_labels) %<-% fashion_mnist$test

# Since we are using a Convultional network, we must add an extra dimension for our algorithm to work properly and to be able to extract spacial relationships
train_imag <- array_reshape(train_imag, c(nrow(train_imag), 28, 28, 1))
test_imag <- array_reshape(test_imag, c(nrow(test_imag), 28, 28, 1))

# Normalize all the values in our matrix so the range is 0-1
train_imag <- train_imag %>%  normalize()
test_imag <- test_imag %>% normalize()

# Convert the respond variable into a factor
train_labels <- to_categorical(train_labels, 10)
test_labels <- to_categorical(test_labels, 10)

# We have decided to use 3 convulsional layers which then feed into 3 1d neuron layers. 
model <- keras_model_sequential() %>%
  layer_conv_2d(filters = 56, kernel_size = c(3,3), activation = 'relu', padding = "same",kernel_initializer = "he_normal",input_shape = c(28,28, 1)) %>%
  layer_conv_2d(filters = 56, kernel_size = c(3,3), activation = 'relu', padding = "same") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_dropout(rate = 0.25) %>%
  layer_conv_2d(filters = 128, kernel_size = c(3,3), activation = 'relu', padding = "same",kernel_initializer = "he_normal",input_shape = c(28,28, 1)) %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_dropout(rate = 0.25) %>%
  layer_flatten() %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = .35) %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = .5) %>%
  layer_dense(units = 10, activation = 'softmax')

model %>% compile(
  loss = loss_categorical_crossentropy,
  optimizer = optimizer_adadelta(),
  metrics = c('accuracy')
)

# Note!! Model will take about 20 minutes to train and will suck up all available CPU juice
model %>% fit(
  train_imag, train_labels,
  batch_size = 40,
  epochs = 10,
  validation_split = 0.2
)

score <- model %>% evaluate(train_imag, train_labels, verbose = 0)

predictions <- model %>% predict(test_imag)

class_pred <- model %>% predict_classes(test_imag)

# Plot the first 25 images and the predicted classes.

class_names = c('T-shirt/top', 'Trouser', 'Pullover', 'Dress', 'Coat', 'Sandal', 'Shirt', 'Sneaker', 'Bag', 'Ankle boot')

par(mfcol=c(5,5))
par(mar=c(0, 0, 1.5, 0), xaxs='i', yaxs='i')

for (i in 1:25) { 
  img <- test_imag[i, , , ]
  img <- t(apply(img, 2, rev)) 
  predicted_label <- which.max(predictions[i, ]) - 1
  true_label <- which.max(test_labels[i, ]) - 1 
  if (predicted_label == true_label) {
    color <- 'green' 
  } else {
    color <- 'red'
  }
  image(1:28, 1:28, img, col = gray((0:255)/255), xaxt = 'n', yaxt = 'n',
        main = paste0(class_names[predicted_label + 1], " (",
                      class_names[true_label + 1], ")"),
        col.main = color)
}


print(score)
