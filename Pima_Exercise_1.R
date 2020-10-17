### Author: EA
### Description:
# I wanted to include an example in which our algorithm does not perform as expected since in our first examples, our 
# classifiers worked very well. In real life scenarios, this is not always the case, and we use Pima's data base to
# highlight this important fact. Pima is a database usually used to test classifiers as it's known to be quite tricky. We 
# must use 8 continuous variables to predict whether an individual has diabetes or not.

library(class)
library(gmodels)
library(caret)
library(rsample)
library(gmodels)
library(splitstackshape)
library(xgboost)
library(tidyverse)

# Get iris data set and set random seed for reproducibility
set.seed(125)
Pima <- readRDS("DOWNLOAD AND SET PATH TO PIMA DATABASE FROM GITHUB")

# We want our samples to be roughly equal in terms of y/n diabetes ratio, so we use a stratified sample.
pimastrat <- stratified(Pima, group = 9, bothSets = T, .7)

# No need to include the answer vector in our matrix.
pimatrain <- as.matrix(pimastrat$SAMP1[,-9])
pimatest <- as.matrix(pimastrat$SAMP2[,-9])

# Create labels and train model
trainlabels <- select(pimastrat$SAMP1, Y = "Yes/No Diabetes")
tlabels <- as.numeric((as.vector(trainlabels$Y)))
testlabels <- select(pimastrat$SAMP2, Y = "Yes/No Diabetes")


nnetm <- train (  
  x = pimatrain, 
  y = tlabels,
  preProcess = c("center", "scale"),
  objective = "binary:logistic",
  trControl = trainControl(method = "cv", number = 10, classProbs = T),
  tuneGrid = expand.grid(eta = .08, nrounds = 5000, max_depth = 30, min_child_weight = 1, subsample = .5, colsample_bytree = .5, gamma = 1),
  method = "xgbTree",
  verbose = T
)
# Print models parameters and confusion matrix

p <- predict(nnetm, pimatest)
p <- as.numeric(p > 0.7)

ct <- CrossTable(p, testlabels$Y, prop.chisq = F, prop.t = F, dnn = c("Pred", "Actual"))