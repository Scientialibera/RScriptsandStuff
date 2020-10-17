### Author: EA
### Description:
# The purpose of this example is to show how cross validation allows us to choose 
# the best parameters for our knn algorithm, k in our case. We perform a 10-fold cross validation. 
# Note that the feature engineering steps shown in previous examples are included here as
# preProcess.

library(class)
library(gmodels)
library(caret)
library(rsample)
library(gmodels)

# Get iris data set and set random seed for reproducibility.

set.seed(125)
irisdta <- iris

# Split data into training and testing batches, prepare labels for test set. We use replacement sampling to increase sample size to 250.

splitdta <- irisdta[sample(nrow(irisdta), 250, replace =T),]
splitdta <- initial_split(splitdta, prop = .8, strata = "Species")


traindta <- training(splitdta)
testdta <- testing(splitdta)

testlabels <- testdta[,5]

# Create grid for the possible values of K to try in our training, train our model.

gridp <- expand.grid(
  k = floor(seq(1, 20, 1)))

cvalknn1 <- train(
  Species ~ ., 
  data = traindta, 
  method = "knn",
  preProcess = c("zv", "center", "scale"),
  trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5, classProbs = T),
  tuneGrid = gridp,
  metric = "ROC"
  
)

# Print models parameters and confusion matrix

plot(cvalknn1)

p <- predict(cvalknn1, testdta)

CrossTable(p, testlabels, prop.chisq = F, prop.t = F, dnn = c("Pred", "Actual"))
