### Author: EA
### Description:
# In this example, I highlight some of the data engineering processes behind algorithms that use words as model features. We build a Naive Bayes
# classifier and we then evaluate the model's performance with a cross table. We include other important metrics such as specificity and recall.

library(caret)
library(tm)
library(tidyverse)
library(SnowballC)
library(recipes)
library(e1071)
library(gmodels)

# Set seed for reproducibility
set.seed(125)

# Download data into variable
texts <- read.csv("C:/Users/Emilio/Desktop/R_Python/R/spam.csv", header =  T, stringsAsFactors = F ) %>%
  select("Category" = v1, "Text" = v2)
texts$Category <- as.factor(texts$Category)

# Randomize set
texts <- texts[sample(nrow(texts)),]

# Custom Functions
convert_to_factor <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}

# Create text corpus
txtcorpus <- VCorpus(VectorSource(texts$Text))


# Create term matrix
txtmatrix <- DocumentTermMatrix(txtcorpus, control = list(
  tolower =T,
  removeNumbers = T,
  stopwords = T,
  removePunctuation = T,
  stemming = T
))

# Train and test set creation
txt_train <- as.matrix(txtmatrix[1:4169,])
txt_test <- as.matrix(txtmatrix[4169:5572,])

# Set labels
labels_train <- texts[1:4169,1]
labels_test <- texts[4169:nrow(texts),1]

# Frequent terms in matrix to reduce features
txtmatrixfreq <- findFreqTerms(txtmatrix, 7)
txtmatrixfreq <- txtmatrixfreq[-1]

# Select features which appear at least 7 times
txt_freq_train <- txt_train[,txtmatrixfreq]
txt_freq_test <- txt_test[,txtmatrixfreq]

#Convert count to factor
txt_train <- apply(txt_freq_train, MARGIN = 2, convert_to_factor)
txt_test <- apply(txt_freq_test, MARGIN = 2, convert_to_factor)

# Train model with Naive Bayes method
m <- naiveBayes(txt_train, labels_train, laplace = 1)

# Evaluate model performance
p <- predict(m, txt_test)


ct <- CrossTable(p, labels_test, prop.chisq = F, prop.t = F, dnn = c("Pred", "Actual"))

TP <- ct$t[1]
FP <- ct$t[3]
TN <- ct$t[4]
FN <- ct$t[2]

accuracy    <- (TP + TN)/(TP + FP + FN + TN)
precision   <- TP/(TP + FP)
sensitivity <- TP/(TP + FN)
specificity <- TN/(TN + FP)

print(list(c(accuracy, precision, sensitivity, specificity)))