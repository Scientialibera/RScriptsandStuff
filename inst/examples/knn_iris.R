#!/usr/bin/env Rscript
# Example: KNN Cross-Validation on Iris
# Demonstrates the classification module

library(RScriptsandStuff)
library(rsample)

data(iris)
set.seed(42)

split <- rsample::initial_split(iris, prop = 0.8, strata = "Species")
train_df <- rsample::training(split)
test_df  <- rsample::testing(split)

model <- train_knn_cv(
  data = train_df,
  formula = Species ~ .,
  k_range = 1:15,
  cv_folds = 10,
  cv_repeats = 3
)

cat("Best k:", model$bestTune$k, "\n")
plot(model)

result <- predict_knn_cv(model, test_df, "Species")
cat("Test accuracy:", round(result$accuracy, 4), "\n")
print(result$confusion_matrix)
