#!/usr/bin/env Rscript
# Example: Spam Detection with Naive Bayes
# Demonstrates the NLP module
#
# Prerequisites: download the SMS Spam Collection dataset
#   https://archive.ics.uci.edu/dataset/228/sms+spam+collection
# and update the path below.

library(RScriptsandStuff)

csv_path <- "path/to/spam.csv"

if (!file.exists(csv_path)) {
  cat("Please download the SMS Spam Collection dataset and update csv_path.\n")
  cat("See: https://archive.ics.uci.edu/dataset/228/sms+spam+collection\n")
  quit(status = 0)
}

texts_df <- read.csv(csv_path, header = TRUE, stringsAsFactors = FALSE)
texts  <- texts_df[[2]]
labels <- as.factor(texts_df[[1]])

detector <- build_spam_detector(texts, labels,
                                train_fraction = 0.75,
                                min_term_freq = 7,
                                laplace = 1)

preds <- predict_spam(detector)
actual <- labels[(detector$train_size + 1):length(labels)]
metrics <- classification_metrics(preds, actual)
cat("Accuracy:   ", round(metrics$accuracy, 4), "\n")
cat("Precision:  ", round(metrics$precision, 4), "\n")
cat("Sensitivity:", round(metrics$sensitivity, 4), "\n")
cat("Specificity:", round(metrics$specificity, 4), "\n")
