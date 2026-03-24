#!/usr/bin/env Rscript
# Example: Bing Sentiment Analysis
# Demonstrates the NLP module

library(RScriptsandStuff)

sample_texts <- c(
  "The company reported record earnings and strong revenue growth.",
  "Investors are worried about rising inflation and supply chain disruptions.",
  "New product launch exceeded expectations with overwhelmingly positive reviews.",
  "Market crash fears intensify as recession indicators worsen.",
  "Quarterly results were mixed, with some bright spots amid overall weakness."
)

sentiment <- analyze_sentiment(sample_texts)
cat("Sentiment scores by document:\n")
print(sentiment)

cat("\nAverage sentiment:", mean(sentiment$sentiment), "\n")
