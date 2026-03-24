#!/usr/bin/env Rscript
# Example: Multiple Regression on Iris
# Demonstrates the regression module

library(RScriptsandStuff)

data(iris)

result <- run_multiple_regression(
  iris,
  Petal.Width ~ Petal.Length + Sepal.Length + Sepal.Width
)
cat("Regression summary:\n")
print(result$summary)
cat("\nCorrelation matrix:\n")
print(round(result$correlation_matrix, 3))

p1 <- plot_regression(iris, Sepal.Length, Petal.Width,
                      colour = Species, facet = TRUE,
                      title = "Sepal Length vs Petal Width by Species")
print(p1)

p2 <- plot_regression(iris, Sepal.Length, Petal.Width,
                      title = "Sepal Length vs Petal Width (All Species)")
print(p2)
