### Author: EA
### Description:
# The script's length is purposely short this time. My intention was to draw a comparison between Excel's and R's data analysis abilities. While
# you can certainly perform descriptive analytics with Excel, R allows for more complex analysis and increased efficiency. With only a few lines of 
# coding we have been able to achieve the same analysis depth as Excel's.

# Load packages
library(ggplot2)
library(dplyr)

# Attach Iris data set
iris1 <- data.frame(iris)
attach(iris1)
summary(iris1)

# Scatter plots of the correlation between Sepal Length and Petal Width
qplot(Sepal.Length, Petal.Width, data = iris, color = Species, xlab="Sepal Length", ylab="Petal Width", main = "Scatter Plot Sepal Length/Petal Width
") + geom_smooth(method=lm, se=TRUE) + facet_wrap(~Species)
qplot(Sepal.Length, Petal.Width, data = iris, color = Species,  xlab="Sepal Length", ylab="Petal Width", main = "Scatter Plot Sepal Length/Petal Width
") + geom_smooth(method=lm, se=TRUE)  + geom_smooth(method=lm, se=TRUE) 
qplot(Sepal.Length, Petal.Width, data = iris, xlab="Sepal Length", ylab="Petal Width", main = "Scatter Plot Sepal Length/Petal Width") + geom_smooth(method=lm, se=TRUE)


# Multiple linear regression on Petal Width with the remaining measurements as features.
MRegr <- lm(Petal.Width ~  Petal.Length + Sepal.Length + Sepal.Width, iris1)
print(summary(MRegr))

cor(iris1[-5])