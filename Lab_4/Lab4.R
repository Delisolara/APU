install.packages("C50")
library(MASS)
require(C50)

data("iris")
head(iris)

str(iris)

iris$Species <- as.factor(iris$Species)
str(iris)
table(iris$Species)

set.seed(123)
train_indices <- sample(1:nrow(iris), 0.8 * nrow(iris))
train_data <- iris[train_indices, ]
test_data <- iris[-train_indices, ]

m1 <- C5.0(Species ~ ., data=train_data)
summary(m1)

plot(m1)

predictions <- predict(m1, test_data)

confusion_matrix <- table(test_data$Species, predictions)
print(confusion_matrix)
