data("iris")
head(iris)
str(iris)
library(ggplot2)
library(e1071)

qplot(Petal.Length,Petal.Width,data = iris, color = Species)
help("svm")
iris
svm_model1 <- svm(Species~., data = iris)
summary(svm_model1)
iris

plot(svm_model1, data = iris, Petal.Width~Petal.Length, slice = list(Sepal.Width = 3, Sepal.Length = 4))
pred1 <- predict(svm_model1, iris)
table1 <- table(Predicted = pred1, Actual = iris$Species)
table1
Model1_accuracyRate = sum(diag(table1))/sum(table1)
Model1_accuracyRate
Model1_MissClassificationRate = 1 - Model1_accuracyRate
Model1_MissClassificationRate


svm_model2 <- svm(Species~., data = iris, kernel = "linear")

summary(svm_model2)

plot(svm_model2, data = iris, Petal.Width~Petal.Length, slice = list(Sepal.Width = 3, Sepal.Length = 4))
pred2 <- predict(svm_model2, iris)
table2 <- table(Predicted = pred2, Actual = iris$Species)
table2




svm_model3 <- svm(Species~., data = iris, kernel = "polynomial")

summary(svm_model3)

plot(svm_model3, data = iris, Petal.Width~Petal.Length, slice = list(Sepal.Width = 3, Sepal.Length = 4))
pred3 <- predict(svm_model3, iris)
table3 <- table(Predicted = pred3, Actual = iris$Species)
table3

iris$Species
