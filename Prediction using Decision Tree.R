#The Sparks Foundation
#Task 6:Prediction using Decision Tree Algorithm
#Problem Statement:Create the Decision Tree classifier and visualize it graphically.
#Presented By: Divya Dinesh Imale

#Installing required packages.
install.packages("rpart")
install.packages("rpart.plot")
library("rpart")
library("rpart.plot")

#Reading dataset
data("iris")
data_iris <- iris
View(data_iris)

#Exploring iris dataset structure by str() command.
str(iris)

#Create training dataset and testing dataset----
indexes = sample(150, 110)
iris_train = iris[indexes,]
iris_test = iris[-indexes,]
View(iris_train)
View(iris_test)

#Building and plotting model
ctree = rpart(Species ~., data = iris_train, method = "class")
ctree
rpart.plot(ctree)

#Predicting:
#Classification Tree: Predicting whether the species is "Setosa" or not,
iris_test$Prediction_Species = predict(ctree, iris_test, type = 'class')
View(iris_test)

#Accuracy (Confusion Matrix)
table_mat <- table(iris_test$Species, iris_test$Prediction_Species)
table_mat
accuracy_Test <- sum(diag(table_mat))/ sum(table_mat)
accuracy_Test

#Regression Decision Tree-
rtree = rpart(Sepal.Length ~., data = iris_train, method = "anova")
rpart.plot(rtree)
print(ctree)

#Predicitng:----
#Regression Tree: Predicting the lenth of Sepal,
iris_test$Prediction_Sepal.Length = predict(rtree, iris_test)
View(iris_test)

#Accuracy MAPE(MeanAbsolutePercentageError):
#Determining Prediction accuracy on test dataset using MAPE
#Lower its value better is the accuracy of the model.

#MAPE Calculation:
mape <- mean(abs((iris_test$Prediction_Sepal.Length - iris_test$Sepal.Length))/iris_test$Sepal.Length)
mape

#Pruning:
#It is a technique used in determining the size of the tree.
tree_ms5 = rpart(Species~., iris_train,control = rpart.control(minsplit = 5))
rpart.plot(tree_ms5, main = "minsplit=5")