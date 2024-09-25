library(class)
library(ggplot2)

testKNN <- read.csv("testKNN.csv")
trainKNN <- read.csv("trainKNN.csv")

plot( X6.51 ~ X3.815, data = testKNN)

plot_data <- data.frame(train.X, Predicted = train.Y)
ggplot(plot_data, aes(x = X4.6096, y = X2.1597, color = Predicted)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "KNN Predicted Classes on Test Data",
       x = "X6.51",
       y = "X3.815",
       color = "Predicted Class") +
  theme_minimal()

standardized.X <- scale(trainKNN[, -3])

var(trainKNN[, 1])
var(trainKNN[, 2])
var(standardized.X[, 1])
var(standardized.X[, 2])
standardized.Xtest <- scale(testKNN)

train.X <- standardized.X
train.Y <- trainKNN[, 3]
test.X <- standardized.Xtest

knn.pred_one <- knn(train.X, test.X, train.Y, k = 1)

plot_data <- data.frame(test.X, Predicted = knn.pred_one)
ggplot(plot_data, aes(x = X6.51, y = X3.815, color = Predicted)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "KNN Predicted Classes on Test Data",
       x = "X6.51",
       y = "X3.815",
       color = "Predicted Class") +
  theme_minimal()


knn.pred_two <- knn(train.X, test.X, train.Y, k = 2)

plot_data <- data.frame(test.X, Predicted = knn.pred_two)
ggplot(plot_data, aes(x = X6.51, y = X3.815, color = Predicted)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "KNN Predicted Classes on Test Data",
       x = "X6.51",
       y = "X3.815",
       color = "Predicted Class") +
  theme_minimal()


knn.pred_five <- knn(train.X, test.X, train.Y, k = 5)

plot_data <- data.frame(test.X, Predicted = knn.pred_five)
ggplot(plot_data, aes(x = X6.51, y = X3.815, color = Predicted)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "KNN Predicted Classes on Test Data",
       x = "X6.51",
       y = "X3.815",
       color = "Predicted Class") +
  theme_minimal()

set.seed(1)
knn.pred_twenty <- knn(train.X, test.X, train.Y, k = 20)

plot_data <- data.frame(test.X, Predicted = knn.pred_twenty)
ggplot(plot_data, aes(x = X6.51, y = X3.815, color = Predicted)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "KNN Predicted Classes on Test Data",
       x = "X6.51",
       y = "X3.815",
       color = "Predicted Class") +
  theme_minimal()

