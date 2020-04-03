Sys.setlocale(category = "LC_ALL", locale = "UTF-8")
path <- "~/Yandex.Disk.localized/Университет/Магистратура/2semester/R/Lecture-8/Datasets/"

library(e1071)
library(kernlab)
library(klaR)
library(caret)

# train - 100
# test - 50
iris <- read.table(file = paste0(path, "iris.txt"), header = F, col.names = c("sepal_l", "sepal_w", "petal_l", "petal_w", "type"), dec = ".", sep = ",")
iris <- data.frame(iris)
iris$type_num <- factor(as.numeric(iris$type))

train_random <- sort(sample(c(1:150), size = 100))
train <- iris[train_random, 1:6]
test <- iris[-train_random, 1:6]
model <- naiveBayes(train, train$type_num, type = "raw")
pred <- predict(model, test, type = "class")
cm1 <- confusionMatrix(pred, test$type_num)
cm1
cm1$table


# train - 50
# test - 100
iris <- read.table(file = paste0(path, "iris.txt"), header = F, col.names = c("sepal_l", "sepal_w", "petal_l", "petal_w", "type"), dec = ".", sep = ",")
iris <- data.frame(iris)
iris$type_num <- factor(as.numeric(iris$type))

train_random <- sort(sample(c(1:150), size = 30))
train <- iris[train_random, 1:6]
test <- iris[-train_random, 1:6]
model <- naiveBayes(train, train$type_num, type = "raw")
pred <- predict(model, test, type = "class")
cm2 <- confusionMatrix(pred, test$type_num)
cm2
cm2$table



# train - 15
# test - 135
iris <- read.table(file = paste0(path, "iris.txt"), header = F, col.names = c("sepal_l", "sepal_w", "petal_l", "petal_w", "type"), dec = ".", sep = ",")
iris <- data.frame(iris)
iris$type_num <- factor(as.numeric(iris$type))

train_random <- sort(sample(c(1:150), size = 15))
train <- iris[train_random, 1:6]
test <- iris[-train_random, 1:6]
model <- naiveBayes(train, train$type_num, type = "raw")
pred <- predict(model, test, type = "class")
cm3 <- confusionMatrix(pred, test$type_num)
cm3
cm3$table