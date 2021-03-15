install.packages("tree")
library(tree)
library(caret)

wine <- read.csv("wine.csv", header = T)
wine$Class <- as.factor(wine$Class)
str(wine)

# Decision Tree
anal_wine <- wine
set.seed(2020)
datatotal <- sort(sample(nrow(anal_wine), nrow(anal_wine)*0.7))
train <- wine[datatotal, ]
test <- wine[datatotal, ]

str(train)

train_x <- train[, 1:13]
train_y <- train[, 14]

test_x <- test[, 1:13]
test_y <- test[, 14]

treeRaw <- tree(Class ~. , data = train)
plot(treeRaw)
text(treeRaw)

cv_tree <- cv.tree(treeRaw, FUN = prune.misclass)
plot(cv_tree)

prune_tree <- prune.misclass(treeRaw, best = 3)
plot(prune_tree)
text(prune_tree, pretty = 0)

pred <- predict(prune_tree, test, type = "class")
confusionMatrix(pred,test$Class)

# Random Forest
ctrl <- trainControl(method = "repeatedcv", repeats = 5)
rfFit <- train(Class ~ .,
               data = train,
               method = "rf",
               trControl = ctrl,
               preProcess = c("center", "scale"),
               metric = "Accuracy")

rfFit
plot(rfFit)

pred_test <- predict(rfFit, newdata = test)
confusionMatrix(pred_test, test$Class)

importance_rf <- varImp(rfFit, scale = F)
plot(importance_rf)