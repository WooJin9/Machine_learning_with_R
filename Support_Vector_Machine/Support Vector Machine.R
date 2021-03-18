library(caret)

wine <- read.csv("wine.csv", header = T)
wine$Class <- as.factor(wine$Class)
str(wine$Class)

dupli_wine <- wine
set.seed(2020)
datatotal <- sort(sample(nrow(dupli_wine), nrow(dupli_wine)*0.7))
train <- wine[datatotal, ]
test <- wine[-datatotal, ]

str(train)

train_x <- train[, 1:13]
train_y <- train[, 14]

test_x <- test[, 1:13]
test_y <- test[, 14]

ctrl <- trainControl(method = "repeatedcv", repeats = 5)

# SVM_Linear
svm_linear_fit <- train(Class ~ .,
                        data = train,
                        method = "svmLinear",
                        trControl = ctrl,
                        preProcess = c("center", "scale"),
                        metric = "Accuracy")
svm_linear_fit

pred_test <- predict(svm_linear_fit, newdata = test)
confusionMatrix(pred_test, test$Class)

importance_linear <- varImp(svm_linear_fit, scale = F)
plot(importance_linear)


# Poly_SVM

ctrl <- trainControl(method = "repeatedcv", repeats = 5)
svm_poly_fit <- train(Class ~ .,
                      data = train,
                      method = "svmPoly",
                      trControl = ctrl,
                      preProcess = c("center", "scale"),
                      metric= "Accuracy")

svm_poly_fit
plot(svm_poly_fit)

pred_test_a <- predict(svm_poly_fit, newdata = test)
confusionMatrix(pred_test_a, test$Class)

importance_poly <- varImp(svm_poly_fit, scale = F)
plot(importance_poly)