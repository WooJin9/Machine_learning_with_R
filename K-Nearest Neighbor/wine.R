install.packages("caret", dependences = T)
library(caret)

wine_raw <- read.csv("wine.csv", header = T)
wine_copy <- wine_raw
wine_copy$Class <- as.factor(wine_copy$Class)
str(wine_copy)

wineanal <- wine_copy
set.seed(2020)
data_total <- sort(sample(nrow(wineanal), nrow(wineanal)*0.7))
train <- wine_copy[data_total, ]
test <- wine_copy[-data_total, ]

train_x <- train[, 1:13]
train_y <- train[, 14]

ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
customGrid <- expand.grid(k = 1:10)
knnfit <- train(Class ~.,
                data = train,
                method = "knn",
                trControl = ctrl,
                preProcess = c("center", "scale"),
                tuneGrid = customGrid,
                metric = "Accuracy")

knnfit
plot(knnfit)

pred_test <- predict(knnfit, newdata = test)
confusionMatrix(pred_test, test$Class)

importance_knn <- varImp(knnfit, scale = F)
plot(importance_knn)