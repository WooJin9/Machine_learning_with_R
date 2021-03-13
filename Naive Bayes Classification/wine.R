library(caret)

wine <- read.csv("wine.csv", header = T)
wine$Class <- as.factor(wine$Class)
str(wine)

wine_dup <- wine
set.seed(2020)
datatotal <- sort(sample(nrow(wine_dup), nrow(wine_dup)*0.7))
train <- wine_dup[datatotal, ]
test <- wine_dup[-datatotal, ]

str(train)

train_x <- train[1:13]
train_y <- train[14]

test_x <- test[1:13]
test_y <- test[14]

ctrl <- trainControl(method = "repeatedcv", repeats = 5)

nbFit <- train(Class ~ .,
               data = train,
               method = "naive_bayes",
               trControl = ctrl,
               preProcess = c("center", "scale"),
               metric = "Accuracy")
# preProcess: 전처리 과정 / center: 데이터에서 평균값을 뺌 / scale: 데이터를 표준편차로 나눔

nbFit
plot(nbFit)

# 예측
pred_test <- predict(nbFit, newdata = test)
confusionMatrix(pred_test, test$Class)

# 변수 중요도
importance_nb <- varImp(nbFit, scale = F)
plot(importance_nb)
