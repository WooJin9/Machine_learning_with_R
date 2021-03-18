raw <- read.csv("heart.csv", header = T)
library(caret)

#target 범주화
str(raw)
raw$target <- as.factor(raw$target)
unique(raw$target)

# 연속형 독립변수 표준화
raw$age <- scale(raw$age)
raw$trestbps <- scale(raw$trestbps)
raw$chol <- scale(raw$chol)
raw$thalach <- scale(raw$thalach)
raw$oldpeak <- scale(raw$oldpeak)
raw$slope <- scale(raw$slope)

# 범주형 독립변수 as.factor
new <- raw
factorVar <- c("sex", "cp", "fbs", "restecg", "exang", "ca", "thal")
new[, factorVar] = lapply(new[, factorVar], factor)

# 트레이닝 / 테스트 나누기
set.seed(2020)
total <- sort(sample(nrow(new), nrow(new)*0.7))
train <- new[total, ]
test <- new[-total, ]

train_X <- train[, 1:12]
train_y <- train[, 13]

test_X <- test[, 1:12]
test_y <- test[, 13]

# Logitboosted
ctrl <- trainControl(method = "repeatedcv" ,repeats = 5)
logitFit <- train(target ~.,
                  data = train,
                  method = "LogitBoost",
                  trControl = ctrl,
                  metric = "Accuracy")
logitFit

plot(logitFit)

# model prediction
pred_test <- predict(logitFit, new = test)
confusionMatrix(pred_test, test$target)

# 변수 중요도
importance <- varImp(logitFit, scale = F)
plot(importance)
