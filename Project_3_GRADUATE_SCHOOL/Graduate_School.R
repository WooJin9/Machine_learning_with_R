# Using Regression Method

df <- read.csv("Graduate_School(MICHIGAN_Univ.).csv", header = T)
str(df)
colSums(is.na(df))
unique(df$GRE.Score)
unique(df$TOEFL.Score)
unique(df$University.Rating)
u_rating_table <- table(df$University.Rating)
u_rating_table
unique(df$SOP)
unique(df$LOR)
unique(df$Research)
research_table <- table(df$Research)
unique(df$Chance.of.Admit)
max(df$Chance.of.Admit)
min(df$Chance.of.Admit)

# 시각화(histogram)
par(mfrow = c(3, 2), mar = c(5.1, 4.1, 4.1, 4.2))
hist(df$GRE.Score, main = "GRE 점수 히스토그램", xlab = "GRE 점수", col = "skyblue")
hist(df$TOEFL.Score, main = "TOEFL 점수 히스토그램", xlab = "TOEFL 점수", col = "green")
hist(df$SOP, main = "SOP 점수 히스토그램", xlab = "SOP 점수", col = "orange")
hist(df$CGPA, main = "CGPA 점수 히스토그램", xlab = "CGPA 점수", col = "darkmagenta")
hist(df$LOR, main = "LOR 점수 히스토그램", xlab = "LOR 점수", col = "yellow")
hist(df$Chance.of.Admit, main = "CoA 히스토그램", xlab = "CoA", col = "red")

# 시각화(boxplot)
par(mfrow = c(2, 3), mar = c(2, 4.1, 4.1, 2.1))
boxplot(df$GRE.Score, main = "GRE 점수 히스토그램", xlab = "GRE 점수", col = "skyblue")
boxplot(df$TOEFL.Score, main = "TOEFL 점수 히스토그램", xlab = "TOEFL 점수", col = "green")
boxplot(df$SOP, main = "SOP 점수 히스토그램", xlab = "SOP 점수", col = "orange")
boxplot(df$CGPA, main = "CGPA 점수 히스토그램", xlab = "CGPA 점수", col = "darkmagenta")
boxplot(df$LOR, main = "LOR 점수 히스토그램", xlab = "LOR 점수", col = "yellow")
boxplot(df$Chance.of.Admit, main = "CoA 히스토그램", xlab = "CoA", col = "red")

# 시각화(pie chart)
par(mfrow = c(1, 2), mar = c(1, 1, 1, 1))
pie(u_rating_table, main = " Univ.Rating", radius = 1)
pie(research_table, main = "Research_EXP.", radius = 1)

plot(df)

# train / test
set.seed(2020)
new_df <- df
train_ratio <- 0.7
total_df <- sort(sample(nrow(new_df), nrow(new_df)*train_ratio))
train <- new_df[total_df, ]
test <- new_df[-total_df, ]

# logistic regression
library(caret)
ctrl <- trainControl(method = "repeatedcv", repeats = 5)
logistic_fit <- train(Chance.of.Admit ~.,
                      data = train,
                      method = "glm",
                      trControl = ctrl,
                      preProcess = c("center", "scale"), 
                      metric = "RMSE")

logistic_fit
logistic_pred <- predict(logistic_fit, newdata = test)
logistic_pred

# Elasticnet Regression
logit_penal_fit <- train(Chance.of.Admit ~.,
                         data = train,
                         method = "glmnet",
                         trControl = ctrl,
                         preProcess = c("center", "scale"),
                         metric = "RMSE")
logit_penal_fit
logit_penal_fit <- predict(logit_penal_fit, newdata = test)
postResample(pred = logit_penal_fit, obs = test$Chance.of.Admit)

# Random Forest
rf_fit <- train(Chance.of.Admit ~.,
                data = train,
                method = "rf",
                trControl = ctrl,
                preProcess = c("center", "scale"),
                Accuracy = "RMSE")
rf_fit
plot(rf_fit)
rf_pred <- predict(rf_fit, newdata = test)
postResample(pred = rf_pred, obs = test$Chance.of.Admit)

# SVM
svm_linear_fit <- train(Chance.of.Admit ~.,
                        data = train,
                        method = "svmLinear",
                        trControl = ctrl,
                        preProcess = c("center", "scale"),
                        Accuracy = "RMSE")
svm_linear_fit
svm_linear_pred <- predict(svm_linear_fit, newdata = test)
postResample(pred = svm_linear_pred, obs = test$Chance.of.Admit)

# Kernalized SVM
svm_poly_fit <- train(Chance.of.Admit ~.,
                      data = train,
                      method = "svmPoly",
                      trControl = ctrl,
                      preProcess = c("center", "scale"),
                      Accuracy = "RMSE")
svm_poly_fit
plot(svm_poly_fit)

svm_poly_pred <- predict(svm_poly_fit, newdata = test)
postResample(pred = svm_poly_pred, obs = test$Chance.of.Admit)

# Best Model in Using Regression method: Elastictnet 

# Using Classification Method
par(mfrow = c(1, 2), mar = c(5.1 , 4.1, 4.1, 2.1))
hist(df$Chance.of.Admit, main = "대학원 합격 확률", xlab = "대학원 합격 확률", col = "red")
boxplot(df$Chance.of.Admit, main = "대학원 합격 확률", xlab = "대학원 합격 확률", col = "red")

# > Median: Admit, < Media: Reject
df_2 <- df
summary(df_2$Chance.of.Admit)
target_median = median(df_2$Chance.of.Admit)
df_2[(df_2$Chance.of.Admit < target_median), "Chance.of.Admit"] = "0"
df_2[(df_2$Chance.of.Admit >= target_median), "Chance.of.Admit"] = "1"
df_2$Chance.of.Admit <- as.factor(df_2$Chance.of.Admit)
str(df_2)
unique(df_2$Chance.of.Admit)

# train / test
set.seed(2020)
new_df_2 <- df_2
train_ratio <- 0.7
total_df_2 <- sort(sample(nrow(new_df_2), nrow(new_df_2)*train_ratio))
train_2 <- new_df_2[total_df_2, ]
test_2 <- new_df_2[-total_df_2, ]                   
