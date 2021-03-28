library(caret)
library(dplyr)
df <- read.csv("bank.csv", header = T)
str(df)
df <- df %>% 
    mutate_if(is.character, as.factor)

# Delete NA rows
df_1 <- df
df_1[df_1 == "unknown"] <- NA
sum(is.na(df_1))
df_1 <- na.omit(df_1)
str(df_1)

# Standardization
df_1$age <- scale(df_1$age)
df_1$duration <- scale(df_1$age)
df_1$campaign <- scale(df_1$campaign)
df_1$previous <- scale(df_1$previous)
df_1$emp.var.rate <- scale(df_1$emp.var.rate)
df_1$cons.price.idx <- scale(df_1$cons.price.idx)
df_1$cons.conf.idx <- scale(df_1$cons.conf.idx)
df_1$euribor3m <- scale(df_1$euribor3m)
df_1$nr.employed <- scale(df_1$nr.employed)

# Drawing histogram
par(mfrow = c(3, 3), mar = c(5.1, 4.1, 4.1, 2.1))
hist(df_1$age, main = "Age Histogram", xlab = "age", col = "skyblue")
hist(df_1$duration, main = "Duration Histogram", xlab = "duration", col = "yellow")
hist(df_1$campaign, main = "Campaign Histogram", xlab = "campaign", col = "green")
hist(df_1$previous, main = "Previous contact Histogram", xlab = "previous", col = "red")
hist(df_1$emp.var.rate, main = "Employment Variation Rate Histogram", xlab = "EVR", col = "orange")
hist(df_1$cons.price.idx, main = "Consumers Price Index Histogram", xlab = "CPI", col = "purple")
hist(df_1$cons.conf.idx, main = "Consumers Confidence Index Histogram", xlab = "CCI", col = "gray")
hist(df_1$euribor3m, main = "Euribor(3 months) Histogram", xlab = "euribor", col = "salmon")
hist(df_1$nr.employed, main = "Number of Employed Histogram", xlab = "num.employed", col = "navy")

# Categorical data plot_bar
par(mfrow = c(3, 3), mar = c(5.1, 4.1, 4.1, 2.1))
barplot(prop.table(table(df_1$job)), main = "Job Rate")
barplot(prop.table(table(df_1$marital)), main = "Marital")
barplot(prop.table(table(df_1$education)), main = "Levels of Education")
barplot(prop.table(table(df_1$default)), main = "Default")
barplot(prop.table(table(df_1$housing)), main = "Housing Loan")
barplot(prop.table(table(df_1$loan)), main = "Individual Loan")
barplot(prop.table(table(df_1$contact)), main = "Ways of Contact")
barplot(prop.table(table(df_1$month)), main = "Final Contact Month")
barplot(prop.table(table(df_1$day_of_week)), main = "Final Day of Week")

# Categorical data plot_pie
par(mfrow = c(3, 3), mar = c(0.1, 0.1, 0.1, 0.1))
pie(prop.table(table(df_1$job)), main = "Job Rate")
pie(prop.table(table(df_1$marital)), main = "Marital")
pie(prop.table(table(df_1$education)), main = "Levels of Education")
pie(prop.table(table(df_1$default)), main = "Default")
pie(prop.table(table(df_1$housing)), main = "Housing Loan")
pie(prop.table(table(df_1$loan)), main = "Individual Loan")
pie(prop.table(table(df_1$contact)), main = "Ways of Contact")
pie(prop.table(table(df_1$month)), main = "Final Contact Month")
pie(prop.table(table(df_1$day_of_week)), main = "Final Day of Week")

# Visualization with Dimensionality Reduction
num_feature <- c("age", "duration", "campaign", "previous", "emp.var.rate", "cons.price.idx", "cons.conf.idx", "euribor3m", "nr.employed")

tar <- df_1[, "target"]
num_data <- df_1[, num_feature]
pca_num <- prcomp(num_data)
plot(pca_num, type = "l", main = "Principal Component Analysis")
summary(pca_num)

pca_matrix <- pca_num$rotation
pca_data <- as.matrix(num_data)%*%pca_matrix
reduced_data <- data.frame(cbind(pca_data[, 1:3], tar))
reduced_data$tar <- as.factor(reduced_data$tar)
str(reduced_data)

library(ggplot2)
ggplot(data = reduced_data, aes(x = PC1, y = PC2)) +
    geom_point(aes(color = tar, shape = tar)) +
    xlab("PC1") +
    ylab("PC2") +
    ggtitle("PCA DATA")

library(scatterplot3d)
shapes = c(16, 17)
shapes <- shapes[as.numeric(reduced_data$tar)]
scatterplot3d(reduced_data[, 1:3], color = reduced_data[, "tar"], pch = shapes, angle = 45)

# Low Efficacy -> Machine Learning with raw data

# train / test
set.seed(2020)
df_2 <- df
datatotal <- sort(sample(nrow(df_2), nrow(df_2)*0.7))
train <- df_2[datatotal, ]
test <- df_2[-datatotal, ]

# Logistic Regression
ctrl <- trainControl(method = "repeatedcv", repeats = 5)
logit_fit <- train(target ~.,
                   data = train,
                   method = "glm",
                   trControl = ctrl,
                   metric = "Accuracy")
logit_fit
logit_pred <- predict(logit_fit, newdata = test)
confusionMatrix(logit_pred, test$target)

# Boosted Logistic Regression
logit_boost_fit <- train(target ~.,
                   data = train,
                   method = "LogitBoost",
                   trControl = ctrl,
                   metric = "Accuracy")
logit_boost_fit
plot(logit_boost_fit)
logit_boost_pred <- predict(logit_boost_fit, newdata = test)
confusionMatrix(logit_boost_pred, test$target)

# Logistic Model Tree
logit_tree_fit<- train(target ~.,
                       data = train,
                       method = "LMT",
                       trControl = ctrl,
                       metric = "Accuracy")
logit_tree_fit
plot(logit_tree_fit)
logit_tree_pred <- predict(logit_tree_fit, newdata = test)
confusionMatrix(logit_tree_pred, test$target)

# Penalized Logistic Regression
logit_plr_fit<- train(target ~.,
                       data = train,
                       method = "plr",
                       trControl = ctrl,
                       metric = "Accuracy")
logit_plr_fit
plot(logit_plr_fit)
logit_plr_pred <- predict(logit_plr_fit, newdata = test)
confusionMatrix(logit_plr_pred, test$target)

# Regularized Logistic Regression
logit_reg_fit<- train(target ~.,
                      data = train,
                      method = "regLogistic",
                      trControl = ctrl,
                      metric = "Accuracy")
logit_reg_fit
plot(logit_reg_fit)
logit_reg_pred <- predict(logit_reg_fit, newdata = test)
confusionMatrix(logit_reg_pred, test$target)

# Naive Bayes
nb_fit<- train(target ~.,
               data = train,
               method = "naive_bayes",
               trControl = ctrl,
               metric = "Accuracy")

nb_fit
plot(nb_fit)
nb_pred <- predict(nb_fit, newdata = test)
confusionMatrix(nb_pred, test$target)

# Random Forest
rf_fit<- train(target ~.,
               data = train,
               method = "rf",
               trControl = ctrl,
               metric = "Accuracy")
rf_fit
plot(rf_fit)
rf_pred <- predict(rf_fit, newdata = test)
confusionMatrix(rf_pred, test$target)

# SVM
svm_linear_fit<- train(target ~.,
                       data = train,
                       method = "svmLinear",
                       trControl = ctrl,
                       metric = "Accuracy")
svm_linear_fit
svm_linear_pred <- predict(svm_linear_fit, newdata = test)
confusionMatrix(svm_linear_pred, test$target)

# Kernel SVM
svm_poly_fit<- train(target ~.,
                     data = train,
                     method = "svmPoly",
                     trControl = ctrl,
                     metric = "Accuracy")
svm_ploy_fit
plot(svm_ploy_fit)
svm_poly_pred <- predict(svm_poly_fit, newdata = test)
confusionMatrix(svm_poly_pred, test$target)

# Best Model to this data: Random Forest