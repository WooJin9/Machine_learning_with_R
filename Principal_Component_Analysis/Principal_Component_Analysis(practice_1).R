head(iris)
colSums(is.na(iris))
summary(iris)
boxplot(iris[, 1:4])
iris_pca <- prcomp(iris[1:4], center = T, scale. = T)
summary(iris_pca)
plot(iris_pca, type = 'l', main = "Scree Plot")

head(iris_pca$x[, 1:2], 10)

install.packages("ggfortify")
library(ggfortify)
autoplot(iris_pca, data = iris, colour = 'Species')
