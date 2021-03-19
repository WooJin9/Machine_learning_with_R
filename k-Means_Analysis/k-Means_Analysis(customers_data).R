customers_data <- read.csv("Wholesale customers data.csv",
                           stringsAsFactors = F, header = T)

library(dplyr)
head(customers_data)
customers_data$Channel <- as.factor(customers_data$Channel)
customers_data$Region <- as.factor(customers_data$Region)
colSums(is.na(customers_data))
summary(customers_data)

boxplot(customers_data[, 3:ncol(customers_data)])
options(scipen = 100)
boxplot(customers_data[ ,3:ncol(customers_data)])

# remove outlier
temp <- NULL
for (i in 3:ncol(customers_data)) {
    temp <- rbind(temp, customers_data[order(customers_data[, 3],
                                             decreasing = T), ] %>% 
                      slice(1:5))
}
temp %>% 
    arrange(Fresh) %>% 
    head()

temp <- distinct(temp)
data_rm_outlier <- anti_join(customers_data, temp)

par(mfrow = c(1, 2))
boxplot(customers_data[ , 3:ncol(customers_data)])
boxplot(data_rm_outlier[ , 3:ncol(data_rm_outlier) ])

# Elbow method
install.packages("factoextra")
library(factoextra)

set.seed(2020)
fviz_nbclust(data_rm_outlier[ , 3:ncol(data_rm_outlier)], kmeans, 
             method = "wss", k.max = 15) + 
    theme_minimal() +
                 ggtitle("Elbow_Method")

# Silhouette method
fviz_nbclust(data_rm_outlier[ , 3:ncol(data_rm_outlier)], kmeans,
             method = "silhouette", k.max = 15) +
    theme_minimal() +
    ggtitle("Silhouette Plot")

# Model
data_kmeans <- kmeans(data_rm_outlier[ , 3:ncol(data_rm_outlier)],
                      center = 5,
                      iter.max = 1000)
data_kmeans

# Visualization
barplot(t(data_kmeans$centers), beside = T, col = 1:6)
legend("topleft", colnames(customers_data[ , 3:8]), 
                           fill = 1:6, cex = 0.5)

data_rm_outlier$cluster <- data_kmeans$cluster
head(data_rm_outlier)