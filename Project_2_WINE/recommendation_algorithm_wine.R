# 추천 순서: 1. 최근 구매 와인 / 2. 1과 유사한 와인 + 가격 상 / 3. 1과 유사한 와인 + 가격 중 / 4. 1과 유사한 와인 + 가격 하 / 5. 판매량이 가장 높은 와인 / 6. 판매량이 두번째로 높은 와인 / 7. 1과 다른 성격의 와인 1 / 2. 1과 다른 성격의 와인 2

# H1. 고객들이 반복구매를 할 때 유사한 종류의 와인을 구매할 것이다.
# H2. 고객들은 간헐적으로 접해보지 못한 종류의 와인을 구매할 것이다.

df <- read.csv("wine.data.csv", header = T)
dim(df)
colnames(df) <- c("Cultivar", "Alcohol", "Malic acid", "Ash", "Alcalinity of ash", "Magnesium", "Total phenols", "Flavanoids", "Nonflavanoid phenols", "Proanthocyanins", "Color intensity", "Hue", "OD280/OD315 of diluted wines", "Proline")
head(df)
colSums(is.na(df))
summary(df[, -1])
boxplot(df[, -1], cex.axis = 0.5)
par(mfrow = c(1, 2))
boxplot(df[, -1], ylim = c(0, 200), cex.axis = 0.5)
boxplot(df[, -1], ylim = c(0, 30), cex.axis = 0.5)
dev.off()

# 표준화
df_train <- df[, -1]
df_train_scale <- scale(df_train) 

# euclidiean distance
df_dist <- dist(df_train_scale, method = "euclidean") %>% 
    as.matrix()

# K means
library(factoextra)
set.seed(2020)
fviz_nbclust(df_train_scale, kmeans, method = "wss", k.max = 15) +
    theme_minimal() +
    ggtitle("the Elbow Method")

fviz_nbclust(df_train_scale, kmeans, method = "silhouette", k.max = 15) +
    theme_minimal() +
    ggtitle("Silhouette Plot")

# K means > 군집화
df_kmeans <- kmeans(df_train_scale, center = 3, iter.max = 1000)
install.packages(("useful"))
library(useful)
plot(df_kmeans, df_train_scale)

# cluster별 평균 시각화
df_kmeans$centers
barplot(t(df_kmeans$centers), beside = T, col = 2:14)
legend("topleft", colnames(df_train_scale), fil = 2:14, cex = 0.3, bty = "n")

# raw data <- cluster
df$kmeans_cluster <- df_kmeans$cluster
head(df, 3)

# K medoids
library(cluster)
set.seed(2020)
fviz_nbclust(df_train_scale, pam, method = "wss", k.max = 15) +
    theme_minimal() +
    ggtitle("the Elbow Method")
fviz_nbclust(df_train_scale, pam, method = "silhouette", k.max = 15) +
    ggtitle("Silhouette Plot")

# K medoids > 군집화
library(cluster)
df_kmedoids <- pam(df_train_scale, k = 3)
plot(df_kmedoids)

# K medoids 군집별 중앙점 시각화
barplot(t(df_kmedoids$medoids), beside = T, col = 2:14, names.arg = c(1:3))
legend("bottomleft", colnames(df_train_scale), fill = 2:14, cex = 0.3, bty = "n")

# raw data <- cluster
df$kmedoids_cluster <- df_kmedoids$clustering

# Hierarchical Clustering
fviz_nbclust(df_train_scale, hcut, method = "wss", k.max = 15) +
    theme_minimal() +
    ggtitle("the Elbow method")
fviz_nbclust(df_train_scale, hcut, method = "silhouette", k.max = 15) +
    theme_minimal() +
    ggtitle("Silhouette Plot")

# 군집 구성 방식별 군집화
df_hclust_single <- hclust(dist(df_train_scale), method = 'single')
df_hclust_cplt <- hclust(dist(df_train_scale), method = "complete")
df_hclust_avg <- hclust(dist(df_train_scale), method = "average")
df_hclust_ward <- hclust(dist(df_train_scale), method = "ward.D")

# Dendrogram -> 군집 구성 방식 선택
par(mfrow = (c(2,2)))
plot(df_hclust_single, hang = -1, cex = 0.4)
rect.hclust(df_hclust_single, k = 3, border = "skyblue")
plot(df_hclust_cplt, hang = -1, cex = 0.4)
rect.hclust(df_hclust_cplt, k = 3, border = "skyblue")
plot(df_hclust_avg, hang = -1, cex = 0.4)
rect.hclust(df_hclust_avg, k =3, border = "skyblue")
plot(df_hclust_ward, hang = -1, cex = 0.4)
rect.hclust(df_hclust_ward, k = 3, border = "skyblue")

# 군집화 & 군집 할당
hclust_cluster <- cutree(df_hclust_ward, k = 3)
df$hclust_cluster <- hclust_cluster
head(df, 3)
