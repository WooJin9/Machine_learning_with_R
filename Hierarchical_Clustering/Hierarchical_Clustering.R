data_frame <- USArrests
head(data_frame)
colSums(is.na(data_frame))
summary(data_frame)
boxplot(data_frame)

# Standardization
library(dplyr)
data_frame_s <- scale(data_frame) %>% 
    as.data.frame()
boxplot(data_frame_s)

# Remove outlier
library(tibble)
df_rm_outlier <- data_frame_s %>% 
    rownames_to_column("rname") %>% 
    arrange(desc("Rape")) %>% 
    slice(-1:-2) %>% 
    column_to_rownames("rname")
boxplot(df_rm_outlier)

# Cluster Distance
df_dist <- dist(df_rm_outlier, method = "euclidean")

df_hclust_sing <- hclust(df_dist, method = "single")
df_hclust_cplt <- hclust(df_dist, method = "complete")
df_hclust_avg <- hclust(df_dist, method = "average")
df_hclust_cent <- hclust(df_dist, method = "centroid")
df_hclust_ward <- hclust(df_dist, method = "ward.D2")

# Dendrogram & Visualization(single)
plot(df_hclust_sing, cex = 0.6 , hang = -1)
rect.hclust(df_hclust_sing, k = 4, border = 2:5)

# Dendrogram & Visualization(complete)
plot(df_hclust_cplt, cex = 0.6 , hang = -1)
rect.hclust(df_hclust_cplt, k = 4, border = 2:5)

# Dendrogram & Visualization(average)
plot(df_hclust_avg, cex = 0.6 , hang = -1)
rect.hclust(df_hclust_avg, k = 4, border = 2:5)

# Dendrogram & Visualization(centroid)
plot(df_hclust_cent, cex = 0.6 , hang = -1)
rect.hclust(df_hclust_cent, k = 4, border = 2:5)

# Dendrogram & Visualization(ward)
plot(df_hclust_ward, cex = 0.6 , hang = -1)
rect.hclust(df_hclust_ward, k = 4, border = 2:5)

#
data_clusters <- cutree(df_hclust_ward, k = 4)
table(data_clusters)

df_rm_outlier$clusters <- data_clusters
head(df_rm_outlier)

# 2 Visualization
library(factoextra)
fviz_cluster(list(data = df_rm_outlier[, 1:ncol(df_rm_outlier)-1],
                  cluster = data_clusters))

# Average per group
library(reshape2)
temp <- df_rm_outlier %>% 
    melt(id = "clusters")
head(temp)

df_means <- dcast(temp, clusters ~ variable, mean)
df_means

barplot(t(df_means[, -1]), beside = T, col = 1:4, names.arg = c(1:4))
legend("topright", colnames(df_rm_outlier[1:4]), fill = 1:4, cex = 0.5)