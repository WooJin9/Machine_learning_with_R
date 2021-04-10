df <- read.csv("code.csv", header = T)
colSums(is.na(df))
df <- na.omit(df)
View(df)
str(df)

df_1 <- df[2:43]
corMat<- cor(df_1, use = "pairwise.complete.obs") 
View(round(corMat, 2))

library(qgraph)
corMat <- cor_auto(df_1) 
View(round(corMat, 2))
Graph_pcor_1 <- qgraph(corMat, graph = "pcor", layout = "spring")
Graph_pcor_2 <- qgraph(corMat, graph = "pcor", layout = "spring",
                     threshold = "bonferroni",
                     sampleSize = nrow(df_1), alpha = 0.05)

Graph_lasso <- qgraph(corMat, graph = "glasso", layout = "spring",
                      tuning = 0.25,
                      sampleSize = nrow(df_1))

centRes_1 <- centrality(Graph_lasso)
centRes_1$OutDegree
centRes_1$Closeness
centRes_1$Betweenness

centRes_2 <- centrality(Graph_pcor_2)
centRes_2$OutDegree
centRes_22Closeness
centRes_2$Betweenness

centralityPlot(Graph_lasso)
centralityPlot(Graph_pcor_1)
centralityPlot(Graph_pcor_2)

Layout <- averageLayout(Graph_pcor_2,Graph_lasso)
layout(t(1:2))
qgraph(corMat, graph = "pcor", layout = Layout, threshold = "bonferroni",
       sampleSize = nrow(df_1), minimum = 0,
       cut = 0.15, maximum = 1, details = TRUE,
       esize = 20, title = "Partial correlations")

qgraph(corMat, graph = "glasso", layout = Layout, tuning = 0.25,
       sampleSize = nrow(df_1), minimum = 0,
       cut = 0.15, maximum = 1, details = TRUE,
       esize = 20, title = "LASSO regularization")

