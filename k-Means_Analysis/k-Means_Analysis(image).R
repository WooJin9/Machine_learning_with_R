library(jpeg)
img <- readJPEG('son_cat.jpg')
class(img)
dim(img)
imgdim <- as.vector(dim(img))
imgRGB <- data.frame(
    x = rep(1:imgdim[2], each = imgdim[1]),
    y = rep(imgdim[1]:1, imgdim[2]),
    R = as.vector(img[, , 1]),
    G = as.vector(img[, , 2]),
    B = as.vector(img[, , 3])
)
head(imgRGB)
tail(imgRGB)

kCluster <- c(3, 5, 10, 15)
set.seed(2020)
for(i in kCluster) {
    img_kmeans <- kmeans(imgRGB[, c("R", "G", "B")],
                         centers = i)
    img_result <- img_kmeans$centers[img_kmeans$cluster, ]
    img_array <- array(img_result, dim = imgdim)
    writeJPEG(img_array, paste("kmeans_", i, "cluster.jpeg", sep = ""))
}
