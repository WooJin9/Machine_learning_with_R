install.packages("jpeg")
library(jpeg)

mother_cat <- readJPEG("mother_cat.jpg")
class(mother_cat)
dim(mother_cat)

r <- mother_cat[, , 1]
g <- mother_cat[, , 2]
b <- mother_cat[, , 3]

mother_cat_r_pca <- prcomp(r, center = F)
mother_cat_g_pca <- prcomp(g, center = F)
mother_cat_b_pca <- prcomp(b, center = F)

rgb_pca <- list(mother_cat_r_pca, mother_cat_g_pca, mother_cat_b_pca)

pc <- c(2, 10, 50, 100, 300)
for (i in pc) {
    pca_image <- sapply(rgb_pca, function(j) {
        compressed_img <- j$x[, 1:i]%*% t(j$rotation[, 1:i])
    }, simplify = "array")
    writeJPEG(pca_image, paste("mother_cat_pca_", i, ".jpeg", sep = ''))
}
