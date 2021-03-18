rawN30 <- read.csv("htest03.csv", header = T)
View(rawN30)

groupA3 <- rawN30[rawN30$group == 'A', 1:2]
groupB3 <- rawN30[rawN30$group == 'B', 1:2]
View(groupA3)

mean(groupA3[,2])
mean(groupB3[,2])

z.test <- function(x1, x2){
    n_x1 = length(x1)
    n_x2 = length(x2)
    mean_x1 = mean(x1)
    mean_x2 = mean(x2)
    cat("\n")
    cat("\tTwo Sample z-test\n")
    cat("\n")
    cat("mean of x1:", mean_x1, "\n")
    cat("mean of x2:", mean_x2, "\n")
    var_x1 = var(x1)
    var_x2 = var(x2)
    z = (mean_x1 - mean_x2)/sqrt((var_x1/n_x1)+(var_x2/n_x2))
    abs_z = abs(z)
    cat("z =", abs_z, "\n")
    p_value = 1-pnorm(abs_z)
    cat("p-value =", p_value)
}

z.test(groupA3[,2], groupB3[,2])
