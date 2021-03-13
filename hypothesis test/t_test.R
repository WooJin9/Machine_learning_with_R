rawN3 <- read.csv(file="htest01.csv", header = T)
View(rawN3)
groupA <- rawN3[rawN3$group == 'A', 1:2]
groupB <- rawN3[rawN3$group == 'B', 1:2]

mean(groupA[,2])
mean(groupB[,2])

shapiro.test(groupA[,2])
qqnorm(groupA[,2])
qqline(groupA[,2])

shapiro.test(groupB[,2])
qqnorm(groupB[,2])
qqline(groupB[,2])

var.test(groupA[,2], groupB[,2])

t.test(groupA[,2], groupB[,2], alternative = "less", var.equal = T)

rawN10 <- read.csv("htest02.csv", header = T)
View(rawN10)
groupA2 <- rawN10[rawN10$group == 'A', 1:2]
groupB2 <- rawN10[rawN10$group == 'B', 1:2]

mean(groupA2[,2])
mean(groupB2[,2])

shapiro.test(groupA2[,2])
qqnorm(groupA2[,2])
qqline(groupA2[,2])

shapiro.test(groupB2[,2])
qqnorm(groupB2[,2])
qqline(groupB2[,2])

var.test(groupA2[,2], groupB2[,2])
t.test(groupA2[,2], groupB2[,2], alternative = "less", var.equal = F)
