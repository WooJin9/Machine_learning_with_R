raw_anova <- read.csv("htest04.csv", header = T)
View(raw_anova)

groupA4 <- raw_anova[raw_anova$group == 'A', 1:2]
groupB4 <- raw_anova[raw_anova$group == 'B', 1:2]
groupC4 <- raw_anova[raw_anova$group == 'C', 1:2]

mean(groupA4[, 2])
mean(groupB4[, 2])
mean(groupC4[, 2])

#정규성 검정
shapiro.test(groupA4[, 2])
qqnorm(groupA4[, 2])
qqline(groupA4[, 2])

shapiro.test(groupB4[, 2])
qqnorm(groupB4[, 2])
qqline(groupB4[, 2])

shapiro.test(groupC4[, 2])
qqnorm(groupC4[, 2])
qqline(groupC4[, 2])

# 분산 동질성 검정(levene, bartlett)
install.packages("lawstat")
library(lawstat)

levene.test(raw_anova$height, raw_anova$group)
bartlett.test(height ~ group, data = raw_anova)

rawAnova <- aov(height ~ group, data = raw_anova)
summary(rawAnova)