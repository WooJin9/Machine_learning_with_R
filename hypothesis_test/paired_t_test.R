raw_d <- read.csv("htest02d.csv", header = T)
View(raw_d)
groupAd <- raw_d[,1]
groupBd <- raw_d[,2]

mean(groupAd)
mean(groupBd)

d = groupAd-groupBd
shapiro.test(d)
qqnorm(d)
qqline(d)

t.test(groupAd, groupBd, alternative = "less", paired = T)
