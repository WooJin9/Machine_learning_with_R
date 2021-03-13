raw_chisq <- read.csv("htest05.csv", header = T)
View(raw_chisq)
rawTable <- table(raw_chisq)
rawTable

chisq.test(rawTable, correct = F)
