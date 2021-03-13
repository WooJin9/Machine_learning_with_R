install.packages("dplyr")
install.packages("ggplots2")
library(dplyr)
library(ggplot2)
head(mpg)
dim(mpg)
str(mpg)
summary(mpg)
View(mpg)

mpg %>%
    group_by(manufacturer) %>%
    summarise(mean.hwy=mean(hwy)) %>%
    arrange(desc(mean.hwy))

mpg %>%
    filter(manufacturer=="ford") %>%
    group_by(model) %>%
    arrange(desc(hwy))

lm.mpg <- lm(data=mpg, hwy ~ displ)
summary(lm.mpg)

qplot(data = mpg, x = displ, y = hwy)

library(ggplot2)
head(mpg)
mean(mpg$hwy)
a <-1
a
b <- 2
c <- 3
ab <- 3.5
a+b
a+b+c
4/b
5*b
d <- c(1,2,3,4,5)
e <- c(1:5)
f <- seq(1,5)
g <- seq(1,10, by=2)
d+2
d+e
a2 <- "a"
a2
b2 <- "text"
b2
c2 <- "Hello world!"
c2
d2 <- c("a", "b", "c")
d2
e2 <- c("Hello!", "world", "is", "good!")
b2+2
a <- c(1,2,3)
mean(a)
max(a)
min(a)
b <- c("a","a","b","c")
qplot(b)
paste(e2, collapse = " ")
e2_paste <- paste(e2, collapse = " ")
e2_paste
e3_paste <- paste(e2, collapse = ",")
e3_paste
head(a)
qplot(data = mpg, x = hwy)
qplot(data = mpg, x = cty)
qplot(data = mpg, y = hwy, x = drv, geom = "point")
qplot(data = mpg, y = hwy, x = drv, geom = "boxplot")
qplot(data = mpg, y = hwy, x = drv, geom = "boxplot", color = hwy)
?qplot
history <- c(90, 80, 60, 70)
math <- c(50, 60, 100, 20)
df_midterm <- data.frame(history, math)
class <- c(1, 1, 2, 2)
df_midterm <- data.frame(history, math, class)
mean(df_midterm$history)
mean(df_midterm$math)
library(ggplot2)
install.packages("readxl")
library(readxl)
df_finalexam <- read_excel("finalexam.xlsx", sheet = 1, col_names = T)
df_finalexam
mean(df_finalexam$math)
mean(df_finalexam$history)
mean(df_finalexam$english)
read.csv("csv_exam.csv", header = T)
?read.csv
csv_exam <- read.csv("csv_exam.csv", header = T)
write.csv(df_finalexam, file = "output_newdata.csv")

exam <- read.csv("csv_exam.csv")
head(exam)
head(exam, 10)
tail(exam)
tail(exam, 10)
View(exam)
dim(exam)
str(exam)
summary(exam)
mpg <- as.data.frame(ggplot2::mpg)
mpg
head(mpg)
head(mpg, 10)
tail(mpg)
tail(mpg, 10)
View(mpg)
summary(mpg)
dim(mpg)
structure(mpg)
str(mpg)
library(dplyr)
df_raw <- data.frame(var1 = c(1, 2, 1), var2 = c(1, 2, 3))
df_raw
df_new <- df_raw
df_new
df_new <- rename(df_new, v2 = var2)
df_new
mpg_raw <- as.data.frame(ggplot2::mpg)
mpg_new <- mpg_raw
mpg_new <- rename(mpg_new, city = cty)
mpg_new <- rename(mpg_new, highway = hwy)
head(mpg_new, 6)
df <- data.frame(var1 = c(2, 4, 6), var2 = c(2, 6, 1))
df
df$var_sum <- df$var1 + df$var2
df
df$var_mean <- df$var_sum/2
df
mpg$total <- (mpg$cty + mpg$hwy)/2
head(mpg)
mean(mpg$total)
summary(mpg$total)
hist(mpg$total)
mpg$test <- ifelse(mpg$total >=20, "pass", "fail")
head(mpg)
table(mpg$test)
library(ggplot2)
qplot(mpg$test)
mpg$grade = ifelse(mpg$total >= 30, "A", ifelse(mpg$total >= 20, "B", "C"))
head(mpg, 20)
table(mpg$grade)
qplot(mpg$grade)

Q1.
midwest_raw <- as.data.frame(ggplot2::midwest)
midwest_new <- midwest_raw
midwest_new

Q2.
library(dplyr)
midwest_new <- rename(midwest_new, total = totla)
midwest_new <- rename(midwest_new, asian = popasian)
str(midwest_new)

Q3.
midwest_new$percentage = (midwest_new$asian/midwest_new$total)*100
hist(midwest_new$percentage)
str(midwest_new)

Q4.
mean(midwest_new$percentage)
midwest_new$test = ifelse(midwest_new$percentage >= 0.4872462, "large", "small")
midwest_new$test

Q5.
table(midwest_new$test)
qplot(midwest_new$test)

library(dplyr)
exam <- read.csv("csv_exam.csv")
exam %>% filter(class == 1)
exam %>% filter(class == 2)
exam %>% filter(class != 1)
exam %>% filter(class != 3)
exam %>% filter(math > 50)
exam %>% filter(math < 50)
exam %>% filter(english >= 80)
exam %>% filter(english <=80)
exam %>% filter(class == 1 & math >=50)
exam %>% filter(class == 2 & english >=80)
exam %>% filter(math >=90 | english >=90)
exam %>% filter(english < 90 | science < 50)
exam %>% filter(class == 1 | class == 3 | class == 5)
exam %>% filter(class %in% c(1, 3, 5))
class1 <- exam %>% filter(class == 1)
class2 <- exam %>% filter(class == 2)
mean(class1$math)
mean(class2$math)

Q1. 
mpg_raw <- as.data.frame(ggplot2::mpg)
mpg_new <- mpg_raw
mpg_new1 <- mpg_new %>% filter(displ <= 4)
mpg_new2 <- mpg_new %>% filter(displ >= 5)
mean(mpg_new1$hwy)
mean(mpg_new2$hwy)

Q2. 
mpg_audi <- mpg_raw %>% filter(manufacturer == "audi")
mpg_toy <- mpg_raw %>% filter(manufacturer == "toyota")
mean(mpg_audi$cty)
mean(mpg_toy$cty)

Q3.
mpg_three <- mpg_new %>% filter(manufacturer %in% c("chavrolet", "ford", "honda"))
mean(mpg_three$hwy)

exam %>% select(math)
exam %>% select(english)
exam %>% select(class, math, english)
exam %>% select(-math)
exam %>% select(-math, -english)
exam %>% 
    filter(class == 1) %>% 
    select(english)
exam %>% 
    select(id, math) %>% 
    head(10)

Q1. 
mpg_raw <- as.data.frame(ggplot2::mpg)
mpg_CC <- mpg_raw %>% select("class", "cty")
mpg_CC

Q2. 
mpg_CC_suv <- mpg_CC %>% filter(class == "suv")
mpg_CC_compact <- mpg_CC %>% filter(class == "compact")
mean(mpg_CC_suv$cty)
mean(mpg_CC_compact$cty)

library(dplyr)
exam %>% arrange(math)
exam %>% arrange(desc(math))
exam %>% arrange(class, math)

mpg_raw %>% 
    filter(manufacturer == "audi") %>% 
    arrange(desc(hwy)) %>% 
    head(5)

exam %>% 
    mutate(total = math + english + science, mean = (math + english + science)/3) %>% 
    head
exam %>% 
    mutate(test = ifelse(science >= 60, "pass", "fail")) %>% 
    head
exam %>% 
    mutate(total = science + english + math) %>% 
    arrange(total) %>% 
    head

Q1.
mpg_raw <- as.data.frame(ggplot2::mpg)
mpg_new <- mpg_raw
mpg_new <- mpg_new %>% mutate(total = cty + hwy)

Q2.
mpg_new <- mpg_new %>% mutate(mean = total/2)

Q3.
mpg_new %>% 
    mutate(total = cty + hwy) %>% 
    mutate(mean = total / 2) %>% 
    arrange(desc(mean)) %>% 
    head(3)

Q4. 
mpg %>% 
    mutate(total = hwy + cty, mean = total / 2) %>% 
    arrage(desc(mean)) %>% 
    head(3)

exam %>% 
    group_by(class) %>% 
    summarise(mean_math = mean(math))

exam %>% 
    group_by(class) %>% 
    summarise(mean_math = mean(math), 
              sum_math = sum(math),
              median_math = median(math),
              n = n())

mpg %>% 
    group_by(manufacturer, drv) %>% 
    summarise(mean_cty = mean(cty)) %>% 
    head(10)

Q1.
mpg_raw %>% 
    group_by(class) %>% 
    summarise(mean_cty = mean(cty))

Q2. 
mpg_raw %>% 
    group_by(class) %>% 
    summarise(mean_cty = mean(cty)) %>% 
    arrange(desc(mean_cty))

Q3.
mpg_raw %>% 
    group_by(manufacturer) %>% 
    summarise(mean_hwy = mean(hwy)) %>% 
    arrange(desc(mean_hwy)) %>% 
    head(3)

Q4.
mpg_raw %>% 
    filter(class == "compact") %>% 
    group_by(manufacturer) %>% 
    summarise(count = n()) %>% 
    arrange(desc(count))

test1 <- data.frame(id = c(1, 2, 3, 4, 5),
                     midterm = c(60, 80, 70, 90, 85))
test2 <- data.frame(id = c(1, 2, 3, 4, 5),
                    final = c(70, 83, 65, 95, 80))
test1
test2
total <- left_join(test1, test2, by = "id")
total

name <- data.frame(class = c(1, 2, 3, 4, 5),
                   teacher = c("kim", "lee", "park", "choi", "jung"))
exam_T <- exam
exam
exam_T <- left_join(exam, name, by = "class")
exam_T
group_a <- data.frame(id = c(1, 2, 3, 4, 5),
                             test = c(60, 80, 70, 90, 85))
group_b <- data.frame(id = c(6, 7, 8, 9, 10), 
                      test = c(70, 83, 65, 95, 80))
group_a
group_b
group_all <- bind_rows(group_a, group_b)
group_all

fuel <- data.frame(fl = c("c", "d", "e", "p", "r"),
                   price_fl = c(2.35, 2.38, 2.11, 2.76, 2.22),
                   stringsAsFactors = F)
fuel
mpg_raw
Q1.
mpg <- as.data.frame(ggplot2::mpg)
mpg <- left_join(mpg, fuel, by = "fl")

Q2.
mpg %>% 
    select(model, fl, price_fl) %>% 
    head(5)

Q1.
midwest <- as.data.frame(ggplot2::midwest)
midwest <- midwest %>% mutate(popchild = poptotal - popadults)
midwest <- midwest %>% mutate(perchild = popchild / poptotal *100)

Q2.
midwest %>% 
    arrange(desc(perchild)) %>%
    select(county, perchild) %>% 
    head(5)

Q3.
midwest <- midwest %>% 
    mutate(scale = ifelse(perchild >= 40, "large", 
                          ifelse(perchild >=30, "middle", "small")))
table(midwest$scale)

Q4.
midwest <- midwest %>% 
    mutate(perasian = popasian / poptotal *100) %>% 
    arrange(perasian) %>% 
    select(state, county, perasian) %>% 
    head(10)
midwest
