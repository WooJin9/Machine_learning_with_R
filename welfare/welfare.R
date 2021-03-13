install.packages("foreign")

library(foreign)
library(dplyr)
library(ggplot2)

raw_welfare <- read.spss("data_spss_Koweps2014.sav", to.data.frame = T)
welfare <- raw_welfare
dim(welfare)
str(welfare)
head(welfare)
summary(welfare)
View(welfare)

welfare <- rename(welfare,
                  sex = h0901_4,
                  birth = h0901_5,
                  income = h09_din)

class(welfare$sex)
summary(welfare$sex)
table(welfare$sex)
welfare$sex <- ifelse(welfare$sex == 1, "male", "female")
table(welfare$sex)
qplot(welfare$sex)

class(welfare$income)
summary(welfare$income)
qplot(welfare$income) + xlim(0, 10000)
table(is.na(welfare$income))

sex_income <- welfare %>% 
    group_by(sex) %>% 
    summarise(mean_income = mean(income))
sex_income
ggplot(data = sex_income, aes(x = sex, y = mean_income)) + geom_col()

class(welfare$birth)
summary(welfare$birth)
qplot(welfare$birth)

welfare$age <- 2014-welfare$birth+1
summary(welfare$age)
qplot(welfare$age)

age_income <- welfare %>% 
    group_by(age) %>% 
    summarise(mean_income = mean(income))
age_income
ggplot(data = age_income, aes(x = age, y = mean_income)) + geom_point()

welfare <- welfare %>% 
    mutate(ageg = ifelse(age < 30, "young", ifelse(age <= 59, "middle", "old")))
table(welfare$ageg)
qplot(welfare$ageg)

welfare_income <- welfare %>% 
    filter(ageg !="young") %>% 
    group_by(ageg) %>% 
    summarise(mean_income = mean(income))
welfare_income
ggplot(data = welfare_income, aes(x = ageg, y = mean_income)) + geom_col()

sex_income <- welfare %>% 
    filter(ageg != "young") %>% 
    group_by(ageg, sex) %>% 
    summarise(mean_income = mean(income))

sex_income
ggplot(data = sex_income, aes(x = ageg, y = mean_income, fill = sex)) + geom_col(position = "dodge")
