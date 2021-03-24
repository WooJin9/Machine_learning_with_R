# H1. 서울 전체 아파트와 강남 아파트의 가격은 변화하는 양상이 다를 것이다.
# H2. 강남 아파트의 가격 변화를 따라서 후행적으로 가격이 변화하는 지역이 있을 것이다.

# 전처리
df <- read.csv("apt_data_2010_2020.csv", header = T)
str(df)

library(dplyr)
df$거래금액 <- gsub(",", "", df$거래금액) %>% as.integer()
df$법정동 <- gsub(" ", "", df$법정동)

df$qrt <- ifelse(df$월 < 4, "Q1", ifelse(df$월 < 7, "Q2", 
                                        ifelse(df$월 < 10, "Q3", "Q4")))
df$yyyyqrt <- paste0(df$년, df$qrt)
df$평수 <- round(df$전용면적 / 3.3)
df$평단가 <- df$거래금액 /df$평수

# H1 검정
# 서울 전체 아파트 매매가격 추이
df_1 <- df %>% 
    group_by(yyyyqrt) %>% 
    summarise(`평균평단가` = mean(`평단가`))

library(ggplot2)
theme_set(theme_grey(base_family = "TT Arial"))
ggplot(df_1, aes(x = yyyyqrt, y = `평균평단가`, group = 1))+
    geom_line() +
    xlab("년도 / 분기") + ylab("평균 가격 (만원)") +
    ggtitle("서울 아파트 평당 가격 변화 추이") +
    theme(axis.text.x = element_text(angle = 90)) + 
    stat_smooth(method = "lm") + 
    ylim(0, max(df_1$평균평단가))

# 강남 주요 동별 가격 변화 추이 비교
# 반포동

dong <- "반포동"
df_2 <- df %>% filter(`법정동` == dong)
df_2 <- df_2 %>% group_by(yyyyqrt) %>% 
    summarise(`평균평단가` = mean(`평단가`))

df_2_plot <- ggplot(df_2, aes(x = yyyyqrt, y = `평균평단가`, group = 1)) + 
    geom_line() + xlab("년도 / 분기") + ylab("평균 가격 (만원)") +
    ggtitle(paste0(dong, "아파트 평당 가격 변화 추이")) +
    theme (axis.text.x = element_text(angle = 90)) +
    stat_smooth(method = "lm") +
    ylim(0, max(df_2$평균평단가))

# 서초동
dong <- "서초동"
df_3 <- df %>% filter(`법정동` == dong)
df_3 <- df_3 %>% group_by(yyyyqrt) %>% 
    summarise(`평균평단가` = mean(`평단가`))

df_3_plot <- ggplot(df_3, aes(x = yyyyqrt, y = `평균평단가`,  group = 1)) +
    geom_line() + xlab("년도 / 분기") + ylab("평균 가격 (만원)") +
    ggtitle(paste0(dong, "아파트 평당 가격 변화 추이")) +
    theme (axis.text.x = element_text(angle = 90)) +
    stat_smooth(method = "lm") +
    ylim(0, max(df_3$평균평단가))

# 삼성동
dong <- "삼성동"
df_4 <- df %>% filter(`법정동` == dong)
df_4 <- df_4 %>% group_by(yyyyqrt) %>% 
    summarise(`평균평단가` = mean(`평단가`))

df_4_plot <- ggplot(df_4, aes(x = yyyyqrt, y = `평균평단가`, group = 1)) +
    geom_line() + xlab("년도 / 분기") + ylab("평균 가격 (만원)") +
    ggtitle(paste0(dong, "아파트 평당 가격 변화 추이")) +
    theme (axis.text.x = element_text(angle = 90)) +
    stat_smooth(method = "lm") +
    ylim(0, max(df_4$평균평단가))

# 압구정동
dong <- "압구정동"
df_5 <- df %>% filter(`법정동` == dong)
df_5 <- df_5 %>% group_by(yyyyqrt) %>% 
    summarise(`평균평단가` = mean(`평단가`))

df_5_plot <- ggplot(df_5, aes(x = yyyyqrt, y = `평균평단가`, group = 1)) +
    geom_line() + xlab("년도 / 분기") + ylab("평균 가격 (만원)") +
    ggtitle(paste0(dong, "아파트 평당 가격 변화 추이")) +
    theme (axis.text.x = element_text(angle = 90)) +
    stat_smooth(method = "lm") +
    ylim(0, max(df_5$평균평단가))

library(gridExtra)
grid.arrange(df_2_plot, df_3_plot, df_4_plot, df_5_plot, nrow = 2, ncol = 2)

# H2. 강남 아파트의 가격 변화를 따라서 후행적으로 가격이 변화하는 지역이 있을 것이다.

df_fil <- df %>% filter(!yyyyqrt %in% c("2020Q1", "2020Q2")) 
dong_list <- unique(df$법정동)

df_ts <- NULL
library(TTR)
for(i in dong_list) {
    print(i)
    temp <- merge(i, df_fil %>% filter(`법정동` == i) %>% 
                    group_by(yyyyqrt) %>% 
                      summarise(`평균평단가` = mean(`평단가`)) %>% 
                      mutate(ma3 = runMean(`평균평단가`, 3)))
df_ts <- rbind(df_ts, temp) %>% 
    na.omit()
}

dong_list <- df_ts %>% group_by(x) %>% 
    summarise(cnt = n_distinct(yyyyqrt)) %>% 
    filter(cnt == 38) %>% 
    select(x)

df_ts_banpo <- df_ts %>% filter(x == "반포동")
df_ts <- df_ts %>% filter(x %in%dong_list$x, x !="반포동")

df_trend <- list()
for (i in dong_list$x) {
    print(i)
    temp <- df_ts %>% filter(x == i) %>% 
        select(ma3) %>% 
        as.matrix()
    df_trend[[i]] <- temp[, 1]
}

df_trend <- as.data.frame(do.call(cbind, df_trend))

df_trend_banpo <- df_ts_banpo[-38, ] %>% 
    select(ma3)
colnames(df_trend_banpo) <- "반포동"

df_trend <- df_trend[-1, ]
df_trend <- cbind(df_trend, df_trend_banpo)
df_cor <- cor(df_trend)

# Hcluster
hc <- hclust(as.dist(df_cor), method = "ward.D2")
plot(hc, hang = -1, cex = 0.35) 

# correlation
cor_banpo <- df_cor[197, 1:197] %>% as.data.frame

par(mfrow = (c(2, 3)))
plot(df_trend$반포동, type = "l")
plot(df_trend$양평동5가, type = "l")
plot(df_trend$사당동, type = "l")
plot(df_trend$문래동6가, type = "l")
plot(df_trend$가락동, type = "l")
plot(df_trend$가산동, type = "l")