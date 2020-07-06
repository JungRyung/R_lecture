df = data.frame(gender = c("M", "F", NA, "M", "F"),
                score = c(5,4,3,4,NA))
df
is.na(df)   # 결측치 확인
table(is.na(df))   # 결측치 빈도 출력

table(is.na(df$gender))   # gender 결측치 빈도 출력
table(is.na(df$score))  # score 결측치 빈도 출력

mean(df$score)   # 결측치 포함된 평균 산출
sum(df$score)   # 결측치 포함된 합계 산출

library(dplyr)
kk=df %>% filter(is.na(score))   # score가 NA인 데이터만 
mean(kk$score)
sum(kk$score)

kk=df %>% filter(!is.na(score))   # score의 결측치 제거
mean(kk$score)
sum(kk$score)

na.omit(df)

mean(df$score,na.rm=T)   # 일시적으로 결측치 제외하고 평균 산출
sum(df$score, na.rm=T)   # 일시적으로 결측치 제외하고 합계 산출

setwd("D:/development/R_lecture/Rdata")
exam <- read.csv("csv_exam.csv")
exam

exam[c(3,8,15),"math"] <- NA # 3,8,15행의 math에 NA 할당
exam

exam %>% summarise(mean_math = mean(math))   # 평균 산출
exam %>% summarise(mean_math = round(mean(math, na.rm=T)))   # 결측치 제외하고 평균 산출

exam %>% summarise(mean_math = mean(math, na.rm=T),
                   sum_math = sum(math, na.rm=T),
                   median_math = median(math, na.rm=T))

## 평균값으로 결측치 대체하기
kk=table(is.na(exam$math))
tt=barplot(kk, col=rainbow(2), ylim=c(0,20))
text(tt,kk,label=paste0(kk,"건"),pos=3)

mean(exam$math, na.rm=T)   # 결측치 제외하고 math 평균 산출
exam$math <- ifelse(is.na(exam$math),55,exam$math) # math가 NA면 55로 대체
table(is.na(exam$math))   # 결측치 빈도표 생성
exam
mean(exam$math)   # math 평균 산출


# 07-2 이상치 제거하기
## 이상치 포함된 데이터 생성
outlier <- data.frame(gender=c(1,2,1,3,2,1),
                      score=c(5,4,3,4,2,6))
outlier

## 이상치 확인하기
table(outlier$gender)
table(outlier$score)

## gender가 3이면 NA할당
outlier$gender <- ifelse(outlier$gender==3,NA,outlier$gender)
outlier

## score가 1~5 아니면 NA 할당
outlier$score <- ifelse(outlier$score > 5, NA, outlier$score)
outlier

## 결측치 제외하고 분석
outlier %>%
  group_by(gender,na.rm=T)%>%
  summarise(mean_score=mean(score,na.rm=T))

outlier %>%
  filter(!is.na(gender) & !is.na(score))%>%
  group_by(gender)%>%
  summarise(mean_score=mean(score))

## 이상치 제거하기 - 2. 극단적인 값
mpg <- as.data.frame(ggplot2::mpg)
boxplot(mpg$hwy,horizontal = T, col=2)
summary(mpg$hwy)
IQR(mpg$hwy) # 25% ~ 75% 구간의 길이
hist(mpg$hwy,probability = T) # 확률밀도
lines(density(mpg$hwy),col=2,type='h')

boxplot(mpg$hwy)$stats # 상자그림 통계치 출력

mpg$hwy=ifelse(mpg$hwy<12 | mpg$hwy>37,NA,mpg$hwy) # 이상치를 NA로 변경
kk=table(is.na(mpg$hwy))
tt=barplot(kk,col=rainbow(2),ylim=c(0,250))
text(tt,kk,pos=3,label=paste0(kk,"건"),col='4',cex=2)

mpg%>%
  group_by(drv)%>%
  summarise(mean_hwy=mean(hwy,na.rm=T))
