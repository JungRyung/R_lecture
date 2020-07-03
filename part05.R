x=rnorm(100,175,2) # 정규분포를 가지는 100개의 난수생성
x
hist(x,breaks = 5, probability = T)
lines(density(x),col=4,type='h',lwd=0.5)
shapiro.test(x) # 정규성 검증

a<-1
a=2
a
b=5
a+b
var1 <- c(1,2,5,7,8)
var1
vae=c(1:5)

#seq()
var3=seq(1,5)
var3
var3=seq(1,10,by=2)
var3
var4=var3+1
var4

str1="a"
str1
str1="abcde"
str1
str5=c("i","am","a boy")
str5+2

x=c(1,4,7)
x
mean(x)
max(x)
min(x)
sd(x)
str5
paste(str5,collapse='-')


# package 설치 및 적용
install.packages("ggplot2")
library(ggplot2)
x <- c("a","a","b","c")
x
qplot(x)

qplot(data = mpg, x = hwy)


# 연습문제
student = c(80,60,70,50,90)
student
mean(student)
sd(student)


# 데이터 프레임
english <- c(90,80,60,70)
math <- c(50,60,100,20)
df_midterm=data.frame(english,math,class)
df_midterm
class <- c(1,1,2,2)
df_midterm$english # 데이터 참조
mean(df_midterm$english)
sd(df_midterm$english)
sqrt(var(df_midterm$english))

sale <-data.frame(fruit=c("사과","딸기","수박"),
                  price = c(1800,1500,3000),
                  volume = c(24,35,14))
sale
mean(sale$price)

install.packages("readxl")
library(readxl)
setwd("D:/development/R_lecture/Rdata")
write.csv(df_midterm, "df_midterm.csv")
df_mid_test <- read.csv("df_midterm.csv")
df_mid_test

plot.new()

hist(mpg$hwy,probability=T)
lines(density(mpg$hwy),col=2, type = 'h',lwd=1)
shapiro.test(mpg$hwy)

## exam 데이터 파악하기
exam <- read.csv("csv_exam.csv")
exam
head(exam)
tail(exam)
View(exam)
dim(exam)

str(exam)
summary(exam)
boxplot(exam$math, horizontal = T, col=4)
hist(exam$math)

x=sample(0:100,80,replace = T)
plot(x,pch=ifelse(x>=60,7,15), col=ifelse(x>=60,2,4))
abline(h=60, col=2, lwd=2)


## mpg 데이터 파악하기
mpg <- as.data.frame(ggplot2::mpg)
head(mpg)
tail(mpg)

View(mpg)
dim(mpg)
str(mpg)

## dplyr 패키지
install.packages("dplyr")
library(dplyr)

df_raw <- data.frame(var1 = c(1, 2, 1),
                     var2 = c(2, 3, 2))
df_raw
df_new <- df_raw  # 복사본 생성
df_new

# rename() 
install.packages("reshape")
library(reshape)

df_new
rename(df_new, v2 = var2)  # var2를 v2로 수정
df_new



## 파생변수 만들기
df <- data.frame(var1 = c(4,3,8),
                 var2 = c(2,6,1))
df$var_sum = df$var1 + df$var2
df$var_mean = (df$var1 + df$var2)/2

mpg$total <- (mpg$cty + mpg$hwy)/2
head(mpg)
summary(mpg$total)
boxplot(mpg$total, horizontal = T)
hist(mpg$total)
mpg$test = ifelse(mpg$total>=20,'pass','fail')
qplot(mpg$test)
head(mpg,3)
table(mpg$test)
mpg$grade <- ifelse(mpg$total >= 30, "A",
                    ifelse(mpg$total >= 20, "B", "C"))
qplot(mpg$grade)

# A, B, C, D 등급 부여
mpg$grade2 <- ifelse(mpg$total >= 30, "A",
                     ifelse(mpg$total >= 25, "B",
                            ifelse(mpg$total >= 20, "C", "D")))
qplot(mpg$grade2)
