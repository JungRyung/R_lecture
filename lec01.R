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
