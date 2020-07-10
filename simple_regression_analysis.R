setwd("~/Desktop/Development/R_lecture/Rdata")
View(attitude)
cov(attitude)
# 공분산
cor(attitude)
with(attitude,cor.test(rating, complaints))
cor.test(attitude$rating,attitude$complaints)
plot(attitude$rating,attitude$complaints)

#단순 회귀식 -> 기울기 절편 계산
fa=c(150,160,170,180,190)
su=c(176,112,142,190,185)
fasu=data.frame(fa,su)
lm(su~fa,data=fasu)


data=read.csv("cars.csv")
data

out = lm(dist~speed,data=data)
summary(out)

plot(dist~speed,data=data,col="blue")
abline(out,col="red")
# 절편값을 0으로 고정
out1 = lm(dist~speed+0,data=data)
summary(lm(dist~speed+0,data=data))
plot(out1)
par(mfrow=c(2,2))
plot(out1)
shapiro.test(data$dist)
shapiro.test(log(data$dist))
shapiro.test(sqrt(data$dist))

out3=lm(sqrt(dist)~speed+0,data=data)
summary(out3)
plot(out3)


# 추정과 예측
out3$fitted.values
cbind(data$speed,out3$fitted.values)
new=data.frame(speed=data$speed)
cbind(new$speed,predict(out3,new,interval="confidence"))
cbind(new$speed,predict(out3,new,interval="prediction"))

# 다중 회귀 분석
data=read.csv("salary.csv")
head(data)
out=lm(salary~experience+score,data=data)
summary(out)

# 잔차분석
plot(out)

# 추정과 예측
cbind(data$experience, data$score, out$fitted.values)

# 다중 공선성
summary(lm(rating~complaints+learning,data=attitude))
summary(lm(rating~learning,data=attitude))

# Backward Selection
out=lm(rating~.-critical,data=attitude)
summary(out)
backward=step(out,direction = "backward",trace=FALSE)
summary(backward)
both=step(out,direction = "both",trace=FALSE)
summary(both)

# ALL subset method
install.packages('leaps')
library(leaps)
leap=regsubsets(rating~.,data=attitude,nbest=5)
summary(leap)

plot(leap)
out_bic=glm(rating~complaints,data=attitude)
summary(out_bic)
plot(leap,scale="Cp")
out_cp=lm(rating~complaints+learning,data=attitude)
summary(out_cp)
plot(leap,scale="adjr2")
out_adjr=lm(rating~complaints+learning+advance,data=attitude)
summary(out_adjr)
