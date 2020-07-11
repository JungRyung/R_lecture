setwd("../Rdata")
View(attitude)
cov(attitude)     # 공분산 함수
cor(attitude)     # 상관계수
with(attitude,cor.test(rating, complaints))
cor.test(attitude$rating,attitude$complaints)
plot(attitude$rating,attitude$complaints)

fa=c(150,160,170,180,190)
su=c(176,179,182,178,185)
fasu=data.frame(fa,su)
fasu
lm(su~fa,data=fasu)
data=read.csv("cars.csv")
data
out=lm(dist~speed,data=data)
summary(out)

plot(dist~speed,data=cars,col="blue")
abline(out,col="red")
out1=lm(dist~speed+0,data=data)
summary(lm(dist~speed+0,data=data))
plot(out1)
par(mfrow=c(1,1))
plot(out1)
shapiro.test(data$dist)
shapiro.test(log(data$dist))
shapiro.test(sqrt(data$dist))
out3=lm(sqrt(dist)~speed+0,data=data)
summary(out3)
plot(out3)

out3$fitted.values
cbind(data$speed,out3$fitted.values)
new=data.frame(speed=data$speed)
cbind(new$speed,predict(out3,new,interval = "confidence"))
cbind(new$speed,predict(out3,new,interval = "prediction"))
      
#????ȸ?ͺм?
data=read.csv("salary.csv")
head(data)
out=lm(salary~experience+score,data=data)
summary(out)
plot(out)
cbind(data$experience, data$score, out$fitted.values)


summary(lm(rating~complaints+learning,data=attitude))
summary(lm(rating~learning,data=attitude))
out=lm(rating~.-critical,data=attitude)
summary(out)
backward=step(out,direction="backward",trace=FALSE)
summary(backward)
both=step(out,direction="both",trace=FALSE)
summary(both)
##All subset method
install.packages("leaps")
library(leaps)
leaps=regsubsets(rating~.,data=attitude,nbest=5)
summary(leaps)

plot(leaps,scale='bic')
out_bic=lm(rating~complaints,data=attitude)
summary(out_bic)
plot(leaps,scale='Cp')
out_cp=lm(rating~complaints+learning,data=attitude)
summary(out_cp)
plot(leaps,scale="adjr2")
out_adjr=lm(rating~complaints+learning+advance,data=attitude)
summary(out_adjr)
