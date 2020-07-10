#setwd("D:/development/R_lecture/Rdata")
setwd("/Users/ryung/Desktop/Development/R_lecture/Rdata")

data=read.csv("programming.csv")
head(data)
model=glm(Success~Experience,data=data,family = binomial(logit))
summary(model)
cbind(data$Experience, model$fitted.values)
plot(Success~Experience, data=data)
points(model$fitted.values~data$Experience,col=2)

# confusion matrix
table(data$Success,model$fitted.values>0.5)
c('민감도'=8/11, '특이도'=11/14) #민감도=true를 true로 답한 비율, 특이도=false를 false로 답한 비율

# 쿠폰의 할인율과 재구매의 상관관계 및 예측
data = read.csv("coupon.csv")
head(data)
model2 = glm(cbind(N_redeemed, N-N_redeemed)~Price_reduc,
             data=data,family = binomial(logit))
summary(model2)

# Multiple Logistic Regression
# ex) Disease Outbreak
data = read.csv("disease.csv")
# full model
model3 = glm(disease~. , data=data, family = binomial(logit))
summary(model3)

# reduced model
model4 = glm(disease~age+sector,data=data,family=binomial(logit))
summary(model4)
anova(model3,model4,test='Chisq') # 독립성 검증


table(data$disease)
31/98
kk=table(data$disease,model4$fitted.values>0.3163265)
kk
sum(kk)
reduce_M=c('민감도'=23/31, '특이도'=47/(47+20))
kk1=table(data$disease,model3$fitted.values>0.3163265)
kk1
fullmode_M=c('민감도'=23/31, '특이도'=49/(49+18))
reduce_M
fullmode_M
err_m1=28/sum(kk)
err_m2=26/sum(kk1)
err_m1
err_m2
install.packages("Deducer")
library(Deducer)
rocplot(model3) # 아래면적이 더 넓을수록 좋은 모델
rocplot(model4)


