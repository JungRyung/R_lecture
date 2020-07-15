setwd("D:/development/R_lecture/term")
getwd()
#par(family = "AppleGothic")

data = read.csv("sales_data_new.csv",encoding = "UTF-8")
head(data)

data[,3:10]

## 건강음료에 대한 다중 회귀 분석
helth_beverage=data[1:60,]
helth_beverage
helth_beverage_feature = helth_beverage[4:10]
helth_beverage_feature
#vif(helth_beverage_feature)
cov(helth_beverage_feature)
cor(helth_beverage_feature)

model = lm(QTY~.,helth_beverage_feature)
summary(model)

## Forward selection
model = lm(QTY~SALEDAY,helth_beverage_feature)
summary(model)
model = lm(QTY~SALEDAY+PRICE,helth_beverage_feature)
summary(model)
model = lm(QTY~SALEDAY+PRICE+ITEM_CNT,helth_beverage_feature)
summary(model)
model = lm(QTY~SALEDAY+PRICE+ITEM_CNT+MAXTEMP,helth_beverage_feature)
summary(model)
model = lm(QTY~SALEDAY+PRICE+ITEM_CNT+MAXTEMP+RAIN_DAY,helth_beverage_feature)
summary(model)
model = lm(QTY~SALEDAY+PRICE+ITEM_CNT+MAXTEMP+RAIN_DAY+HOLIDAY,helth_beverage_feature)
summary(model)

model = step(lm(QTY~.,data=helth_beverage_feature),direction="both")
summary(model)

## All Subsets Regression
library(leaps)
leaps=regsubsets(QTY~.,data=helth_beverage_feature,nbest=5)
summary(leaps)
plot(leaps)
#best
model = lm(QTY~SALEDAY+PRICE+RAIN_DAY,helth_beverage_feature)
summary(model)

helth_beverage_feature_train = helth_beverage_feature[1:42,]
helth_beverage_feature_test = helth_beverage_feature[42:60,]
dim(helth_beverage_feature_train)
dim(helth_beverage_feature_test)
## 모든 feature를 포함한 다중회귀
lm.fit = lm(QTY~., data = helth_beverage_feature)
summary(lm.fit)

## forward와 backward 방식을 전부 써서 좋은 결과만 가져온다.(both)
lm.fit2 = step(lm.fit,method="both")
summary(lm.fit2)
lm.yhat2=predict(lm.fit2,newdata=helth_beverage_feature)
kk=mean((lm.yhat2-helth_beverage_feature$QTY)^2)
sqrt(kk)
plot(lm.yhat2,helth_beverage_feature$QTY)
abline(a=0,b=1,col=2)

## 잔차분석
anova(lm.fit2)
plot(lm.fit2)


## 과즙음료에 대한 다중 회귀 분석
juice_beverage=data[61:120,]
juice_beverage
juice_beverage_feature = juice_beverage[4:10]
juice_beverage_feature
cov(juice_beverage_feature)
cor(juice_beverage_feature)

lm.fit = lm(QTY~., data = juice_beverage_feature)
summary(lm.fit)

## Forward selection
model = lm(QTY~SALEDAY,juice_beverage_feature)
summary(model)
model = lm(QTY~SALEDAY+PRICE,juice_beverage_feature)
summary(model)
model = lm(QTY~SALEDAY+PRICE+ITEM_CNT,juice_beverage_feature)
summary(model)
model = lm(QTY~SALEDAY+PRICE+ITEM_CNT+MAXTEMP,juice_beverage_feature)
summary(model)
model = lm(QTY~SALEDAY+PRICE+ITEM_CNT+MAXTEMP+RAIN_DAY,juice_beverage_feature)
summary(model)
model = lm(QTY~SALEDAY+PRICE+ITEM_CNT+MAXTEMP+RAIN_DAY+HOLIDAY,juice_beverage_feature)
summary(model)

## All Subsets Regression
library(leaps)
leaps=regsubsets(QTY~.,data=juice_beverage_feature,nbest=6)
summary(leaps)
plot(leaps)
#best
model = lm(QTY~ITEM_CNT+PRICE+MAXTEMP+SALEDAY+RAIN_DAY,juice_beverage_feature)
summary(model)

## 모든 feature를 포함한 다중회귀
lm.fit = lm(QTY~., data = juice_beverage_feature)
summary(lm.fit)

## forward와 backward 방식을 전부 써서 좋은 결과만 가져온다.(both)
lm.fit2 = step(lm.fit,method="both")
summary(lm.fit2)
lm.yhat2=predict(lm.fit2,newdata=juice_beverage_feature_test)
kk=mean((lm.yhat2-Boston_test$medv)^2)
sqrt(kk)
plot(lm.yhat2,Boston_test$medv)
abline(a=0,b=1,col=2)


## 차 음료에 대한 다중 회귀 분석
tea_beverage=data[120:180,]
tea_beverage
tea_beverage_feature = tea_beverage[4:10]
tea_beverage_feature
cov(tea_beverage_feature)
cor(tea_beverage_feature)

## Forward selection
model = lm(QTY~SALEDAY,tea_beverage_feature)
summary(model)
model = lm(QTY~SALEDAY+PRICE,tea_beverage_feature)
summary(model)
model = lm(QTY~SALEDAY+PRICE+ITEM_CNT,tea_beverage_feature)
summary(model)
model = lm(QTY~SALEDAY+PRICE+ITEM_CNT+MAXTEMP,tea_beverage_feature)
summary(model)
model = lm(QTY~SALEDAY+PRICE+ITEM_CNT+MAXTEMP+RAIN_DAY,tea_beverage_feature)
summary(model)
model = lm(QTY~SALEDAY+PRICE+ITEM_CNT+MAXTEMP+RAIN_DAY+HOLIDAY,tea_beverage_feature)
summary(model)

## All Subsets Regression
library(leaps)
leaps=regsubsets(QTY~.,data=tea_beverage_feature,nbest=6)
summary(leaps)
plot(leaps)
#best
model = lm(QTY~ITEM_CNT+MAXTEMP+SALEDAY,tea_beverage_feature)
summary(model)

## 모든 feature를 포함한 다중회귀
lm.fit = lm(QTY~., data = tea_beverage_feature)
summary(lm.fit)

## forward와 backward 방식을 전부 써서 좋은 결과만 가져온다.(both)
lm.fit2 = step(lm.fit,method="both")
summary(lm.fit2)
lm.yhat2=predict(lm.fit2,newdata=tea_beverage_feature)
kk=mean((lm.yhat2-Boston_test$medv)^2)
sqrt(kk)
plot(lm.yhat2,Boston_test$medv)
abline(a=0,b=1,col=2)


data_feature = data[,2:3]
View(data)
cov(data)     # 공분산 함수
cor(attitude)     # 상관계수
