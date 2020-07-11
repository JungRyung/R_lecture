setwd("/Users/ryung/Desktop/Development/R_lecture/term") 

data = read.csv("sales_data_new.csv")
head(data)

data[,3:10]

helth_beverage=data[1:60,]
helth_beverage
juice_beverage=data[61:120,]
juice_beverage
tea_beverage=data[120:180,]
tea_beverage

data_feature = data[,2:3]
View(data)
cov(data)     # 공분산 함수
cor(attitude)     # 상관계수