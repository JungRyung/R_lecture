setwd("D:/development/R_lecture/Rdata")

data = read.csv("flushot.csv")
head(data)

# 1. 주어진 세 개의 설명변수로 예방접종 확률을 예측하는 모형을 추정하여
#     추정된 로지스틱 회귀식을 써라.
log_model = glm(flushot~., data=data, family=binomial(logit))
summary(log_model)

# 2. EXP를 해석하라.
exp(0.07279)
exp(-0.09899)
exp(0.43397)

# 4. 유의하지 않은 설명변수가 있는지 Deviance goodness-of-fit test를
#   통해 판단하여 최종모형을 추정하라.
log_model2=glm(flushot~age+aware, data=data, family = binomial())
summary(log_model2)
table(data$flushot)
24/(134+24)
tt=table(data$flushot, log_model2$fitted.values>0.1518987)
c('민감도'=19/(5+19), '특이도'=95/(95+40), '에러율'=45/sum(tt))
rocplot(log_model2)

# 7. cutoff을 0.1, 0.15, 0.2로 두었을 때의 총 error rate와 민감도, 특이도를 계산하라.
tab_01=table(data$flushot, log_model2$fitted.values>0.1)
tab_015=table(data$flushot, log_model2$fitted.values>0.15)
tab_02=table(data$flushot, log_model2$fitted.values>0.2)
tab_01
tab_015
tab_02

res01=c('민감도'=tab_01[2,2]/sum(tab_01[2,]),
        '특이도'=tab_01[1,1]/sum(tab_01[1,]),
        '에러율'=(tab_01[1,2]+tab_01[2,1])/sum(tab_01))
res015=c('민감도'=tab_015[2,2]/sum(tab_015[2,]),
         '특이도'=tab_015[1,1]/sum(tab_015[1,]),
         '에러율'=(tab_015[1,2]+tab_015[2,1])/sum(tab_015))
res02=c('민감도'=tab_02[2,2]/sum(tab_02[2,]),
        '특이도'=tab_02[1,1]/sum(tab_02[1,]),
        '에러율'=(tab_02[1,2]+tab_02[2,1])/sum(tab_02))
res01
res015
res02 # cutoff이 0.2일 때 에러율이 가장 낮다

