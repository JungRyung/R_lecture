setwd("/Users/ryung/Desktop/Development/R_lecture/Rdata")
par(family = "AppleGothic")
# 인공신경망 (ANN) :XOR 처리
install.packages("nnet")
library(nnet)
input <-matrix(c(0,0,1,1,0,1,0,1),ncol=2) #4행 2열로 만듬 #입력자료
input
output <- matrix(c(0,1,1,0)) #답을 먼저줌 #출력자료
output

ann <-nnet(input,output,maxit = 100, size = 2, decay = 0.001) #decay = 0.001 -> 가중치
ann # 2-2-1 => 2개 입력값 2개 입력치 1개 출력값

result <- predict(ann, input)
result
#[1,] 0.1199095  0 이들어왔을떄 0에가깝고
#[2,] 0.8895589  1 이들어왔을떄 1에가깝고
#[3,] 0.8900116  1 이들어왔을떄 1에가깝고
#[4,] 0.1156781  0 이들어왔을떄 0에가깝고

#-----------------------
df <-data.frame(
  x1=c(1:6),
  x2=c(6:1),
  y=factor(c('n','n','n','y','y','y'))
)
df
str(df)
#1  1  6 n 독립변수 y값 'n','n','n','y','y','y'이렇게 준다.
#2  2  5 n
#3  3  4 n
#4  4  3 y
#5  5  2 y
#6  6  1 y

library(nnet)
model_net1 <- nnet(y ~ .,df,size=1) #size=1레이어 (적당한 레이어를 줘야함)
# weights:  5 값이 학습을 하면서 점점 오차값이 줄어드는 것을 알수 있다.
model_net1
summary(model_net1)

install.packages("devtools")
#ㄴㄴㅁㅇㅁㄴ
library(devtools)
#ㄴㅇㅁ

source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
plot.nnet(summary(model_net1))


#분류모형 예측
model_net1$fitted.values
predict(model_net1,df)

p<-predict(model_net1,df,type="class")
p

table(p,df$y)