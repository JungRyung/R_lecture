# 07-1
library(ggplot2)
## mpg 데이터 불러오기
mpg = as.data.frame(ggplot2::mpg)
mpg

# NA 할당하기
mpg[c(65,124,131,153,212), "hwy"] <- NA
table(is.na(mpg$hwy))

### Q. drv 별로 hw 평균이 어떻게 다른지 확인
## Q1. drv변수와 hwy변수에 결측치가 몇 개 있는지 확인 
table(is.na(mpg$hwy))
table(is.na(mpg$drv))

## Q2
df_mpg=mpg%>%
  group_by(drv)%>%
  summarise(mean_hwy=round(mean(hwy,na.rm=T),1))
df_mpg
ggplot(data=df_mpg,aes(x=drv,y=mean_hwy)) + geom_col()
ggplot(data=mpg,aes(x=drv)) + geom_bar()


# 07-2
library(dplyr)
mpg <- as.data.frame(ggplot2::mpg)
mpg[c(10,14,58,93), "drv"] <- "k" # drv 이상치 할당
mpg[c(29,43,129,203), "cty"] <- c(3,4,39,42) # cty 이상치 할당

## Q1. drv에 이상치가 있는지 확인, 결측처리하고 다시 확인
table(mpg$drv)
mpg$drv = ifelse(mpg$drv %in% c('4','f','r'),mpg$drv,NA)
table(mpg$drv)  

## Q2. 상자그림을 이용해서 cty에 이상치가 있는지 확인
boxplot(mpg$cty)
mpg$cty = ifelse(mpg$cty>27 | mpg$cty<5,NA,mpg$cty)
boxplot(mpg$cty)

## 결측치 그래프로 확인
kk=table(is.na(mpg$cty))
tt=barplot(kk,col=rainbow(2),ylim=c(0,250))
text(tt,kk,pos=3,label=paste0(kk,"건"),col='4',cex=2)

## Q3. 평균 확인
df_mpg=mpg %>%
  filter(!is.na(drv) & !is.na(cty)) %>%
  group_by(drv) %>%
  summarise(mean_cty=mean(cty))

df_mpg
install.packages("plotly")
library(plotly)
ggplot(data=df_mpg,aes(x=reorder(drv,-mean_cty),y=mean_cty))+
  geom_col(fill=rainbow(3)) + coord_flip()
ggplotly(tt)
