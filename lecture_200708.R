setwd("D:/development/R_lecture/Rdata")
library(dplyr)
library(ggplot2)
df=data(package="ggplot2")
df$results

mpg=as.data.frame(ggplot2::mpg)
head(mpg)
table(mpg$drv)

df_g=mpg%>%
  group_by(drv)%>%
  summarise(mean_cty=mean(cty))
df_g
ggplot(data=df_g,aes(x=reorder(drv,mean_cty),y=mean_cty)) +
  geom_col(fill=c('red','blue','orange')) +
  geom_text(aes(label=mean_cty),hjust=-0.2,col='red') +
  coord_flip() +
  xlab("구동타입") + ylab("도시연비")

##Seed함수
runif(3)
rnorm(3,mean=0,sd=1)
set.seed(1234)
runif(15)
