# 08-2
mpg <- as.data.frame(ggplot2::mpg)
midwest <- as.data.frame(ggplot2::midwest)

## Q1. x축은 cty, y축은 hwy로 된 산점도
ggplot(data = mpg, aes(x = cty, y = hwy)) + geom_point()

## Q2. x축은 poptotal, y축은 popasian으로 된 산점도
ggplot(data = midwest, aes(x = poptotal, y = popasian)) + geom_point() +
  xlim(0,500000) +
  ylim(0,10000) 


# 08-3
## Q1. 
kk=table(mpg$class)
tt=barplot(kk,col=rainbow(8),ylim=c(0,70))
text(tt,kk,paste0(kk,"대"),pos=3,col=2,cex=2)
df=as.data.frame(mpg %>%
  filter(class=="suv")%>%
  group_by(manufacturer)%>%
  summarise(mean_cty=mean(cty),
            mean_hwy=mean(hwy))%>%
  arrange(desc(mean_cty))%>%
  head(5))
df
ggplot(data=df,aes(x=reorder(manufacturer,mean_cty),y=mean_cty)) + 
  geom_col(fill=rainbow(5)) +
  coord_flip() +
  xlab("차종") +
  ylab("평균도시연비")

# 08-4
## Q1. psavert의 시간에 따른 변화
tt=ggplot(data = economics, aes(x = date, y = psavert)) +
  geom_line()
ggplot(tt)


# 08-5
## Q1
class_mpg <- mpg %>%
  filter(class %in% c("compact", "subcompact", "suv"))

ggplot(data = class_mpg, aes(x = class, y = cty)) +
  geom_boxplot()
