install.packages("magrittr") # package installations are only needed the first time you use it
install.packages("dplyr")    # alternative installation of the %>%
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
mpg <- as.data.frame(ggplot2::mpg)
mpg
df_mpg=mpg %>%
  group_by(drv) %>%
  summarise(mean_hwy=mean(hwy,na.rm=T))

df_mpg

install.packages("plotly")
library(plotly)
tt = ggplot(data=df_mpg,aes(x=reorder(drv, -mean_cty),y=mean_cty))+
  geom_col(fill=rainbow(3)) + coord_flip()
ggplot(tt)
  
  
library(ggplot2)
ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point() + 
  xlim(3,6) + ylim(10,30)



kk=ggplot(data = mpg, aes(x = cty, y = hwy)) +
  geom_point()
kk

## 08-4
ggplot(data = economics, aes(x = date, y = unemploy)) +
  geom_line()


## 08-5. 상자 그림 - 집단 간 분포 차이 표현하기
ggplot(data = mpg, aes(x = drv, y = hwy)) +
  geom_boxplot()

install.packages("extrafont")
library(extrafont)
font_import()

ggplot(data=economics, aes(x=date, y=psavert)) +
  geom_line()
ggplot(data=mpg, aes(x=drv, y=hwy)) + geom_boxplot()
