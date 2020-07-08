setwd("D:/development/R_lecture/Rdata")
library(dplyr)
install.packages("ggmap")
library(ggmap)
library(stringr)
library(rvest)

register_google(key = "AIzaSyAFkPA5W-eQJX7Z02oN7bGdUPnvKR32Axs")
my_map=get_map(location = "제주특별자치도 서귀포시 성산읍 성산리 78",
        zoom=15,
        maptype='satellite',
        source='google')
ggmap(my_map)

qmap(location = "제주특별자치도 서귀포시 성산읍 성산리 78",
     zoom=15,
     maptype='satellite',
     source='google')

qmap(location = "00120 Vatican City",
     zoom=15,
     maptype='satellite',
     source='google')

qmap(location = "부산 해운대구 송정광어골로 58-1",
     zoom=15,
     maptype='satellite',
     source='google')

## 다시 reset
plot.new()
frame()
geocodeQueryCheck()
geocode(location="제주특별자치도 서귀포시 성산읍 성산리 78",
        output='latlona',
        source='google')

geocode(location=enc2utf8(x="제주특별자치도 서귀포시 성산읍 성산리 78$language=ko"),
        output='latlona',
        source='google')


myloc=geocode(location="강원 강릉시 창해로 514",
              output='latlon',
              source='google')
myloc
center=c(myloc$lon,myloc$lat)
qmap(location = center,
     zoom=18,
     maptype='hybrid',
     source='google') +
  geom_point(data=myloc,
             mapping=aes(x=lon,y=lat),
             shape='*',
             color='red',
             stroke=18,size=10)

### 서울시의 대학목록 추출하기
url="https://namu.wiki/w/%EC%84%9C%EC%9A%B8%ED%8A%B9%EB%B3%84%EC%8B%9C%EC%9D%98%20%EB%8C%80%ED%95%99%EA%B5%90%20%EB%AA%A9%EB%A1%9D"

hdoc=read_html(url,encoding = "UTF-8")
df=hdoc%>%
  html_nodes(".wiki-paragraph a")%>%
  html_text()
head(df,50)
univ = ifelse(str_detect(df,pattern = "대학교"),df,"")
univ
kk=univ%>%
  data.frame()
kk=Filter(function(x){nchar(x)>=5},univ)
kk
univName=kk[2:28]
univName
univCord=geocode(location=univName,
                 output='latlon',
                 source='google')
univDF=data.frame(univ=univName,
                  lon=univCord$lon,
                  lat=univCord$lat)
univDFna=na.omit(univDF)
univDFna
center = c(mean(x=univDFna$lon),mean(x=univDFna$lat))
center

qmap(location = center,
     zoom=12,
     maptype='satellite',
     source='google') +
  geom_point(data=univDFna,
             aes(x=lon,y=lat),
             shape='*',
             color='red',size=6) +
  geom_text(data=univDFna,
            aes(x=lon,y=lat,label=univ),
            color='green',hjust=0.5,
            vjust=-0.1,
            size=3,
            fontface='bold',
            family='NanumGodic')
