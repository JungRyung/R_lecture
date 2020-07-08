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
