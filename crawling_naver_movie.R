setwd("D:/development/R_lecture/Rdata")
install.packages("rvest")
install.packages("stringr")
library(rvest)
library(stringr)
library(dplyr)

rm(list=ls())
title=c()
grade=c()
url_b="https://movie.naver.com/movie/point/af/list.nhn?&page="

for(i in 1:100){
craw_url=paste0(url_b,i,sep="")

t_css=".color_b"
title_part=read_html(craw_url,encoding = "CP949")%>%
  html_nodes(t_css)%>%
  html_text
title=c(title,title_part)
}
View(title)
