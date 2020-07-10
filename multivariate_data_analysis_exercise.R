#setwd("D:/development/R_lecture/Rdata")
setwd("/Users/ryung/Desktop/Development/R_lecture/Rdata")

data=read.csv("20140528_baseball.csv")
head(data)

rownames(data) = data[,1]
head(data)

# 1.
stars(data[,2:6],flip.labels = F, key.loc = c(9,3), draw.segments = TRUE)
library(aplpack)
faces(data[,2:6])

# 2.
bb2013=read.csv("2013_baseball.csv")
head(bb2013)

position=bb2013$포지션

base2_pos=bb2013[,c(2,4:11)]
base2_pos2=aggregate(base2_pos[,2:9],by = list(포지션=base2_pos$포지션), mean)
