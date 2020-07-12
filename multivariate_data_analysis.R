crime = read.csv("http://datasets.flowingdata.com/crimeRatesByState-formatted.csv")
setwd("/Users/ryung/Desktop/Development/R_lecture/Rdata")
par(family = "AppleGothic")

head(crime)

rownames(crime)
rownames(crime) = crime[,1]
rownames(crime)

stars(crime[,2:8],flip.labels = FALSE, draw.segments = TRUE,key.loc=c(15,1.5))

#install.packages("aplpack")
library(aplpack)
faces(crime[,2:8])

education = read.csv("http://datasets.flowingdata.com/education.csv")
head(education)
library(lattice)
parallel(education[,2:7],horizontal.axis=FALSE,col=1)
summary(education$reading)
color=education$reading>523
color
color+1
parallel(education[,2:7],horizontal.axis=FALSE,col=color+1)



# 주성분 분석
data=read.csv("20140528_baseball.csv")
head(data)
model = prcomp(data[,2:6],scale = T)
model
summary(model)

plot(model)

model2 = prcomp(data[,2:6])
summary(model2)
model2

biplot(model)

