## mpg 데이터 로드
mpg <- as.data.frame(ggplot2::mpg)
head(mpg)
tail(mpg)

View(mpg)
dim(mpg)
str(mpg)

## mpg colmn명 변경
mpg
mpg <- rename(mpg, city=cty)
mpg <- rename(mpg, highway=hwy)

## mpg의 일부 출력
head(mpg)

write.csv(mpg, "mpg_new.csv")


## midwest 분석
midwest <- as.data.frame(ggplot2::midwest)
head(midwest)

midwest <- rename(midwest, total=poptotal)
midwest <- rename(midwest, asian=popasian)
midwest$ratio = midwest$asian / midwest$total * 100
head(midwest,3)

hist(midwest$ratio)
mean(midwest$ratio)
midwest$group <- ifelse(midwest$ratio > 0.4872462, "large", "small")
qplot(midwest$group)
table(midwest$group)
