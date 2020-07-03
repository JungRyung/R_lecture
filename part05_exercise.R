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
