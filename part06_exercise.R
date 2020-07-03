## mpg 데이터 로드
mpg <- as.data.frame(ggplot2::mpg)
head(mpg)
tail(mpg)

## mpg 데이터를 이용한 분석 문제
## 조건에 맞는 데이터 추출
## Q1
mpg_a <- mpg %>% filter(displ <= 4)
mpg_b <- mpg %>% filter(displ >= 5)

mean(mpg_a$hwy)
mean(mpg_b$hwy)


## Q2
mpg_audi <- mpg %>% filter(manufacturer=='audi')
mpg_toyota <- mpg %>% filter(manufacturer=='toyota')

mean(mpg_audi$cty)
mean(mpg_toyota$cty)

## Q3
mpg_cfh <- mpg %>% filter(manufacturer %in% c('chevrolet','ford','honda'))
mean(mpg_cfh$hwy)


### select와 filter 함수
## Q1
tt=mpg %>% 
  select(class, cty)
tt

## Q2
mpg_suv = tt %>%
  filter(class=='suv')
mpg_compact = tt %>%
  filter(class=='compact')
mean(mpg_suv$cty)
mean(mpg_compact$cty)


### 순서대로 정렬
## Q1
mpg %>%
  filter(manufacturer=='audi') %>%
  arrange(desc(hwy)) %>%
  head(5)


### 파생변수 추가하기
## Q1
mpg_new=mpg
mpg_new <- mpg_new %>% mutate(total=cty+hwy)

## Q2
mpg_new <- mpg_new %>% mutate(mean=(total/2))

## Q3
mpg_new %>% 
  arrange(desc(mean))  %>%
  head(3)

## Q4
mpg %>%
  mutate(total=cty+hwy,
         mean=total/2) %>%
  arrange(desc(mean)) %>%
  head(3)


### 집단별로 요약하기
## Q1
mpg %>%
  group_by(class) %>%
  summarise(mean_cty=mean(cty))

## Q2
mpg %>%
  group_by(class) %>%
  summarise(mean_cty=mean(cty)) %>%
  arrange(desc(mean_cty))

## Q3
mpg %>%
  group_by(manufacturer) %>%
  summarise(mean_hwy=mean(hwy)) %>%
  arrange(desc(mean_hwy)) %>%
  head(3)

## Q4
mpg %>%
  filter(class=='compact') %>%
  group_by(manufacturer) %>%
  summarise(compact_cnt=n()) %>%
  arrange(desc(compact_cnt))


### 데이터 합치기
fuel <- data.frame(fl = c("c", "d", "e", "p", "r"),
                   price_fl = c(2.35, 2.38, 2.11, 2.76, 2.22),
                   stringsAsFactors = F)
fuel  # 출력

## Q1
mpg <- left_join(mpg,fuel,by='fl')
mpg

## Q2
mpg %>%
  select(model,fl,price_fl) %>%
  head(5)

## Q3


### 분석 도전
## Q1
midwest <- as.data.frame(ggplot2::midwest)
midwest <- midwest %>%
  mutate(ratio=(poptotal-popadults)/poptotal * 100)

## Q2
midwest %>%
  select(county,ratio) %>%
  arrange(desc(ratio)) %>%
  head(5)

## Q3
midwest <- midwest %>%
  mutate(ratio_grade=ifelse(ratio>=40,'large',ifelse(ratio>=30,'middle','small')))
table(midwest$ratio_grade)

## Q4
midwest <- midwest %>%
  mutate(asian_ratio=popasian/poptotal*100)
midwest %>%
  select(state,county,asian_ratio) %>%
  arrange(asian_ratio) %>%
  head(10)
