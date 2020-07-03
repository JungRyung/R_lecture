library(dplyr)
exam <- read.csv("csv_exam.csv")
class13 = exam %>% filter(class==1 || class==3)
class13

## 조건에 맞는 결과만 추출
exam %>% filter(class != 1)
exam %>% filter(class==1 & math>=50 & english>=60 & science>=50)
exam %>% filter(class==1 | class==3 | class==5)
exam %>% filter(class %in% c(1,3,5))

10%/%3
10%%3

tt=exam %>%
  select(math,english,science) %>%
  head(5)
tt

exam %>% select(-math, -english)

exam %>% 
  filter(class == 3) %>% 
  select(class, english) %>%
  head(3)

## 정렬하기
exam %>% arrange(math)
exam %>% arrange(desc(math))

exam %>% arrange(class, math)

## 파생변수 추가하기
kk=exam %>% mutate(total=math+english+science) %>%
  head(10)
kk %>%
  mutate(round(avg=total/3),1) %>%
  head(10)

## 추가한 변수를 dplyr 코드에 바로 활용하기
exam %>%
  mutate(total = math + english + science) %>%
  arrange(total) %>%
  head

## 집단별로 요약하기
exam %>%
  group_by(class)%>%
  summarise(mean_math=mean(math))
exam %>%
  group_by(class)%>%
  summarise(mean_english = mean(english),
            sum_eng=sum(english),
            median_eng=median(english),
            stuent_number=n())

mpg=as.data.frame(ggplot2::mpg)
mpg %>%
  group_by(manufacturer,drv) %>%
  summarise(mean_cty = mean(cty)) %>%
  arrange(desc(mean_cty)) %>%
  head(10)


## 데이터 합치기

# 중간고사 데이터 생성
test1 <- data.frame(id = c(1, 2, 3, 4, 5),
                    midterm = c(60, 80, 70, 90, 85))
# 기말고사 데이터 생성
test2 <- data.frame(id = c(1, 2, 3, 4, 5),
                    final = c(70, 83, 65, 95, 80))
test1
test2

total <- left_join(test1, test2, by = "id")
total


name <- data.frame(class = c(1, 2, 3, 4, 5),
                   teacher = c("kim", "lee", "park", "choi", "jung"))
name

exam_new <- left_join(exam, name, by = "class")
exam_new


## 세로로 합치기

# 학생 1~5번 시험 데이터 생성
group_a <- data.frame(id = c(1, 2, 3, 4, 5),
                      test = c(60, 80, 70, 90, 85))
# 학생 6~10번 시험 데이터 생성
group_b <- data.frame(id = c(6, 7, 8, 9, 10),
                      test = c(70, 83, 65, 95, 80))

group_all <- bind_rows(group_a,group_b)
group_all
