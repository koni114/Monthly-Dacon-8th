library(dplyr);library(data.table);library(ggplot2);library(outliers);library(plotly)

###############
## 컬럼 정보 ##
###############
## monthly dacon 8
## 데이터 셋 간략 정리
## train Dataset
## row num : 45532, col num : 78
## 컬럼 종류
## 1. index
## 2. Q_A / Q_E (a~t) : 비식별화를 위해 일부 질문은 Secret 처리
## 3. Q_E(a~t)        : 질문을 답할 때까지의 시간
## 4. age_group       : 연령
## 5. education       : 교육 수준
##                      (1 = Less than high school, 2=High school, 3=University degree, 4=Graduate degree, 0=무응답)
## engnat             : 모국어가 영어
##                      1 = Yes, 2 = No, 0 = 무응답
## 6. familysize      : 형제자매 수
## 7. gender          : 성별
##                      Male, Female

## 8. hand            : 필기하는 손
##                      1=Right, 2=Left, 3=Both, 0 = 무응답
## 9. married         : 혼인 상태
##                      1=Never married, 2=Currently married, 3=Previously married, 0=Other
## 10. race           : 인종
##                      Asian, Arab, Black, Indigenous Australian, Native American, White, Other
## 11. religion       : 종교
##                      Agnostic, Atheist, Buddhist, Christian_Catholic, Christian_Mormon, 
##                      Christian_Protestant, Christian_Other, Hindu, Jewish, Muslim, Sikh, Other
## tp__(01~10)        : items were rated "I see myself as:" _____ such that
##                      tp01 : Extraverted, enthusiastic.
##                      tp02 : Critical, quarrelsome.
##                      tp03 : Dependable, self-disciplined.
##                      tp04 : Anxious, easily upset.
##                      tp05 : Open to new experiences, complex.
##                      tp06 : Reserved, quiet.
##                      tp07 : Sympathetic, warm.
##                      tp08 : Disorganized, careless.
##                      tp09 : Calm, emotionally stable.
##                      tp10 : Conventional, uncreative.
## urban               : 유년기의 거주 구역
##                       1=Rural (country side), 2=Suburban, 3=Urban (town, city), 0=무응답
## wr_(01~13)          : 실존하는 해당 단어의 정의을 앎
##                       1=Yes, 0=No
## wf_(01~03)          : 허구인 단어의 정의를 앎
##                       1=Yes, 0=No

##################
## Data Loading ##
##################
setwd("C:/r/Monthly-Dacon-8th/")

train <- data.table::fread(
  "train.csv",
  stringsAsFactors = F,
  data.table = F,
  na.strings = c("NA", "NaN", "NULL", "\\N"))

# train <- testData_wrong

test <- data.table::fread(
  "test_x.csv",
  stringsAsFactors = F,
  data.table = F,
  na.strings = c("NA", "NaN", "NULL", "\\N"))

##################
## 변수타입설정 ##
##################
# 명목형 변수 factor 형으로 변경
factor_var <- c("engnat",
                "age_group",
                "gender",
                "hand",
                "married",
                "race",
                "religion",
                "urban",
                "voted")

train[factor_var] <- train %>% select(all_of(factor_var)) %>% mutate_all(as.factor)

#########
## EDA ##
#########

# 1. 컬럼명 EDA 확인
# 1.1 index
#-    0 ~ 45531 까지 편성.
#-    int로 편성되어 있기 때문에 character로 변경.
train <- train %>% mutate(index = as.character(index))
test  <- test  %>% mutate(index = as.character(index))

# 1.2 QaA
#- 5점척도 데이터. 1
train %>% select(QaA) %>%  distinct()
train %>% group_by(QaA) %>%  tally()


# 1.3 QaE
# a 질문을 답할때 까지의 시간
#- 중앙값은 557, 최대값은 2413960.0로, 해당 데이터에 대한 이상치 처리가 필요해 보임.
#- 10000 이상은 이상치로 간주할 필요성 있음. 
summary(train$QaE)
train_QaE_outliers <- boxplot.stats(train$QaE)$out
sum(is.na(train_QaE_outliers))
summary(train_QaE_outliers[train_QaE_outliers > median(train$QaE)])
ggplot(data = train %>% select(QaE) %>%  filter(QaE < 10000), aes(x = QaE)) +geom_histogram()

# 1.4 QbA
# 5점척도 데이터
# 질문 : 범죄자와 일반 사람과의 가장 큰 차이는 잡힐만큼 충분히 멍청하다는 것이다.
#- 1 -- 4,5 에 선택지가 몰리는 현상 존재
#- 2 -- 선택지 별로 b 질문을 답할때 까지의 시간은 크게 관련없는 것으로 보임
train %>% select(QbA) %>%  distinct()

# 1.5 QbE
# b 질문을 답할때까지의 시간
# 7500 ~ 10000 이상은 이상치로 간주할 필요성 있음
summary(train$QbE)
train_QbE_outliers <- boxplot.stats(train$QbE)$out
sum(is.na(train_QbE_outliers))
summary(train_QbE_outliers[train_QbE_outliers > median(train$QbE)])
ggplot(data = train %>% select(QbE) %>%  filter(QbE < 10000), aes(x = QbE)) +geom_histogram()

# 1.6 QcA
# 5점 척도 데이터
# 다른 사람을 100% 믿는 행위는 문제를 야기함
#- 5점 선택지가 가장 많음
train %>% select(QcA) %>%  distinct()
train %>% group_by(QcA) %>%  tally()
train %>% group_by(QcA) %>%  summarise(mean   = mean(QcE), 
                                       median = median(QcE),
                                       sd     = sd(QcE))

# 1.7 QcE
# c 질문을 답할때까지의 시간
# 7500 ~ 10000 이상은 이상치로 간주할 필요성 있음
summary(train$QcE)
train_QcE_outliers <- boxplot.stats(train$QcE)$out
sum(is.na(train_QcE_outliers))
summary(train_QcE_outliers[train_QcE_outliers > median(train$QcE)])
ggplot(data = train %>% select(QcE) %>%  filter(QcE < 10000), aes(x = QcE)) +geom_histogram()

# 1.8 QdA
# 5점 척도 데이터
# secret
#- 1점 선택지가 가장 많고, 척도가 높아질수록 선택한 사람 수가 줄어듬
#- 걸린 시간은 척도별로 큰 차이가 없음
train %>% select(QdA) %>%  distinct()
train %>% group_by(QdA) %>%  tally()
train %>% group_by(QdA) %>%  summarise(mean   = mean(QdE), 
                                       median = median(QdE),
                                       sd     = sd(QdE))


# 1.9 QdE
# c 질문을 답할때까지의 시간
# 7500 ~ 10000 이상은 이상치로 간주할 필요성 있음
summary(train$QdE)
train_QdE_outliers <- boxplot.stats(train$QdE)$out
sum(is.na(train_QdE_outliers))
summary(train_QdE_outliers[train_QcE_outliers > median(train$QdE)])
ggplot(data = train %>% select(QdE) %>%  filter(QdE < 10000), aes(x = QdE)) +geom_histogram()

# 1.10 QeA
# 5점 척도 데이터
# 매 시간마다 x같은 놈들은 태어난다라고 얘기한 P.T.barnom 의 말은 틀렸다.
# 1점 척도가 가장 많음. 4,5점이 가장 적음
train %>% select(QeA) %>%  distinct()
train %>% group_by(QeA) %>%  tally()
train %>% group_by(QeA) %>%  summarise(mean   = mean(QeE), 
                                       median = median(QeE),
                                       sd     = sd(QeE))

# 1.11 QeE
# e 질문을 답할때까지 걸린 시간
summary(train$QeE)
train_QeE_outliers <- boxplot.stats(train$QeE)$out
sum(is.na(train_QeE_outliers))
summary(train_QeE_outliers[train_QcE_outliers > median(train$QeE)])
ggplot(data = train %>% select(QeE) %>%  filter(QeE < 10000), aes(x = QeE)) +geom_histogram()


# 1.42 age_group ----
# 연령대
# 연령대별 10 > 20 > 30 > 40 > 50 > 60 > 70
# 연령대별 투표비율은 나이가 많아질수록 투표율이 높아짐
train_age_group <- train %>% group_by(age_group, voted) %>% tally()
ggplot(data = train_age_group, aes(x = age_group, y = n, fill = voted)) + geom_col()

# 1.43 education ----
# 교육 수준
# 1 = Less than high school, 2 = High school, 3 = University degree, 4 = Graduate degree, 0 = 무응답
# 교육수준이 높아질수록 투표율은 높아짐
train_education <- train %>% group_by(education, voted) %>% tally()
ggplot(data = train_education, aes(x = education, y = n, fill = voted)) + geom_col()

# 1.44 engnat ----
# 모국어가 영어
# 1 = Yes, 2 = No, 0 = 무응답
#- 모국어가 영어인 경우, 투표율이 더 높아짐
train_engnat <- train %>% group_by(engnat, voted) %>% tally()
ggplot(data = train_engnat, aes(x = engnat, y = n, fill = voted)) + geom_col()

# 1.55 familysize ----
# 형제자매 수
#- 형제자매 데이터 중 이상치 값이 존재.
#- 형제자매수가 18이상인 데이터에 대해서는 이상치 처리 필요
#- 형제자매수가 증가할수록(~15까지) 투표율이 어느정도 증가함을 확인
train_familysize <- train %>% group_by(familysize) %>% tally()
train_familysize <- train_familysize %>%  filter(train_familysize$n > 5)
ggplot(data = train_familysize, aes(x = as.factor(familysize), y = n)) + geom_col()

#- 형제자매 수 별 인원 대비 투표율
train_familysize <- train %>% group_by(familysize, voted) %>% tally()
train_familysize <- reshape2::dcast(data      = train_familysize, 
                formula   =  familysize ~ voted,
                value.var = 'n')

train_familysize[is.na(train_familysize)] <- 0
colnames(train_familysize)[2:3] <- c("Yes", "No")
train_familysize <- train_familysize %>% mutate(value = round(Yes / (Yes + No),3))
ggplot(data = train_familysize, aes(x = as.factor(familysize), y = value)) + geom_col()


#- 형제 자매 수가 15 이상인 데이터의 통계값 확인
train_fs15 <- train %>% filter(familysize >= 15)
View(train_fs15)
summary(train_fs15)

# 1.56 gender ----
# 성별
#- 성별에 따른 투표율은 크게 변화 없음
train_gender <- train %>% group_by(gender, voted) %>%  tally()
train_gender <- reshape2::dcast(data      = train_gender, 
                                    formula   =  gender ~ voted,
                                    value.var = 'n')
colnames(train_gender)[2:3] <- c("Yes", "No")
train_gender <- train_gender %>% mutate(value = round(Yes / (Yes + No),3))

# 1.57 hand ----
# 1=Right, 2=Left, 3=Both, 0=무응답
train_hand <- train %>% group_by(hand, voted) %>% tally()
ggplot(data = train_hand, aes(x = hand, y = n, fill = voted)) + geom_col()

# 1.58 married ----
# 혼인 상태
# 1=Never married, 2=Currently married, 3=Previously married, 0=Other
#- 현재 결혼한 상태 인 경우, 투표율이 더 높음
train_married <- train %>% group_by(married, voted) %>% tally()
ggplot(data = train_married, aes(x = married, y = n, fill = voted)) + geom_col()

train_married <- reshape2::dcast(data      = train_married, 
                                formula   =  married ~ voted,
                                value.var = 'n')
colnames(train_married)[2:3] <- c("Yes", "No")
train_married <- train_married %>% mutate(value = round(Yes / (Yes + No),3))
ggplot(data = train_married, aes(x = married, y = value)) + geom_col()

# 1.59 race ----
# 인종
# Asian, Arab, Black, Indigenous Australian, Native American, White, Other
#- 사람 수       : white > Asian, Black.. 
#- 인종별 투표율 : white > Aurtralian > American > Black > Asian > Arab
#- 인종별 투표율은 차이를 보이고 있음
train_race <- train %>% group_by(race) %>% tally()
ggplot(data = train_race, aes(x = race, y = n)) + geom_col()

train_race <- train %>% group_by(race, voted) %>% tally()
train_race <- reshape2::dcast(data      = train_race, 
                                 formula   =  race ~ voted,
                                 value.var = 'n')
colnames(train_race)[2:3] <- c("Yes", "No")
train_race <- train_race %>% mutate(value = round(Yes / (Yes + No),3))
ggplot(data = train_race, aes(x = race, y = value)) + geom_col()

# 1.60 religion ----
# 종교
# Agnostic, Atheist, Buddhist, Christian_Catholic, Christian_Mormon, Christian_Protestant, Christian_Other, 
# Hindu, Jewish, Muslim, Sikh, Other
#- 사람 수 :  Atheist(무신론자) > Agnostic(불가지론) > Christian_Catholic > Christian_Other > Christian_Protestant
#- 투표율  :  Christian_Protestant > Jewish > Christian_Catholic > Christian_Other > Agnostic...
#- 종교에 따른 투표율은 차이를 보이고 있음
train_religion <- train %>% group_by(religion) %>% tally()
ggplot(data = train_religion, aes(x = reorder(religion, -n), y = n)) + geom_col()

train_religion <- train %>% group_by(religion, voted) %>% tally()
train_religion <- reshape2::dcast(data      = train_religion, 
                              formula   =  religion ~ voted,
                              value.var = 'n')
colnames(train_religion)[2:3] <- c("Yes", "No")
train_religion <- train_religion %>% mutate(value = round(Yes / (Yes + No),3))
ggplot(data = train_religion, aes(x = reorder(religion, -value), y = value)) + geom_col()

# 1.61 urban ----
# 유년기의 거주 구역
# 1 = Rural (country side), 2 = Suburban, 3 = Urban (town, city), 0 = 무응답
#- 사람 수 : Suburban > Urban > country side > no anwser
#- 투표 율 : country side > Suburban > Urban
train_urban <- train %>% group_by(urban) %>% tally()
train_urban <- train %>% group_by(urban, voted) %>% tally()
train_urban <- reshape2::dcast(data         = train_urban, 
                                  formula   =  urban ~ voted,
                                  value.var = 'n')
colnames(train_urban)[2:3] <- c("Yes", "No")
train_urban <- train_urban %>% mutate(value = round(Yes / (Yes + No),3))
ggplot(data = train_urban, aes(x = reorder(urban, -value), y = value)) + geom_col()