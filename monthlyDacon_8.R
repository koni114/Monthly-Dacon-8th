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
##                      1=Right, 2=Left, 3=Both, 0=무응답
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



setwd("./monthlyDacon_8/")
library(data.table)
train <- data.table::fread("train.csv")
View(head(train))
