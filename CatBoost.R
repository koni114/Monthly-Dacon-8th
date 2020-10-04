#####################
## CatBoost 사용법 ##
#####################

######################
## CatBoost install ##
######################
#- 내가 CatBoost package 를 설치하고자 하는 folder 생성
# ex) C:/CatBoostRepository

#- git clone을 통해 해당 폴더에 설치.(그냥 git clone https://github.com/catboost/ 에 들어가서 다운로드 직접 받아도 상관 없음)
#- devtools package 내 build, install function을 통해 CatBoost package 설치

library(devtools)
setwd("C:/CatBoostRepository/catboost/catboost/R-package")
devtools::build()
devtools::install()

#################
## Quick Start ##
#################
# 1. catboost.load_pool 함수를 사용하여 dataset 준비
#- 종속 변수는 반드시 0, 또는 1로 setting

library(catboost)
features   <- data.frame(feature1 = c(1, 2, 3), feature2 = c('A', 'B', 'C'))
labels     <- c(0, 0, 1)
train_pool <- catboost.load_pool(data = features, label = labels)

# 2. catboost.train 함수를 이용하여 train
model <- catboost.train(
  train_pool,                                  #- 학습에 사용하고자 하는 train_pool  
  NULL,                                        #- 
  params = list(loss_function = 'Logloss',     #- loss function 지정(여기서는 분류모형이므로 Logloss)
                random_seed   = 123,           #- seed number
                custom_loss   = "AUC",         #- 모델링 할 때 추가로 추출할 값들 (train_dir로 지정한 곳으로 해당 결과를 파일로 내보내준다)
                train_dir     = "../data/CatBoost_R_output", # 모델링 한 결과를 저장할 directory
                iterations    = 100,           #- 학습 iteration 수
                metric_period = 10)            #- 
  )           

# 3. catboost.predict function
real_data  <- data.frame(feature1 = c(2, 1, 3), feature2 = c('D', 'B', 'C'))
real_pool  <- catboost.load_pool(real_data)
prediction <- catboost.predict(
  model, 
  real_pool,
  prediction_type = c('class'))



