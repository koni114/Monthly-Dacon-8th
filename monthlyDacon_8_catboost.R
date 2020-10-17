library(DMwR);library(dplyr);library(data.table);library(caret);library(catboost);library(Matrix);library(ROCR);library(lightgbm)
setwd("C:/r/Monthly-Dacon-8th/")
source('C:/r/Monthly-Dacon-8th/monthlyDacon_8_common.R')

##################
## Data Loading ##
##################
sample_submission <- data.table::fread(
  "sample_submission.csv",
  stringsAsFactors = F,
  data.table       = F
)

train <- data.table::fread(
  "train.csv",
  stringsAsFactors = F,
  data.table = F,
  na.strings = c("NA", "NaN", "NULL", "\\N"))

test  <- data.table::fread(
  "test_x.csv",
  stringsAsFactors = F,
  data.table = F,
  na.strings = c("NA", "NaN", "NULL", "\\N"))

# ###########################
# ## 파생변수 생성 및 변경 ##
# ###########################
# #- 1. reverse 
# #- QaA, QdA, QeA, QfA, QgA, QiA, QkA, QnA, QqA, QrA --> reverse 
# revVar  <- c("QaA", "QdA", "QeA", "QfA", "QgA", "QiA", "QkA", "QnA", "QqA", "QrA")
# train[revVar] <- train %>% select(revVar) %>% mutate_all(list(~6 - .))
# test[revVar]  <- test %>% select(revVar) %>% mutate_all(list(~6 - .))
# 
# #- 2. machia score = 전체 점수의 평균 값 계산
# machiaVar             <- train %>% select(matches("Q.A")) %>%  colnames
# train$machiaScore     <- train %>% select(machiaVar) %>% transmute(machiaScore = rowMeans(across(where(is.numeric)))) %>% unlist %>% as.numeric
# test$machiaScore      <- test  %>% select(machiaVar) %>% transmute(machiaScore = rowMeans(across(where(is.numeric)))) %>% unlist %>% as.numeric

##############################
## 변수타입설정 & 변수 선택 ##
##############################
colnames(train)

#- 범주형(명목형) 변환
factor_var <- c("engnat",
                "age_group",
                "gender",
                "hand",
                "married",
                "race",
                "religion",
                "urban",
                "voted")

train[factor_var]        <- train %>% select(all_of(factor_var))        %>% mutate_all(as.factor)
test[factor_var[c(-9)]]  <-  test %>% select(all_of(factor_var[c(-9)])) %>% mutate_all(as.factor)

#- 범주형(순서형) 변환
ordered_var1 <- colnames(train)[grep("Q.A", colnames(train))]
ordered_var2 <- colnames(train)[grep("tp|wr|wf.", colnames(train))]

train[c(ordered_var1, ordered_var2)]   <- train %>% select(all_of(ordered_var1), all_of(ordered_var2)) %>% mutate_all(as.ordered)
test[c(ordered_var1, ordered_var2) ]   <- test %>% select(all_of(ordered_var1), all_of(ordered_var2)) %>% mutate_all(as.ordered)

#-  변수 제거
remv_var <- c("index")
train    <- train %>%  select(-remv_var)
test     <- test  %>%  select(-remv_var)


############
## 모델링 ##
############
set.seed(1)
trainIdx <- createDataPartition(train[,"voted"], p = 0.7, list = F)
trainData <- train[ trainIdx, ]
testData  <- train[-trainIdx, ]

## final 제출시, 적용
# trainData <- train
# testData  <- test

#################
## 2. CatBoost ##
#################
##- 기본 catBoost function을 이용한 모델 생성

# voted  1 --> 0, 2 --> 1로 변경 
trainData_cat <- trainData
testData_cat  <- testData

YIdx       <- which(colnames(trainData_cat) %in% c('voted'))
features   <- trainData_cat[-YIdx]
labels     <- ifelse(trainData_cat[,YIdx] == 1, 0, 1)
train_pool <- catboost.load_pool(data = features, label = labels)

# 2. catboost.train 함수를 이용하여 train
set.seed(1)
model <- catboost.train(
  train_pool,                                  #- 학습에 사용하고자 하는 train_pool  
  NULL,                                        #- 
  params = list(loss_function = 'Logloss',     #- loss function 지정(여기서는 분류모형이므로 Logloss)
                random_seed   = 123,           #- seed number
                custom_loss   = "AUC",         #- 모델링 할 때 추가로 추출할 값들 (train_dir로 지정한 곳으로 해당 결과를 파일로 내보내준다)
                train_dir     = "./model/CatBoost_R_output", # 모델링 한 결과를 저장할 directory
                iterations    = 100,                      #- 학습 iteration 수
                metric_period = 10)            
)           

# catboost importance 
catboost_imp           <- data.frame(model$feature_importances)
catboost_imp$variables <- rownames(model$feature_importances)
colnames(catboost_imp) <- c("importance", 'variables')
catboost_imp           <- catboost_imp %>% arrange(-importance)
View(catboost_imp)

# 3. catboost.predict function
real_pool  <- catboost.load_pool(testData_cat)
YHat_cat   <- catboost.predict(
  model, 
  real_pool,
  prediction_type = c('Probability'))  # Probability, Class

caret::confusionMatrix(
  factor(YHat_cat),
  factor(ifelse(testData$voted == 2, 1, 0))
)

AUC_catboost <- mkAUCValue(
  YHat = YHat_cat, 
  Y    = ifelse(testData$voted == 2, 1, 0))

##- Caret package이 이용한 CatBoost 생성

trControl <- trainControl(
  method = "repeatedcv",
  search = "random",
  number = 10, 
  verboseIter = T,
  savePredictions = TRUE, 
  classProbs      = T)


modelResult <- train(
  x = trainData_cat[-yIdx],
  y = as.factor(trainData_cat[,yIdx]),
  method           = catboost.caret,
  preProcess       = NULL,
  weights          = NULL,       
  metric           = 'ROC',
  trControl        = trControl)
  

modelResult$results  # 튜닝별 모델링 결과
modelResult$bestTune # 최적 파라미터 결과

YHat_cat <- predict.train(
  object  = modelResult,  # caret model 객체
  newdata = testData,     # 예측하고자 하는 data
  type    = c("prob")     # 예측 타입
)

# 'No' == 1, "Yes" == 0
AUC_catboost <- mkAUCValue(
  YHat = YHat_cat[,2], 
  Y    = ifelse(testData_cat$voted == 'Yes', 1, 0))



