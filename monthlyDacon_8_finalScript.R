library(DMwR);library(dplyr);library(data.table);library(caret);library(catboost);library(Matrix);library(ROCR)
setwd("C:/r/monthlyDacon_8/")
source('C:/r/monthlyDacon_8/monthlyDacon_8_common.R')

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

##################
## 변수타입설정 ##
##################
#- 범주형 변환
factor_var <- c("engnat",
                "age_group",
                "gender",
                "hand",
                "married",
                "race",
                "religion",
                "urban",
                "voted")

train[factor_var] <- train %>% select(factor_var) %>% mutate_all(as.factor)
test[factor_var]  <-  test %>% select(factor_var[c(-9)]) %>% mutate_all(as.factor)

#################
## 이상치 처리 ##
#################
#- Q_E          :  10000 이상 값들은 전부 중앙값으로 대체
#- familysize   :  18이상인 데이터에 대해서는 이상치 처리 필요 --> 중앙값으로 대체

Q_E <- c("QaE",  "QbE",   "QcE",  "QdE",  "QeE",  "QfE",  "QgE" 
         , "QhE",   "QiE" ,  "QjE",  "QkE"  , "QlE",  "QmE",  "QnE" , "QoE",   "QpE" , "QqE",  "QrE" , "QsE" , "QtE") 

train[Q_E] <- train %>% select(matches("Q.E")) %>% mutate_all(~ifelse(.x >= 10000, NA, .x))
train      <- train %>% mutate(familysize = ifelse(familysize >= 18, NA, familysize)) 

test[Q_E] <- test %>% select(matches("Q.E")) %>% mutate_all(~ifelse(.x >= 10000, NA, .x))
test      <- test %>% mutate(familysize = ifelse(familysize >= 18, NA, familysize)) 

#################
## 결측치 처리 ##
#################

train <- DMwR::centralImputation(
  data = train  # 데이터 프레임
)

test  <- DMwR::centralImputation(
  data = test  # 데이터 프레임
)

colSums(is.na(train))
colSums(is.na(test))


############
## 모델링 ##
############
set.seed(1)
trainIdx <- createDataPartition(train[,"voted"], p = 0.7, list = F)
trainData <- train[ trainIdx, ]
testData  <- train[-trainIdx, ]

################
## 1. xgboost ##
################
# xgboost : nrounds = 734, max_depth = 6, eta = 0.0758, gamma = 4.6, colsample_bytree = 0.537, min_child_weight = 17, subsample = 0.983 

trainData_xgb <- trainData
testData_xgb  <- testData
set.seed(1)

trControl <- caret::trainControl(
  method          = "cv", 
  number          = 10, 
  verboseIter     = T,
  savePredictions = TRUE, 
  classProbs      = T)

levels(trainData_xgb$voted) <- c("Yes", "No")
levels(testData_xgb$voted)  <- c("Yes", "No")

tnGrid <- expand.grid(
  nrounds = 734, max_depth = 6, eta = 0.0758, gamma = 4.6, colsample_bytree = 0.537, min_child_weight = 17, subsample = 0.983 
)

set.seed(1)
modelResult <- caret::train(
  voted ~ .,
  data             = trainData_xgb, 
  method           = 'xgbTree',
  preProcess       = NULL,
  weights          = NULL,       
  metric           = 'ROC',
  trControl        = trControl,
  tuneGrid         = tnGrid
)

save(modelResult, file = "xgboost_model.RData")
# load("xgboost_model.RData")

modelResult$results  # 튜닝별 모델링 결과
modelResult$bestTune # 최적 파라미터 결과

YHat_xgb <- predict.train(
  object  = modelResult,  # caret model 객체
  newdata = testData,     # 예측하고자 하는 data
  type    = c("prob")     # 예측 타입
)

# 'No' == 1, "Yes" == 0
AUC_xgboost <- mkAUCValue(
  YHat = YHat_xgb[,2], 
  Y    = ifelse(testData$voted == 2, 1, 0))


#################
## 2. CatBoost ##
#################
# voted  1 --> 0, 2 --> 1로 변경 
trainData_cat <- trainData
testData_cat  <- testData

YIdx       <- which(colnames(trainData_cat) %in% c('voted'))
features   <- trainData_cat[-YIdx]
labels     <- ifelse(trainData_cat[,YIdx] == 1, 0, 1)
train_pool <- catboost.load_pool(data = features, label = labels)

# 2. catboost.train 함수를 이용하여 train
model <- catboost.train(
  train_pool,                                  #- 학습에 사용하고자 하는 train_pool  
  NULL,                                        #- 
  params = list(loss_function = 'Logloss',     #- loss function 지정(여기서는 분류모형이므로 Logloss)
                random_seed   = 123,           #- seed number
                custom_loss   = "AUC",         #- 모델링 할 때 추가로 추출할 값들 (train_dir로 지정한 곳으로 해당 결과를 파일로 내보내준다)
                train_dir     = "./model/CatBoost_R_output", # 모델링 한 결과를 저장할 directory
                iterations    = 300,                         #- 학습 iteration 수
                metric_period = 10)            
)           

# 3. catboost.predict function
real_pool  <- catboost.load_pool(testData_cat)
YHat_cat <- catboost.predict(
  model, 
  real_pool,
  prediction_type = c('Class'))  # Probability, 

AUC_catboost <- mkAUCValue(
  YHat = YHat_cat, 
  Y    = ifelse(testData$voted == 2, 1, 0))

nrow(testData)
testData_wrong <- testData[!YHat_cat == ifelse(testData$voted == 2, 1, 0),]


#####################
## 3. randomForest ##
#####################

trainData_rf <- trainData
testData_rf  <- testData
set.seed(1)

trControl <- caret::trainControl(
  method          = "none", 
  number          = 10, 
  verboseIter     = T,
  savePredictions = TRUE, 
  classProbs      = T)

levels(trainData_rf$voted) <- c("Yes", "No")
levels(testData_rf$voted)  <- c("Yes", "No")

tnGrid <- expand.grid(
  mtry = c(5)
)


modelResult <- caret::train(
  voted ~ .,
  data             = trainData_rf, 
  method           = "rf",
  preProcess       = NULL,
  weights          = NULL,       
  metric           = 'ROC',
  trControl        = trControl,
  tuneGrid         = tnGrid,
  sampsize         = 1000,
  ntree            = 500
)

save(modelResult, file = "rf_model.RData")

modelResult$results  # 튜닝별 모델링 결과
modelResult$bestTune # 최적 파라미터 결과

YHat_rf  <- predict.train(
  object  = modelResult,  # caret model 객체
  newdata = testData,     # 예측하고자 하는 data
  type    = c("prob")     # 예측 타입
)


# 'No' == 1, "Yes" == 0
AUC_rf <- mkAUCValue(
  YHat = YHat_rf[,2], 
  Y    = ifelse(testData$voted == 2, 1, 0))

#########
## SOM ##
#########




####################
## final assemble ##
####################

AUC_final <- mkAUCValue(
  YHat = (YHat_xgb[,2] + YHat_cat) / 2, 
  Y    = ifelse(testData$voted == 2, 1, 0))
