library(DMwR);library(dplyr);library(data.table);library(caret);library(catboost);library(Matrix);library(ROCR);library(lightgbm)
setwd("C:/r/Monthly-Dacon-8th/")
source('C:/r/Monthly-Dacon-8th/monthlyDacon_8_common.R')
finalVarBoolean <- F

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
#- 1. reverse 
#- QaA, QdA, QeA, QfA, QgA, QiA, QkA, QnA, QqA, QrA --> reverse 
revVar  <- c("QaA", "QdA", "QeA", "QfA", "QgA", "QiA", "QkA", "QnA", "QqA", "QrA")
train[revVar] <- train %>% select(revVar) %>% mutate_all(list(~6 - .))
test[revVar]  <- test %>% select(revVar) %>% mutate_all(list(~6 - .))

QAvar <- train %>% select(matches("Q.A")) %>%  colnames
# train$QA_var <- train %>% dplyr::select(c(QAvar)) %>% transmute(test = round(RowVar(across(where(is.numeric))), 4)) %>%  unlist %>% as.numeric
# test$QA_var  <-  test %>% dplyr::select(c(QAvar)) %>% transmute(test = round(RowVar(across(where(is.numeric))), 4)) %>%  unlist %>% as.numeric
  
#- 2. machia score = 전체 점수의 평균 값 계산
machiaVar             <- train %>% select(matches("Q.A")) %>%  colnames
train$machiaScore     <- train %>% select(machiaVar) %>% transmute(machiaScore = rowMeans(across(where(is.numeric)))) %>% unlist %>% as.numeric
test$machiaScore      <- test  %>% select(machiaVar) %>% transmute(machiaScore = rowMeans(across(where(is.numeric)))) %>% unlist %>% as.numeric

#- 3 wf_mean, wr_mean, voca_mean(실제 단어를 아는 경우(wr)  - 허구인 단어를 아는 경우(wf) / 13)
wfVar <- train %>% select(matches("wf.")) %>%  colnames
wrVar <- train %>% select(matches("wr.")) %>%  colnames

#- 3.1 wf_mean
# train$wf_mean <- train %>% select(wfVar) %>% transmute(wf_mean = round(rowSums(across(where(is.numeric))), 8)) %>% unlist %>% as.numeric
# test$wf_mean  <- test %>% select(wfVar)  %>% transmute(wf_mean = round(rowSums(across(where(is.numeric))), 8)) %>% unlist %>% as.numeric

#- 3.2 wr_mean
# train$wr_mean <- train %>% select(wrVar) %>% transmute(wr_mean = round(rowSums(across(where(is.numeric))), 8)) %>% unlist %>% as.numeric
# test$wr_mean  <- test %>% select(wrVar)  %>% transmute(wr_mean = round(rowSums(across(where(is.numeric))), 8)) %>% unlist %>% as.numeric

#- 3.3 voca_mean
train$voca_mean <- train %>% transmute(voca_mean = (wr_01 + wr_02 + wr_03 + wr_04 + wr_05 + wr_06 + wr_07 + wr_08 + wr_09 + wr_10 + wr_11 + wr_12 + wr_13 - wf_01 - wf_02 - wf_03)) %>% unlist %>% as.numeric
test$voca_mean <- test %>% transmute(voca_mean = (wr_01 + wr_02 + wr_03 + wr_04 + wr_05 + wr_06 + wr_07 + wr_08 + wr_09 + wr_10 + wr_11 + wr_12 + wr_13 - wf_01 - wf_02 - wf_03)) %>% unlist %>% as.numeric

#- tp variable
tpPs <- c("tp01", "tp03", "tp05", "tp07", "tp09")
tpNg <- c("tp02", "tp04", "tp06", "tp08", "tp10")

#- 3.4 tp_positive
train$tp_positive  <- train %>% select(all_of(tpPs)) %>% transmute(tp_positive = round(rowMeans(across(where(is.numeric))), 8)) %>%  unlist %>% as.numeric 
test$tp_positive   <- test  %>% dplyr::select(all_of(tpPs)) %>% transmute(tp_positive = round(rowMeans(across(where(is.numeric))), 8)) %>%  unlist %>% as.numeric 

#- 3.5 tp_negative 
train$tp_negative  <- train %>% dplyr::select(all_of(tpNg)) %>% transmute(tp_negative = round(rowMeans(across(where(is.numeric))), 8)) %>%  unlist %>% as.numeric 
test$tp_negative   <- test  %>% dplyr::select(all_of(tpNg)) %>% transmute(tp_negative = round(rowMeans(across(where(is.numeric))), 8)) %>%  unlist %>% as.numeric 

#- 3.6 tp_variance
train$tp_var       <- train %>% dplyr::select(c(tpPs, tpNg)) %>% transmute(test = round(RowVar(across(where(is.numeric))), 4)) %>%  unlist %>% as.numeric 
test$tp_var        <- test %>% dplyr::select(c(tpPs, tpNg)) %>% transmute(test = round(RowVar(across(where(is.numeric))), 4)) %>%  unlist %>% as.numeric 

#- 3.7 tp_mean
train$tp_mean <- train %>% transmute(tp_mean = round(((tp01 + tp03 + tp05 + tp07 + tp09 + (7 - tp02) + (7 - tp04) + (7 - tp06) + (7 - tp08) + (7 - tp10)) / 10), 8)) %>%  unlist %>% as.numeric
test$tp_mean  <- test %>% transmute(tp_mean = round(((tp01 + tp03 + tp05 + tp07 + tp09 + (7 - tp02) + (7 - tp04) + (7 - tp06) + (7 - tp08) + (7 - tp10)) / 10), 8)) %>%  unlist %>% as.numeric

#- 4. QE derived Var
#- 4.1 QE_median
QE_var          <- train %>% select(matches("Q.E")) %>%  colnames
train$QE_median <- apply(train[, QE_var], 1,  median)
test$QE_median  <- apply(test[, QE_var], 1,   median)

# #- 4.2 QE_median
train$QE_min <- apply(train[, QE_var], 1,  min)
test$QE_min  <- apply(test[, QE_var], 1,   min)

##############################
##변수타입설정 & 변수 선택 ##
##############################

#- 범주형(명목형) 변환
factor_var <- c("engnat",
                "age_group",
                "education",
                "gender",
                "hand",
                "married",
                "race",
                "religion",
                "urban",
                "voted")

orderedFacVar <- c(
  # "wf_mean", 
  # "wr_mean" ,
  "voca_mean",
  # "machiaScore",
  "tp_positive",
  "tp_negative",
  "tp_var",
  "tp_mean"
)

Y_idx <- which(factor_var %in% c('voted'))
train[factor_var]         <- train %>% dplyr::select(all_of(factor_var))        %>% mutate_all(as.factor)
test[factor_var[-Y_idx]]  <-  test %>% dplyr::select(all_of(factor_var[-Y_idx])) %>% mutate_all(as.factor)

#- 범주형(순서형) 변환
ordered_var1 <- colnames(train)[grep("Q.A", colnames(train))]
ordered_var2 <- colnames(train)[grep("tp.[0-9]|wr.[0-9]|wf.[0-9]", colnames(train))]

train[c(ordered_var1, ordered_var2, orderedFacVar)]   <- train %>% dplyr::select(  all_of(ordered_var1)
                                                                                 , all_of(ordered_var2)
                                                                                 , all_of(orderedFacVar)) %>% mutate_all(as.ordered)
test[c(ordered_var1, ordered_var2, orderedFacVar) ]   <- test %>% dplyr::select(  all_of(ordered_var1)
                                                                                , all_of(ordered_var2)
                                                                                , all_of(orderedFacVar)) %>% mutate_all(as.ordered)


#-  변수 제거
remv_var <- c("index")
train    <- train %>%  dplyr::select(-all_of(remv_var))
test     <- test  %>%  dplyr::select(-all_of(remv_var))

############
## 모델링 ##
############
#########################
## 변수 제거 안한 경우 ##
#########################
if(finalVarBoolean){
  set.seed(1)
  trainIdx <- createDataPartition(train[,"voted"], p = 0.7, list = F)
  trainData <- train[ trainIdx, c(finalVar, "voted")]
  testData  <- train[-trainIdx, c(finalVar, "voted")]
}else{
#######################
## 변수 제거 한 경우 ##
#######################
  set.seed(1)
  trainIdx  <- createDataPartition(train[,"voted"], p = 0.7, list = F)
  trainData <- train[ trainIdx, ]
  testData  <- train[-trainIdx, ]
}


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
                # train_dir     = "./model/CatBoost_R_output", #- 모델링 한 결과를 저장할 directory
                iterations    = 1000,                         #- 학습 iteration 수
                metric_period = 10)
)         

# catboost importance 
catboost_imp           <- data.frame(model$feature_importances)
catboost_imp$variables <- rownames(model$feature_importances)
colnames(catboost_imp) <- c("importance", 'variables')
catboost_imp           <- catboost_imp %>% arrange(-importance)
View(catboost_imp)
catboost_imp$variables
catboost_imp$importance

finalVar <- catboost_imp$variables[1:70]

real_pool  <- catboost.load_pool(testData_cat)
YHat_cat   <- catboost.predict(
  model, 
  real_pool,
  prediction_type = c('Probability'))  # Probability, Class

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


