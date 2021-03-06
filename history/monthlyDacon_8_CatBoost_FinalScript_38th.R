library(DMwR);library(dplyr);library(data.table);library(caret);library(catboost);library(Matrix);library(ROCR);library(lightgbm);library(foreach)
setwd("C:/r/Monthly-Dacon-8th/")
source('C:/r/Monthly-Dacon-8th/monthlyDacon_8_common.R')
finalVarBoolean <- F
ver             <- 30
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

#- 2. machia score = 전체 점수의 평균 값 계산
machiaVar             <- train %>% select(matches("Q.A")) %>%  colnames
train$machiaScore     <- train %>% select(machiaVar) %>% transmute(machiaScore = rowMeans(across(where(is.numeric)))) %>% unlist %>% as.numeric
test$machiaScore      <- test  %>% select(machiaVar) %>% transmute(machiaScore = rowMeans(across(where(is.numeric)))) %>% unlist %>% as.numeric

#- 3 wf_mean, wr_mean, voca_mean(실제 단어를 아는 경우(wr)  - 허구인 단어를 아는 경우(wf) / 13)
wfVar <- train %>% select(matches("wf.")) %>%  colnames
wrVar <- train %>% select(matches("wr.")) %>%  colnames

#- 3.1 wf_mean
train$wf_mean <- train %>% select(wfVar) %>% transmute(wf_mean = round(rowMeans(across(where(is.numeric))), 8)) %>% unlist %>% as.numeric
test$wf_mean  <- test %>% select(wfVar)  %>% transmute(wf_mean = round(rowMeans(across(where(is.numeric))), 8)) %>% unlist %>% as.numeric

#- 3.2 wr_mean
train$wr_mean <- train %>% select(wrVar) %>% transmute(wr_mean = round(rowMeans(across(where(is.numeric))), 8)) %>% unlist %>% as.numeric
test$wr_mean  <- test %>% select(wrVar)  %>% transmute(wr_mean = round(rowMeans(across(where(is.numeric))), 8)) %>% unlist %>% as.numeric

#- 3.3 voca_mean
train$voca_mean <- train %>% transmute(voca_mean = round((wr_01 + wr_02 + wr_03 + wr_04 + wr_05 + wr_06 + wr_07 + wr_08 + wr_09 + wr_10 + wr_11 + wr_12 + wr_13 - wf_01 - wf_02 - wf_03 / 16), 8)) %>% unlist %>% as.numeric
test$voca_mean <- test %>% transmute(voca_mean = round((wr_01 + wr_02 + wr_03 + wr_04 + wr_05 + wr_06 + wr_07 + wr_08 + wr_09 + wr_10 + wr_11 + wr_12 + wr_13 - wf_01 - wf_02 - wf_03 / 16), 8)) %>% unlist %>% as.numeric

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

##############################
## 변수타입설정 & 변수 선택 ##
##############################
#- 수치형 변수 

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


train[factor_var]        <- train %>% dplyr::select(all_of(factor_var))        %>% mutate_all(as.factor)
test[factor_var[c(-10)]]  <-  test %>% dplyr::select(all_of(factor_var[c(-10)])) %>% mutate_all(as.factor)

#- 범주형(순서형) 변환
ordered_var1 <- colnames(train)[grep("Q.A", colnames(train))]
ordered_var2 <- colnames(train)[grep("tp|wr|wf.", colnames(train))]

train[c(ordered_var1, ordered_var2)]   <- train %>% dplyr::select(all_of(ordered_var1), all_of(ordered_var2)) %>% mutate_all(as.ordered)
test[c(ordered_var1, ordered_var2) ]   <- test %>% dplyr::select(all_of(ordered_var1), all_of(ordered_var2)) %>% mutate_all(as.ordered)

#-  변수 제거
remv_var <- c("index")
train    <- train %>%  dplyr::select(-all_of(remv_var))
test     <- test  %>%  dplyr::select(-all_of(remv_var))

############
## 모델링 ##
############
trainData <- train
testData  <- test 

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
train_pool <- catboost.load_pool(data  = features, 
                                 label = labels)


# 2. catboost.train 함수를 이용하여 train
set.seed(1)
model <- catboost.train(
  train_pool,                                  #- 학습에 사용하고자 하는 train_pool  
  NULL,                                        #- 
  params = list(loss_function = 'Logloss',     #- loss function 지정(여기서는 분류모형이므로 Logloss)
                random_seed   = 123,           #- seed number
                custom_loss   = "AUC",         #- 모델링 할 때 추가로 추출할 값들 (train_dir로 지정한 곳으로 해당 결과를 파일로 내보내준다)
                #  train_dir     = "./model/CatBoost_R_output", #- 모델링 한 결과를 저장할 directory
                iterations    = 1000,                         #- 학습 iteration 수
                border_count  = 32,
                depth         = 7,
                learning_rate = 0.02,
                l2_leaf_reg   = 3.5,
                metric_period = 10)            
)         


# catboost importance 
catboost_imp           <- data.frame(model$feature_importances)
catboost_imp$variables <- rownames(model$feature_importances)
colnames(catboost_imp) <- c("importance", 'variables')
catboost_imp           <- catboost_imp %>% arrange(-importance)

finalVar <- catboost_imp$variables[1:70]

trainData <- train[c(finalVar, "voted")]
testData  <- test[c(finalVar)]

# voted  1 --> 0, 2 --> 1로 변경 
trainData_cat <- trainData
testData_cat  <- testData
YIdx       <- which(colnames(trainData_cat) %in% c('voted'))
features   <- trainData_cat[-YIdx]
labels     <- ifelse(trainData_cat[,YIdx] == 1, 0, 1)
train_pool <- catboost.load_pool(data  = features, 
                                 label = labels)


grid <- data.frame(
  learningRate = c(0.02, 0.03, 0.03),
  maxDepth     = c(7, 5, 7), 
  l2_leaf_reg  = c(3.5, 3.5, 7.5)
)

# i <- 1
# 2. catboost.train 함수를 이용하여 train
testResult <- foreach(i = 1:nrow(grid), .combine = function(a,b){ cbind(a, b)}) %do% {
  
  g <- grid[i, ]
  depth             <- g$maxDepth
  learning_rate     <- g$learningRate
  l2_leaf_reg       <- g$l2_leaf_reg
  
  set.seed(1)
  model <- catboost.train(
    train_pool,                                  #- 학습에 사용하고자 하는 train_pool  
    NULL,                                        #- 
    params = list(loss_function = 'Logloss',     #- loss function 지정(여기서는 분류모형이므로 Logloss)
                  random_seed   = 123,           #- seed number
                  custom_loss   = "AUC",         #- 모델링 할 때 추가로 추출할 값들 (train_dir로 지정한 곳으로 해당 결과를 파일로 내보내준다)
                  # train_dir     = "./model/CatBoost_R_output", #- 모델링 한 결과를 저장할 directory
                  iterations    = 1000,                         #- 학습 iteration 수
                  border_count  = 32,
                  depth         = depth,
                  learning_rate = learning_rate,
                  l2_leaf_reg   = l2_leaf_reg,
                  metric_period = 10)            
  )         
  
  
  real_pool  <- catboost.load_pool(testData_cat)
  YHat_cat   <- catboost.predict(
    model, 
    real_pool,
    prediction_type = c('Probability'))  # Probability, Class
  
  gridCom       <- paste0(depth, "_", learning_rate, "_", l2_leaf_reg)
  tmp           <- data.frame(YHat_cat)
  colnames(tmp) <- gridCom
  tmp
}

YHat_cat <- testResult_Catboost %>% transmute(finalScore = rowMeans(across(where(is.numeric)))) %>% unlist %>% as.numeric

YHat_cat <- testResult_Catboost[,1 ]

save(YHat_cat, file = "YHat_cat.RData")

sample_submission$voted <- (YHat_NN$voted - 1) * 0.6 + YHat_cat * 0.4 
write.csv(sample_submission, "submission_data.csv", row.names = F)
