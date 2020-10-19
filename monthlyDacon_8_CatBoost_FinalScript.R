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


###########################
## 파생변수 생성 및 변경 ##
###########################
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
train$tp_positive  <- train %>% select(tpPs) %>% transmute(tp_positive = round(rowMeans(across(where(is.numeric))), 8)) %>%  unlist %>% as.numeric 
test$tp_positive   <- test  %>% select(tpPs) %>% transmute(tp_positive = round(rowMeans(across(where(is.numeric))), 8)) %>%  unlist %>% as.numeric 

#- 3.5 tp_negative 
train$tp_negative  <- train %>% select(tpNg) %>% transmute(tp_negative = round(rowMeans(across(where(is.numeric))), 8)) %>%  unlist %>% as.numeric 
test$tp_negative   <- test  %>% select(tpNg) %>% transmute(tp_negative = round(rowMeans(across(where(is.numeric))), 8)) %>%  unlist %>% as.numeric 

#- 3.6 tp_variance
train$tp_var       <- train %>% dplyr::select(c(tpPs, tpNg)) %>% transmute(test = round(RowVar(across(where(is.numeric))), 4)) %>%  unlist %>% as.numeric 
test$tp_var        <- test %>% dplyr::select(c(tpPs, tpNg)) %>% transmute(test = round(RowVar(across(where(is.numeric))), 4)) %>%  unlist %>% as.numeric 

#- 3.7 tp_mean
train$tp_mean <- train %>% transmute(tp_mean = round(((tp01 + tp03 + tp05 + tp07 + tp09 + (6 - tp02) + (6 - tp04) + (6 - tp06) + (6 - tp08) + (6 - tp10)) / 10), 8)) %>%  unlist %>% as.numeric
test$tp_mean  <- test %>% transmute(tp_mean = round(((tp01 + tp03 + tp05 + tp07 + tp09 + (6 - tp02) + (6 - tp04) + (6 - tp06) + (6 - tp08) + (6 - tp10)) / 10), 8)) %>%  unlist %>% as.numeric


###############################
## 변수타입 설정 & 변수 선택 ##
###############################
#- 수치형 변수 
num_var <- train %>%  select_if(is.numeric) %>%  colnames 

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

train[factor_var]        <- train %>% select(all_of(factor_var))        %>% mutate_all(as.factor)
test[factor_var[c(-10)]]  <-  test %>% select(all_of(factor_var[c(-10)])) %>% mutate_all(as.factor)

#- 범주형(순서형) 변환
ordered_var1 <- colnames(train)[grep("Q.A", colnames(train))]
ordered_var2 <- c("tp01","tp02","tp03","tp04","tp05","tp06","tp07" ,"tp08", "tp09" ,"tp10", 
                  "wf_01" , "wf_02" , "wf_03", "wr_01", "wr_02", "wr_03",  "wr_04", "wr_05", 
                  "wr_06" , "wr_07", "wr_08","wr_09", "wr_10",  "wr_11", "wr_12" ,"wr_13") 

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

trainData['voted'] <- ifelse(trainData[,'voted'] == 1, "Yes", "No")
testData['voted'] <- ifelse(testData[,'voted'] == 1, "Yes", "No")
yIdx <- which(colnames(train) %in% 'voted')

## final 제출시, 적용
trainData <- train
testData  <- test

rm(ls = train)
rm(ls = test)
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
set.seed(1)
model1 <- catboost.train(
  train_pool,                                  #- 학습에 사용하고자 하는 train_pool  
  NULL,                                        #- 
  params = list(loss_function = 'Logloss',     #- loss function 지정(여기서는 분류모형이므로 Logloss)
                random_seed   = 1,             #- seed number
                custom_loss   = "AUC",         #- 모델링 할 때 추가로 추출할 값들 (train_dir로 지정한 곳으로 해당 결과를 파일로 내보내준다)
                train_dir     = "./model/CatBoost_R_output", #- 모델링 한 결과를 저장할 directory
                iterations    = 2000,                         #- 학습 iteration 수
                metric_period = 10)            
)           
# save(model1, file = "catBoost_model.RData")
# load("catBoost_model.RData")

# catboost importance 
catboost_imp           <- data.frame(model1$feature_importances)
catboost_imp$variables <- rownames(model1$feature_importances)
colnames(catboost_imp) <- c("importance", 'variables')
catboost_imp           <- catboost_imp %>% arrange(-importance)
View(catboost_imp)
catboost_imp$variables

# 3. catboost.predict function
real_pool    <- catboost.load_pool(testData_cat)
YHat_cat_1   <- catboost.predict(
  model1, 
  real_pool,
  prediction_type = c('Probability'))  # Probability, Class


set.seed(123)
model2 <- catboost.train(
  train_pool,                                  #- 학습에 사용하고자 하는 train_pool  
  NULL,                                        #- 
  params = list(loss_function = 'Logloss',     #- loss function 지정(여기서는 분류모형이므로 Logloss)
                random_seed   = 123,           #- seed number
                custom_loss   = "AUC",         #- 모델링 할 때 추가로 추출할 값들 (train_dir로 지정한 곳으로 해당 결과를 파일로 내보내준다)
                train_dir     = "./model/CatBoost_R_output", #- 모델링 한 결과를 저장할 directory
                iterations    = 1000,                         #- 학습 iteration 수
                metric_period = 10)            
)           

catboost_imp           <- data.frame(model2$feature_importances)
catboost_imp$variables <- rownames(model2$feature_importances)
colnames(catboost_imp) <- c("importance", 'variables')
catboost_imp           <- catboost_imp %>% arrange(-importance)
View(catboost_imp)
catboost_imp$variables

real_pool    <- catboost.load_pool(testData_cat)
YHat_cat_2   <- catboost.predict(
  model2, 
  real_pool,
  prediction_type = c('Probability'))  # Probability, Class

set.seed(2020)
model3 <- catboost.train(
  train_pool,                                  #- 학습에 사용하고자 하는 train_pool  
  NULL,                                        #- 
  params = list(loss_function = 'Logloss',     #- loss function 지정(여기서는 분류모형이므로 Logloss)
                random_seed   = 2020,           #- seed number
                custom_loss   = "AUC",         #- 모델링 할 때 추가로 추출할 값들 (train_dir로 지정한 곳으로 해당 결과를 파일로 내보내준다)
                train_dir     = "./model/CatBoost_R_output", #- 모델링 한 결과를 저장할 directory
                iterations    = 1000,                         #- 학습 iteration 수
                metric_period = 10)            
)           

catboost_imp           <- data.frame(model3$feature_importances)
catboost_imp$variables <- rownames(model3$feature_importances)
colnames(catboost_imp) <- c("importance", 'variables')
catboost_imp           <- catboost_imp %>% arrange(-importance)
View(catboost_imp)
catboost_imp$variables

real_pool    <- catboost.load_pool(testData_cat)
YHat_cat_3   <- catboost.predict(
  model3, 
  real_pool,
  prediction_type = c('Probability'))  # Probability, Class

AUC_catboost_1 <- mkAUCValue(
  YHat = YHat_cat_1, 
  Y    = ifelse(testData$voted == 2, 1, 0))

AUC_catboost_2 <- mkAUCValue(
  YHat = YHat_cat_2, 
  Y    = ifelse(testData$voted == 2, 1, 0))

AUC_catboost_3 <- mkAUCValue(
  YHat = YHat_cat_3, 
  Y    = ifelse(testData$voted == 2, 1, 0))

AUC_catboost <- mkAUCValue(
  YHat = (YHat_cat_1 + YHat_cat_2 + YHat_cat_3) / 3, 
  Y    = ifelse(testData$voted == 2, 1, 0))



caret::confusionMatrix(
  factor(YHat_cat),
  factor(ifelse(testData$voted == 2, 1, 0))
)

AUC_catboost <- mkAUCValue(
  YHat = YHat_cat, 
  Y    = ifelse(testData$voted == 2, 1, 0))

#- 투표를 했는데,(Yes, 0), 투표를 하지 않았다고 예측한 경우,(No, 1)
testData_wrongNo  <- testData[!YHat_cat  == ifelse(testData$voted == 2, 1, 0),] %>% filter(voted == 1)  
save(testData_wrongNo, file = "testData_wrongNo_CatBoost.RData")

#- 투표를 하지 않았는데(No, 1), 투표를 했다고 예측한 경우, (Yes, 0)
testData_wrongYes <- testData[!YHat_cat  == ifelse(testData$voted == 2, 1, 0),] %>% filter(voted == 2)
save(testData_wrongYes, file = "testData_wrongYes_CatBoost.RData")



##############
## 3. Caret ##
##############
fit_control <- caret::trainControl(
  method          = "cv",
  search          = 'grid',
  number          = 10, 
  verboseIter     = T,
  savePredictions = TRUE, 
  classProbs      = T,
  summaryFunction = prSummary)

grid <- expand.grid(depth = c(4, 6, 8),
                    learning_rate = c(0.1, 0.3, 0.5, 0.7),
                    iterations = c(400, 600, 800, 1000, 1200),
                    l2_leaf_reg = 1e-3,
                    rsm = 0.95,
                    border_count = 200)

modelResult <- caret::train(
  trainData[-yIdx], 
  trainData[,yIdx],
  method = catboost.caret,
  metric = "AUC",
  logging_level = 'Verbose',
  preProc       = NULL,
  tuneGrid      = grid,
  trControl     = fit_control)


modelResult$results  # 튜닝별 모델링 결과
modelResult$bestTune # 최적 파라미터 결과

Yhat_CatBoost <- predict.train(
  object = modelResult,
  newdata = testData, 
  type = c('prob')   
)

AUC_CatBoost <- mkAUCValue(
  YHat = Yhat_CatBoost[, 1], 
  Y    = ifelse(testData$voted == "No", 1, 0))


importance <- varImp(modelResult, scale = FALSE)
importance$importance



####################
## final assemble ##
####################
AUC_final <- mkAUCValue(
  YHat = (YHat_cat_1 + YHat_cat_2 + YHat_cat_3) / 3, 
  Y    = ifelse(testData$voted == 2, 1, 0))

sample_submission$voted <- (YHat_cat_1 + YHat_cat_2 + YHat_cat_3) / 3
write.csv(sample_submission, "submission_data.csv", row.names = F)
