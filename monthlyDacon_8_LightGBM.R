library(DMwR);library(dplyr);library(data.table);library(caret);library(catboost);library(Matrix);library(ROCR);library(lightgbm);library(CatEncoders);library(foreach)
setwd("C:/r/Monthly-Dacon-8th-master/")
source('C:/r/Monthly-Dacon-8th-master/monthlyDacon_8_common.R')

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

QAvar <- train %>% select(matches("Q.A")) %>%  colnames
train$QA_var <- train %>% dplyr::select(c(QAvar)) %>% transmute(test = round(RowVar(across(where(is.numeric))), 4)) %>%  unlist %>% as.numeric
test$QA_var  <-  test %>% dplyr::select(c(QAvar)) %>% transmute(test = round(RowVar(across(where(is.numeric))), 4)) %>%  unlist %>% as.numeric

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

save(train, file = "train.RData")
save(test, file = "test.RData")
##############################
## 변수타입설정 & 변수 선택 ##
##############################
#- 수치형 변수 

#- 범주형(명목형) 변환
factor_var <- c("engnat",`
                "age_group",`
                "education",
                "gender",
                "hand",
                "married",
                "race",
                "religion",
                "urban",
                "voted")


for(i in factor_var){
  encode      <- CatEncoders::LabelEncoder.fit( train[,i])
  train[,i]   <- CatEncoders::transform(encode, train[,i])
  
  if(i  != 'voted'){
    encode      <- CatEncoders::LabelEncoder.fit(test[,i])
    test[,i]    <- CatEncoders::transform(encode, test[,i])
  }
}


train[factor_var]        <- train %>% dplyr::select(all_of(factor_var))        %>% mutate_all(as.factor)
test[factor_var[c(-10)]]  <-  test %>% dplyr::select(all_of(factor_var[c(-10)])) %>% mutate_all(as.factor)


#- 범주형(순서형) 변환
ordered_var1 <- colnames(train)[grep("Q.A", colnames(train))]
ordered_var2 <- colnames(train)[grep("tp|wr|wf.", colnames(train))]

#-  변수 제거
remv_var <- c("index")
train    <- train %>%  dplyr::select(-all_of(remv_var))
test     <- test  %>%  dplyr::select(-all_of(remv_var))


############
## 모델링 ##
############
set.seed(1)
trainIdx <- createDataPartition(train[,"voted"], p = 0.7, list = F)
trainData <- train[ trainIdx, ]
testData  <- train[-trainIdx, ]

trainData <- train[ trainIdx, c(finalVar, "voted")]
testData  <- train[-trainIdx, c(finalVar, "voted")]

## final 제출시, 적용
trainData <- train
testData  <- test

trainData <- train[c(finalVar, "voted")]
testData  <- test[c(finalVar)]

rm(ls = train)
rm(ls = test)

#################
## 5. LightGBM ##
#################
varnames     = setdiff(colnames(trainData), c("voted"))
train_sparse = Matrix(as.matrix(trainData[, varnames]), sparse=TRUE)
test_sparse  = Matrix(as.matrix(testData[,  varnames]), sparse=TRUE)
y_train      = trainData[, c("voted")]

# binary, auc 계산시, 반드시 Y 값은 0 또는 1이어야 함
train.lgb        <- lgb.Dataset(data  = train_sparse, label = ifelse(y_train == 2, 1, 0))
test.lgb         <- lgb.Dataset.create.valid(train.lgb, data = test_sparse, label = ifelse(testData[c('voted')] == 2, 1, 0))
valids           <- list(train = train.lgb, test = test.lgb)

categoricals.vec <- c(factor_var[-10], ordered_var1, ordered_var2)
categoricals.vec <- categoricals.vec[!categoricals.vec %in% c('tp_var')]
# categoricals.vec <- categoricals.vec[categoricals.vec %in% finalVar]

## grid search
## 1. 훈련량 0.01, 0.02 0.05, 0.07, 0.1
## 2. 반복량 7000
## 3. 나무 깊이 3,4,5,6,7
## 4. 부스팅 방법 gbdt, dart
# grid <- expand.grid(
#   learningRate = c(0.01, 0.02, 0.05, 0.07, 0.1),
#   maxDepth     = c(3, 4, 5, 6, 7),
#   booster      = c('gbdt','dart')
# )

grid <- data.frame(learningRate = c(0.01, 0.02, 0.02),
                   maxDepth     = c(6, 7, 6),
                   booster      = c('gbdt', 'gbdt', 'gbdt'),
                   iter         = c(811, 341, 346)
                   )

# i <- 1
testResult <- foreach(i = 1:nrow(grid), .combine = function(a,b){ cbind(a, b)})%do% {
  tryCatch({
    g <- grid[i, ]
    learningRate <- g$learningRate
    maxDepth     <- g$maxDepth
    booster      <- g$booster
    iter         <- g$iter
    
    lgb.grid = list(objective = "binary",
                    metric    = "auc",
                    min_sum_hessian_in_leaf = 1,
                    feature_fraction = 0.7,
                    bagging_fraction = 0.7,
                    bagging_freq = 5,
                    max_depth    = maxDepth,
                    #min_data = 100,
                    #max_bin = 50,
                    lambda_l1 = 8,
                    lambda_l2 = 1.3,
                    #min_data_in_bin=100,
                    #min_gain_to_split = 10,
                    #min_data_in_leaf = 30,
                    is_unbalance = F)
    
    set.seed(1)
    lgb_model = lgb.train(
      params                = lgb.grid, 
      data                  = train.lgb, 
      learning_rate         = learningRate,                  #- *** 훈련량 
      boosting              = booster,                       #- "gbdt", "rf", "dart" or "goss".
      #num_leaves            = 25,                           #- * 트리가 가질수 있는 최대 잎사귀 수
      num_threads           = 2,                             #- * 병렬처리시 처리할 쓰레드
      nrounds               = iter,                          #- *** 계속 나무를 반복하며 부스팅을 하는데 몇번을 할것인가이다. 1000이상정도는 해주도록 함
      # valids                = valids,
      # early_stopping_rounds = 50,
      categorical_feature   = categoricals.vec,
      eval_freq             = 50,
      verbose               = 1)
    
    
    bestScore <- lgb_model$best_score
    bestIter  <- lgb_model$best_iter
    
    #- Create and Submit Predictions
    YHat_lgbm <- predict(lgb_model, test_sparse)
    
    # AUC_lgbm  <- mkAUCValue(
    #   YHat = YHat_lgbm,
    #   Y    = ifelse(testData$voted == 2, 1, 0))

    gridCom       <- paste0(learningRate, "_", maxDepth, "_", booster)
    tmp           <- data.frame(YHat_lgbm)
    colnames(tmp) <- gridCom
    tmp
  }, error = function(err) {return(NULL)})
}

YHat_lgbm <- testResult %>% transmute(finalScore = rowMeans(across(where(is.numeric)))) %>% unlist %>% as.numeric

AUC_lgbm  <- mkAUCValue(
  YHat = YHat_lgbm_final, 
  Y    = ifelse(testData$voted == 2, 1, 0))
