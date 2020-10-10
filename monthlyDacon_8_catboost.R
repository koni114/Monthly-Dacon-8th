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

##############################
## 변수타입설정 & 변수 선택 ##
##############################
#- 수치형 변수 
num_var <- train %>%  select_if(is.numeric) %>%  colnames 

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
trainData <- train
testData  <- test

#################
## 2. CatBoost ##
#################
# voted  1 --> 0, 2 --> 1로 변경 
trainData_cat <- trainData
testData_cat  <- testData

levels(trainData_cat$voted) <- c("Yes", "No")
levels(testData_cat$voted)  <- c("Yes", "No")

yIdx <- which(colnames(trainData_cat) %in% c('voted'))

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



