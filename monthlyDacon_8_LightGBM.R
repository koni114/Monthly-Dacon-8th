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
train$machiaScore     <- train %>% select(machiaVar) %>% transmute(machiaScore = rowMeans(across(where(is.numeric)))) 
test$machiaScore      <- test  %>% select(machiaVar) %>% transmute(machiaScore = rowMeans(across(where(is.numeric)))) 


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

#-  변수 제거
remv_var <- c("index")
train_f    <- train %>%  select(-remv_var)
test_f     <- test  %>%  select(-remv_var)

#- 수치형 변수 Q_E --> log 변환
Q_E.names          <- train_f %>% select(matches("Q.E")) %>%  colnames

train_f[Q_E.names] <- train_f %>% select(matches("Q.E")) %>% mutate_all(list(~log(.)))
test_f[Q_E.names]  <- test_f  %>% select(matches("Q.E")) %>% mutate_all(list(~log(.)))

#- one-hot encoding (필요시)
oneHotVar       <- c(factor_var[-9])
train_fac       <- train_f %>% select(all_of(oneHotVar))
dmy_model       <- caret::dummyVars("~ .", data = train_fac)
train_oneHot    <- data.frame(predict(dmy_model, train_fac))

train_f  <- train_f %>% select(-oneHotVar) 
train_f  <- dplyr::bind_cols(train_f, train_oneHot)

test_fac       <- test_f %>% select(all_of(oneHotVar[c(-9)]))
dmy_model      <- caret::dummyVars("~ .", data = test_fac)
test_oneHot    <- data.frame(predict(dmy_model, test_fac))

test_f  <- test_f %>% select(-oneHotVar) 
test_f  <- dplyr::bind_cols(test_f, test_oneHot)

rm(ls = test_oneHot)
rm(ls = train_oneHot)
rm(ls = train_fac)
rm(ls = test_fac)

############
## 모델링 ##
############
set.seed(1)
trainIdx <- createDataPartition(train_f[,"voted"], p = 0.7, list = F)
trainData <- train_f[ trainIdx, ]
testData  <- train_f[-trainIdx, ]

## final 제출시, 적용
# trainData <- train_f
# testData  <- test_f

rm(ls = train_f)
rm(ls = test_f)

#################
## 5. LightGBM ##
#################
varnames     = setdiff(colnames(trainData), c("voted"))
train_sparse = Matrix(as.matrix(trainData[, varnames]), sparse=TRUE)
test_sparse  = Matrix(as.matrix(testData[,  varnames]), sparse=TRUE)
y_train      = trainData[, c("voted")]

# binary, auc 계산시, 반드시 Y 값은 0 또는 1이어야 함
train.lgb <- lgb.Dataset(data  = train_sparse, label = ifelse(y_train == 2, 1, 0))
test.lgb  <- lgb.Dataset(data  = test_sparse)

categoricals.vec <- c(ordered_var1, ordered_var2)

lgb.grid = list(objective = "binary",
                metric    = "auc",
                #min_sum_hessian_in_leaf = 1,
                feature_fraction = 0.7,
                bagging_fraction = 0.7,
                bagging_freq = 5,
                #min_data = 100,
                #max_bin = 50,
                lambda_l1 = 8,
                lambda_l2 = 1.3,
                #min_data_in_bin=100,
                #min_gain_to_split = 10,
                #min_data_in_leaf = 30,
                is_unbalance = F)

#- Setting up Gini Eval Function
lgb.normalizedgini = function(preds, dtrain){
  actual = getinfo(dtrain, "label")
  score  = MLmetrics::NormalizedGini(preds,actual)
  return(list(name = "gini", value = score, higher_better = TRUE))
}


#- Cross Validation
set.seed(1)
lgb.model.cv = lgb.cv(
  params                = lgb.grid,
  data                  = train.lgb,
  learning_rate         = 0.02,                    #- *** 훈련량
  #num_leaves            = 25,
  num_threads           = 2,                       #- * 병렬처리시 처리할 쓰레드
  nrounds               = 7000,
  early_stopping_rounds = 50,                      #- ** 더이상 발전이 없으면 그만두게 설정할때 이를 몇번동안 발전이 없으면 그만두게 할지 여부
  eval_freq             = 20,
  eval                  = lgb.normalizedgini,
  categorical_feature   = categoricals.vec,
  nfold                 = 10,
  stratified            = TRUE)

best.iter = lgb.model.cv$best_iter

lgb_model = lgb.train(
  params              = lgb.grid, 
  data                = train.lgb, 
  learning_rate       = 0.02,                        #- *** 훈련량  
  #num_leaves          = 25,                         #- * 트리가 가질수 있는 최대 잎사귀 수
  num_threads         = 2,                           #- * 병렬처리시 처리할 쓰레드
  nrounds             = best.iter,                   #- *** 계속 나무를 반복하며 부스팅을 하는데 몇번을 할것인가이다. 1000이상정도는 해주도록 함
  #-     early_stopping이 있으면 최대한 많이 줘도 (10,000~)별 상관이 없음
  eval_freq           = 20, 
  eval                = lgb.normalizedgini,
 categorical_feature = categoricals.vec)

save(lgb_model, file = "lgb_model.RData")
# load("lgb_model.RData")

#- Create and Submit Predictions 
YHat_lgbm <- predict(lgb_model, test_sparse)

AUC_lgbm <- mkAUCValue(
  YHat = YHat_lgbm, 
  Y    = ifelse(testData$voted == 2, 1, 0))


caret::confusionMatrix(
  factor(ifelse(YHat_lgbm <= 0.5, 0, 1)),
  factor(ifelse(testData$voted == 2, 1, 0))
)

# 투표를 하지 않았는데, 했다고 예측한 testData
testData_wrong <- testData[!ifelse(YHat_lgbm <= 0.5, 0, 1)  == ifelse(testData$voted == 2, 1, 0),]
testData_wrong <- testData_wrong[testData_wrong$voted == 2,] #- 
testData_wrong <- train[as.numeric(row.names(testData_wrong)),]


# importance
tree_imp1 <- lgb.importance(lgb_model, percentage = TRUE)
View(tree_imp1)
