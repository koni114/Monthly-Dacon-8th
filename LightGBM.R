######################
## LightGBM install ##
######################
# R-에서 LightGBM을 설치하는 것은 원래 굉장히 까다로웠었다.
# 하지만 R-version이 4.0.0 이상인 경우에 CRAN에서 설치하는 것과 
# 동일하게 편하게 설치할 수 있도록 변경되었다.
# (원래는 CMAKE, Visual Studio, RTools를 설치해야 가능했다..)

# 구체적인 사항은 다음의 홈페이지의 R-package 설치 설명을
# 보고 따라하면 문제 없다!
  
# 1. Installing the CRAN package
# 현재 CRAN에 lightGBM은 존재하지 않는다.
# 하지만 CRAN에서 설치하는 것과 동일하게 쉽게 설치할 수있도록 배포된 LightGBM 3.0.0 을 설치하면 쉽게 설치가 가능하다.
# CMake, Visual Studio, Rtools를 설치하지 않아도 가능

PKG_URL <- "https://github.com/microsoft/LightGBM/releases/download/v3.0.0/lightgbm-3.0.0-r-cran.tar.gz"
remotes::install_url(PKG_URL)

# 2. Installing Precompiled Binaries
# they will be faster and easier to install than packages that are built from source

PKG_URL <- "https://github.com/microsoft/LightGBM/releases/download/v3.0.0rc1/lightgbm-3.0.0-1-r40-windows.zip"
local_file <- paste0("lightgbm.", tools::file_ext(PKG_URL))
download.file(
  url = PKG_URL
  , destfile = local_file
)

install.packages(
  pkgs = local_file
  , type = "binary"
  , repos = NULL
)

remove.packages("dplyr")
remove.packages("tidyselect")
install.packages("tidyselect")

## 실행이 잘 되는지 수행
library(lightgbm)
data(agaricus.train, package='lightgbm')
train  <- agaricus.train
dtrain <- lgb.Dataset(
  train$data, 
  label = train$label)
params <- list(objective = "regression", metric="l2")
model  <- lgb.cv(
  params, 
  dtrain, 
  10, 
  nfold    =  5, 
  min_data =  1, 
  learning_rate = 1, 
  early_stopping_rounds = 10)

######################
## LightGBM example ##
######################
library(data.table)
library(Matrix)
library(dplyr)
library(MLmetrics)
library(lightgbm)

setwd("C:/r")
set.seed(257)
train = fread("train.csv") %>% as.data.frame()
test  = fread("test.csv")  %>% as.data.frame()

# 결측치 --> median 치환 function
median.impute = function(x){
  x = as.data.frame(x)
  for (i in 1:ncol(x)){
    x[which(x[,i]== -1),i] = NA
  }
  
  x = x %>% mutate_all(~ifelse(is.na(.), median(., na.rm = TRUE), .)) %>% as.data.table()
  return(x)
}

#- Pre Processing
train = median.impute(train)
test  = median.impute(test)

#- Feature Engineering
test$target = NA
data        = rbind(train, test)

data[, fe_amount_NA := rowSums(data == -1, na.rm = T)]
data[, ps_car_13_ps_reg_03 := ps_car_13*ps_reg_03]
data[, ps_reg_mult := ps_reg_01*ps_reg_02*ps_reg_03]

#- Create LGB Dataset
varnames = setdiff(colnames(data), c("id", "target"))

train_sparse = Matrix(as.matrix(data[!is.na(target), varnames, with = F]), sparse=TRUE)
test_sparse  = Matrix(as.matrix(data[is.na(target),  varnames, with = F]), sparse=TRUE)

y_train  = data[!is.na(target),target]
test_ids = data[is.na(target) ,id]

lgb.train = lgb.Dataset(
  data  = train_sparse, 
  label = y_train)

categoricals.vec = colnames(train)[c(grep("cat",colnames(train)))]

#- Setting up LGBM Parameters
lgb.grid = list(objective = "binary",
                metric    = "auc",
                min_sum_hessian_in_leaf = 1,
                feature_fraction = 0.7,
                bagging_fraction = 0.7,
                bagging_freq = 5,
                min_data = 100,
                max_bin = 50,
                lambda_l1 = 8,
                lambda_l2 = 1.3,
                min_data_in_bin=100,
                min_gain_to_split = 10,
                min_data_in_leaf = 30,
                is_unbalance = TRUE)

#- Setting up Gini Eval Function
lgb.normalizedgini = function(preds, dtrain){
  actual = getinfo(dtrain, "label")
  score  = MLmetrics::NormalizedGini(preds,actual)
  return(list(name = "gini", value = score, higher_better = TRUE))
}

#- Cross Validation
lgb.model.cv = lgb.cv(
  params                = lgb.grid, 
  data                  = lgb.train, 
  learning_rate         = 0.02,                    #- *** 훈련량  
  num_leaves            = 25,
  num_threads           = 2,                       #- * 병렬처리시 처리할 쓰레드
  nrounds               = 7000, 
  early_stopping_rounds = 50,                      #- ** 더이상 발전이 없으면 그만두게 설정할때 이를 몇번동안 발전이 없으면 그만두게 할지 여부
                                                   #-    validation셋이 없으면 무용지물
  eval_freq             = 20, 
  eval                  = lgb.normalizedgini,
  categorical_feature   = categoricals.vec, 
  nfold                 = 5, 
  stratified            = TRUE)

best.iter = lgb.model.cv$best_iter

#- Train Final Model
lgb.model = lgb.train(
  params              = lgb.grid, 
  data                = lgb.train, 
  learning_rate       = 0.02,                        #- *** 훈련량  
  num_leaves          = 25,                          #- * 트리가 가질수 있는 최대 잎사귀 수
  num_threads         = 2 ,                          #- * 병렬처리시 처리할 쓰레드
  nrounds             = best.iter,                   #- *** 계속 나무를 반복하며 부스팅을 하는데 몇번을 할것인가이다. 1000이상정도는 해주도록 함
                                                     #-     early_stopping이 있으면 최대한 많이 줘도 (10,000~)별 상관이 없음
  eval_freq           = 20, 
  eval                = lgb.normalizedgini,
  categorical_feature = categoricals.vec)

#- Create and Submit Predictions
preds = data.table(id=test_ids, target=predict(lgb.model,test_sparse))
colnames(preds)[1] = "id"
fwrite(preds, "submission.csv")
