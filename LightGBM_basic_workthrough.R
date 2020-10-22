library(lightgbm)
library(methods)

#-  agaricus dataset example
#- 버섯이 먹을수 있는지, 먹을수 없는지를 분류
#- data는 sparseMatrix로 구성. label은 수치형 vector(0, 1)으로 구성

data(agaricus.train, package = "lightgbm")
data(agaricus.test, package = "lightgbm")
train <- agaricus.train
test  <- agaricus.test

class(train$label)
class(train$data)

#######################################
## lightgbm을 사용하여 모델 학습하기 ##
#######################################
#- data field에 matrix를 넣어 lightgbm model을 구현 할 수 있음
#- lightgbm 모델은 자동으로 sparse input을 처리
#- feature가 sparse한 matrix를 그대로 사용 가능(ex) one-hot encoding 된 경우 등..)

print("Training lightgbm with sparseMatrix")
bst <- lightgbm(
  data  = train$data, 
  label = train$label,  
  num_leaves = 4L,        #- 트리가 가질 수 있는 최대 잎사귀 수
  learning_rate = 1.0,    #- 학습율
  nrounds = 2L,           #- 훈련 interation 수 
  objective = "binary"
)

#-  lgb.Dataset object로 생성해 data를 지정하여 lightGBM 모델을 생성할 수도 있다
#- 자세한 기능을 모르지만, 좀 더 확장된 meta data 관리 기능을 제공하는 듯 하다..
print("Training lightgbm with lgb.Dataset")
dtrain <- lgb.Dataset(
  data  = train$data,
  label = train$label
)
bst <- lightgbm(
  data  = dtrain, 
  num_leaves = 4L,        #- 트리가 가질 수 있는 최대 잎사귀 수
  learning_rate = 1.0,    #- 학습율
  nrounds = 2L,           #- 훈련 interation 수 
  objective = "binary"
)

#- verbose = 0 지정시, message 출력 안함
#- verbose = 1 지정시, evaluation metric 출력
#- verbose = 2 지정시, tree 정보 출력
bst <- lightgbm(
  data = dtrain
  , num_leaves = 4L
  , learning_rate = 1.0
  , nrounds = 2L
  , objective = "binary"
  , verbose = 2L
)

##################################
## lightgbm을 사용하여 예측하기 ##
##################################
#- 예측시, testData는 Matrix, sparseMatrix, lgb.Dataset으로 구성되어야 함
pred <- predict(bst, test$data)
err <- mean(as.numeric(pred > 0.5) != test$label)
print(paste("test-error=", err))

##################################
## lightgbm 모델 저장, Load하기 ##
##################################
#- lgb.save :  binary local file로 모델 저장
lgb.save(bst, "lightgbm.model")

#- lgb.load : Load binary model to R
bst2  <- lgb.load("lightgbm.model")
pred2 <- predict(bst2, test$data)

# pred2 should be identical to pred
print(paste("sum(abs(pred2-pred))=", sum(abs(pred2 - pred))))

#######################
## Advanced features ##
#######################
#- advanced feature를 사용하고 싶다면, lgb.Dataset에 입력
dtrain <- lgb.Dataset(data = train$data, label = train$label, free_raw_data = FALSE)
dtest  <- lgb.Dataset.create.valid(dtrain, data = test$data, label = test$label)

#########################
## Validation set 구성 ##
#########################
#- lgb.Dataset, lgb.Dataset.create.valid 함수를 이용하여 dtrain, dtest 객체를 생성하고, 
#- list로 묶어 valids parameter에 할당

valids <- list(train = dtrain, test = dtest)
bst <- lgb.train(
  data          = dtrain,
  num_leaves    = 4L,
  learning_rate = 1.0,
  nrounds       = 2L,
  valids        = valids,
  nthread       = 2L,
  objective     = "binary"
)

#- metric 변경 가능. multi metric도 지정 가능
bst <- lgb.train(
  data          = dtrain,
  num_leaves    = 4L,
  learning_rate = 1.0,
  nrounds       = 2L,
  valids        = valids,
  eval          = c("binary_error", "binary_logloss"),
  nthread       = 2L,
  objective     = "binary"
)

############################
## lgb.Dataset 저장, load ##
############################
#- save
lgb.Dataset.save(dtrain, "dtrain.buffer")

#- load
dtrain2 <- lgb.Dataset("dtrain.buffer")




