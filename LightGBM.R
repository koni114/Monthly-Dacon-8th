##############
## LightGBM ##
##############
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






