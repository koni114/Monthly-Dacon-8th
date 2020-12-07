## LightGBM R 설치 방법
과거 LightGBM 3.0.0 version 전까지만 해도 R에서 LightGBM package를 설치하기란 굉장히 까다로웠다.   
하지만 3.0.0 version 부터는 비록 현재 CRAN 에는 해당 패키지가 없지만,  
굉장히 쉽게 설치가 가능하다. (원래는 CMake, Visual Studio, RTools 를 설치하여 package를 
설치해야 했었다..)

하지만 LightGBM 3.0.0 version을 설치하려면, R version이 4.0.0 이상이어야 한다!  
만약 현재 설치하고자 하는 R의 version이 4.0.0 이하라면, LightGBM 3.0.0 version을 설치하고  
모델 생성 함수를 실행하면, R이 강제적으로 terminate 되는 오류가 발생한다..  

따라서 나는 R-version을 4.0.0 이상 최신 버전으로 upgrade 했다 

~~~
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
~~~


