# Monthly-Dacon-8th

- 월간 데이콘 8 - 심리 성향 예측 AI 경진대회
- 2020.09.28 ~ 2020.11.16

### 주요 내용
- 심리학 테스트 분석 알고리즘 개발
- 마키아벨리즘 심리 테스트를 활용하여 테스트 참가자의 국가 선거 투표 여부 예측

### 결과 및 성과
- 1171팀 중 8위(상위 1%)로 입상
- 많이 사용하는 Boosting Ensemble Model 사용 및 Study
  - LightGBM
  - CatBoost    

### Language
- CatBoost : R
- NN : python

### 폴더 구조
```
├── main
    └── dacon-8th-forthewinner.ipynb : 최종 pipeline source code
├── etc : 분석 결과 정리 및 모델 성능 정리 등
├── data : 발표 자료 원본 노트북 파일
    └── train.csv : train dataset
    └── test_x.csv : test dataset
├── history : 기타 스크립트 파일
└── study : 
    └── CatBoost
        └── CatBoost_install_example.R : CatBoost 설치 및 예제
    └── LightGBM 
        └── LightGBM_install_example.R : LightGBM 설치 및 예제
        └── LightGBM_basic_walkthrough.R : Basic walkthrough of wrappers
        └── LigthGBM_boost_from_prediction.R : Boosting from existing prediction
        └── LightGBM_categorical_features_rules : categorical_features_rules
        └── LightGBM_weight_param : Weight-Parameter Adjustment Relationship
        └── LightGBM_leaf_stability : leaf_stability
```

### 어려웠던 점 & 개선 점
- 변수 조합 별 모델의 성능 history를 관리하고 싶은데, 엑셀로 정리하려고 하니 관리가 잘 안됨  
  - 어떻게 하면 잘 관리할 수 있을까?  
  - 관리 시스템이나 관리 tool 같은 건 없을까?
- pytorch 공부가 전혀 안되어 있다보니, 소스 공유방에 올라온 NN 모델을 개선 할 방법이 없었음
  - NN 모델을 간단하게나마 개선했더라면 더 좋은 성과를 이룰 수 있을 것임

### 생각해보기
- ML pipeline 설계의 고민
  - 항상 고집해오던 [마트 편성 - 이상치, 결측치 처리 - 파생변수 생성 - 모델링] process가 과연 정답일까? 
- 추측만 있고, 확신은 없다
  - T-Test나 chi-sqaure 검정을 통해 투표 여부 별 X인자의 유의미한 차이가 있는지를 파악하고  
    최대한 근거 있는 분석을 해보려고 노력했지만, 성능은 좋아지지 않고  
    나빠지는 경우도 많았음.
    --> statistics-based analysis 라서 그런 것일까? 
  - explainable AI 서적을 좀 보면 해결되지 않을까?  역시 해답은... 공부!
