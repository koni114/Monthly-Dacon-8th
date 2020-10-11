#- 투표율  : country side > Suburban > Urban
#- 투표율  : Christian_Protestant > Jewish > Christian_Catholic > Christian_Other > Agnostic...
#- 인종별 투표율 : white > Aurtralian > American > Black > Asian > Arab
#- 성별에 따른 투표율은 크게 변화 없음
#- 형제자매수가 증가할수록(~15까지) 투표율이 어느정도 증가함을 확인
#- 모국어가 영어인 경우, 투표율이 더 높아짐
#- 교육수준이 높아질수록 투표율은 높아짐
#- 연령대별 투표비율은 나이가 많아질수록 투표율이 높아짐
##############
## CatBoost ##
##############


#- 투표를 했는데,(Yes, 0), 투표를 하지 않았다고 예측한 경우,(No, 1)
load("testData_wrongNo_CatBoost.RData")




