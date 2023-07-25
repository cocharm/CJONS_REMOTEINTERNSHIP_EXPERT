install.packages("psych")
install.packages("GPArotation")
library(psych)
library(GPArotation)

#상품 중분류/구매개수 기준
#데이터 가공
food_mcls_N_product=read.csv("food_mcls_N_product_by_cust.csv", header = T, stringsAsFactors=T)
name=food_mcls_N_product[,1]
food_mcls_N_product = data.frame(food_mcls_N_product, row.names = name)
food_mcls_N_product = food_mcls_N_product[,-1]
View(food_mcls_N_product)
str(food_mcls_N_product)

#유효한 요인의 개수 판단
#고유값 중 1이상('Kaiser 규칙')
state.factor=principal(food_mcls_N_product, rotate="none")
state.factor$values #유효한 요인의 개수=34개 

#요인분석
state.varimax=principal(food_mcls_N_product, nfactors = 4, rotate="varimax")
state.varimax
#34개의 요인들로 전체 분산의 62%를 설명함.

#상품 중분류/장바구니 기준
#데이터 가공
food_mcls_N_rct_by_cust=read.csv("food_mcls_N_rct_by_cust.csv", header = T, stringsAsFactors=T)
name=food_mcls_N_rct_by_cust[,1]
food_mcls_N_rct_by_cust = data.frame(food_mcls_N_rct_by_cust, row.names = name)
food_mcls_N_rct_by_cust = food_mcls_N_rct_by_cust[,-1]
View(food_mcls_N_rct_by_cust)
str(food_mcls_N_rct_by_cust)

#유효한 요인의 개수 판단
#고유값 중 1이상('Kaiser 규칙')
state.factor=principal(food_mcls_N_rct_by_cust, rotate="none")
state.factor$values #유효한 요인의 개수=32개 

#요인분석
state.varimax=principal(food_mcls_N_rct_by_cust, nfactors = 4, rotate="varimax")
state.varimax
#32개의 요인들로 전체 분산의 62%를 설명함.

##분석결과
#요인1 – 갑각류, 건면, 건어물, 계란류, 과채음료, 국산과일, 국산돼지고기, 국산소고기, 나물류, 냉동간편식, 냉장간편식
#요인2 – 가공유, 기능성음료, 냉장간편식,
#요인3 - 간식,안주형과자, 견과류, 껌
#요인4 – 갑각류, 건면, 기능성음료
#너무 변수들이 많아서 인사이트가 잘 나오지 않음

#상품 대분류/구매개수 기준
food_mcls_N_rct_by_cust=read.csv("food_hlv_N_product_by_cust.csv", header = T, stringsAsFactors=T)
name=food_mcls_N_rct_by_cust[,1]
food_mcls_N_rct_by_cust = data.frame(food_mcls_N_rct_by_cust, row.names = name)
food_mcls_N_rct_by_cust = food_mcls_N_rct_by_cust[,-1]
View(food_mcls_N_rct_by_cust)
str(food_mcls_N_rct_by_cust)

#유효한 요인의 개수 판단
#고유값 중 1이상('Kaiser 규칙')
state.factor=principal(food_mcls_N_rct_by_cust, rotate="none")
state.factor$values #유효한 요인의 개수=3개 

#요인분석
state.varimax=principal(food_mcls_N_rct_by_cust, nfactors = 3, rotate="varimax")
state.varimax
#33개의 요인들로 전체 분산의 52%를 설명함.

##분석결과
#요인1-채소 조미료 과일 축산물 양곡 건해산물 병통조림
#요인2-음료 과자 냉동식품 냉장식품 조리식품 대용식 유제품 주류 
#요인3-커피/차 수산물
#이 데이터에서는 건강식품 데이터가 작아서인지 어느 요인에도 속하지 않았음

#상품 대분류/장바구니 기준
food_mcls_N_rct_by_cust=read.csv("food_hlv_N_rct_by_cust.csv", header = T, stringsAsFactors=T)
name=food_mcls_N_rct_by_cust[,1]
food_mcls_N_rct_by_cust = data.frame(food_mcls_N_rct_by_cust, row.names = name)
food_mcls_N_rct_by_cust = food_mcls_N_rct_by_cust[,-1]
View(food_mcls_N_rct_by_cust)
str(food_mcls_N_rct_by_cust)

#유효한 요인의 개수 판단
#고유값 중 1이상('Kaiser 규칙')
state.factor=principal(food_mcls_N_rct_by_cust, rotate="none")
state.factor$values #유효한 요인의 개수=3개 

#요인분석
state.varimax=principal(food_mcls_N_rct_by_cust, nfactors = 3, rotate="varimax")
state.varimax
#33개의 요인들로 전체 분산의 63%를 설명함.

#장바구니 데이터가 더 타당(여기서는 건강식품 데이터가 유의미했음)
#요인1-채소 조미료 축산물 과일 수산물 건해산물 양곡 병통조림 유제품 >> 요리하는 사람
#요인2-음료 과자 조리식품 대용식 냉동식품 냉장식품 주류 >> 요리 안하는 사람
#요인3-건강식품 커피/차
#유제품이 전 데이터에서는 요인1에 이번 데이터에서는 요인2에 속하는 것이 차이점, 요리 안하는 사람에 주류가 속해있는 것이 특징




