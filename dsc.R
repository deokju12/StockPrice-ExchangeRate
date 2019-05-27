ex <- read.csv("finally.csv")
# 공분산행렬로 상관관계파악
pairs(ex[,c("one","two","three","four",	"five",	"six",	"seven"	,"eight",	
                      "nine",	"ten",	"ele",	"binary")])


# 각 종목별 분포 확인
par(mfrow=c(3,4))
for(i in 2:12){plot(ex$change_val ~ ex[,i], data=ex)}

class(ex$one)
# 로지스틱 함수로 적합해보기
# 종목별 적합 후 유의미한지 파아
library(ISLR)
#건설
glm.fit1= glm(binary~one, data=ex, family=binomial)
summary(glm.fit1)
#금융
glm.fit2= glm(binary~two, data=ex, family=binomial)
summary(glm.fit2)
#경기소비재
glm.fit3= glm(binary~three, data=ex, family=binomial)
summary(glm.fit3)
#산업재
glm.fit4= glm(binary~four, data=ex, family=binomial)
summary(glm.fit4)
#생활소비재 p값이 0.06
glm.fit5= glm(binary~five, data=ex, family=binomial)
summary(glm.fit5)
#에너지/화학 p값이 0.06
glm.fit6= glm(binary~six, data=ex, family=binomial)
summary(glm.fit6)
#정보기술
glm.fit7= glm(binary~seven, data=ex, family=binomial)
summary(glm.fit7)
#중공업 p값이 0.87
glm.fit8= glm(binary~eight, data=ex, family=binomial)
summary(glm.fit8)
#철강/소재
glm.fit9= glm(binary~nine, data=ex, family=binomial)
summary(glm.fit9)
#커뮤니케이션서비스 p값이 0.117
glm.fit10= glm(binary~ten, data=ex, family=binomial)
summary(glm.fit10)
#헬스케어 p값이 0.525
glm.fit11= glm(binary~ele, data=ex, family=binomial)
summary(glm.fit11)

# 결과적으로 코스피200을 3년치 조사한 결과
# 건설, 금융, 건설, 경기소비재, 산업재, 정보기술, 철강/소재 분야 
# 는 P값이 0.05보다 작아 유의미하므로 환율과 주가가 음의 상관관계를 보인고 볼 수 있다.
# 그 외에 종목들은 P값이 너무 높아 유의미하다고 볼 수 없다.
# 그 중에 특히 중공업, 커뮤니케이션, 헬스케어는 거의 관련 없다고 볼수 있다.










