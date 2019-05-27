ex <- read.csv("finally.csv")
# ���л���ķ� ��������ľ�
pairs(ex[,c("one","two","three","four",	"five",	"six",	"seven"	,"eight",	
                      "nine",	"ten",	"ele",	"binary")])


# �� ���� ���� Ȯ��
par(mfrow=c(3,4))
for(i in 2:12){plot(ex$change_val ~ ex[,i], data=ex)}

class(ex$one)
# ������ƽ �Լ��� �����غ���
# ���� ���� �� ���ǹ����� �ľ�
library(ISLR)
#�Ǽ�
glm.fit1= glm(binary~one, data=ex, family=binomial)
summary(glm.fit1)
#����
glm.fit2= glm(binary~two, data=ex, family=binomial)
summary(glm.fit2)
#���Һ���
glm.fit3= glm(binary~three, data=ex, family=binomial)
summary(glm.fit3)
#�����
glm.fit4= glm(binary~four, data=ex, family=binomial)
summary(glm.fit4)
#��Ȱ�Һ��� p���� 0.06
glm.fit5= glm(binary~five, data=ex, family=binomial)
summary(glm.fit5)
#������/ȭ�� p���� 0.06
glm.fit6= glm(binary~six, data=ex, family=binomial)
summary(glm.fit6)
#�������
glm.fit7= glm(binary~seven, data=ex, family=binomial)
summary(glm.fit7)
#�߰��� p���� 0.87
glm.fit8= glm(binary~eight, data=ex, family=binomial)
summary(glm.fit8)
#ö��/����
glm.fit9= glm(binary~nine, data=ex, family=binomial)
summary(glm.fit9)
#Ŀ�´����̼Ǽ��� p���� 0.117
glm.fit10= glm(binary~ten, data=ex, family=binomial)
summary(glm.fit10)
#�ｺ�ɾ� p���� 0.525
glm.fit11= glm(binary~ele, data=ex, family=binomial)
summary(glm.fit11)

# ��������� �ڽ���200�� 3��ġ ������ ���
# �Ǽ�, ����, �Ǽ�, ���Һ���, �����, �������, ö��/���� �о� 
# �� P���� 0.05���� �۾� ���ǹ��ϹǷ� ȯ���� �ְ��� ���� ������踦 ���ΰ� �� �� �ִ�.
# �� �ܿ� ������� P���� �ʹ� ���� ���ǹ��ϴٰ� �� �� ����.
# �� �߿� Ư�� �߰���, Ŀ�´����̼�, �ｺ�ɾ�� ���� ���� ���ٰ� ���� �ִ�.









