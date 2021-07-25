# EDA
# 데이터를 수집,이해 -> 전처리 -> 데이터셋 생성 ->분석모델

dataset<-read.csv("E:/RData/Part-II/dataset.csv")

summary(dataset)

plot(dataset$price,ylim = c(-10,20))


install.packages("carData")
library(carData)

room.calss<-TitanicSurvival$passengerClass
room.calss %>% str()
tbl<-table(room.calss) #도수분포
sum(tbl)


barplot(tbl,main="선실별 탑승객",xlab='선실등급',
        ylab='탑승객수', col=rainbow(3))

tbl/sum(tbl)
pie(tbl,main="선실별 탑승객",col=rainbow(3))

# 단일변수 범주형
room.calss

# 단일변수 수치형
# state.x77   미국 50개주에대한 1970년대 통계
# HS Grad 고등학교 졸업률

state.x77 %>% head() %>% str()
summary(state.x77)
grad<-state.x77[,'HS Grad']
names(grad)
#사분위수
summary(grad)
var(grad)   # 분산
sd(grad)    # 표준편차
#히스토그램
hist(grad, main="주별 고등학교 졸업률",
      xlab="졸업율",ylab="주의 개수",col="orange")

# Q1 히스토그램에서 해당하는주가 어떤 주인지?

boxplot(grad,main="주별 졸업률")

# 졸업률이 가장 낮은주  높은주 평균 이하인주
# index 방식
index<-which(grad == min(grad) | grad == max(grad))
sort(grad[index]) %>% names()

# 평균이하인 주들..
index<-which(grad<mean(grad))
grad[index] %>% names()

#단일변수 수치형 : 영국 페 질환자 사망자 분석
ds<-read.csv("E:/RData/fdeaths.csv",row.names = 'year')
my.col<-c('black','blue','red','green','purple','dark gray')
my.lty <- 1:6
plot(1:12,ds[1,],main="월별 사망자 추이",type='b',
     lty=my.lty[1],xlab="Month",ylab="사망자수",ylim=c(300,1200),
     col=my.col[1])
for(n in 2:6){
  lines(1:12,ds[n,],type='b',lty=my.lty[n],col=my.col[n])  
}
legend(x='topright',lty=my.lty,
       col=my.col,legend = 1974:1979)

# 단일변수는 
#   범주형  타이타닉의 선실등급  1st 2st 3st
#   수치형  졸업률, 사망률


# 다중변수 데이터  화씨온도에대한 기압
# 개별분석이 아니라 변수간의 관계

pressure %>% head()
plot(pressure$temperature,pressure$pressure,
     main="온도와 기압",xlab='온도(화씨)',ylab="기압")

cor(pressure$temperature,pressure$pressure)

# 자동차 속도와 제공거리
cars %>% head()
plot(cars$speed,cars$dist)
cor(cars$speed,cars$dist)

# 다중변수가 수치상으론재 할때... 3개 이상 데이터
class(state.x77)

# matrix array --> data.frame
st<-data.frame(state.x77)
plot(st)
cor_data<-cor(st)
# 산점도를 보는 이유  다중 산점도 포함
# 서로 연관 있는 데이터들을 추출하기 위함
#cor_data[cor_data>=0.65 | cor_data <= -0.65]

# Q2 데이터 프레임 또는 매트릭스 구조에서 상관관계에있는 데이터만
# 추출출
ifelse(cor_data>=0.65 | cor_data <= -0.65,cor_data,0)


# 상관과계는  인관관계를 뜻하지 않습니다.

# 주택담보대출 데이터 탐색
install.packages("Ecdat")
library(Ecdat)
Hdma %>% tail()

tbl<-table(Hdma$deny)
tbl<-tbl / sum(tbl)


names(tbl)<-c("승인","거절")
barplot(tbl, main="주담대 승인/거절",col=rainbow(2),
        ylim = c(0,2)
        )


# 흑인
Hdma$black
#대출 승인 거절여부
Hdma$deny

# 흑인 신청자 중에 거절 비율 black.deney
# 거절비율 =  거절건수 / 전체건수
black.deney<-sum(Hdma$black == 'yes' & Hdma$deny == 'yes') / sum(Hdma$black == 'yes')
# 비흑인 신청자 중에 거절 비율 non.black.deney
non.black.deney<- sum(Hdma$black == 'no' & Hdma$deny == 'yes') / sum(Hdma$black == 'no')
black.deney;non.black.deney


# 평균.... 흑인과 비 흑인의 신용등급
length(Hdma$dir)
length(Hdma$ccs)

black.credit<- Hdma$ccs[Hdma$black == 'yes']
non.black.credit<- Hdma$ccs[Hdma$black == 'no']
mean(black.credit);mean(non.black.credit)


temp<-c(10,15,13,14,15,300)
mean(temp)


str(iris)

iris[iris$Sepal.Length > 5.0,]

iris[order(iris$Sepal.Length),]

max(iris$Sepal.Length)

iris[132,]



# order vs sort
# sort 는 vector 바로 정렬

# order vector 가능 data.frame 및 기다터 

