path<- "D:/R/Part-II/dataset.csv"
dataset<- read.csv(path,header = T)
View(dataset)
head(dataset)
tail(dataset)

names(dataset)
attributes(dataset)
str(dataset)

# job age price 데이터만 조회
dataset[c('job','age','price')]
dataset$job
plot(dataset$price)

#결측치 제거  확인하기 가장좋은 방법
summary(dataset$price)
# 결측치가 제대로 제거되었는지 확인
sum(dataset$price,na.rm = T)

# 1 is.na
price1<-dataset$price[!is.na(dataset$price)]
sum(price1)
# 2 na.omit
price2<-na.omit(dataset$price)
sum(price2)
# 3. complete.cases  == !is.na
price3<-dataset$price[complete.cases(dataset$price)]
sum(price3)

x<-dataset$price
x[1:30]
#1  0 으로 대체
dataset$price2<- ifelse(is.na(x),0,x)
dataset$price2[1:30]
#2  평균으로 대체
dataset$price3<-ifelse(is.na(x),mean(x,na.rm = T),x)
dataset$price3[1:30]
# 문자가 들어왔다...... 모든 데이터는 문자형태로 변환
# 정수가 있을때.. 실수가 들어오면  실수형태로
# 실수가 있을때 .. 정수가 들어오면 해당정수는 실수로 변경
mean(x,na.rm = T)
round(mean(x,na.rm = T),2)

names(dataset)
dataset[c(6,8,9)]

#결측치를 전처리
#극단치(이상치) 전처리

# 데이터 전처리 --- 결측치 & 극단치 제거

dataset$gender
table(dataset$gender)
pie(table(dataset$gender))

dataset<-subset(dataset,gender ==1 | gender == 2)
pie(table(dataset$gender))

# 연속형 데이터의 극단치 제거
path<- "D:/R/Part-II/dataset.csv"
dataset<- read.csv(path,header = T)
names(dataset)
dataset$price
#  2 ~ 8사이의 데이터만 추출
dataset2<-subset(dataset,price >=2 & price <=8)
dataset2$price
table(dataset2$price)
stem(dataset2$price)

# 기술통계량을 구하면 결측치의존재 및극단치를 확인할수 있다
summary(dataset2$age)

boxplot(dataset2$price)
boxplot(dataset$price)
summary(dataset$price)
names(boxplot(dataset$price))
boxplot(dataset$price)$stats



dataset_sub<- subset(dataset, price>=2.1 & price<=7.9)
summary(dataset_sub$price)
boxplot(dataset_sub$price)


# 1 서울특별시
# 2 인천광역시
# 3 대전광역시
# 4 대구 광역시
# 5 시구군

n<-c("1.서울특별시","2.인천광역시","3.대전광역시"
     ,"4.대구광역시","5.시구군")
for( i in 1:5){
  dataset2$resident2[dataset2$resident == i]<-n[i]
}

n<-c("공무원","회사원","개인사업")
for( i in 1:length(n)){
  dataset2$job2[dataset2$job == i]<-n[i]
}
dataset2[c('job2','resident2')]

dataset2$age2[dataset2$age <=30]<- "청년층"
dataset2$age2[dataset2$age > 30 & dataset2$age <= 55]<- "중년층"
dataset2$age2[dataset2$age > 55]<- "장년층"

dataset2$survey2<- 6-dataset2$survey

head(dataset2)

dataset2$resident2
dataset2$job2

par(mfrow = c(1,2))
regidence_job <- table(dataset2$job2,dataset2$resident2)
barplot(regidence_job,beside = T, 
        col = rainbow(3)
        ,legend = row.names(regidence_job),ylim = c(0,45))

# 범주형 vs 범주형
# barplot 

# 범주형 vs 연속형

install.packages("lattice")
library(lattice)
dataset2$pri
densityplot(~price,data=dataset2,groups = job2,
            plot.points=T,auto.key = T)

names(dataset2)

#가격 성별, 직급

names(dataset2)

table(dataset2$gender)
dataset2 <- subset(dataset2, gender == 1 | gender==2)
# gender2 컬럼에 1은 남자  2는 여자로 

dataset2$gender2[dataset2$gender == 1] <- "남자"
dataset2$gender2[dataset2$gender == 2] <- "여자"

table(dataset2$gender2)



densityplot(~price|factor(dataset2$resident2),
            data = dataset2,
            group = position,
            plot.point = T, auto.key = T
            )
factor(dataset2$resident2)
xyplot(price ~ age|factor(gender2), data = dataset2)

user_data<- read.csv("E:/RData/Part-II/user_data.csv",header = T)

user_data$house_type2<- ifelse(user_data$house_type==1 
                               |user_data$house_type==2,0,1 )


pay_data<-read.csv("E:/RData/Part-II/pay_data.csv",header = T)
head(pay_data)

install.packages("reshape2")
library(reshape2)
names(pay_data)
head(pay_data)
# dcase 는 원본데이터를 가로로 배치하기 때문에..  data override 되느몰
# 컬럼명을 변경해야 함
                # row ~ col  
product_price<-dcast(pay_data, user_id ~ product_type, sum,na.rm=T )

names(product_price)<-c("user_id",'식료품(1)','생필품(2)'
                        ,'의류(3)','잡화(4)','기타(5)')

head(product_price)

pay_price<-dcast(pay_data,user_id ~ pay_method,length)
tail(pay_price,20)

for( i in 2:5){
  names(pay_price)[i] <- paste(names(pay_price)[i],"(",i-1,")",sep="")
}

# dcast  넓은 표현으로 돌릴때... reshape2
# join   plyr
install.packages("plyr")
library(plyr)

head(user_data);head(product_price)

user_pay_data<- join(user_data,product_price,by = 'user_id')
head(user_pay_data)

# dcast  이용해서 원하는 형태로 변경(파생변수)
# join  공통 컬럼이 있어야 함(기준) 그리고 모든 데이터를 표시
head(pay_price)

# user_pay_data 업데이트
user_pay_data<- join(user_pay_data,pay_price,by='user_id')
head(user_pay_data)

# user_pay_data 구매금액 지불유형 구매상품 등...
 str(user_pay_data)
 #7 8 9 10 11
 #user_pay_data$총구매금액<-user_pay_data[,7]+user_pay_data[,8]+user_pay_data[,9]+user_pay_data[,10]
 apply(user_pay_data[7:11],1,sum)
 user_pay_data$총구매금액2 <- apply(user_pay_data[7:11],1,sum)
 #test<-data.frame(name=c(1,2,3,4,5),age=c(10,20,30,40,50))
 #apply(test, 1, sum)

 head(user_pay_data)

 
 # apply  dcast, join , head, tail, str, summary, attributes, class, dim
 # barplot,pie --> 범주형
 # densityplot --> 연속 데이터1개   범주형 범주형...
 # xyplot -->  범주 연속데이터가 여러개
 
 
 # 데이터 전처리.....
 # 결측치를 제거 또는 대처
 # 이상치를 제거 -- boxplot()$stats  boxplot  (3q-1q)  irq    
 # min -irq*1.5< x < max+irq*1.5
 
 
 user_pay_data
 
 write.csv(user_pay_data, "E:/RData/Part-II/cleanData.csv", row.names = F,quote = F)
 data<-read.csv("E:/RData/Part-II/cleanData.csv",header = T)


choice1<-sample(nrow(data),30)
choice2<-sample(50:nrow(data),30)
choice3<-sample(50:100,30)
choice4<-sample(c(10:50,80:150,160:190),30)

data[choice1,]
par(mfrow = c(1,1))
barplot(table(data[choice1,]$house_type,
              data[choice1,]$resident),
        beside = T,col = rainbow(data[choice1,]$house_type),
        #,legend = names(data[choice1,]$resident)
        ,legend.text = T
        )
# 데이터를 수집 -> 분석(인사이트를 도출) ->
# 전처리  -> 적절한 시각화
# -> 분석을 위한 clean  Data set
# 공부........ -> 시험  
# 학습데이터  검정데이터
dim(iris)
idx<-sample(1:nrow(iris),nrow(iris)*0.7)

install.packages("cvTools")
library(cvTools)

name<-c("a",'b','c','d','e','f')
score<-sample(80:99,6)
df<-data.frame(name=name,score=score)
nrow(df)

cross<-cvFolds(n=nrow(df),K=3,R=1,type='random')
str(cross)

# 검증데이터하고 훈련데이터를 추출
index<-cross$subsets[cross$which==1]
cross$subsets[cross$which==2]
cross$subsets[cross$which==3]
df[index,]


k <- 1:3
for(i in k){
  index<-cross$subsets[cross$which==i]
  check<-df[index,] # 검증데이터
  training<-df[-index,] # 훈련데이터
  print("====================")
  cat('k=',i,'검증데이터\n')
  print(check)
  cat('k=',i,'훈련데이터\n')
  print(training)
}
cross<- cvFolds(n=nrow(iris),K=3,type = 'random')

install.packages("mlmRev")
library(mlmRev)
data("Chem97")

str(Chem97)
summary(Chem97)
boxplot(Chem97)
par(mfrow = c(1,1))

hist(Chem97$gcsescore)
histogram(~gcsescore|factor(score),data=Chem97)
str(factor(Chem97$score))
str(Chem97$score)


