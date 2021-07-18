getwd()


# emp2.csv
df<-read.csv("E:/RData/Part-I/emp.txt",header = T)
df
names(df)<- c("NO","NAME","VALUE")
df

# 반복문 구구단  4 단  
4 * 1
4 * 2
4 * 3
4 * 4
4 * 5
4 * 6
4 * 7
4 * 8
4 * 9

"4"+"\n"

for( data in c(1:9)) {
  result<- 4 * data
  #result <-as.character(result)
  cat("4 x ",data," = ", result , "\n")
}  

# 짝수만 출력
for (n in c(1:10)) {
  if (n%%2 ==0) {
    print(n)
  }
}
# 홀수만 출력
#     n %% 2 != 0
#     !(n %% 2 == 0)
for(n in c(1:10)){
  if( n %% 2 != 0 ) {
    print(n)
  }
}

i = c(1:10)
for(n in i){
  if(n %% 2 == 1){
    print(n)
  }
}

#화면 분활
par(mfrow = c(1,1))
barplot(table(df$NAME))
table(df$NAME)

str(df$NAME)

df$NAME[2] ; df$VALUE[2]


# 강감찬 -> 500
str(df$NAME)


df<-read.csv("E:/RData/Part-I/emp.txt",header = T)
names(df)<- c("NO","NAME","VALUE")

n<-1
for (s in df$NAME) {
  cat(s,"->",df$VALUE[n],"\n")
  n <- n+1 
}

for (i in c(1:length(df$NAME))) {
  cat(df$NAME[i],"->",df$VALUE[i],"\n")
}


for (i in df$NAME) {
  print(i)
}
n<-1
for (s in df$NAME){
  cat(s, "->", df$VALUE[n], "\n")
  n = n+1
  
}

# 순환문을 중간에 제어할때
# next
# break
i = 0
while (TRUE) {
  i = i+1
  if(i %%2 == 0){
    next
  }
  if(i >= 10){
    break
  }
  print(i)
}


data<-10  # 변수를 정의하고 데이터를 셋팅

greeting <- function(name = "아무개"){
  cat(name,"님 안녕하세요")
  return ("success")
}

result = greeting("이규영" )    # 함수를 사용할때는 함수명() 호출해야 함

ADD <- function(x=0,y=0){
  return (x+y)
}

result<- ADD(10,50)
result

install.packages("readxl")
library(readxl)
read_excel("E:/RData/Part-I/studentexcel.xlsx")



df<-read.csv("E:/RData/Part-I/emp2.csv"
         ,fileEncoding = "CP949",encoding = "UTF-8" )


source("E:/RData/Part-I/myutil.R")
datadf<- readcsv("E:/RData/Part-I/emp2.csv","mac")

2**3
2^3

# 4 단 출력
gugudan <- function(dan){
  for (index in c(1:9)) {
    cat(dan," x ",index," = ",dan*index,"\n")
  }  
}


# 2 ~ 9 단까지 출력

# gugudan(2)   ~  gugudan(9)

for(index in c(2:9)){
  gugudan(index)
  cat("----------------------------------\n")
}

source("E:/RData/Part-I/myutil.R")
datadf<- rcsv("E:/RData/Part-I/emp2.csv","win")


test <- read.csv("E:/RData/Part-I/test.csv",header = T)
head(test,2)

#요약통계량
summary(test)
#빈도수
table(test$A)
#시각화
barplot(table(test$A),col = rainbow(5))


# test data.fram 의 각 컬럼에 대한 빈도수를 구해보자
table(test$A)
table(test[1])

1:length(names(test))



names(test)[4]


barTest<-function(){
  test <- read.csv("E:/RData/Part-I/test.csv",header = T)
  colCount<-length(names(test))
  par(mfrow = c(2,3))
  for(index in 1:colCount){
    print(table(test[index]))
    barplot( table(test[index])
             ,col=rainbow(colCount),main=names(test)[index]  
    )
  }
}

barTest()


x<-c(7,5,12,9,15,6)
length(x)
sum(x - mean(x)/2) / length(x)-1

Sys.setlocale("LC_ALL","korean")

source("E:/RData/Part-I/myutil.R")
var_sd(x)


titanic<-read.csv("E:/RData/Part-I/train.csv",header = T)
head(titanic)
summary(titanic)

mean(titanic$Age,na.rm = T)

# 결측치를 제거하는 방법
# 1. 해당 값을 제거하는 방법( 실제 제거, 통계량을 구할때 계산에 미포함)
# 2. N/A  를 0으로 대처하거나. .평균으로 대처


is.na(titanic$Age)

x<- c(1,2,NA,NA,5)
!is.na(x)
x[!is.na(x)]

# NA를 찾아서 0으로 대체

ifelse(is.na(x),0,x)

avg<-mean(titanic$Age,na.rm = T)

titanic$NotNAage2<- ifelse(is.na(titanic$Age),avg,titanic$Age)
summary(titanic)

str(titanic$Age)


# 891
length(titanic$Age) 

# 714
length(titanic$Age[!is.na(titanic$Age)])

titanic$test<-titanic$Age[!is.na(titanic$Age)]

# is.na()는 결측치를 true로 보고
# !is.na()는 결측치를 false로 보고 그래서 베터에 []안에 넣으면
# 결측치가 제거된 데이터셋을 얻는다... 그러나. 기존
# data.frame에 추가를 못한다. (why?? row의 갯수가 안 맞아서)

# 난수생성  실수
runif(10,min=0,max=100)

# 난수생성  정수
str(sample(1:100,size=10))

v<-sample(1:100,size=10)
# [1]  53 100  64  65  85
v[1]
v[c(TRUE,FALSE,FALSE,TRUE,FALSE)]


# 벡터에서 60보다 큰 수를 추출
v + 10
v > 60
v[c(v > 60)]

# 벡터에서 60보다 큰 수는 0으로 대처
ifelse(v > 60,0,v)

# 벡터에서 60보다 큰 수는 평균값으로 대처
ifelse(v>60, mean(v), v)

age<-sample(1:100,size=10)
gender<-sample(0:100,size=10)
gender<-gender %% 2
gender<-ifelse(gender == 0,"woman","man")
# 0 woman
# 1 man
# 0 0 0  1 1 0 1 0 1 0

df<-data.frame(age,gender)
df$agetype<-0

summary(titanic)
titanic$AgeType<- ifelse(titanic$Age>19,"adult","child")
titanic$AgeType<-as.factor(titanic$AgeType)

titanic$Embarked<-as.factor(titanic$Embarked)

table(titanic$AgeType)
table(titanic$Sex)
table(titanic$Survived)


# 이어서서
par(mfrow = c(1,1))
titanic$AgeType<- ifelse(is.na(titanic$AgeType),"NON",titanic$AgeType)
barplot(table(titanic$AgeType),table(titanic$Survived))
