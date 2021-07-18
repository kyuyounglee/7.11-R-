no <-c(1,2,3)
name<-c("hong","lee","kim")
pay<-c(150,250,300)
vemp<-data.frame(no=no,name=name,pay=pay)
vemp

#matrix(data = NA, nrow = 1, ncol = 1, byrow = FALSE,dimnames = NULL)
m<-matrix( c(1,"hong",150,2,"lee",250,3,"kim",300)
           ,nrow=3,byrow=T)

getwd()
setwd("E:/RData/Part-I")

# full path   절대경로
#'E:/RData/Part-I/emp.txt'
# 'E:/RData/Part-IV/admit.csv'

# 상대경로 
read.table("emp.txt")
# 절대경로
read.table("E:/RData/Part-I/emp.txt")

# 1은  true   0 : false
emptxt =  read.table("emp.txt",header = T)


read.table("emp.txt")
empcsv = read.csv("emp.csv",header = T)

str(emptxt)
str(empcsv)

empcsv$no # vector
empcsv[1] # data.frame
# , 로 이루어진 [] --> 가로
empcsv[1,] # data.frame
empcsv[,1] # vector

empcsv$name
empcsv[2]
empcsv[2,]


empcsv$pay
empcsv[3]

str(empcsv)
summary(empcsv)

empcsv[1:5,1]   #1,1  2,1  3,1 4,1  5,1
empcsv[1,1:3]

member <- list(name="홍길동", age=30, addr = "한양")

member$age

names(member)


nonkeymembers= list("홍길동",30,"한양")

nonkeymembers[[1]]

list = unlist(nonkeymembers) 
str(list)

##############################################
# data frame 2
empcsv = read.csv("emp.csv",header = T)
no <-c(1,2,3)
name<-c("hong","lee","kim")
pay<-c(150,250,300)
vemp<-data.frame(no=no,name=name,pay=pay)

list =  list(data1=empcsv,data2=vemp,data3=c(20:100))

list$data3

# list data.fram

# 자료구조
# vector 
# matrix
a = matrix(c(1:10),nrow = 2)
data.frame(a)


h<-data.frame(id=c(1,2), h=c(180,175))
w<-data.frame(id=c(1,2),w=c(80,75))

h;w
merge(h,w)

install.packages("stringr")
library(stringr) # 메모리 적재
strdata = "123456789"
str_extract(strdata,"[0-9]{2}")
str_extract_all(strdata,"[0-9]{2}")
str_replace_all(strdata,"df","")

substr=str_sub(strdata,5,9)

df<-data.frame()
exam= edit(df)
names(exam) <-c("학번","성명","국어","영어","수학")
exam[-1,]

df<-data.frame(exam[-1,])
names(df) <- c("학번","성명","국어","영어","수학")
df$성명

index<-which(df$국어==70)
index<-which(df$성명=="이순신")

df[index,]["성명"]

# 성명이 이순신인 데이터를 찾고자
findCol = "국어"
findData = 70
findIndex = which(df[findCol] == findData)
df[findIndex,][findCol]


getwd()
df<- read.table("student3.txt",header = T, na.strings = '-')
read.table(file.choose(),header = T,sep="\t")

write.table(df,file="student3_1.txt")
df<- read.table("student3_1.txt",header = T, na.strings = '&')


df[1,4]<- ""

install.packages("readxl")
library(readxl)
read_excel("studentexcel.xlsx")
read.csv("studentexcel.xlsx")

sum(df["키"],na.rm = T)

#titanic<-read.csv("https://github.com/agconti/kaggle-titanic/blob/master/data/train.csv")

getwd()
setwd("E:/RData/Part-I")
titanic<-read.csv("train.csv",header = T)

str(titanic)
nrow(titanic)
ncol(titanic)

summary(titanic)
titanic$Sex  # 결측치  NA
genderT<-table(titanic$Sex)
survivedT<-table(titanic$Survived)

head(titanic)
tail(titanic)

par(mfrow=c(1,2))
barplot(genderT, col=rainbow(2))
barplot(survivedT,col=rainbow(2))

par(mfrow=c(1,1))
tab<-table(titanic$Survived, titanic$Sex)
barplot(tab,col=rainbow(2),main="성별에 따른 생존율")


# 데이터 구조    
# 그 데이터를 가지고 가공......
table(titanic$Pclass)

# 연산자.... 사칙연산자  + - * /
# %% 나머지 연산  
# 이세상 존재 하는 모든수  %% 2   0 과 1 이다 

#  오른쪽에 값을 왼쪽에 대입하는 대입연산자
test = c(6:15)
a = 10
b = 11
# 비교연산자  같다 크다 작다 크거나같다 작거나같다
# 같지 않다
a == b
!(a != b)     # not 연산자

a >= B
a <= b

# ~~~ 이고 ~~ 이다  교집합  *
# 저렴하고 좋은곳   1.저렴하다  2.좋은곳

# ~~ 또는(이거나) ~~ 이다  합집합  +
# 싸거나 맞있는 집  1. 값이 싸다, 2 맛이 좋다

# true    false
#   1    x   0  = 0
#   0    x   1  = 0
   
num1 <- 10   
num2 <- 50   

num1<0 & num2 < 100  # and  
num1<0 | num2 < 100  # or  
year <- 2024
#100으로 나누어지지 않고 4로 나누어지면 윤년
year %% 100 != 0 & year %% 4 == 0 
#400으로 나누어 지는 해는 윤년
year %% 400 == 0

((year %% 100 != 0) & (year %% 4 == 0) | year %% 400 == 0)


# 국어 영어  수학의 평균이 60점이상이고 
# 각 과목당 점수가 40점이상이면 합격
kor<-70
eng<-80
math<-40
avg<- (kor+eng+math)/3

kor>=40 & eng>=40 & math>=40 & avg>=60


if(kor>=40 & eng>=40 & math>=40 & avg>=60){
  cat("합격")  
}else{
  cat("불합격")  
}

# 변수 x가 50이상 100미만
x <- 20

x>=50 & x<=100

x<50 | x > 100 # -->!(x>=50 & x<=100)



if(x>90){  #1
}else if(x>50){
  #2
}else{
  #3
}
#1 숫자--> 101 102 103


data <-59


if(data>=90){
  cat("A")
}else if(data >= 80){
  cat("B")
}else if(data >= 70){
  cat("C")
}else if(data >= 60){
  cat("D")
}else{
  cat("F")
}


ifelse(data>=60,"우수","노력")

choice<-"status"

switch (choice,
  age = 100,
  name = "100",
  id = 1,
  status = "GOOD",
)


test<- c(10:15)
index <- which(test == 12)
test[index]

getwd()

df<-read.csv("emp.csv",header = T)

index<-which(df$pay >= 350 & df$pay <= 450)
df[index,]

summary(titanic$Age)

#891
length(titanic$Age)

# 어린이를 0 ~ 19
index<-which(titanic$Age<19)
length(index)
titanic[index,]

139/891*100
