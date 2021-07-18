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


