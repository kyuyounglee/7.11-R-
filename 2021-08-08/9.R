setwd("D:/R/Part-III")
getwd()
data<- read.csv("descriptive.csv",header = T)
dim(data)
str(data)
summary(data)
table(data$gender)
data<-subset(data, data$gender == 1 | data$gender == 2)
barplot(table(data$gender))

#which -- index 방식
data<-subset(data,!is.na(data$level))

table(data$level)
barplot(table(data$level))

data$survey

data<-subset(data,!is.na(data$survey))
table(data$survey)

par(mfrow = c(1,1))
barplot(table(data$survey))
hist(data$survey)

data$cost

data<-subset(data,!is.na(data$cost))

plot(data$cost)

#1 boxplot stats 를 이용해서 이상치 제거
boxplot(data$cost)$stats

sr<-summary(data$cost)


IQR(data$cost)
IL<- sr[2] - 1.5*IQR(data$cost)
IU<- sr[5] + 1.5*IQR(data$cost)

# IL < data$cost < IU

data<-subset(data,data$cost > IL & data$cost < IU)


x <-data$cost

length(x)

x.t = table(x)

x.m<-rbind(x.t)
str(x.m)

x.df<-as.data.frame(x.m)
head(x.df,1)

attributes(x.df)



install.packages("moments")
library(moments)

cost<-data$cost

skewness(cost)
kurtosis(cost)
hist(cost,freq = F)
lines(density(cost),col = 'blue')
x<-c(2,8)
curve(dnorm(x,mean(cost),sd(cost)),col="red",add=T)

boxplot(cost)

install.packages("Hmisc")
library(Hmisc)
describe(data)

install.packages("prettyR")
library(prettyR)
freq(data)


