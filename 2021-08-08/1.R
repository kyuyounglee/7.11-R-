#분류분석석
install.packages("party")
library(party)

install.packages("datasets")
library(datasets)
str(airquality)

formula <- Temp ~ Solar.R + Wind + Ozone
air_ctree<- ctree(formula,data=airquality)

plot(air_ctree)

set.seed(10000)
idx<-sample(1:nrow(iris), nrow(iris)*0.7)
train<-iris[idx,]
test<-iris[-idx,]
names(iris)
formula<-Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width
iris_tree<-ctree(formula =formula,data=train )
plot(iris_tree,type="simple")

pred <-predict(iris_tree)
table(pred,test$Species)
(12+14+15)/nrow(test)

names(iris)
plot(iris$Petal.Length~iris$Petal.Width
     ,col = iris$Species)

(12+14+15)/nrow(test)

library(cvTools)



cross <- cvFolds(nrow(iris),K=3,R=2)

R <- 1:2

CNT = 0
for(r in R){
  CNT = CNT+1  
}
print(CNT)



