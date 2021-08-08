product<-read.csv("D:/R/Part-IV/product.csv",header = T)
head(product)
x = product$제품_적절성
y = product$제품_만족도
df<-data.frame(x,y)
result<-lm(formula = y~x,data=df)

names(result)
#모델의 적합한 값
fitted.values(result)[1:2]
head(df,1)

y1 = 0.7789 + 0.7393*4
3-3.735963

#모델의 잔차
residuals(result)[1:2]
-0.7359630 + 3.735963 

plot(formula=y~x, data=product)

result.lm = lm(formula = y~x, data=product)
abline(result.lm,col='red')

summary(result.lm)

x1<-product$제품_친밀도
x2<-product$제품_적절성
y <- product$제품_만족도 

df<-data.frame(x1,x2,y)
head(df)
#lm(formula = y~x1 + x2, data = df)
result.lm<- lm(y~x1 + x2, df)
result.lm

#다중 공선성 확인
install.packages("car")
library(car)

vif(result.lm)

summary(result.lm)

#독립변수 제거
names(iris)
table(iris$Species)
data("iris")

model<-lm(formula = Sepal.Length ~ Sepal.Width+
            Petal.Length+Petal.Width,data=iris)

sqrt(vif(model))>2

#상관계수 cor
head(iris[1:4])
cor(iris[,-5])


x<-sample(1:nrow(iris),nrow(iris)*0.7)
train<- iris[x,]
test<-iris[-x,]

# Petal.Width 제거
model<-lm(formula =Sepal.Length ~ Sepal.Width+
            Petal.Length,data=train)
model

summary(model)


lm(formula =Sepal.Length ~ Sepal.Width+Petal.Length,data=train)

head(train,1)

y1<-2.3693+ 0.5350*3+0.4799*4.5

5.6-y1


pred<- predict(model,test)

cor(pred,test$Petal.Length)

formula =Sepal.Length ~ Sepal.Width+Petal.Length+Petal.Width

model<-lm(formula = formula,data=iris)

install.packages("lmtest")
library(lmtest)
dwtest(model)
plot(model,which = 1)

attributes(model)

res<-residuals(model)
shapiro.test(res) # 정규성 검증

hist(res,freq = F)

summary(model)
