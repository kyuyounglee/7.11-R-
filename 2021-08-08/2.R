filePath = "D:/R/Part-IV/"
fileName = "weather.csv"
str<-paste(filePath,fileName,sep  = "")
weather<-read.csv(str,header = T)
str(weather)
names(weather)
#1,6,8,14
weather_df<-weather[,c(-1,-6,-8,-14)]
weather_df$RainTomorrow<- ifelse(weather_df$RainTomorrow == 'Yes',1,0)

index<-sample(1:nrow(weather_df),nrow(weather_df)*0.7)
train<-weather_df[index,]
test<-weather_df[-index,]

names(weather_df)
# 학습
weather_model<- glm(RainTomorrow~., data=train,family = 'binomial')
summary(weather_model)
# 검증하기위해서 검증데이터를 모델로 값을 추출했을때 나오는 값
pred<-predict(weather_model,newdata = test,type="response")

result_pred <-ifelse(pred>0.5,1,0)

table(result_pred)
66/300*100
table(weather_df$RainTomorrow)
17/93*100

install.packages("ROCR")
library(ROCR)

pr<-prediction(pred,test$RainTomorrow)
class(pr)
prf<-performance(pr,measure="tpr", x.measure="fpr")
plot(prf)
