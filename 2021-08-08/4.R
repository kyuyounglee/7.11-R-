data<-read.csv("D:/R/Part-III/product.csv",header=T)
table(data$제품_친밀도)
summary(data)
cor(data)

install.packages("corrgram")
library(corrgram)

corrgram(data,upper.panel = panel.conf)
corrgram(data,lower.panel = panel.conf)

install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

 package_version(R.version)
# T F 구분할때 만약 숫자로 한다면.. 0이면 F 그외 나머지는 T 
chart.Correlation(data, pch="+") 


#data$제품_친밀도
#data$제품_적절성
#plot(data$제품_친밀도,data$제품_적절성)
#df<- data.frame(a=data$제품_친밀도,b=data$제품_적절성)
#head(df)
#table(data$제품_친밀도,data$제품_적절성)

