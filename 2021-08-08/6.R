data<-read.csv("D:/R/Part-III/one_sample.csv",header=T)
head(data)
x<-data$time
head(x)
#결측치 제거
summary(x)
x <- na.omit(x)
# 0.05보다 작으면 정규분포가 아님
shapiro.test(data$time)  # 정규분포

par(mfrow=c(1,2))
hist(x)
qqnorm(x)
qqline(x,lty=1,col="blue")


#5.2시간
t.test(x,mu=5.2)
# 0.0001417 < 0.05 귀무가설 기각  연구가설 채택



