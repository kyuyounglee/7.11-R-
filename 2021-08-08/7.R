# 150 ~ 160 : 6
# 160 ~ 170 : 4
data<-sample(1:100,50)
sort(data)
hist(data,freq = F,breaks = 30)
lines(density(data),col="red")
x<-seq(0,100,0.1)
curve(dnorm(x,mean(data),sd(data)),col="blue",add=T)



data<-rnorm(n=1000000,mean = 175,sd=5)
hist(data,freq = F,breaks = 1000)
lines(density(data),col="red")

