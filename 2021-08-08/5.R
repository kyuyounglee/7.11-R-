s1<-c(1,2,1,2,3,4,2,3,4,5)
s2<-c(1,3,1,2,3,4,2,4,3,4)
s3<-c(2,3,2,3,2,3,5,3,4,2)
s4<-c(2,4,2,3,2,3,5,3,4,1)
s5<-c(4,5,4,5,2,1,5,2,4,3)
s6<-c(4,3,4,4,2,1,5,2,4,2)
subject <- data.frame(s1,s2,s3,s4,s5,s6)
pc<-prcomp(subject)
summary(pc)
plot(pc,ylim=c(0,7))
par(mfrow=c(1,1))

en<-eigen(cor(subject))
str(en)

en$values

plot(en$values,type = 'o')

cor(subject)
#s1 = s2  s3= s4 s5 = s6

#
result<-factanal(subject, factors = 2, rotation = "varimax")
result

result<-factanal(subject, factors = 3
                 , rotation = "varimax"
                 ,scores = "regression")
result


