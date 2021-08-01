.libPaths()
Sys.getenv()
install.packages("lattice")
library(lattice)
str(VADeaths)
class(VADeaths)
head(VADeaths)
dft<-as.data.frame.table(VADeaths)
head(dft)
#기본차트
barplot(VADeaths,beside = T)

barchart(Var1~Freq|Var2,data = dft,layout=c(4,1)
         ,origin=0)

dotplot(Var1~Freq, data=dft,layout=c(4,1)
        ,type="o"
        ,groups=Var2
        ,auto.key = list(space="right",points=T,lines=T)
        )
library(datasets)
head(airquality)

xyplot(Ozone~Wind|factor(Month),data=airquality
       ,main="월별 바람에 세기에 따른 오존측정"
       ,layout=c(5,1))
table(airquality$Month)

head(quakes)

xyplot(lat~long,data=quakes)
summary(quakes$depth)
# 40 ~ 150   x = 150
# 151 ~ 250  x = x+1 ~ x = x+99
# 251 ~ 350  x = x+1 ~ x = x+99
# 351 ~ 450  x = x+1 ~ x = x+99
# 451 ~ 550  x = x+1 ~ x = x+99
# 551 ~ 680  x = x+1 ~ x = x+99

quakes$depth2[quakes$depth >=40 & quakes$depth<=150]<-1
x<-150
for(i in 2:6){
  quakes$depth2[quakes$depth >=(x = x+1) 
                & quakes$depth<=(x = x+99)]<-i
}
table(quakes$depth2)
xyplot(lat~long|factor(depth2),data=quakes,pch="."
       ,layout = c(6,1))

depthgroup<-equal.count(quakes$depth,number=5,overlap = 0)

xyplot(lat~long |depthgroup,data=quakes
       ,col="red",pch='.')

cloud(depth ~ lat*long,data=quakes
      ,zlim=rev(range(quakes$depth))
      ,col=quakes$color
    )

col<-c("red","green","yellow","blue","black","orange")

for(i in 1:6){
  quakes$color[quakes$depth2 == i] <-col[i]
}

install.packages("ggplot2")
library(ggplot2)
data(mpg)
summary(mpg)
mpg$hwy
qplot(hwy,data=mpg,fill=class)

qplot(displ,hwy,data=mpg,color=drv)

summary(diamonds)
p<-ggplot(diamonds,aes(carat,price,color=cut))
p+geom_point()
