# 2011 ~ 2013 소매가격 지역별 돼지고기 가격

install.packages("corrplot")

#  installed 된 여러 lib 가져오기
install_library <- c("plyr","ggplot2","stringr","zoo"
                     ,"corrplot","RColorBrewer")
unlist(lapply(install_library, require,character.only=T))


product<- read.csv("D:/R/lecture/Data/product.csv",header = T)
str(product)
code<- read.csv("D:/R/lecture/Data/code.csv",header = T)
str(code)

View(product)
View(code)

# 가공... 필요한 요소만 추출
names(product)<-c('date','category','item','region'
                  ,'mart','price')

category <- subset(code,code$구분코드설명=='품목코드')

names(category)<-c('code','exp','item','name')

# 코드성 데이터중에 구분코드가  지역코드
region<-subset(code,code$구분코드설명=='지역코드')

names(region)<-c('code','exp','region','name')

#  정리
# product / code
# catagory / region --- from code

# 돼지고기 값  514
total.pig<- subset(product,product$item == 514)
region

day.pig <- merge(total.pig,region,by="region",all=T)
head(day.pig)
# ddply() : 프레임형태로 분리하여 각 함수적용시킨후 데이터 프레임 현태로 출력
# dlply() :  프레임형태로 품목별로 list 형태로 출력

temp<- ddply(day.pig, .(date), summarise, name=name,region=region,price=price)
temp2<-ddply(temp, .(date,name),summarise,mean.price = mean(price))

total.pig.mean<-dlply(temp2,.(name))


# 각 지역별 데이터의 크기를 확인
attributes(total.pig.mean)
for( i in 1:length(total.pig.mean) ){
  cat(names(total.pig.mean)[i],"의 데이터의 길이는"
      , nrow(total.pig.mean[[i]])
      ,"이다","\n"
      )  
}

# na  이상치........ 길이가 안맞는 지역을 제외 
# 데이터의 길이가 745가 아닌 지역명을 다 찾자.
rmArea <-c()
for( i in 1:length(total.pig.mean) ){
  if(nrow(total.pig.mean[[i]]) != 745){
    print(names(total.pig.mean)[i])
    rmArea<- append(rmArea,names(total.pig.mean)[i])
  }
}

# total.pig.mean에서 제거한다.
# 데이터[! 변수 %in% 조건, ]
day.pig<-day.pig[! day.pig$name %in% rmArea, ]
str(day.pig)
table(day.pig$name)

#regin(지역), date(일자) 별 평균 가격
pig.region.daily.mean <- ddply(day.pig, .(name,region,date)
                               , summarise,mean.price=mean(price) )

head(pig.region.daily.mean)

# 년도별 월별 평균가격
month<-str_sub(pig.region.daily.mean$date,1,7)
prig.region.monthly.mean <- ddply(pig.region.daily.mean, .(name,region,month)
      , summarise,mean.price=mean(mean.price))
head(prig.region.monthly.mean)

# 년도별 평균 가격
year<-str_sub(prig.region.monthly.mean$month,1,4)
prig.region.yearly.mean <- ddply(prig.region.monthly.mean, .(name,region,year)
                                  , summarise,mean.price=mean(mean.price))
head(prig.region.yearly.mean)

# 월별 돼지고기 가격 시각화
prig.region.monthly.mean$month<- as.Date(as.yearmon(prig.region.monthly.mean$month, "%Y-%m"))

ggplot(prig.region.monthly.mean,
       aes(x=month, y=mean.price, colour=name, group=name)) +
       geom_line()+theme_bw()+geom_point(size=6,shape=20,alpha=0.5)+
       ylab("돼지고기 가격") + xlab("")


