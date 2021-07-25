install.packages("dplyr")
library(dplyr)

install.packages("hflights")
library(hflights)
data(hflights)

dim(hflights)

df<-filter(hflights,hflights$Month == 1 , hflights$DayofMonth==1)

str(hflights)

hflights %>% filter(hflights$Month == 1 
                    , hflights$DayofMonth==1) 
df %>% filter(df$ArrDelay > 10)

arrange(hflights, hflights$Year, desc(hflights$Month),hflights$ArrTime)

dim(hflights)
names(hflights)

df<- select(hflights,Year:ArrTime)

dim(df)

# gain = arrdelay - depDelay
# gain_per_hour = gain/(airtime/60))


hflights_df<- hflights

names(hflights_df)
hflights_df<-mutate(hflights_df,
       gain = hflights_df$ArrDelay - hflights_df$DepDelay,
       gain_per_hour = gain/(AirTime/60)
       )

df<-select(hflights_df,Year,Month,ArrDelay,DepDelay,gain,gain_per_hour)

# 4분위수, 평균, 결측값(NA)
summary(df)
dim(df)

nrow(df)

df %>% nrow()
df %>% summarise(n())

hflights_df %>% summarise(cnt = n(), delay = mean(DepDelay))

str(hflights_df)

summary(hflights_df)

sum(hflights_df$DepDelay,na.rm = T)

!is.na(hflights_df$DepDelay)

hdf<- hflights

names(hdf)

group_by(hdf,hdf$TailNum) %>% tail() %>% select(Month:TailNum)

planes <- group_by(hdf,hdf$TailNum)
dim(hdf)
dim(planes)

planinfo<- summarise(planes, count=n()
          , dist=mean(Distance, na.rm=T)
          ,delay=mean(ArrDelay,na.rm=T))
result<-planinfo %>% filter(count>20,dist<2000)
result %>% head()


name<-c("홍길동","이순신","강감찬","철이","미애")
addr<-c("서울","서울","대구","부산","경기")

df<-data.frame(name,addr)

df %>% group_by(df$addr) %>% summarise(n())


# 서울 2
#  대구 1
# 부산 1
# 경기 1



df %>% select(name)

df[1]



# 서울  2
#  대구  1
#  부산 1
#  경기  1

getwd()
#E:\RData\Part-II

data<-read.csv("E:/RData/Part-II/data.csv")
str(data)
summary(data)
data %>% head()
install.packages("reshape2")
library(reshape2)
data %>% dcast(Date ~ Customer_ID, sum)
data %>% dcast(Customer_ID ~ Date)

head(data) %>% dcast(Customer_ID ~ Buy)

temp<-head(data)

temp_group<-group_by(temp,temp$Date,temp$Customer_ID)

temp_group_sum<-summarise(temp_group,sum(Buy))

names(temp_group_sum)<-c("Date","Customer_ID","Buy")
temp_group_sum
dcast(temp_group_sum,Customer_ID ~ Date)

# dcast 는 오후 다시 조사







# customer id   byu  의 합산
