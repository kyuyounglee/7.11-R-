# dplyr 패키지 
install.packages("dplyr")
library(dplyr)
iris %>% head()

iris %>% filter(iris$Species == 'setosa')  # 검색 조건
iris %>% select(Species)                  # 원하는 컬럼정보만 추출

# arrange를 활용하면..  min max 도 구할수 있다
iris %>% arrange(Sepal.Length) %>% tail()
dim(iris)
iris %>% summarise(mean(Sepal.Length),n())

iris_group<- iris %>% group_by(Species)

iris_group %>% summarise(mean(Sepal.Length),
                         mean(Sepal.Width))



