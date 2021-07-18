# os --> mac
rcsv<-function(filepath,os="mac"){
  # 리턴값을 저장할 df 변수 정의
  df<- data.frame()
  #조건문 os 파라메터값이 mac 과 win일때의 로직일 다름
  if(os == "mac"){
    df<-read.csv(filepath
                 ,fileEncoding = "CP949",encoding = "UTF-8"
                 ,header = T )  
  }else{
    df<-read.csv(filepath,header = T )  
  }
  # 최종 결과값을 함수를 호출한 곳으로 df 값을 전달함함
  return (df)
}

# 분산과 표준편차
# param 
#       x : 벡터
var_sd<-function(x){
  #표본분산
  var<-sum(x - mean(x)/2) / length(x)-1
  sd<-sqrt(var)
  cat("var : ",var,"\n")
  cat("sd : ",sd)
}

# is.na()는 결측치를 true로 보고
# !is.na()는 결측치를 false로 보고 그래서 베터에 []안에 넣으면
# 결측치가 제거된 데이터셋을 얻는다... 그러나. 기존
# data.frame에 추가를 못한다. (why?? row의 갯수가 안 맞아서)
rmna<- function(x){
  return (x[!is.na(x)])
}

# 원본 data.fram에 컬럼을 추가 na은 0으로
# df    ,colname    ,addcolname
# 원본    찾을컬럼     추가할 컬럼
replacena0<-function(df,colname,addcolname){
  newcol<- ifelse(is.na(df$colname),0,df$colname)
  df$addcolname<- newcol
}






