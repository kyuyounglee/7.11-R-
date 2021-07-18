# os --> mac
readcsvWinOrMac<-function(filepath,os="win"){
  # 리턴값을 저장할 df 변수 정의
  df<- data.frame()
  #조건문 os 파라메터값이 mac 과 win일때의 로직일 다름
  if(os == "mac"){
    df<-read.csv(filepath
                 ,fileEncoding = "CP949",encoding = "UTF-8"
                 ,header = T )  
  }else if(os=="win"){
    df<-read.csv(filepath,header = T )  
  }
  # 최종 결과값을 함수를 호출한 곳으로 df 값을 전달함함
  return (df)
}

