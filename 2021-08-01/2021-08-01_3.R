mySetEnv<-function(){
  Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk-16.0.2")
  install.packages("rJava")
  
  install.packages(c("Sejong","wordcloud","tm"
                     ,"RSQLite","httr","XML"))
  
  library("Sejong")
  library("wordcloud")
  library("tm")
  library("RSQLite")
  library("httr")
  library("XML")
}

myKoNLP<-function(){
  install.packages("remotes")
  remotes::install_github('haven-jeon/KoNLP'
                          , upgrade = "never"
                          , INSTALL_opts=c("--no-multiarch")
  )
  library(KoNLP)
}

myCrolling<-function(url,myClass,userDic,filepath){

  web<-GET(url)
  
  html<-htmlTreeParse(web,useInternalNodes = T, trim = T
                      ,encoding = 'UTF-8')
  rootNode<- xmlRoot(html)
  #class='cluster_text_headline'
  #news<-xpathSApply(rootNode,"//a[@class='link_txt']",xmlValue)
  news<-xpathSApply(rootNode,myClass,xmlValue)
  #news<- news[1:46]
  
  news_pre<-gsub("[\r\n\t]",' ',news)
  news_pre<-gsub("[[:punct:]]",' ',news_pre)
  news_pre<-gsub("[[:cntrl:]]",' ',news_pre)
  news_pre<-gsub("[\"]",' ',news_pre)
  news_pre<-gsub("[0-9]",' ',news_pre)
  news_pre<-gsub("[a-z]",' ',news_pre)
  news_pre<-gsub("[A-Z]",' ',news_pre)
  news_pre<-gsub("\\s+",' ',news_pre)
  news_pre
  
  write.csv(news_pre, filepath,quote = F)
  news_data<-read.csv(filepath
                      ,header = T,stringsAsFactors = F)
  
  
  str(news_data)
  
  head(news_data)
  names(news_data)<-c('no','news_text')
  str(news_data)
  
  news_text <- news_data$news_text
  #단어추가
  user_dic<-data.frame(term=userDic)
  buildDictionary(ext_dic='sejong',user_dic = user_dic)
  
  
  exNouns<-function(x){
    paste(extractNoun(x),collapse = " ")
  }
  news_nouns<- sapply(news_text, exNouns)
  #말뭉치
  newsCorpus<- Corpus(VectorSource(news_nouns))
  inspect(newsCorpus[1:5])
  
  # 2음절 ~8음절
  tdm<-TermDocumentMatrix(newsCorpus,control=list(wordLengths=c(4,16)))
  tdm.df<-as.data.frame(as.matrix(tdm))
  dim(tdm.df)
  
  wordResult<-sort(rowSums(tdm.df),decreasing =T )
  wordResult[1:10]
  
  library(wordcloud)
  myNames<-names(wordResult)
  myNames
  
  df<-data.frame(word=myNames,freq=wordResult )
  pal<-brewer.pal(12,'Paired')
  
  wordcloud(df$word, df$freq,scale = c(4,0.7)
            ,min.freq = 2, random.order = F
            ,lot.per=1,colors = pal,family="malgun"
  )
}


# 함수 실행
#환경셋팅 함수 실행
mySetEnv()
myKoNLP()
#함수에 필요한 데이터
url<-"https://www.ytn.co.kr/issue/corona.php"
myClass<-"//span[@class='til']"
userDic<-c("이재명","청해부대","이준석"
           ,"이낙연·정세균","경기"
           ,"북도","주장")
filepath <-"D:/R/Part-II/news_daum.csv"
#구름 클라우드 실행
myCrolling(url,myClass,userDic,filepath)
