
# 워드클라우드
Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk-16.0.2")
#Sys.getenv()

install.packages("rJava")
install.packages("remotes")
remotes::install_github('haven-jeon/KoNLP'
                        , upgrade = "never"
                        , INSTALL_opts=c("--no-multiarch")
)
install.packages("Sejong")
install.packages("wordcloud")
install.packages("tm")
install.packages("RSQLite")
library(KoNLP)
library(tm)
library(wordcloud)
# file 이라는 함수는 해당 파일을 다룰수 있는 권한을 준다
# file 은  file 객체를 반환한다. 핸들러

textcloud<-function(f,d){
  book<-file(f ,encoding='UTF-8')
  book_data<-readLines(book)
  head(book_data,1)
  #str(facebook_data)
  # 사전에 단어 추가
  user_dic<-data.frame(term=d,tag='ncn')
  buildDictionary(ext_dic = "sejong",user_dic=user_dic)
  
  # 문장에서 단어추출
  #paste(extractNoun("동해물과 백두산이 마르고 닳도록"),
  #      collapse=" ")
  exNouns<-function(x){
    paste(extractNoun(x),collapse=" ")
  }
  book_nouns<-sapply(book_data,exNouns)
  
  myCorpus<- Corpus(VectorSource(book_nouns))
  
  # 단어들의 집합에서 불필요한 단어들을 삭제
  # 단어들의 전처리(  VS 데이터들의 전처리 )
  myCorpusPrepro<-tm_map(myCorpus,removePunctuation)
  myCorpusPrepro<-tm_map(myCorpusPrepro,removeNumbers)
  myCorpusPrepro<-tm_map(myCorpusPrepro,tolower)
  myCorpusPrepro<-tm_map(myCorpusPrepro,removeWords
                         ,stopwords('english'))
  #inspect(myCorpusPrepro[1:2])
  
  # a   1byte   - character
  # 가  2byte   - string
  # 가나  4byte  
  
  myCorpusPrepro_term <- TermDocumentMatrix(myCorpusPrepro
       ,control = list(wordLengths=c(4,16)))
  
  myTerm_df<-as.data.frame(as.matrix(myCorpusPrepro_term))
  #dim(myTerm_df)
  #head(myTerm_df,1)
  #wordResult<-sort(rowSums(myTerm_df),decreasing=T)
  #wordResult[1:10]
  
  # 불용어 제거를 또함
  myCorpusPrepro<-tm_map(myCorpus,removePunctuation)
  myCorpusPrepro<-tm_map(myCorpusPrepro,removeNumbers)
  myCorpusPrepro<-tm_map(myCorpusPrepro,tolower)
  myStopword<-c(stopwords('english'),'사용','하기')
  myCorpusPrepro<-tm_map(myCorpusPrepro,removeWords,myStopword)
  # 단어뭉치를 평서문으로 전환
  myCorpusPrepro_term <- TermDocumentMatrix(myCorpusPrepro
                ,control = list(wordLengths=c(4,16)))
  myTerm_df<-as.data.frame(as.matrix(myCorpusPrepro_term))
  wordResult<-sort(rowSums(myTerm_df),decreasing=T)
  #wordResult[1:10]
  
  myName<- names(wordResult)
  word.df<-data.frame(word=myName,freq=wordResult)
  #head(word.df)
  #12가지 색상
  pal<- brewer.pal(12,"Paired")
  
  # 단어구름 시각화
  wordcloud(word.df$word, word.df$freq,scale = c(5,1)
            ,min.freq = 3, random.order = F
            ,lot.per=1,colors = pal,family="malgun"
  )
}

filepath<-"D:/R/Part-II/marketing.txt"
appendDic <- c("염전","노숙자")
facebook<-file(filepath ,encoding='UTF-8')
facebook_data<-readLines(facebook)
head(facebook_data,1)

textcloud(filepath,appendDic)
