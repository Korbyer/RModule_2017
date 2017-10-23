# https://cran.r-project.org/web/packages/Rfacebook/Rfacebook.pdf
#install.packages("KoNLP")
#install.packages("ggplot2")
#install.packages("scales")
#install.packages("Rfacebook")
#install.packages("rJava", type='source')
#install.packages("rvest")
#install.packages("xml2")
#install.packages("wordcloud2")
#install.packages("extrafont")
library(rjson)
library(xlsx)
library(stringr)

Sys.setenv(JAVA_HOME = '/Library/Java/JavaVirtualMachines/1.6.0.jdk/Contents/Home')
#Sys.setlocale("LC_COLLATE", "ko_KR.UTF-8")
library(wordcloud2)
library(rJava)
library(Rfacebook)
library(ggplot2)
library(scales)
library(KoNLP)
library(rvest)
library(qgraph)
library(tm)
library('xml2')
library(httr)
# get auth token
fb_oauth = fbOAuth(app_id = "1798552803791858", app_secret = "e93479f5c3f7f2d7fc581d58c262ba3f", extended_permissions = FALSE)


# 해당 페이지의 시작날짜와 종료날짜 사이의 모든 포스트 가져옴
getPosts <- function(page_name, start_date, end_date) {
  # 날짜 sequence
  scrape_days <- seq(from=as.Date(start_date), to=as.Date(end_date), by='days')
  posts = c()
  for(scrape_day in scrape_days){
    daypost = c()
    tryCatch({
      daypost = getPage(page=page_name, token=fb_oauth,
                        since=as.Date(scrape_day, origin='1970-01-01'),
                        until=as.Date(scrape_day, origin='1970-01-01')+1)},
      error = function(e){})
    posts = rbind(posts, daypost)
  }
  return(posts)
}

drawLikeGraph <- function(data){
  ggplot(data, aes(x=created_time, y=likes_count)) + geom_line() + 
    theme_bw() + scale_x_date(labels = date_format("%m-%Y")) +
    labs(x = "Date(MM-yyyy)", y = "NumOfLike(n)") + 
    ggtitle(paste(start_date, end_date, sep="~"))
}

drawPostNumGraph <- function(data){
  ggplot(data=data, aes(x=name, y=num, fill=name)) +
    geom_bar(stat="identity", width=0.8) +
    labs(x="Candidates", y="NumOfPosts") +
    geom_text(aes(label=num), vjust=1.6, color="white", size=3.5) +
    theme_minimal() +
    ggtitle(paste(start_date, end_date, sep="~"))
}

drawAverageLikeGraph <- function(data){
  ggplot(data=data, aes(x=name, y=average_like, fill=name)) +
    geom_bar(stat="identity", width=0.8) +
    labs(x='Candidates', y='AvgLike') +
    geom_text(aes(label=round(average_like, 0)), vjust=1.6, color="white", size=3.5) +
    theme_minimal() +
    ggtitle(paste(start_date, end_date, sep="~"))
}

getSummaryDataFrame <- function(posts, name){
  average_like = sum(posts$likes_count)/nrow(posts)
  summary <- data.frame(name=name, num=nrow(posts),
                        average_like=average_like)
  return(summary)
}

getPostMessage<-function(data){
  Encoding(data$message) # 크롤링 자료 message를 Code화 되어있는것을 처리
  localeToCharset()
  Sys.setlocale("LC_ALL","en_US.UTF-8")
  useNIADic() ## KoNLP 라이브러리 메소드, 형태소 다운로드
  all.reviews<-data$message
  all.reviews<-all.reviews[!is.na(all.reviews)]
  return(all.reviews)
}

makeMessageNoun<-function(data){
  nouns<-extractNoun(data)
  nouns_norm<-Map(function(x){if(!is.na(x)&&is.ascii(x))toupper(x)else x},unlist(nouns))
  return(nouns_norm)
}

drawWordCloud<-function(data){
  cnts<-table(unlist(data))
  cnts_<-cnts[cnts>2 & nchar(names(cnts))>1]
  wordcloud2(data.frame(word=names(cnts_),freq=as.numeric(cnts_)),color="random-light",backgroundColor = "white",shape = "cloud")
}

survey_url="https://namu.wiki/w/%EC%A0%9C19%EB%8C%80%20%EB%8C%80%ED%86%B5%EB%A0%B9%20%EC%84%A0%EA%B1%B0/%EC%97%AC%EB%A1%A0%EC%A1%B0%EC%82%AC"
html<-read_html(survey_url)
htmlInfos <- html_nodes(html, css='.wiki-table')

moon_page = "moonbyun1" # 문재인 페이지 
#hong_page = "joonpyohong21" # 홍준표 페이지
ahn_page = "ahncs111" # 안철수 페이지
sim_page = "simsangjung" # 심상정 페이지
#yoo_page = "sminyoo/" # 유승민 페이지



start_date <- '2017/04/03' # 시작 날짜
end_date <- '2017/05/04' # 종료 날짜

moon_posts <- getPosts(moon_page, start_date, end_date)
#hong_posts <- getPosts(hong_page, start_date, end_date)
ahn_posts <- getPosts(ahn_page, start_date, end_date)
sim_posts <- getPosts(sim_page, start_date, end_date)
#yoo_posts <- getPosts(yoo_page, start_date, end_date)

print(nrow(moon_posts))

# preprocess => 날짜처리
moon_posts$created_time <- as.Date(moon_posts$created_time) # string to date
#hong_posts$created_time <- as.Date(hong_posts$created_time) 
ahn_posts$created_time <- as.Date(ahn_posts$created_time)
sim_posts$created_time <- as.Date(sim_posts$created_time)
#yoo_posts$created_time <- as.Date(yoo_posts$created_time)

# summary 데이터 프레임 만듦
moon_summary <- getSummaryDataFrame(moon_posts, 'Moon')
#hong_summary <- getSummaryDataFrame(hong_posts, '홍준표')
ahn_summary <- getSummaryDataFrame(ahn_posts, 'Ahn')
sim_summary <- getSummaryDataFrame(sim_posts, 'Sim')
#yoo_summary <- getSummaryDataFrame(yoo_posts, '유승민')

summary <- data.frame() # 빈 데이터 프레임을 만듦
summary <- rbind(summary, moon_summary) # 행추가
#summary <- rbind(summary, hong_summary)
summary <- rbind(summary, ahn_summary)
summary <- rbind(summary, sim_summary)
#summary <- rbind(summary, yoo_summary)

drawPostNumGraph(summary)
drawAverageLikeGraph(summary)

# reactions <- getReactions(post=posts$id, fb_oauth, verbose=TRUE)
drawLikeGraph(moon_posts)
#drawLikeGraph(hong_posts)
drawLikeGraph(ahn_posts)
drawLikeGraph(sim_posts)
#drawLikeGraph(yoo_posts)

all_posts <- data.frame()
all_posts <- rbind(all_posts, moon_posts)
#all_posts <- rbind(all_posts, hong_posts)
all_posts <- rbind(all_posts, ahn_posts)
all_posts <- rbind(all_posts, sim_posts)
#all_posts <- rbind(all_posts, yoo_posts)


moon_message<-getPostMessage(moon_posts)
ahn_message<-getPostMessage(ahn_posts)
sim_message<-getPostMessage(sim_posts)

moon_message_final<-makeMessageNoun(moon_message)
ahn_message_final<-makeMessageNoun(ahn_message)
sim_message_final<-makeMessageNoun(sim_message)

drawWordCloud(moon_message_final)
drawWordCloud(ahn_message_final)
drawWordCloud(sim_message_final)
export_message<-data.frame(moon_message)

write.csv(moon_posts,"moon_posts.csv")
write.csv(moon_message,"moon_message.csv")

#명사 형용사 추출 함수 생성
ko.words<-function(doc){
  d<-as.character(doc)
  pos<-paste(SimplePos09(d))
  extracted<-str_match(pos,'([가-힣]+)/[NP]')
  keyword<-extracted[,2]
  keyword[!is.na(keyword)]
  return(keyword)
}
showApriori<-function(data){
  x<-c()
  data<-repair_encoding(data,from='utf-8')
  if(length(data)==0){break}
  data<-str_trim(data)
  x<-c(x,data)
  options(mc.cores=1)
  cps<-Corpus(VectorSource(x))
  tdm<-TermDocumentMatrix(cps,
                          control=list(tokenize=ko.words,
                                       removePunctuation=T,
                                       removeNumbers=T,
                                       wordLengths=c(2,6),
                                       weighting=weightBin))
  dim(tdm)
  tdm.maxtrix<-as.matrix(tdm)
  rownames(tdm.maxtrix)[1:100]
  word.count<-rowSums(tdm.maxtrix)
  word.order<-order(word.count,decreasing = T)
  freq.words<-tdm.maxtrix[word.order[1:20],]
  co.matrix<-freq.words%*%t(freq.words)
  
  qgraph(co.matrix,labels=rownames(co.matrix),
         diag=F,
         layout='spring',
         vsize=log(diag(co.matrix))*2,family='AppleGothic')
}

moon_apriori<-showApriori(moon_message)
ahn_apriori<-showApriori(ahn_message)
sim_apriori<-showApriori(sim_message)

write.csv(moon_message_final,"m_message_2.csv")

ggplot(all_posts, aes(x=created_time, y=likes_count, group=from_name, colour=from_name)) + geom_line(size=0.5) + 
  geom_smooth() +
  theme_minimal() + scale_x_date(labels = date_format("%m-%Y")) +
  labs(x = "Date(MM-yyyy)", y = "NumOfLike(n)") + 
  ggtitle(paste(start_date, end_date, sep="~"),family='AppleGothic')

