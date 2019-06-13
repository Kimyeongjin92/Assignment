setwd("D:/dudwlsrla92/Assignment/[과제 6] wordcloud_pie_bar_graph")
getwd()

library(KoNLP)
library(tm)
library(rJava)
library(wordcloud)
library(ggplot2)
library(dplyr)

useSejongDic()

# 1. 데이터 불러오기
hiphop <- readLines("hiphop.txt")

# 2. 데이터 변환하기.
hiphop_corp <- Corpus(VectorSource(hiphop))
inspect(hiphop_corp)

# 3. 불필요한 단어 제거
hiphop_corp2  <- tm_map(hiphop_corp,stripWhitespace) 
hiphop_corp2  <- tm_map(hiphop_corp2,tolower)
hiphop_corp2  <- tm_map(hiphop_corp2,removeNumbers)
hiphop_corp2  <- tm_map(hiphop_corp2,removePunctuation)
sword2        <- c(stopwords('en'),'and','but','not') # 영어의 관사 대명사 접속사 등 
hiphop_corp2  <- tm_map(hiphop_corp2,removeWords,sword2)

# 4. Matrix로 변환
tdm2   <- TermDocumentMatrix(hiphop_corp2)
m2     <- as.matrix(tdm2)
View(m2)

# 5. 단어별 집계 (table화)
freq1 <- sort(rowSums(m2),decreasing = T)
head(freq1,20)

# 6.
hiphop_corp2 <- Filter(function(x){nchar(x) >= 2},hiphop_corp2) 



