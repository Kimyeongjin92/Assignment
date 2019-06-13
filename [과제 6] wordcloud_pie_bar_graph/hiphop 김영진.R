# [과제 6]

# hiphop.txt
# 1) wordcloud 만들기
# 2) pie 그래프 (Top10)
# 3) 막대 그래프 (Top10)

setwd("D:/dudwlsrla92/Assignment/[과제 6] wordcloud_pie_bar_graph")
getwd()

install.packages('KoNLP')
install.packages('tm')
install.packages('rjava')
install.packages('wordcloud')
install.packages('ggplot2')
install.packages('dplyr')
install.packages('rJava')
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

# 4. 명사 추출.

hiphop_nouns <- sapply(hiphop_corp2,extractNoun, USE.NAMES = F)
hiphop_nouns

head(unlist(hiphop_nouns),30)
hiphop_nouns_unlist <- unlist(hiphop_nouns)
hiphop_nouns_unlist

# 5. 걸러내기
hiphop_filter <- Filter(function(x){nchar(x) >= 2 & nchar(x) <= 6},hiphop_nouns_unlist)
head(hiphop_filter,20)

txt     <- readLines("hiphop_gsub.txt")
for(i in 1:length(txt)){
  hiphop_filter <- gsub((txt[i]),"",hiphop_filter)
}

# 6. 저장 + read.table
write(unlist(hiphop_filter),"hiphop_final.txt")
hiphop_final <- read.table('hiphop_final.txt') ;
#nrow(hiphop_final) #제거 확인.
## 6. 명사의 개수와 추가적으로 제거할 명사 구분.
wordcount <- table(hiphop_final) ; wordcount
head(sort(wordcount, decreasing=T),20)

# 연습문제 1번 wordcloud 만들기.
windowsFonts(baedal=windowsFont("배달의민족 도현"))
palete <- brewer.pal(10,"Set3") 

par(bg="black")
wordcloud(names(wordcount),
          freq=wordcount,
          scale=c(4,0.3),
          min.freq=1,
          max.freq=100,
          colors=palete,
          random.order=F,
          random.color=F,
          family="baedal")
legend(0.3,0.9,
       "1번 hiphop_WordCloud 만들기 ",
       cex=0.8,
       fill=NA,
       border=NA,
       bg="white",
       text.col="red",
       text.font=2,
       box.col="red")

# 연습문제 2번 Top 10 단어에 대해서 원 그래프 만들기.
top10 <- head(sort(wordcount,decreasing = T),10)
str(top10)
df_top10 <- as.data.frame(top10)
library(dplyr)
options(digits = 2) 

df_top10 <- df_top10 %>% 
  mutate(pct= round(Freq / sum(Freq)*100,1)) %>% 
  mutate(ylabel = paste0(hiphop_final,'\n',Freq,'개')) %>%  
  arrange(desc(hiphop_final))
df_top10

par(bg="black")
ggplot(df_top10,aes(x='',y=Freq,fill=hiphop_final, family='baedal')) + 
  geom_bar(width=1, stat='identity',color="black") +
  geom_text(aes(y=ypos,label=ylabel),color='black',size=5) +
  coord_polar("y",start=0) +
  ggtitle('2번 Hiphop.txt_Top10 원그래프') +
  theme_bw(base_family="baedal",base_size = 15) +
  theme(plot.title = element_text(family="baedal",
                                  face = "bold",
                                  hjust = 0.5, 
                                  size = 25, 
                                  color = "darkblue"))


# 연습문제 3번 Top 10 단어에 대해서 막대 그래프 만들기.
ggplot(df_top10,aes(x=hiphop_final,y=Freq,fill=hiphop_final)) + 
  geom_bar(width=0.9, stat='identity') +
  ggtitle('3번 Hiphop.txt_Top10 막대그래프') +
  theme_bw(base_family="baedal",base_size = 15) +
  geom_text(aes(y=Freq, label=ylabel), color='black',family="baedal",size=5)+
  theme(plot.title = element_text(family="baedal",
                                  face = "bold",
                                  hjust = 0.5,
                                  vjust = 2,
                                  size = 30, 
                                  color = "darkblue"))

