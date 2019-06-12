###=======================================================
### 0. 패키지, 라이브러리 설치. ==========================
###=======================================================

setwd("D:/dudwlsrla92/Assignment/[과제 4] 워드클라우드")
getwd()

install.packages('KoNLP')     # 한글 형태소 분석 
install.packages('wordcloud') # wordcloud 작업
install.packages('rJava')
install.packages("RColorBrewer")
install.packages("stringr")
install.packages("dplyr")
install.packages("extrafont")
library(KoNLP)
library(wordcloud)
library(rJava)
library(RColorBrewer)
library(stringr)
library(dplyr)
library(extrafont)

useSejongDic()

###=======================================================
### 1. 데이터 합치기. ====================================
###=======================================================
# data1 <- readLines("응답소_2015_01.txt")
# data1 <- as.data.frame(data1, stringsAsFactors = F) 
# data1 <- rename(data1, V1 = data1)

data1 <- readLines("응답소_2015_01.txt")
data2 <- readLines("응답소_2015_02.txt")
data3 <- readLines("응답소_2015_03.txt")
data4 <- readLines("응답소_2015_04.txt")
data5 <- readLines("응답소_2015_05.txt")
data6 <- readLines("응답소_2015_06.txt")
data7 <- readLines("응답소_2015_07.txt")
data8 <- readLines("응답소_2015_08.txt")
data9 <- readLines("응답소_2015_09.txt")
data10 <- readLines("응답소_2015_10.txt")
data11 <- readLines("응답소_2015_11.txt")
data12 <- readLines("응답소_2015_12.txt")

total <- c(data1,data2,data3,data4,data5,data6,data7,data8,data9,data10,data11,data12)

str(total)
head(total,10)

###=======================================================
### 2. 데이터 전처리하기. ================================
###=======================================================

total1 <- sapply(total,extractNoun, USE.NAMES = F)
total2 <- unlist(total1)

#write(unlist(total2),"응답소total.txt")
#total3 <- read.table('응답소total.txt') 

total3   <- str_replace_all(total2,"[^[:alpha:]]","")
total3   <- gsub(" ","",total3)


###=======================================================
### 3. gsub 파일로 삭제하기. =============================
###=======================================================

txt     <- readLines("응답소gsub.txt")

for(i in 1:length(txt)){
  total3 <- gsub((txt[i]),"",total3)
}

total3 <- Filter(function(x){nchar(x) >= 2 & nchar(x) <= 5 },total3)

write(unlist(total3),"응답소total2.txt")
rev   <- read.table("응답소total2.txt")
nrow(rev)
wordcount <- table(rev)
head(sort(wordcount, decreasing=T),30)

###=======================================================
### 4. 색상 지정하기 =====================================
###=======================================================

# 9는 사용할 색상개수, 색상타입이름.
palete <- brewer.pal(9,"Set3") 
pal <- brewer.pal(5,"YlGn")
pal1<- brewer.pal(7,"YlOrRd")
green <- brewer.pal(6,"RdYlGn")
red <- brewer.pal(7,"YlOrRd")
blue <- brewer.pal(6,"Blues")
set <- brewer.pal(7, "Set3")
pp <- brewer.pal(7,"RdYlGn")


###=======================================================
### 5. 글꼴 지정하기 =====================================
###=======================================================


windowsFonts(baedal=windowsFont("배달의민족 도현"))


###=======================================================
### 6. wordcloud 그리기 ==================================
###=======================================================

par(bg="black")
wordcloud(names(wordcount),
          freq=wordcount,   #
          scale=c(3,0.1),   # 빈도가 가장 큰 단어와 가장 빈도가 작은 단어 폰트 사이 크기
          rot.per=0.25,     # 90도 회정해서 보여줄 단어 비율.
          min.freq=1,       # 이 값 이상 언급된 단어만 출력.
          max.words=1000,    # 빈도 3이상 100미만 단어 표현.
          random.order=F,   # (F)빈도가 큰 단어를 중앙에 두도록 함.
          random.color=F,   # (T)색상랜덤/(F)빈도수순으로 색상표현.
          colors=palete,
          family="baedal")
legend(0.2,1,"서울시 응답소 2015",
         cex=0.8,
         fill='black',
         border=NA,
         bg="white",
         text.col="red",
         text.font=2,
         box.col="red")

#write(unlist(total),"응답소total.txt")
#total <- read.table('응답소total.txt') 







