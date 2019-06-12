---
title: "sample"
author: "Kim"
date: '2019 6 10 '
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

### 연습문제 1. <br>
#### mpg 데이터의 cty(도시 연비)와 hwy(고속도로 연비) 간에 어떤 관계가 있는지 알아보려고 합니다. x축은 cty, y축은 hwy로 된 산점도를 만들어 보세요.
```{r}
library(ggplot2)
library(dplyr)

### 연습문제 1
ggplot(mpg,aes(x=cty,y=hwy)) +
  geom_point(color='steel blue',size=3)+
  ggtitle('1번 도시 연비와 고속도로 연비의 산점도')
```
<br><br>
### 연습문제 2. <br>
####  미국 지역별 인구통계 정보를 담은 ggplot2 패키지의 midwest 데이터를 이용해서 전체 인구와 아시아인 인구 간에 어떤 관계가 있는지 알아보려고 합니다. x축은 poptotal(전체 인구), y축은 popasian(아시아인 인구)으로 된 산점도를 만들어 보세요. 전체 인구는 50만 명 이하, 아시아인 인구는 1만 명 이하인 지역만 산점도에 표시되게 설정하세요.
```{r}
### 연습문제 2
midwest2 <- midwest %>% filter(poptotal<=500000,popasian<=10000) 
ggplot(midwest2,aes(x=poptotal,y=popasian))+
  geom_point(color='steel blue',size=2)+
  ggtitle('2번 \n전체인구 50만 이하 \n아시아인 인구 1만 이하인 지역')
```
<br><br>
### 연습문제 3. <br>
#### 어떤 회사에서 생산한 "suv" 차종의 도시 연비가 높은지 알아보려고 합니다. "suv" 차종을 대상으로 평균 cty(도시 연비)가 가장 높은 회사 다섯 곳을 막대 그래프로 표현해 보세요. 막대는 연비가 높은 순으로 정렬하세요. 
```{r}
### 연습문제 3
suv <- mpg %>% filter(class=='suv') %>%
  select(manufacturer,cty,class) %>% 
  group_by(manufacturer) %>% 
  summarise_each(funs(mean),cty) %>%
  arrange(desc(cty)) %>%
  head(5)

ggplot(suv,aes(x=reorder(manufacturer,-cty),y=cty,
               fill=manufacturer)) +
  geom_bar(stat="identity", color="black")+
  ggtitle('3번 [SUV] 평균 cty(도시연비) top5 ')
```
<br><br>
### 연습문제 4. <br>
#### 어떤 회사에서 생산한 "suv" 차종의 도시 연비가 높은지 알아보려고 합니다. "suv" 차종을 대상으로 평균 cty(도시 연비)가 가장 높은 회사 다섯 곳을 막대 그래프로 표현해 보세요. 막대는 연비가 높은 순으로 정렬하세요. 
```{r}
### 연습문제 4
class<-mpg %>% 
  select(class) %>%
  group_by(class) %>% 
  summarise(n=n())

ggplot(class,aes(x=reorder(class,-n),y=n,fill=factor(class)))+
  geom_bar(stat="identity",color="black")+
  ggtitle('4번 자동차 종류별 빈도')
```
<br><br>
### 연습문제 5. <br>
#### economics 데이터를 이용해서 psavert(개인 저축률)가 시간에 따라서 어떻게 변해왔는지 알아보려고 합니다. 시간에 따른 개인 저축률의 변화를 나타낸 시계열 그래프를 만들어 보세요.
```{r}
### 연습문제 5
ggplot(economics,aes(x=date,y=psavert))+
  geom_line(color="steel blue")+
  ggtitle('5번 시간에 따른 저축률의 변화.')
```
<br><br>
### 연습문제 6. <br>
#### class(자동차 종류)가 "compact", "subcompact", "suv"인 자동차의 cty(도시 연비)가 어떻게 다른지 비교해보려고 합니다. 세 차종의 cty를 나타낸 상자 그림을 만들어보세요.
```{r}
### 연습문제 6
a <- mpg %>% 
  filter(class %in% c('compact','subcompact','suv')) %>% 
  select(class,cty)

ggplot(a, aes(x=class,y=cty ,group=class,fill=class))+
  geom_boxplot() +
  ggtitle("6번 'compact', 'subcompact', 'suv'의 cty")
```
<br><br>
### 연습문제 7. <br>
#### Diamonds 데이터 셋을 이용하여 다음 문제를 해결하세요.단, 컬러, 제목, x축, y축 등 그래프를 예쁘게 작성하세요.
#1) cut의 돗수를 보여주는 그래프를 작성하세요. <br>
#2) cut에 따른 가격의 변화를 보여주는 그래프를 작성하세요.<br>
#3) cut에 따른 가격의 변화를 보여주는 그래프를 작성하세요.<br>
```{r}
library(gridExtra)

### 연습문제 7
diamonds
# 1) cut의 도수가.n()을....말한다..?
ggplot(diamonds,aes(x=cut,fill=cut)) +
  geom_bar(color="black")+
  ggtitle("7번(1) cut 돗수 그래프")

# 2) cut에 따른 가격의 변화를 보여주는 그래프
cut <- diamonds %>% 
  select(cut,price) %>% 
  group_by(cut) %>% 
  summarise(price_mean=mean(price,na.rm=T))

ggplot(cut,aes(x=cut,y=price_mean,fill=cut))+
  geom_bar(stat="identity",color="black")+
  ggtitle("7번(2) cut에 따른 가격의 변화 ")+
  ylim(0,5000)

# 3) 3번문제는 cut(2번)과 color를 따로 해서 grid
diamonds %>%
  group_by(cut,color) %>%
  summarise(mean_cut_color_price = mean(price)) %>% 
  arrange(desc(mean_cut_color_price)) %>%
  ggplot(aes(x=color, y=mean_cut_color_price, fill=color)) +
  geom_bar(stat="identity") +
  facet_wrap(~cut) +
  theme_bw() +
  labs(x="색상", y="가격") +
  theme(legend.position = "none") +
  ggtitle("커팅 방법에 따른 가격 변화") +
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "black"))

#ggplot(diamonds, aes(x=price))+geom)histogram(bins=10)+facet_wrap(-cut,)
#ggplot(diamonds, aes(x=price))+geom)histogram(bins=10)+facet_wrap(-cut,)
#grid.arrange(gcut,gcolor)
```





*글자체 Italic* <br>
**글자체 Bold** <br>
***Bold Italic*** <br>
~~취소선~~  
문장 맨뒤에 스페이스가 2개 있어도 줄바뀐다.
하이퍼 링크 - [이젠]("http://dj.ezenac.co.kr")

#### 순서없는 목록(Unordered List)
* 수박
* 참외
* 토마토

* 토마토 (*, +, - 동일한 효과)
  + 완숙 토마토(문장 시작에 탭을 두번 입력)
  - 방울 토마토
  
#### 순서있는 목록(ordered List)
1. R
2. Python
3. Java
  1) Eclipse
  2) Tomcat
  
#### 표도 그릴 수 있음
header 1 | header2 | header3
row1 col1 | row2 col2 | row3 col3 





This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r cars}
summary(cars)
```

## Including Plots

You can also embed plots, for example:

```{r pressure, echo=FALSE}
plot(pressure)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.

