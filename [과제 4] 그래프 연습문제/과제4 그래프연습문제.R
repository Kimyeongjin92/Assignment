###=============================================================================
### 연습문제 ==================================================================
###=============================================================================

#ggplot2 R markdown 형식으로 제출
# 김영진.html로 제출

# 연습문제 1
ggplot(mpg,aes(x=cty,y=hwy)) +
  geom_point(color='steel blue',size=5)+
  ggtitle('1번 도시 연비와 고속도로 연비의 산점도')

# 연습문제 2
midwest2 <- midwest %>% filter(poptotal<500000,popasian<10000) 
ggplot(midwest2,aes(x=poptotal,y=popasian))+
  geom_point(color='steel blue',size=2)+
  ggtitle('2번 \n전체인구 50만 이하 \n아시아인 인구 1만 이하인 지역')

# 연습문제 3
suv <- mpg %>% filter(class=='suv') %>%
  select(manufacturer,cty,class) %>% 
  group_by(manufacturer) %>% 
  summarise_each(funs(mean),cty) %>%
  arrange(desc(cty)) %>%
  head(5)

ggplot(suv,aes(x=reorder(manufacturer,-cty),y=cty)) +
  geom_bar(stat="identity",fill="steel blue", color="black")+
  ggtitle('3번 [SUV] 평균 cty(도시연비) top5 ')

# 연습문제 4
class<-mpg %>% 
  select(class) %>%
  group_by(class) %>% 
  summarise(n=n())

ggplot(class,aes(x=reorder(class,-n),y=n))+
  geom_bar(stat="identity",fill="steel blue",color="black")+
  ggtitle('4번 자동차 종류별 빈도')

.# 연습문제 5
ggplot(economics,aes(x=date,y=psavert))+
  geom_line(color="steel blue")+
  ggtitle('5번 시간에 따른 저축률의 변화.')

# 연습문제 6
a <- mpg %>% 
  filter(class %in% c('compact','subcompact','suv')) %>% 
  select(class,cty)

ggplot(a, aes(x=class,y=cty ,group=class))+
  geom_boxplot(fill=c('Orange red','steel blue','yellow')) +
  ggtitle("6번 'compact', 'subcompact', 'suv'의 cty")


# 연습문제 7
diamonds
View(diamonds)
# 1) cut의 돗수를 보여주는 그래프
ggplot(diamonds,aes(x=cut,fill=cut)) +
  geom_bar(color="black")
ggtitle("7번(1) cut의 돗수 그래프")

# 2) cut에 따른 가격의 변화를 보여주는 그래프
cut <- diamonds %>% 
  select(cut,price) %>% 
  group_by(cut) %>% 
  summarise(price_mean=mean(price,na.rm=T))

ggplot(cut,aes(x=cut,y=price_mean))+
  geom_bar(stat="identity",fill=c('Tan1','steel blue','yellow2','green3','pink'),color="black")+
  ggtitle("7번(2) cut에 따른 가격의 변화 ")+
  ylim(0,5000)

# 3) 3번문제는 cut과 color를 따로 해서 grid로 합침..ㄷㄷ..
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
