
###===============================================================================
###===============================================================================
###===============================================================================

install.packages("ggplot2")
library(ggplot2)
mpg # ggplot2가 갖고있는 데이터셋
    # A tibble: 234의 행 x 11의 열

# 1번 정답 : displ이 4 이하인 자동차의 hwy가 26으로 더 높다.
mpg %>% 
  filter(displ<=4) %>% 
  select(displ,hwy) %>% 
  summarise(mean=mean(hwy)) #;mean(group4$hwy)
mpg %>% 
  filter(displ>=5) %>% 
  select(displ,hwy) %>% 
  summarise(mean=mean(hwy)) #;mean(group5$hwy)

# 2번 정답 : audi 17.6 / toyota 18.5로 toyota가 더 높다.
mpg %>% 
  filter(manufacturer %in% c('audi','toyota')) %>% 
  group_by(manufacturer) %>% 
  summarise(average=mean(cty,na.rm=T))

# 3번 정답 : 22.27778
mpg %>% 
  filter(manufacturer==c('chevrolet','ford','honda')) %>% 
  summarise(average=mean(hwy))

# 4번 정답 : 밑에 코드
newdata <- mpg %>% 
  select(class,cty) %>%
  head(10)

# 5번 정답 : compact가 20.12766으로 더 높다.
suv     <- mpg %>% 
  filter(class %in% 'suv') %>%
  summarise(average=mean(cty))
compact <- mpg %>% 
  filter(class %in% 'compact') %>%
  summarise(average=mean(cty))

# 6번 
audi <- mpg %>% 
  filter(manufacturer == 'audi') %>% 
  arrange(desc(hwy)) %>%
  head(5)

# 7번_1 : 합산 연비 변수
mpgcopy <- mpg #복사본
mpgcopy <- mpg %>% 
  mutate(합산연비변수 = cty+hwy)

# 7번_2 : 평균 연비 변수
mpgcopy <- mpgcopy %>%
  mutate(평균연비변수 = 합산연비변수/2)

# 7번_3 :
mpgcopy <- mpgcopy %>% 
  select(class,평균연비변수) %>% 
  arrange(desc(평균연비변수)) %>%
  head(3)

# 7번_4 : 
mpg %>% 
  mutate(합산연비변수 = cty+hwy) %>% 
  mutate(평균연비변수 = 합산연비변수/2) %>% 
  select(class,합산연비변수,평균연비변수) %>% 
  arrange(desc(평균연비변수)) %>%
  head(3)

# 8번 
mpg %>% 
  select(class,cty) %>% 
  group_by(class) %>% 
  summarise(평균_cty = mean(cty,na.rm=T)) %>%
  
# 9번
mpg %>% 
  select(class,cty) %>% 
  group_by(class) %>% 
  summarise(평균_cty = mean(cty,na.rm=T)) %>% 
  arrange(desc(평균_cty))

# 10번 :
mpg %>% 
  select(manufacturer,hwy) %>% 
  group_by(manufacturer) %>% 
  summarise(평균_hwy = mean(hwy,na.rm=T)) %>%
  arrange(desc(평균_hwy)) %>% 
  head(3)

# 11번 
mpg %>%
  filter(class == 'compact') %>% 
  group_by(manufacturer) %>% 
  summarise(차종수=n()) %>%
  arrange(desc(차종수))

mpg %>%
  filter(class == 'compact') %>% 
  group_by(manufacturer) %>% 
  summarise(차종수=length(class)) %>%
  arrange(desc(차종수))

###===============================================================================
###===============================================================================
###===============================================================================
