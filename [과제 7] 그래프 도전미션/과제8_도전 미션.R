### ============================================================================
### 도전미션 3-1 프로야구 선수들은 밥값을 하는가유? ============================
### ============================================================================

setwd("D:/dudwlsrla92/Assignment/[과제 7] 그래프 도전미션")
getwd()

library(dplyr)
library(ggplot2)
library(reshape2)

windowsFonts(baedal=windowsFont("배달의민족 도현"))


baseball <- read.csv('주요선수별성적-2013년.csv',stringsAsFactors=F)

options(digits = 2)

# 1) bar 차트 ==================================================================
baseball %>% 
  mutate(OPS=출루율+장타율) %>%
  mutate(연봉대비OPS율=round(OPS/연봉,1)) %>% 
  select(선수명,연봉대비OPS율) %>%
  ggplot(aes(x=선수명,y=연봉대비OPS율,fill=선수명)) +
  geom_bar(stat="identity",position="dodge",color="black") +
  geom_text(aes(y=연봉대비OPS율+0.03, label=paste0(연봉대비OPS율,"%")), color='black',size=7) +
  geom_hline(yintercept=seq(0,1,0.125),lty='dashed',size=0.5) +
  theme_bw(base_family='baedal',base_size = 20) +
  theme(axis.title.x = element_blank()) +
  theme(axis.text.x  = element_text(angle=45, hjust=1)) +
  ggtitle('야구 선수별 연봉 대비 OPS율 분석') +
  theme(plot.title = element_text(family="baedal",
                                  face = "bold",
                                  hjust = 0.5,
                                  vjust = 2.5,
                                  size = 40, 
                                  color = "black")) 


# 2) 나이팅게일 차트 coor_polar() =================================================
baseball2 <-baseball %>% 
  mutate(OPS=출루율+장타율) %>%
  mutate(연봉대비OPS율=round(OPS/연봉,1)) %>% 
  select(선수명,홈런,안타,득점,출루율,타율,볼넷,도루,타점)

baseball2_melt <-melt(baseball2,id=c('선수명'))

  ggplot(baseball2_melt,aes(x=variable ,y=value,fill=variable)) +
  geom_bar(stat="identity",color="black") +
  coord_polar() +
  facet_wrap(~선수명,ncol=5) +
#  scale_fill_brewer(palette="Greens") +
#  xlab("")+
#  ylab("")+
  theme_bw(base_family='baedal',base_size = 12) +
  theme(axis.title.x = element_blank()) +
  theme(axis.text.x  = element_text(angle=0, hjust=1)) +
  ggtitle('야구 선수별 주요 성적 분석 -2013') +
  theme(plot.title = element_text(family="baedal",
                                    face = "bold",
                                    hjust = 0.5,
                                    vjust = 2.5,
                                    size = 40, 
                                    color = "black")) 

  
  # 3) 연봉대비 출루율과 연봉대비 타점율의 비교 ===================================
baseball3 <-baseball %>% 
  mutate(연봉대비출루율 = 출루율/연봉*100) %>%
  mutate(연봉대비타점율 = (타점/안타)/연봉*100) %>%
    select(선수명,연봉대비출루율,연봉대비타점율)
  
baseball3_melt <-melt(baseball3,id=c('선수명'))

ggplot(baseball3_melt,aes(x=선수명,y=value,fill=variable,color=variable,group=variable)) +
  geom_line(linetype=5,size=1)+
  geom_point(shape=1,size=3)+
  geom_hline(yintercept=seq(0,50,5),lty='dashed',size=0.1)+
  theme_bw(base_family='baedal',base_size = 20) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.x  = element_text(angle=45, hjust=1)) +
  ggtitle('한국프로야구선수별 기록분석 2013') +
  theme(plot.title = element_text(family="baedal",
                                  face = "bold",
                                  hjust = 0.5,
                                  vjust = 2.5,
                                  size = 40, 
                                  color = "black")) 
       

### ============================================================================
### 도전미션 3-3 취직하기가 정말 어렵네요   ====================================
### ============================================================================

unemployment <- read.csv('2000-2013년 연령별실업율_연령별평균_세로.csv')

unemployment_melt <- melt(unemployment,id=c('연도'),variable.name='연령',value.name='실업률')
unemployment_melt$연령 <- gsub("X","",unemployment_melt$연령)

ggplot(unemployment_melt,aes(x=연도,y=실업률,fill=연령,color=연령,group=연령)) +
  geom_line(linetype=1,size=1.5)+
  geom_point(shape=19,size=4)+
  geom_hline(yintercept=seq(0,10,0.5),lty='dashed',size=0.1)+
  theme_bw(base_family='baedal',base_size = 20) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.x  = element_text(angle=45, hjust=1)) +
  ggtitle('연령별 실업률 현황(단위:%) 출처:통계청') +
  theme(plot.title = element_text(family="baedal",
                                  face = "bold",
                                  hjust = 0.5,
                                  vjust = 2.5,
                                  size = 40, 
                                  color = "black")) 


### ============================================================================
### 도전미션 3-4 정규직은 정녕 꿈인가요.    ====================================
### ============================================================================

# 1) 고용형태별 근무일수
permanent_job <- read.csv('고용형태별_취업자현황_2007_2013년.csv')
permanent_job <- select(permanent_job,고용형태,X2007,X2008,X2009,X2010,X2011,X2012,X2013) 
permanent_job <- permanent_job[c(-1,-3,-6,-7,-8,-13),]

permanent_job_melt <- melt(permanent_job,id=c('고용형태'),variable.name='연도',value.name='근무일수')
permanent_job_melt$연도 <- gsub("X","",permanent_job_melt$연도)

## vector형태로 묶여있기 때문에 as.numeric이 필요하다.
ggplot(permanent_job_melt,aes(x=연도,y=as.numeric(근무일수),fill=고용형태,color=고용형태,group=고용형태)) +
  geom_line(linetype=1,size=1.5)+
  geom_point(shape=19,size=4)+
  geom_hline(yintercept=seq(10,25,1),lty='dashed',size=0.1)+
#  lims(y=c(0,25)) +
#  scale_y_continuous(limits = c(10, 25)) +
#  ylim(0,25)+
  theme_bw(base_family='baedal',base_size = 20) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.x  = element_text(angle=45, hjust=1)) +
  ggtitle('고용형태별 근무일수(단위:일) 출처:통계청') +
  theme(plot.title = element_text(family="baedal",
                                  face = "bold",
                                  hjust = 0.5,
                                  vjust = 2.5,
                                  size = 40, 
                                  color = "black")) 

# 2) 고용형태별 급여현황
permanent_m <- read.csv('고용형태별_취업자현황_2007_2013년.csv',header=T)
permanent_m <- select(permanent_m,고용형태,X2007.4,X2008.4,X2009.4,X2010.4,X2011.4,X2012.4,X2013.4) 
permanent_m <- permanent_m[c(-1,-3,-6,-7,-8,-13),]

colnames(permanent_m) <- c('고용형태','2007','2008','2009','2010','2011','2012','2013')

permanent_m_melt <- melt(permanent_m,id=c('고용형태'),variable.name='연도',value.name='월급여')
permanent_m_melt$월급여 <- gsub(",","",permanent_m_melt$월급여)

ggplot(permanent_m_melt,aes(x=연도,y=as.numeric(월급여),fill=고용형태,color=고용형태,group=고용형태)) +
  geom_line(linetype=1,size=1.5)+
  geom_point(shape=19,size=4)+
  geom_hline(yintercept=seq(500,3000,100),lty='dashed',size=0.1)+
  theme_bw(base_family='baedal',base_size = 20) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.x  = element_text(angle=45, hjust=1)) +
  ggtitle('고용형태별 급여현황(단위:천원) 출처:통계청') +
  theme(plot.title = element_text(family="baedal",
                                  face = "bold",
                                  hjust = 0.5,
                                  vjust = 2.5,
                                  size = 40, 
                                  color = "black")) 

### ============================================================================
### 도전미션 3-5. 전염병을 미리 막아 보자구요!    ==============================
### ============================================================================

# 1) 년도별 발병 현황
disease <- read.csv('1군전염병발병현황_년도별.csv')

disease_melt <- melt(disease,id=c('년도별'),variable.name='질병',value.name='건수')

ggplot(disease_melt,aes(x=년도별,y=건수,fill=질병,color=질병,group=질병)) +
  geom_line(linetype=1,size=1.5)+
  geom_point(shape=19,size=4)+
  geom_hline(yintercept=seq(0,6000,100),lty='dashed',size=0.1)+
  theme_bw(base_family='baedal',base_size = 30) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.x  = element_text(angle=90, hjust=1)) +
  ggtitle('1군 전염병 발병현황-년도별(단위:건수) 출처:통계청') +
  theme(plot.title = element_text(family="baedal",
                                  face = "bold",
                                  hjust = 0.5,
                                  vjust = 2.5,
                                  size = 40, 
                                  color = "black")) 

# 2) 월별 발병 현황 *** 1~12월 factor 순서를 levels=unique로 바꿔준다.
disease_month <- read.csv('1군전염병발병현황_월별.csv')

disease_month_melt <- melt(disease_month,id=c('월별'),variable.name='질병',value.name='건수')
disease_month_melt$월별 <- factor(disease_month_melt$월별, levels=unique(disease_month_melt$월별))

ggplot(disease_month_melt,aes(x=월별,y=건수,fill=질병,color=질병,group=질병)) +
  geom_line(linetype=1,size=1.5)+
  geom_point(shape=19,size=4)+
  geom_hline(yintercept=seq(0,1500,100),lty='dashed',size=0.1)+
  theme_bw(base_family='baedal',base_size = 25) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.x  = element_text(angle=90, hjust=1)) +
  ggtitle('1군 전염병 발병현황-월별(단위:건수) 출처:통계청') +
  theme(plot.title = element_text(family="baedal",
                                  face = "bold",
                                  hjust = 0.5,
                                  vjust = 2.5,
                                  size = 40, 
                                  color = "black")) 

# 3) A형 간염을 제외한 발생현황 (년도별)
disease <- read.csv('1군전염병발병현황_년도별.csv')
disease <- disease[,-6]

disease_melt <- melt(disease,id=c('년도별'),variable.name='질병',value.name='건수')

ggplot(disease_melt,aes(x=년도별,y=건수,fill=질병,color=질병,group=질병)) +
  geom_line(linetype=1,size=1.5)+
  geom_point(shape=19,size=4)+
  geom_hline(yintercept=seq(0,1500,100),lty='dashed',size=0.1)+
  theme_bw(base_family='baedal',base_size = 30) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.x  = element_text(angle=90, hjust=1)) +
  ggtitle('1군 전염병 발병현황-년도별(단위:건수) 출처:통계청') +
  theme(plot.title = element_text(family="baedal",
                                  face = "bold",
                                  hjust = 0.5,
                                  vjust = 2.5,
                                  size = 40, 
                                  color = "black")) 

### ============================================================================
### 도전미션 3-6. 서울 주요 마을버스 이용승객 현황    ==========================
### ============================================================================

bus <- read.csv('버스노선별이용현황합계.csv')

bus_melt <- melt(bus,id=c('버스노선번호'))
bus_melt$value<- bus_melt$value/1000

ggplot(bus_melt,aes(x=버스노선번호,y=value,fill=variable)) +
  geom_bar(stat="identity",position="dodge",color="black") +
  geom_text(aes(y=value, label=value), color='black',size=7) +
  labs(x='노선명',y='이용승객수(단위:천명)') +
  theme_bw(base_family='baedal',base_size = 20) +
  theme(axis.text.x  = element_text(angle=90, hjust=1)) +
  ggtitle('서울 주요 마을버스 이용승객 현황(2014년 1월)') +
  theme(plot.title = element_text(family="baedal",
                                  face = "bold",
                                  hjust = 0.5,
                                  vjust = 2.5,
                                  size = 40, 
                                  color = "black")) 

### ============================================================================
### 도전미션 3-7. 마포09번 이용현황 분석 =======================================
### ============================================================================

mapo <- read.csv('마포09번이용현황.csv')
#mapo$no    <- c(1:length(mapo$승차인원))  
mapo$정류소명 <- paste0(1:length(mapo$정류소명),'.',mapo$정류소명)
mapo_melt  <- melt(mapo,id=c('정류소명')) 
mapo_melt$정류소명 <- factor(mapo_melt$정류소명,levels=unique(mapo_melt$정류소명))


ggplot(mapo_melt,aes(x=정류소명,y=value,fill=variable,color=variable,group=variable)) +
  geom_line(linetype=1,size=1.5)+
  geom_point(shape=1,size=4)+
  geom_hline(yintercept=seq(0,40000,1000),lty='dashed',size=0.1)+
  geom_vline(xintercept=seq(0,32,1),lty='dashed',size=0.1)+
  labs(x='노선명',y='이용승객수(단위:천명)') +
  theme_bw(base_family='baedal',base_size = 15) +
  theme(axis.text.x  = element_text(angle=90, hjust=1)) +
  ggtitle('마포09번 이용 승객수9(단위:명)_2014년 1월') +
  theme(plot.title = element_text(family="baedal",
                                  face = "bold",
                                  hjust = 0.5,
                                  vjust = 2.5,
                                  size = 20, 
                                  color = "black"))
