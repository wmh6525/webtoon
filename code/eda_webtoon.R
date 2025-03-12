## 필수 라이브러리 호출

library(data.table)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(viridis)
library(MASS)
library(stringr)

## 데이터 불러오기
webtoon = read.csv(file='./webtoon.csv',header=T)

dim(webtoon)
head(webtoon)

# 웹툰 제목, 회차 제목 삭제
webtoon = webtoon[,-c(1,8,13,18,22,27,32)]
#summary(webtoon)

# 데이터 타입 변환
webtoon$startDay <- as.Date(webtoon$startDay)
webtoon$day2 <- as.Date(webtoon$day2,"%Y.%m.%d")
webtoon$day3 <- as.Date(webtoon$day3,"%Y.%m.%d")
webtoon$day4 <- as.Date(webtoon$day4,"%Y.%m.%d")
webtoon$day5 <- as.Date(webtoon$day5,"%Y.%m.%d")
webtoon$day6 <- as.Date(webtoon$day6,"%Y.%m.%d")
webtoon$isPublic <- as.factor(webtoon$isPublic)
webtoon$typeGenre <- as.factor(webtoon$typeGenre)
webtoon$heart <- as.integer(gsub(',','',webtoon$heart))

# 결측치 제거

#apply(is.na(webtoon),2,sum) #column별 결측치 개수

missing_ind <- unique(which(is.na(webtoon),arr.ind=T)[,1])
#webtoon[missing_ind,]

webtoon$orderCount <- 6-is.na(webtoon[,c(9,13,17,20,24,28)]) %>% apply(.,1,sum)
webtoon$orderCount %>% table()
webtoon = webtoon[webtoon$orderCount>3,] #회차 개수 3개 이하 제거

## 가설 1 : 초반, 후반 조회수 vs 정식연재
webtoon$logearlyViews <- apply(log(webtoon[,c(9,13,17)]),1,mean,na.rm=T)
webtoon$loglaterViews <- apply(log(webtoon[,c(20,24,28)]),1,mean,na.rm=T)
webtoon$logviewRatio <- webtoon$loglaterViews/webtoon$logearlyViews

ggplot(webtoon, aes(x=logviewRatio,
                    color=factor(isPublic),
                    fill=factor(isPublic)))+
  geom_density(alpha=0.8) +
  scale_fill_viridis(discrete = TRUE)+
  scale_color_viridis(discrete = TRUE)

## 가설 2 : 
webtoon$meanview <- apply(webtoon[,c(9,13,17,20,24,28)], 1, mean, na.rm=T)
webtoon$meanview[webtoon$isPublic==0] %>% quantile()

uppermeanview <- webtoon[webtoon$meanview > 595.3333,]
uppermeanview <- uppermeanview[uppermeanview$totalStar>8.5,]

ggplot()+
  geom_point(data=uppermeanview, aes(x=meanview,y=totalStar,color=isPublic))+
  theme(legend.position = 'top')

## contentGenre fisher의 정확검정

contentGenreGab <- xtabs(~isPublic + contentGenre, data=webtoon)
fisher.test(contentGenreGab,simulate.p.value=TRUE)

## contentGenre 중요도 계산

webtoon$noGenre <- 1
for (i in 1:length(webtoon$contentGenre)){
  if (length(strsplit(webtoon$contentGenre, split=",")[i][[1]])==2){
    webtoon$noGenre[i] <- 2
  }
}

level = c('action','comic','daily','drama','fantasy','historical','pure','sensibility','sports','thrill')
fit_genre_thrill <- glm(isPublic ~ factor(webtoon$contentGenre[webtoon$noGenre==1],levels=level[c(10,1:9)]) ,family=binomial, data=webtoon[webtoon$noGenre==1,])
fit_genre_thrill %>% summary()

# 중요도 기반으로 한 장르에만 포함시키기
webtoon$oneGenre <- webtoon$contentGenre
for (i in 1:length(webtoon$oneGenre)){
  if (str_detect(webtoon$contentGenre[i], 'thrill')){
    webtoon$oneGenre[i] <- 'thrill'
    next
  }
  if (str_detect(webtoon$contentGenre[i], 'daily')){
    webtoon$oneGenre[i] <- 'daily'
    next
  }
  if (str_detect(webtoon$contentGenre[i], 'action')){
    webtoon$oneGenre[i] <- 'action'
    next
  }
  if (str_detect(webtoon$contentGenre[i], 'fantasy')){
    webtoon$oneGenre[i] <- 'fantasy'
    next
  }
  if (str_detect(webtoon$contentGenre[i], 'sensibility')){
    webtoon$oneGenre[i] <- 'sensibility'
    next
  }
  if (str_detect(webtoon$contentGenre[i], 'pure')){
    webtoon$oneGenre[i] <- 'pure'
    next
  }
  if (str_detect(webtoon$contentGenre[i], 'comic')){
    webtoon$oneGenre[i] <- 'comic'
    next
  }
  if (str_detect(webtoon$contentGenre[i], 'drama')){
    webtoon$oneGenre[i] <- 'drama'
    next
  }
  if (str_detect(webtoon$contentGenre[i], 'sports')){
    webtoon$oneGenre[i] <- 'sports'
    next
  }
  if (str_detect(webtoon$contentGenre[i], 'historical')){
    webtoon$oneGenre[i] <- 'historical'
    next
  }
}
webtoon$oneGenre %>% table()