library(dplyr)
library(ggplot2)
library(anytime)

data <-  read.csv('./UserBehavior.csv',col.names = c('UserID','ItemID','CategoryID','BehaviorType','Timestamp'))

#Data Cleaning
#convert UNIX epoch to Data
data$Timestamp <- anytime(data$Timestamp)
data$Date <- as.Date(data$Timestamp)
summary(data)
#remove outliers
data <- filter(data,between(Date,as.Date('2017-11-25'),as.Date('2017-12-3')))

#User-Production Interaction
#product ranking, for example, top 10 bestselling items
result <- ranking('buy',10)
# auxiliary function (x: a string, behavior type; y:an integer, ranking factor)
ranking <- function(x,y){
  res_ranking <- data %>% 
    filter(BehaviorType == x) %>%
    select(ItemID, CategoryID) %>%
    group_by(ItemID) %>%
    summarise(count = n()) %>%
    arrange(desc(count)) %>%
    slice(1:y)
  return(res_ranking)
}


#User Behavior Pattern
#weekly
data_weekly <-  filter(data,between(Date,as.Date('2017-11-27'),as.Date('2017-12-3')))
data_weekly_pv <- filter(data_weekly, BehaviorType == 'pv')
data_weekly_others <- setdiff(data_weekly,data_weekly_pv) %>% group_by(Date,BehaviorType) %>% summarise(counts=n())
data_weekly_pv <- data_weekly_pv %>% group_by(Date) %>% summarise(pv_count = n())
data_weekly_merge(data_weekly_pv, data_weekly_others, by='Date')
options(scipen=1000000)
ggplot(data_weekly_merge, aes(x=Date)) + 
  geom_bar(stat='identity',aes(y=pv_count/3),fill = 'grey50') +
  geom_line(aes(y=counts*10,color = BehaviorType),size=2) +
  scale_x_date(breaks =seq(as.Date('2017-11-27'),as.Date('2017-12-3'),by='days'), labels=c('Mon.','Tues.','Wed.','Thurs.','Fri.','Sat.','Sun.'),expand=c(0,0)) +
  scale_y_continuous(name='number of pv',sec.axis = sec_axis(~./10,name= 'number of fav/cart/buy')) 
#   annotate(
#    geom='rect', 
#    ymin=-Inf, 
#    ymax=Inf, 
#    xmin=c(as.Date('2017-11-27'),as.Date('2017-12-1')),
#    xmax=c(as.Date('2017-12-1'),as.Date('2017-12-3')),
#    fill=c('#619cff','#f8766d'),
#    alpha=0.2)

#daily
data$Hour <- strftime(data$Timestamp, format="%H")
data_wed <- filter(data,Date == '2017-11-29')
data_sat <- filter(data,Date == '2017-12-2')

#Purchase Funnel  
#awareness/interest: measurement of efficacy of the page
# UV click rate: PV/UV
data_pv <- filter(data, BehaviorType == 'pv')
nrow(data_pv) / length(unique(data$UserID))
# bounce rate: number of visitos viewing one page only / total entires to page
group_by(data,UserID) %>% summarise(frequency = n()) %>% filter(frequency == 1) %>% nrow() / nrow(data)

#consideration: conversion(click an item -> add an item to shopping cart / favor an item)
data_consid <- filter(data, BehaviorType == 'fav' | BehaviorType == 'cart'  )
nrow(data_consid) / nrow(data_pv)

#purchase: conversion rate(add an item to shopping cart / favor an item -> purchase)
# by visit
data_buy <- data %>% filter(BehaviorType == 'buy')
nrow(data_buy) / nrow(data_consid)
# by user
length(unique(data_buy$UserID)) / length(unique(data$UserID))


#RFM
#recency
data_buy$recency <- abs(as.numeric(data_buy$Date - as.Date('2017-12-3')))
data_buy$recency <- ifelse(data_buy$recency <= 2, 1,
                           ifelse((data_buy$recency <=4 & data_buy$recency >2), 2,
                                  ifelse((data_buy$recency <=6 & data_buy$recency >4), 3, 4)))
#frequency
frequency <- data_buy %>% group_by(UserID) %>% summarise(frequency = n()) 
data_buy <- merge(data_buy, frequency, by='UserID')
data_buy$frequency <- ifelse(data_buy$frequency<=2, 4,
                            ifelse((data_buy$frequency<=4 & data_buy$frequency >2), 3,
                                  ifelse((data_buy$frequency <=7 & data_buy$frequency >4), 2, 1)))
#monetarty value is not provided in the dataset
#segment
data_buy$segment <- paste(data_buy$recency, '-', data_buy$frequency)
#Correia, J. (2020). How to segment your customers and increase sales with RFM analysis. Retrieved 3 May 2020, from https://joaocorreia.io/blog/rfm-analysis-increase-sales-by-segmenting-your-customers.html
