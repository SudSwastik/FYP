setwd("C:/Users/saurabh/FYP/Data2")
getwd()
library(dplyr)
library(ggplot2)
library(readr)
library(plyr)
library(scales)
library(gridExtra)
library(grid)
library(reshape2)
library(zoo)



coinData<- read.csv('$$$.csv')
btcData <- read.csv('btc.csv')
ethData <- read.csv('ETH.csv')
head(coinData)
head(btcData)
typeof(coinData$Date)
coinData$Date<-as.character(coinData$Date)
typeof(coinData$Date)
coinData$Date <- as.Date(coinData$Date,format='%Y-%m-%d')


ethData$Date<-as.character(ethData$Date)
typeof(ethData$Date)
ethData$Date <- as.Date(ethData$Date,format='%Y-%m-%d')


btcData$Date<-as.character(btcData$Date)
typeof(btcData$Date)
btcData$Date <- as.Date(btcData$Date,format='%Y-%m-%d')

class(coinData$Date)
head(coinData)

openData <-  function(coinData){
  qplot(data=coinData,x=Date,y=Open)
}
openData(coinData)

closeData <-  function(coinData){
  ggplot(data=coinData,aes(x=coinData$Date))+
    geom_line(aes(y=coinData$Close),color="blue")
}
closeData(coinData)

highData <-  function(coinData){
  ggplot(data=coinData,aes(x=coinData$Date))+
    geom_line(aes(y=coinData$High),color="green")
}
highData(coinData)

lowData <-  function(coinData){
  ggplot(data=coinData,aes(x=coinData$Date))+
    geom_line(aes(y=coinData$Low),color="yellow")
}
lowData(coinData)


multiple.timeseries.coin<- function(ethData,btcData){
  
  merged.dataframe<- ethData %>%
    bind_rows(btcData) 

  ggplot(merged.dataframe,aes(x=merged.dataframe$Date,y=merged.dataframe$High,color=coin))+
    geom_line()
  
}

multiple.timeseries.coin(ethData,btcData)



##Iterate through files to obtain prices of coins greater than 1000$ Unfinished
setwd("C:/Users/Saurabh/Desktop/8th Sem Project/Data")
file.list <- list.files(path="C:/Users/Saurabh/Desktop/8th Sem Project/Data")
highCoins = c()
for (i in 1:length(file.list)){
  file.df <- read.csv(file.list[i],header = TRUE)
  max.value <-max(file.df$High,na.rm=TRUE)
  if(max.value >= 1000)
  {
    highCoins <- c(highCoins,as.character(file.df$coin[1]))
    next
  }
}
length(highCoins)

##Treemap
library(coinmarketcapr)
plot_top_5_currencies()## This is based on Market Cap
market_today <- get_marketcap_ticker_all() ##To extract Global Market Cap of Leading Cryptocurrencies
head(market_today[,1:8])
library(treemap)
df1 <- na.omit(market_today[,c('id','market_cap_usd')])
df1$market_cap_usd <- as.numeric(df1$market_cap_usd)
df1$formatted_market_cap <-  paste0(df1$id,'\n','$',format(df1$market_cap_usd,big.mark = ',',scientific = F, trim = T))
treemap(df1, index = 'formatted_market_cap', vSize = 'market_cap_usd', title = 'Cryptocurrency Market Cap', fontsize.labels=c(12, 8), palette='RdYlGn')




total.file<- read.csv("C:/Users/sudarshan/FYP/Data/CryptocoinsHistoricalPrices.csv")
length(total.file$High)
head(total.file)

#changed date from Numeric to charcter
total.file$Date<-as.character(total.file$Date)

#changed date from character to Date format
total.file$Date <- as.Date(total.file$Date,format='%Y-%m-%d')
total.file$Date

#added new column year and month to the dataset
new.total.file<- total.file %>% mutate(Year= format(as.Date(total.file$Date, format="%Y-%m-%d"),"%Y"))%>%
  mutate(Month=format(as.Date(total.file$Date, format="%Y-%m-%d"),"%m"))

head(new.total.file)
summary(new.total.file)



