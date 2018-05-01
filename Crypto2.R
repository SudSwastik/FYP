setwd("C:/Users/sudarshan/FYP/Data2")
library("dplyr")
library("ggplot2")
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
  ggplot(data=coinData,aes(x=coinData$Date))+
    geom_line(aes(y=coinData$Open),color="red")
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


