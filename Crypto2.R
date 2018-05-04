setwd("C:/Users/saurabh/FYP/Data2")
getwd()
library("dplyr")
library("ggplot2")
coinData<- read.csv('$$$.csv')
head(coinData)
typeof(coinData$Date)
coinData$Date<-as.character(coinData$Date)
typeof(coinData$Date)
coinData$Date <- as.Date(coinData$Date,format='%Y-%m-%d')
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



try <- function (){
  ggplot(data=coinData,aes(x=coinData$Date,y=value,color=variable))+
    geom_line(aes(y=coinData$Open,col="coinData$Open"))+
    geom_line(aes(y=coinData$Close,col="coinData$Close"))
  
}

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



