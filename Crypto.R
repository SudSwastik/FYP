setwd("C:/Users/sudarshan/FYP")
library("dplyr")
dataFilePath <- "C:/Users/sudarshan/FYP/Data/CryptocoinsHistoricalPrices.csv"
data <- read.csv(dataFilePath)
str(data)
sapply(data,class)

head(data)

coinData <- split(data,data$coin)
bitcoinData



function(){
  print(bitcoinData)
  bitcoinData[[1]]
  Map(function(bitcoinData, i) paste(i,bitcoinData), bitcoinData, names(bitcoinData))
  length(bitcoinData)
  typeof(bitcoinData)
  dput[bitcoinData[1:3]]
  for(i in 1:length(bitcoinData)){
    dput[bitcoinData[1:3]]
    
    
  }
  stri=bitcoinData[[i]]$coin
  typeof(stri)
  print(stri)
  write.csv(bitcoinData[[i]],file=paste(stri,".csv",sep=""))
  bitcoinData[[i]]$coin<-bitcoinData[[i]]
  print(bitcoinData[[i]]$coin)
  
  bitcoinData$ROOFS
  head(bitcoinData)
  write.csv(bitcoinData,file="C:/Users/sudarshan/FYP/Data/bitcoinData.csv")
  df_uniq<- unique(data$coin)
  length(df_uniq)
  
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
