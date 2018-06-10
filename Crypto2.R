setwd("C:/Users/sudarshan/FYP/Data2")
library(dplyr)
library(ggplot2)
#library(plotly)


btcData<- read.csv('btc.csv')
bchData <- read.csv('bch.csv')
ethData <- read.csv('ETH.csv')
DogeData <- read.csv('Doge.csv')
KinData <- read.csv('KIN.csv')

btcData$Date<-as.character(btcData$Date)
btcData$Date <- as.Date(btcData$Date,format='%Y-%m-%d')


ethData$Date<-as.character(ethData$Date)
ethData$Date <- as.Date(ethData$Date,format='%Y-%m-%d')


bchData$Date<-as.character(bchData$Date)
bchData$Date <- as.Date(bchData$Date,format='%Y-%m-%d')

DogeData$Date<-as.character(DogeData$Date)
DogeData$Date <- as.Date(DogeData$Date,format='%Y-%m-%d')

KinData$Date<-as.character(KinData$Date)
KinData$Date <- as.Date(KinData$Date,format='%Y-%m-%d')

openData <-  function(coinData){
  print(ggplot(data=coinData,aes(x=coinData$Date,y=coinData$Close))+
          geom_line(color="blue"))
}

closeData <-  function(coinData){
  print(ggplot(data=coinData,aes(x=coinData$Date,y=coinData$Close))+
          geom_line(color="yellow"))
}
highData <-  function(coinData){
  print(ggplot(data=coinData,aes(x=coinData$Date,y=coinData$High))+
          geom_line(color="red"))
}

lowData <-  function(coinData){
  print(ggplot(data=coinData,aes(x=coinData$Date,y=coinData$Low))+
          geom_line(color="green"))
}

openData(btcData)
closeData(btcData)
lowData(btcData)
highData(btcData)

openData(bchData)
closeData(bchData)
lowData(bchData)
highData(bchData)

openData(ethData)
closeData(ethData)
lowData(ethData)
highData(ethData)

openData(DogeData)
closeData(DogeData)
lowData(DogeData)
highData(DogeData)

openData(KinData)
closeData(KinData)
lowData(KinData)
highData(KinData)




#Redundant
multiple.timeseries.coin<- function(ethData,btcData){
  
  merged.dataframe<- ethData %>%
    bind_rows(btcData) 

  ggplot(merged.dataframe,aes(x=merged.dataframe$Date,y=merged.dataframe$High,color=coin))+
    geom_line()
  
}

multiple.timeseries.coin(ethData,btcData)



##Iterate through files to obtain prices of coins greater than 1000$
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
highCoins

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




#created a vector for taking yearly delta of all coin 
year<-c("2014","2015","2016","2017","2018")
typeof(year)


typeof(new.total.file$Year)


#plot for analysing the distribution of the delta of all coins 
#in a yearly format
for(y in c("2014","2015","2016","2017","2018")){
  delta.yearly<-new.total.file%>%group_by(Year)%>%filter(Year==y)
  delta.yearly
  print(ggplot(delta.yearly[delta.yearly$Delta > 1  & delta.yearly$Delta < 10, ], aes(x=Date,y=Delta, color=coin)) +
          geom_point()+ scale_x_date(date_breaks = "1 month", date_labels = "%B")+
          theme(legend.position="none"))
}




#distribution of the count of all coins delta whose value exceeds 1
ggplot(total.file[total.file$Delta > 1  & total.file$Delta < 10, ], aes(x=Delta, color=coin)) +
  geom_histogram() +
  theme(legend.position="none")


#distribution of the count of all coins delta whose value is less than  1
ggplot(total.file[total.file$Delta <1, ], aes(x=Delta, color=coin)) +
  geom_histogram() +
  theme(legend.position="none")

#distribution of the count of all coins delta whose value is less than 0
ggplot(total.file[total.file$Delta <0, ], aes(x=Delta, color=coin)) +
  geom_histogram() +
  theme(legend.position="none")


#calculating all the monthly delta for the whole year of top 5 coin 
#by market share

coin.name<-c("BTC","BCH","DOGE","ETH","KIN")

total.delta.yearly.2017$Month<- as.numeric(total.delta.yearly.2017$Month)
total.delta.yearly.2017

typeof(coin.name)

f<-list()
for(c in coin.name){
  monthly.delta=c()
  for(i in 1:12){
    total.delta.yearly.2017.months<-total.delta.yearly.2017%>%filter(coin==c)%>%filter(Month==i)%>%summarise(TotalSum=sum(Delta))
    monthly.delta[i]<-total.delta.yearly.2017.months
  }
  f[[c]]<-monthly.delta
  
}

f

#converting list to df
df1 <- data.frame(matrix(unlist(f), nrow=5, byrow=T),stringsAsFactors=FALSE)
df1
df1 <- as.data.frame(t(df1))
df1
colnames(df1)<-c("BTC","BCH","DOGE","ETH","KIN")
df1
df1$month <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
df1

# bar graph of monthly delta value
ggplot(data= df1,aes(y=df1$BTC,x=df1$month))+geom_bar(stat='identity')+
  labs(x="Month",y="Delta")+
  ggtitle("Monthly delta of BTC for the year 2017")


ggplot(data= df1,aes(y=df1$BCH,x=df1$month))+geom_bar(stat='identity')+
  labs(x="Month",y="Delta")+
  ggtitle("Monthly delta of BCH for the year 2017")



ggplot(data= df1,aes(y=df1$DOGE,x=df1$month))+geom_bar(stat='identity')+
  labs(x="Month",y="Delta")+
  ggtitle("Monthly delta of DOGE for the year 2017")


ggplot(data= df1,aes(y=df1$ETH,x=df1$month))+geom_bar(stat='identity')+
  labs(x="Month",y="Delta")+
  ggtitle("Monthly delta of ETH for the year 2017")



ggplot(data= df1,aes(y=df1$KIN,x=df1$month))+geom_bar(stat='identity')+
  labs(x="Month",y="Delta")+
  ggtitle("Monthly delta of KIN for the year 2017")






# checking outliers of open column for btc
BTC <- new.total.file%>%filter(coin=="BTC")%>%filter(Year==2014)
Open<-BTC$Open

boxplot(Open,
        horizontal = TRUE,
        las=1,
        notch = TRUE,
        col="slategray3",
        ylim=c(100,1000),
        boxwex=0.5,
        whisklty=1,
        main="Opening of btc for the year 2014"
        ,xlab="btc high ")
