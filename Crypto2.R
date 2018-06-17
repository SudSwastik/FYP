setwd("C:/Users/sudarshan/FYP/Data2")
library(dplyr)
library(ggplot2)
#library(plotly)

path <- "C:/Users/Saurabh/Desktop/FYP/Data2"
fs <- list.files(path, pattern = glob2rx("*.csv"))
R> fs
for (f in fs) {
  fname <- file.path(path, f)             ## current file name
  df <- read.csv(fname)                   ## read file
  df <- select(df,-1)                     ## delete column B
  write.csv(df, fname, row.names = FALSE) ## write it out
}

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



#function used for analysis of all coin data for open varibale 
analysis<-function(openDataCoin){
  print(mean(openDataCoin))
  print(median(openDataCoin))
  print(sqrt(var(openDataCoin)))
  print(max(openDataCoin)-min(openDataCoin))
  print(hist(openDataCoin))
  print(skewness(openDataCoin))
  print(kurtosis(openDataCoin))
  print(hist(openDataCoin,prob=TRUE))
  print(curve(dnorm(x,mean(openDataCoin),sd(openDataCoin)),col="red",add=TRUE))
  print(qqnorm(openDataCoin))
  print(qqline(openDataCoin,col="red"))
  print(qqPlot(openDataCoin,distribution = "norm"))
}



#           checking outliers of open column for btc


BTC.2014 <- new.total.file%>%filter(coin=="BTC")%>%filter(Year==2014)
BTC.Open.2014<-BTC.2014$Open


#boxplot shows no outlier
boxplot(BTC.Open.2014,
        horizontal = TRUE,
        las=1,
        notch = TRUE,
        col="slategray3",
        ylim=c(100,1000),
        boxwex=0.5,
        whisklty=1,
        main="Opening of btc for the year 2014"
        ,xlab="btc Open ")


BTC.2015 <- new.total.file%>%filter(coin=="BTC")%>%filter(Year==2015)
BTC.Open.2015<-BTC.2015$Open

# boxplot shows outlier
boxplot(BTC.Open.2015,
        horizontal = TRUE,
        las=1,
        notch = TRUE,
        col="slategray3",
        ylim=c(100,500),
        boxwex=0.5,
        whisklty=1,
        main="Opening of btc for the year 2015"
        ,xlab="btc Open ")

#analysis
analysis(BTC.Open.2015)


BTC.2016 <- new.total.file%>%filter(coin=="BTC")%>%filter(Year==2016)
BTC.Open.2016<-BTC.2016$Open

# boxplot shows  no outlier
boxplot(BTC.Open.2016,
        horizontal = TRUE,
        las=1,
        notch = TRUE,
        col="slategray3",
        ylim=c(100,1000),
        boxwex=0.5,
        whisklty=1,
        main="Opening of btc for the year 2016"
        ,xlab="btc Open ")


BTC.2017 <- new.total.file%>%filter(coin=="BTC")%>%filter(Year==2017)
BTC.Open.2017<-BTC.2017$Open

# boxplot shows outlier
boxplot(BTC.Open.2017,
        horizontal = TRUE,
        las=1,
        notch = TRUE,
        col="slategray3",
        ylim=c(800,12000),
        boxwex=0.5,
        whisklty=1,
        main="Opening of btc for the year 2017"
        ,xlab="btc Open ")

#analysis
analysis(BTC.Open.2017)




#             checking outliers of open column for bch


BCH.2017 <- new.total.file%>%filter(coin=="BCH")%>%filter(Year==2017)
BCH.Open.2017<-BCH.2017$Open

#boxplot shows outlier
boxplot(BCH.Open.2017,
        horizontal = TRUE,
        las=1,
        notch = TRUE,
        col="slategray3",
        ylim=c(100,5000),
        boxwex=0.5,
        whisklty=1,
        main="Opening of bch for the year 2017"
        ,xlab="bch Open ")

#analysis
analysis(BCH.Open.2017)


BCH.2018 <- new.total.file%>%filter(coin=="BCH")%>%filter(Year==2018)
BCH.Open.2018<-BCH.2018$Open

#boxplot shows no outlier
boxplot(BCH.Open.2018,
        horizontal = TRUE,
        las=1,
        notch = TRUE,
        col="slategray3",
        ylim=c(100,5000),
        boxwex=0.5,
        whisklty=1,
        main="Opening of bch for the year 2018"
        ,xlab="bch Open ")


#           checking outliers of open column for eth


ETH.2015 <- new.total.file%>%filter(coin=="ETH")%>%filter(Year==2015)
ETH.Open.2015<-ETH.2015$Open

#boxplot shows  outlier
boxplot(ETH.Open.2015,
        horizontal = TRUE,
        las=1,
        notch = TRUE,
        col="slategray3",
        ylim=c(0,4),
        boxwex=0.5,
        whisklty=1,
        main="Opening of eth for the year 2015"
        ,xlab="eth Open ")

#analysis
analysis(ETH.Open.2015)



ETH.2017 <- new.total.file%>%filter(coin=="ETH")%>%filter(Year==2017)
ETH.Open.2017<-ETH.2017$Open

#boxplot shows outlier
boxplot(ETH.Open.2017,
        horizontal = TRUE,
        las=1,
        notch = TRUE,
        col="slategray3",
        ylim=c(0,4),
        boxwex=0.5,
        whisklty=1,
        main="Opening of eth for the year 2017",
        xlab="eth Open ")

#analysis
analysis(ETH.Open.2017)




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

#Removing first column indicating count from each data subset 
path <- "C:/Users/Saurabh/Desktop/FYP/Data2"
fs <- list.files(path, pattern = glob2rx("*.csv"))
R> fs
for (f in fs) {
  fname <- file.path(path, f)             ## current file name
  df <- read.csv(fname)                   ## read file
  df <- select(df,-1)                     ## delete column B
  write.csv(df, fname, row.names = FALSE) ## write it out
}


