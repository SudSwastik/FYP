setwd("C:/Users/sudarshan/FYP/Data2")
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



try <- function (){
  ggplot(data=coinData,aes(x=coinData$Date,y=value,color=variable))+
    geom_line(aes(y=coinData$Open,col="coinData$Open"))+
    geom_line(aes(y=coinData$Close,col="coinData$Close"))
  
}



