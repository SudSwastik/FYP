setwd("C:/Users/sudarshan/FYP")
library("dplyr")
dataFilePath <- "C:/Users/sudarshan/FYP/Data/CryptocoinsHistoricalPrices.csv"
data <- read.csv(dataFilePath)
str(data)
sapply(data,class)

head(data)

coinData <- split(data,data$coin)
bitcoinData
