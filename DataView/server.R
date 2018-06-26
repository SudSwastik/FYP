library(shiny)
library(ggplot2)
library(plotly)
shinyServer(
  function(input,output){
    output$table <- renderTable({
      coinName <- paste(input$selectedCoin,".csv",sep="",collapse ="")
      coinData <- read.csv(coinName)
      coinData
    })
   }
)
