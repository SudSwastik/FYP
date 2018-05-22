library(shiny)
library(ggplot2)
library(plotly)

shinyServer(
  function(input,output){
    
    output$summary <- renderPrint({
      coinName <- paste(input$selectedCoin,".csv",sep="",collapse ="")
      coinData <- read.csv(coinName)
      summary(coinData)
    })
    output$structure <- renderPrint({
      coinName <- paste(input$selectedCoin,".csv",sep="",collapse ="")
      coinData <- read.csv(coinName)
      str(coinData)
    })
    
    output$highPlot <- renderPlot({
      coinName <- paste(input$selectedCoin,".csv",sep="",collapse ="")
      coinData <- read.csv(coinName)
      coinData$Date<-as.character(coinData$Date)
      coinData$Date <- as.Date(coinData$Date,format='%Y-%m-%d')
      a <- list(title="Date")
      b <- list(title="Price in USD")
      ggplot(data=coinData,aes(x=coinData$Date,y=coinData$High))+
        geom_line(color=input$color)+ labs(x = input$selectedCoin, y ="Price in USD")+theme_minimal()
      }
      
    )
    output$lowPlot <- renderPlot({
      coinName <- paste(input$selectedCoin,".csv",sep="",collapse ="")
      coinData <- read.csv(coinName)
      coinData$Date<-as.character(coinData$Date)
      coinData$Date <- as.Date(coinData$Date,format='%Y-%m-%d')
      a <- list(title="Date")
      b <- list(title="Price in USD")
      ggplot(data=coinData,aes(x=coinData$Date,y=coinData$Low))+
        geom_line(color=input$color)+ labs(x = input$selectedCoin, y ="Price in USD")+theme_minimal()
    }
    
    )

  }
)
