library(shiny)
shinyUI(fluidPage(
  titlePanel(title=h1("Time Series of different Cryptocurrencies",align="center")),
  sidebarLayout(
    sidebarPanel(selectInput("selectedCoin","Select the Coin",choices=list("BTC","ETH","ABC")),
                 radioButtons("color","Choose a color",choices=list("Black","mediumorchid4","deeppink4","slateblue4"))),
    mainPanel(
      tabsetPanel(type="tab",
                  tabPanel("Summary",verbatimTextOutput("summary")),
                  tabPanel("Structure",verbatimTextOutput("structure")),
                  tabPanel("High",plotOutput("highPlot")),
                  tabPanel("Low",plotOutput("lowPlot"))
                  ))
  )
))
