library(shiny)
library(shinythemes)

shinyServer(function(input, output) {
  output$distPlot <- renderPlot({
    x    <- h_mean$HM
    bini <- seq(min(x), max(x), length.out = input$bini + 1)
    hist(x, breaks = bini, col = "yellow", border = "black",
         xlab = "Pojavljenost plače",
         ylab = "Frekvenca",
         main = "Histogram povprečnih plač")
    })
  })
  
