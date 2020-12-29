library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)

shinyServer(function(input, output) {
  output$distPlot <- renderPlot({
    x    <- h_mean$HM
    bini <- seq(min(x), max(x), length.out = input$bini + 1)
    hist(x, breaks = bini, col = "yellow", border = "black",
         xlab = "Pojavljenost plače",
         ylab = "Frekvenca",
         main = "Histogram povprečnih plač")
    })
    
    output$distPlot2 <- renderPlot({
      y <- h_med_s %>% filter(STATE==input$drzava) %>% filter(leto==input$leto) 
      print( ggplot(y) +
               aes(x = leto, y = HME) +
               geom_boxplot(fill="green", colour="green" , alpha=I(0.7)) +
               geom_point(size=0.2, colour="yellow") +
               geom_jitter(alpha=I(0.1)) +
               xlab("Leto") +
               ylab("Urna mediana plača glede na poklic") +
               labs(tite=input$drzava)
       )
    })
})
