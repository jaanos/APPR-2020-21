library(shiny)

function(input, output) {
  
  output$graf_vrste <- renderPlot({
    graf_vrste <- ggplot(vrste_dohodka %>% filter(Leto == input$Leto,
                                                  Vrsta.dohodka %in% input$Vrsta)) + 
      aes(x=Vrsta.dohodka, y=Dohodek, fill=Vrsta.dohodka) +
      geom_col(position = "dodge") +
      labs(title = "Dohodek glede na vrsto") + theme(plot.title = element_text(hjust = 0.5)) +
      ylab("Dohodek") +
      labs(fill = "Vrste dohodka") +
      theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
      scale_y_continuous(breaks=seq(0, 7000, 1000))
    print(graf_vrste)
  })

}
