library(shiny)

shinyServer(function(input, output) {
  
  output$tabela <- DT::renderDataTable({
    
    tabela1
  })
  
  
  output$tocke <- renderPlot({
    
    tabela__1 <- tabela3 %>% filter(Drzava == input$drzava)
    
    print(ggplot(data=tabela__1, aes(x = Leto, y = Tocke), group=1) + geom_line(col="#02e9cc") + geom_point(col="#339999") + 
            ylab("Število točk") + xlab("Leto")) + 
      #scale_x_continuous(breaks = seq(1961, 1992, by=2), limits = c(1961,1992))
      theme_minimal()+
      theme(axis.title = element_text(size=13,face="bold"), axis.text = element_text(size=10), axis.text.x=element_text(angle=90, vjust=0.5, hjust=0.5))
    
  })
  
})