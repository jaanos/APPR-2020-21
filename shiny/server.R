library(shiny)
library(dplyr)

shinyServer(function(input, output) {
  
  output$tabela<- DT::renderDataTable({
    
    tabela14%>% 
      rename("Statistične regije"=Regija, "Število priseljenih ljudi iz tujine"=Stevilo_priseljenih_iz_tujine)
  })
  
  
  output$priseljevanje<- renderPlot({
    
    tabela__1 <- shiny_tab2 %>% filter(Drzava == input$drzava)
    
    print(ggplot(data=tabela__1, aes(x = Leto, y = Stevilo), group=1) + geom_line(col="#02e9cc") + geom_point(col="#339999") + 
            ylab("Število priseljenih ljudi iz tujine") + xlab("Leto")) + scale_x_continuous(breaks = seq(2011, 2019, by=2), limits = c(2011,2019)) + theme_minimal()+
      theme(axis.title = element_text(size=13,face="bold"), axis.text = element_text(size=10), axis.text.x=element_text(angle=45, vjust=0.5, hjust=0.5))
    
  })
  
  
  output$izobrazba <- renderPlot({
    
    tabela__2 <-  shiny_tab1 %>% filter(Izobrazba == input$izob) 
    
    print(ggplot(data=tabela__2, aes(x = Leto, y = Stevilo), group=1) + geom_line(color="#f9009f") +
            ylab("Število priseljenih ljudi") + xlab("Leto")) + scale_x_continuous(breaks = seq(2011, 2019, by=1), limits = c(2011,2019)) + theme_minimal() +
      theme(axis.title = element_text(size=13,face="bold"), axis.text = element_text(size=10), axis.text.x=element_text(angle=45, vjust=0.5, hjust=0.5))
  })
  
  output$dejavnost <- renderPlot({
    
    tabela__3 <-  shiny_tab3 %>% filter(Dejavnost == input$dej) 
    
    print(ggplot(data=tabela__3, aes(x = Leto, y = Stevilo), group=1) + geom_line(color="#CC0033") +
            ylab("Število priseljenih ljudi") + xlab("Leto")) + scale_x_continuous(breaks = seq(2011, 2019, by=1), limits = c(2011,2019)) + theme_minimal() +
      theme(axis.title = element_text(size=13,face="bold"), axis.text = element_text(size=10), axis.text.x=element_text(angle=45, vjust=0.5, hjust=0.5))
  })
  
  
  
  output$struktura <- renderPlot({
    
    tabela__6 <-  tabela1 %>% filter(Leto == input$leto) %>%  filter(Spol == input$spol)
    
  print(ggplot(data=tabela__6, aes(x = factor(1), y = Priseljeni_iz_tujine, fill = Drzava)) +
    xlab("") + ylab("") +
    geom_bar(width = 1, stat = "identity") + ggtitle("Države iz katerih prihajajo priseljenci") +
  theme_minimal() +
    theme(legend.title=element_blank(),plot.title = element_text(hjust = 0.7, face = "bold"),axis.title = element_text(size=13,face="bold"), axis.text = element_text(size=10), axis.text.x=element_text(angle=45, vjust=0.5, hjust=0.5))
  + coord_polar("y", start=0)+ scale_fill_manual(values=cols1))
  
  })
  
  
  
  
  
  output$izobrazba2 <- renderPlot({
    
    tabela__4 <-  shiny_tab4 %>% filter(Izobrazba == input$izob2) 
    
    print(ggplot(data=tabela__4, aes(x = Leto, y = Stevilo), group=1) + geom_line(color="#eaaa00") +
            ylab("Število izseljenih ljudi") + xlab("Leto")) + scale_x_continuous(breaks = seq(2011, 2019, by=1), limits = c(2011,2019)) + theme_minimal() +
      theme(axis.title = element_text(size=13,face="bold"), axis.text = element_text(size=10), axis.text.x=element_text(angle=45, vjust=0.5, hjust=0.5))
  })
  
  output$dejavnostiz <- renderPlot({
    
    tabela__5 <-  shiny_tab5 %>% filter(Dejavnost == input$dejiz) 
    
    print(ggplot(data=tabela__5, aes(x = Leto, y = Stevilo), group=1) + geom_line(color="#6d009b") +
            ylab("Število izseljenih ljudi") + xlab("Leto")) + scale_x_continuous(breaks = seq(2011, 2019, by=1), limits = c(2011,2019)) + theme_minimal() +
      theme(axis.title = element_text(size=13,face="bold"), axis.text = element_text(size=10), axis.text.x=element_text(angle=45, vjust=0.5, hjust=0.5))
  })
  

  
  
})