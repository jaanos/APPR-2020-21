# Shiny

Tukaj bomo imeli program Shiny aplikacijo, ki jo bomo vključili v poročilo.

 tabPanel("Priseljevanje v Slovenijo glede na dejavnost",
             sidebarPanel(
               
               selectInput("stevilopris", label = "Izberite dejavnost:",
                           choices=(sort(unique(shiny_tab3$Dejavnost)))),
               
               mainPanel(plotOutput("dejavnost"))),




             
             
             
        output$dejavnost- renderPlot({
    
    tabela__3 <- shiny_tab3 %>% filter(Dejavnost == input$stevilopris)
    
    print(ggplot(data=tabela__3, aes(x = Leto, y = Stevilo), group=1) + geom_line(color="purple") +
            ylab("Število priseljenih ljudi") + xlab("Leto")) + scale_x_continuous(breaks = seq(2011, 2019, by=1), limits = c(2011,2019)) +
      theme_minimal() + theme(axis.title = element_text(size=13,face="bold"), axis.text = element_text(size=10), axis.text.x=element_text(angle=45, vjust=0.5, hjust=0.5))
  })         