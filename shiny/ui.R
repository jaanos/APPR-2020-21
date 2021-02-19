

ui=shinyUI(fluidPage(checkboxGroupInput("tip", label = "Izberi tip",
                                        choices = c("Zdraviliške občine","Gorske občine",
                                                    "Obmorske občine","Ljubljana",
                                                    "Mestne občine","Ostale občine"),
                                        selected=c("Zdraviliške občine","Gorske občine",
                                                   "Obmorske občine","Ljubljana",
                                                   "Mestne občine","Ostale občine"),
),

plotOutput("graf")
))

server=function(input,output){
  #data manipulation
  data_1=reactive({
    return(prenocitve.tipi.mio[prenocitve.tipi.mio$Tip%in%input$tip,])
  })
  #plot
  output$graf <- renderPlot({
    ggplot(data=data_1(), aes(x=Leto, y=Stevilo, group=Tip, colour=Tip)) +
      geom_line() +
      geom_point()
  })
}

shinyApp(ui,server)