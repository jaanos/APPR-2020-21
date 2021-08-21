library(shiny)

server <- function(input, output) {
  output$graf <- renderPlotly({
    
    if (input$r == "Vse") {podatki <- kom19 %>% filter(minutes >= input$q * 90)}
    else {podatki <- kom19 %>% filter(minutes >= input$q * 90, Pozicija == input$r)}
    
    podatki <- podatki %>% group_by(Ime, Priimek, Pozicija, id, Ekipa, Igralec, Uvrstitev) %>%
      summarise(across(everything(),function(x) {round(x/minutes * 90, 2)})) %>% select(-minutes)
    
    vrednost1 <- quantile(podatki[input$x], input$z, na.rm = TRUE)
    
    vrednost2 <- quantile(podatki[input$y], input$w, na.rm = TRUE)
    
    # print(vrednost1)
    # print(vrednost2)
    
    if (input$x == input$y) {

      vrednost = min(vrednost1, vrednost2)

      podatki <- podatki %>% rename(statistika = input$x) %>% filter(statistika >= vrednost) %>% ungroup() %>%
        select(Igralec, Ekipa, statistika, Pozicija)
      # View(podatki)
      ggplotly(ggplot(podatki) + aes(x=statistika, y=statistika, color = Pozicija, Igralec = Igralec, Ekipa = Ekipa) + geom_point() +
                 xlab(input$x) + ylab(input$x) + labs(title = "Primerjava izbranih statistik na 90 minut") +
                 theme(plot.title = element_text(color = "orchid4", size = 18)))
    }
    else {
      podatki <- podatki %>% rename(statistika1 = input$x, statistika2 = input$y) %>%
      filter(statistika1 >= vrednost1 | statistika2 >= vrednost2) %>% ungroup() %>% select(Igralec, Ekipa, statistika1, statistika2, Pozicija)
      # View(podatki)
      ggplotly(ggplot(podatki) + aes(x=statistika1, y=statistika2, color = Pozicija, Igralec = Igralec, Ekipa = Ekipa) + geom_point() +
                 xlab(input$x) + ylab(input$y) + labs(title = "Primerjava izbranih statistik na 90 minut") +
                 theme(plot.title = element_text(color = "orchid4", size = 18)))
    }
    
    #View(podatki)
    })
}






# + xlab(input$x) + ylab(input$y)


# library(shiny)
# 
# shinyServer(function(input, output) {
#   output$druzine <- DT::renderDataTable({
#     druzine %>% pivot_wider(names_from="velikost.druzine", values_from="stevilo.druzin") %>%
#       rename(`Občina`=obcina)
#   })
#   
#   output$pokrajine <- renderUI(
#     selectInput("pokrajina", label="Izberi pokrajino",
#                 choices=c("Vse", levels(obcine$pokrajina)))
#   )
#   output$naselja <- renderPlot({
#     main <- "Pogostost števila naselij"
#     if (!is.null(input$pokrajina) && input$pokrajina %in% levels(obcine$pokrajina)) {
#       t <- obcine %>% filter(pokrajina == input$pokrajina)
#       main <- paste(main, "v regiji", input$pokrajina)
#     } else {
#       t <- obcine
#     }
#     ggplot(t, aes(x=naselja)) + geom_histogram() +
#       ggtitle(main) + xlab("Število naselij") + ylab("Število občin")
#   })
# })
