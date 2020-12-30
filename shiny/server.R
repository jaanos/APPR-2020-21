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
      y <- h_med_s %>% filter(STATE==input$drzava, leto==input$leto) 
      print( ggplot(y) +
               aes(x = leto, y = HME) +
               geom_boxplot(fill="green", colour="green" , alpha=I(0.7)) +
               geom_point(size=0.2, colour="blue") +
               scale_x_continuous(name = "leto", breaks = seq(input$leto,input$leto,1)) + 
               geom_jitter(alpha=I(0.4)) +
               xlab("Leto") +
               ylab("Urna mediana plača glede na poklic") 
       )
    })
    
      output$distPlot3 <- renderPlot({ 
          z1 <- tabela %>% filter(State==input$drzava2)
          z2 <- tabela %>% filter(State=="United States")
          z3 <- rbind(z1, z2)
          print( ggplot(z3) +
                   aes(x = leto, y=GDP, col=State) + 
                   geom_point(size=2) +
                   geom_line(size=1)  +
                   xlab("Leto") +
                   ylab("BDP per capita") +
                   labs(title="Primerjava BDP per capita.") 
          )
      })
      
      output$distPlot4 <- renderPlot ({
        w1 <- h_med_s %>% filter(STATE==input$drzava2)
        w2 <- h_mean_c
        names(w1) <- names(w2) 
        w3 <- rbind(w1, w2)

        print( ggplot(w3) +
                 aes(x = leto, y=HM, col=STATE) + 
                # geom_point(size=2) +
                 xlab("Leto") +
                 ylab("Povprečna urna postavka") +
                 labs(title="Primerjava urnih postavk.") +
                 stat_smooth(method = "lm") 
                 
        
        )
        
      })
    
})
