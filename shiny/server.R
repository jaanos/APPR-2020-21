shinyServer(function(input, output) {
  output$distPlot <- renderPlot({
    x    <- nat.ha %>% drop_na(h) %>% filter(leto=="2019",sredina=="mean") %>% filter(h > 100)
    x.2 <- nat.ha$h
    bini <- seq(min(x.2), max(x.2), length.out = input$bini + 1)
    hist(x.2, breaks = bini, col = "yellow", border = "black",
         xlab = "Pojavljenost plače",
         ylab = "Frekvenca",
         main = "Histogram povprečnih plač")
    })
    
    output$distPlot2 <- renderPlot({
      y <- st.ha %>% filter(state==input$drzava, leto==input$leto) %>% 
                    filter(sredina=="mean")
      print( ggplot(y) +
               aes(x = leto, y = h) +
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
        w1 <- st.ha %>% filter(state==input$drzava2, sredina=="median") 
        w2 <- h_mean_c
        names(w1) <- names(w2) 
        w3 <- rbind(w1, w2)
        print( ggplot(w3) +
                 aes(x = leto, y=h, col=state) + 
                # geom_point(size=2) +
                 xlab("Leto") +
                 ylab("Povprečna urna postavka") +
                 labs(title="Primerjava urnih postavk.") +
                 stat_smooth(method = "lm") 
        )
      })
    
      output$distPlot5 <- renderPlot({
        if ( input$tabela=="h_mean" | input$tabela=="a_mean") 
        { mapdb <- st.ha %>% filter(sredina=="mean") 
        } else if ( input$tabela=="h_median" | input$tabela=="a_meadian") 
        { mapdb <- st.ha %>% filter(sredina=="median") 
        } else {  mapdb <-  mapdb <- t_e_s
        }
        if (input$tabela=="h_mean" | input$tabela=="h_median")
        {mapdb1 <- mapdb %>%
          drop_na(h) %>%
          group_by(state) %>% 
          summarise(povprecje= mean(h))
        a <- tmap_style("col_blind") 
        } else if (input$tabela=="a_mean" | input$tabela=="a_meadian")
        {mapdb1 <- mapdb %>%
          drop_na(a) %>%
          group_by(state) %>% 
          summarise(povprecje= mean(a))
        a <- tmap_style("natural") 
        } else { mapdb1 <- mapdb %>%
          drop_na(emp) %>%
          filter(occ_code=="00-0000") %>%
          group_by(state) %>%
          summarise(povprecje= mean(emp)) 
        a <- tmap_style("gray") 
        }  
        print (
          tm_shape(merge(zemljevid, mapdb1, by.x="STATE_NAME", by.y="state")) +
          tm_polygons("povprecje") +
          a
              )
      })  
})
