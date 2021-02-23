
shinyServer(function(input, output){
  output$map <- renderPlot({
    if(input$izbira == "2018"){
      obcine <- uvozi.zemljevid("http://baza.fmf.uni-lj.si/OB.zip", "OB",
                                pot.zemljevida="OB", encoding="Windows-1250")
      proj4string(obcine) <- CRS("+proj=utm +zone=10+datum=WGS84")
      tm_shape(obcine) + tm_polygons("OB_UIME") + tm_legend(show=FALSE)
      obcine$OB_UIME <- as.factor(obcine$OB_UIME)
      
      lvls <- levels(obcine$OB_UIME) %>% str_replace("Slov[.]", "Slovenskih") %>%
        sort()
      
      prihodi <- obcine_prihodi_2018$Obcina %>% 
        str_replace(" - ","-") %>%
        str_replace("/.*","") %>%
        str_replace("Slov[.]", "Slovenskih") %>%
        sort() %>% parse_factor()
      
      obcine_prihodi2 <- obcine_prihodi_2018
      obcine_prihodi2$Obcina <- prihodi
      
      zdruzena <- merge(obcine, obcine_prihodi2, by.x="OB_UIME", by.y="Obcina")
      zdruzena$Stevilo[95] <- 0
      obcine_podatki <- data.frame("id"=zdruzena$OB_ID, "Stevilo"=zdruzena$Stevilo) 
      obcine_podatki <- mutate(obcine_podatki, id = as.character(obcine_podatki$id))
      zdruzena_fort <- tidy(zdruzena, region="OB_ID")
      
      fort_stevilo <- zdruzena_fort %>% left_join(obcine_podatki, by="id")
      
      vrednosti <- round((quantile(obcine_prihodi_2018$Stevilo) + quantile(obcine_prihodi_2018$Stevilo)) / c(2, 2, 2, 2, 2))
      
      brezOzadja <- theme_bw() +
        theme(
          axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank()
        ) 
      
      legenda <- c("Prvi kvartil", "Drugi kvartil", "Tretji kvartil", "Četrti kvartil")
      
      zemljevid <- fort_stevilo %>% mutate(vrednost=factor(findInterval(fort_stevilo$Stevilo,
                                                                       vrednosti, all.inside=TRUE))) %>%
        ggplot() + geom_polygon(color="black", size=0.001) + 
        aes(x=long, y=lat, group=group, fill=vrednost) +
        scale_fill_brewer(type = 4, palette="Reds", labels=legenda) +
        guides(fill=guide_legend(title="Legenda")) +
        brezOzadja
      return(zemljevid)
    }else{
      obcine <- uvozi.zemljevid("http://baza.fmf.uni-lj.si/OB.zip", "OB",
                                pot.zemljevida="OB", encoding="Windows-1250")
      proj4string(obcine) <- CRS("+proj=utm +zone=10+datum=WGS84")
      tm_shape(obcine) + tm_polygons("OB_UIME") + tm_legend(show=FALSE)
      obcine$OB_UIME <- as.factor(obcine$OB_UIME)
      
      lvls <- levels(obcine$OB_UIME) %>% str_replace("Slov[.]", "Slovenskih") %>%
        sort()
      
      prihodi <- obcine_prihodi_2019$Obcina %>% 
        str_replace(" - ","-") %>%
        str_replace("/.*","") %>%
        str_replace("Slov[.]", "Slovenskih") %>%
        sort() %>% parse_factor()
      
      obcine_prihodi2 <- obcine_prihodi_2019
      obcine_prihodi2$Obcina <- prihodi
      
      zdruzena <- merge(obcine, obcine_prihodi2, by.x="OB_UIME", by.y="Obcina")
      zdruzena$Stevilo[95] <- 0
      obcine_podatki <- data.frame("id"=zdruzena$OB_ID, "Stevilo"=zdruzena$Stevilo) 
      obcine_podatki <- mutate(obcine_podatki, id = as.character(obcine_podatki$id))
      zdruzena_fort <- tidy(zdruzena, region="OB_ID")
      
      fort_stevilo <- zdruzena_fort %>% left_join(obcine_podatki, by="id")
      
      vrednosti <- round((quantile(obcine_prihodi_2018$Stevilo) + quantile(obcine_prihodi_2018$Stevilo)) / c(2, 2, 2, 2, 2))
      
      brezOzadja <- theme_bw() +
        theme(
          axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank()
        ) 
      
      legenda <- c("Prvi kvartil", "Drugi kvartil", "Tretji kvartil", "Četrti kvartil")
      
      zemljevid <- fort_stevilo %>% mutate(vrednost=factor(findInterval(fort_stevilo$Stevilo,
                                                                      vrednosti, all.inside=TRUE))) %>%
        ggplot() + geom_polygon(color="black", size=0.001) + 
        aes(x=long, y=lat, group=group, fill=vrednost) +
        scale_fill_brewer(type = 4, palette="Reds", labels=legenda) +
        guides(fill=guide_legend(title="Legenda")) +
        brezOzadja
      return(zemljevid)
    }
  })
})
