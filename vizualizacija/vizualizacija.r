# 3. faza: Vizualizacija podatkov

# Prenocitve, tipi turisticnih obcin

graf.prenocitve.tipi <- function(datoteka){
  graf <- ggplot(datoteka) +
    aes(x=Leto, y=Stevilo, group=Tip, colour=Tip) +
    geom_point(size=2) +
    geom_line(size=1) +
    labs(title="Število prenočitev po tipu občin",
        y="Število prenočitev (milijoni)", x="Leto") +
    theme_hc() +
    scale_x_continuous(limits=c(2010, 2019), breaks=seq(2010, 2019, 1)) +
    scale_y_continuous(limits=c(0, 5),
                              breaks=seq(0,5, 0.5)) +
    scale_color_discrete(name = "Tip občine")
  return(graf)
}
  
graf.prenocitve.tipi <- graf.prenocitve.tipi(prenocitve.tipi.mio)


# Stolpicni diagram vseh gostov

gostje <- vsi.gosti[-c(34:66),] %>%
  filter(Leto %in% c(2000:2020))

gostje.mio <- gostje %>% mutate(Stevilo = Stevilo / 1000000)

diagram.vseh.gostov <- ggplot(gostje.mio) +
  aes(x=Leto, y=Stevilo, fill=Tip) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Število vseh gostov milijoni", y="Število gostov (milijoni)",
       x="Leto") + theme_bw() +
  scale_x_continuous(limits=c(1999, 2021), breaks=seq(2000, 2020, 2)) +
  scale_y_continuous(limits=c(0, 5),
                     breaks=seq(0,5, 0.5)) +
  scale_color_discrete(name = "Gosti")



# Graf vseh prenocitev

prenocitve <- filter(vse.prenocitve, Tip=="Skupaj",
                     Leto %in% c(2000:2020)) %>%
  mutate(Stevilo = Stevilo / 1000000)

graf.vseh.prenocitev <- ggplot(prenocitve) +
  aes(x=Leto, y=Stevilo) +
  geom_point(size=2) +
  geom_line(size=1, colour="blue") +
  scale_y_continuous(limits=c(5,16),
                     breaks=seq(5, 16, 1)) +
  scale_x_continuous(limits=c(2000, 2020), breaks=seq(2000, 2020, 2)) +
  labs(title="Število vseh prenočitev", y="Število prenočitev (milijoni)") +
  theme_hc()


# Zemljevid obcin


zemljevid <- function(datoteka){
obcine <- uvozi.zemljevid("http://baza.fmf.uni-lj.si/OB.zip", "OB",
                          pot.zemljevida="OB", encoding="Windows-1250")
proj4string(obcine) <- CRS("+proj=utm +zone=10+datum=WGS84")
tm_shape(obcine) + tm_polygons("OB_UIME") + tm_legend(show=FALSE)
obcine$OB_UIME <- as.factor(obcine$OB_UIME)

lvls <- levels(obcine$OB_UIME) %>% str_replace("Slov[.]", "Slovenskih") %>%
  sort()

prihodi <- datoteka$Obcina %>% 
  str_replace(" - ","-") %>%
  str_replace("/.*","") %>%
  str_replace("Slov[.]", "Slovenskih") %>%
  sort() %>% parse_factor()

obcine_prihodi2 <- datoteka
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
  labs(title="Število prihodov po posameznih občinah") +
  guides(fill=guide_legend(title="Število prihodov")) +
  brezOzadja 

return(zemljevid)
}

zemljevid_2019 <- zemljevid(obcine_prihodi_2019)
zemljevid_2018 <- zemljevid(obcine_prihodi_2018)

# Graf rasti stevila prenocitvenih zmogljivosti

kapacitete.vrste.obcin$Rast <- 0

zdraviliske.obcine <- subset(kapacitete.vrste.obcin, Tip=="Zdraviliške občine")
gorske.obcine <- subset(kapacitete.vrste.obcin, Tip=="Gorske občine")
obmorske.obcine <- subset(kapacitete.vrste.obcin, Tip=="Obmorske občine")
ljubljana <- subset(kapacitete.vrste.obcin, Tip=="Ljubljana")
mestne.obcine <- subset(kapacitete.vrste.obcin, Tip=="Mestne občine")
ostale.obcine <- subset(kapacitete.vrste.obcin, Tip=="Ostale občine")

rast <- function(data){
  k <- ncol(data)
  for (n in c(2:10)){
    data[n,k] <- (data[n,k-1] - data[n-1,k-1]) / data[n-1,k-1] * 100
  }
  data[,k] <- round(data[,k], digits=2)
  return(data)
}


zdraviliske.obcine.rast <- rast(zdraviliske.obcine)
gorske.obcine.rast <- rast(gorske.obcine)
obmorske.obcine.rast <- rast(obmorske.obcine)
ljubljana.rast <- rast(ljubljana)
mestne.obcine.rast <- rast(mestne.obcine)
ostale.obcine.rast <- rast(ostale.obcine)

kapacitete.vrste.obcin.rast <- data.frame(rbind(zdraviliske.obcine.rast,
  gorske.obcine.rast, obmorske.obcine.rast, ljubljana.rast,
  mestne.obcine.rast, ostale.obcine.rast))

graf.zmogljivosti.rast <- ggplot(kapacitete.vrste.obcin.rast) +
  aes(x=Leto, y=Rast, colour=Tip) +
  geom_point(size=2) +
  geom_line(size=1) +
  facet_wrap(.~Tip, ncol=2) +
  labs(title="Rast prenocitvenih zmogljivosti za posamezen tip turističnih občin",
       y="Rast prenocitvenih zmogljivosti (%)", x="Leto") +
  scale_x_continuous(limits=c(2010, 2019), breaks=seq(2010, 2019, 2)) +
  scale_y_continuous(limits=c(-20, 20), breaks=seq(-20, 20, 10)) +
  scale_color_discrete(name = "Tip občine")


# Graf prenocitvenih zmogljivosti

kapacitete.vrste.obcin.tisoci <- kapacitete.vrste.obcin %>%
  mutate(Stevilo = Stevilo / 1000)

graf.zmogljivosti.tipi <- ggplot(kapacitete.vrste.obcin.tisoci) +
  aes(x=Leto, y=Stevilo, group=Tip, colour=Tip) +
  geom_point(size=2) +
  geom_line(size=1) +
  labs(title="Prenočitvene zmogljivosti za posamezen tip turističnih občin",
       y="Prenočitvena zmogljivost (tisoči)", x="Leto") +
  scale_x_continuous(limits=c(2010, 2019), breaks=seq(2010, 2019, 3)) +
  facet_wrap(.~Tip, ncol=2, scales="free") +
  scale_color_discrete(name = "Tip občine") 













 

  


 


  
   





