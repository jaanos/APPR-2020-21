# 3. faza: Vizualizacija podatkov

# Prenocitve, tipi turisticnih obcin

graf.prenocitve.tipi <- ggplot(prenocitve.tipi) +
  aes(x=Leto, y=Stevilo, group=Tip, colour=Tip) +
  geom_point(size=2) +
  geom_line(size=1) +
  labs(title="Število prenoÄitev po tipu obèin",
       y="Število prenoèitev", x="Leto") +
  theme_hc() +
  scale_x_continuous(limits=c(2010, 2019), breaks=seq(2010, 2019, 1)) +
  scale_y_continuous(limits=c(0, 4800000),
                            breaks=seq(0,4800000,500000)) +
  scale_color_discrete(name = "Tip obèine")



# Stolpicni diagram vseh gostov

gostje <- vsi.gosti[-c(33:64),] %>%
  filter(Leto %in% c(2000:2019))

diagram.vseh.gostov <- ggplot(gostje) +
  aes(x=Leto, y=Stevilo, fill=Tip) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Število vseh gostov", y="Število gostov", x="Leto") +
  theme_bw() +
  scale_x_continuous(limits=c(1999, 2020), breaks=seq(2000, 2020, 2)) +
  scale_y_continuous(limits=c(0, 4800000),
                     breaks=seq(0,4800000,500000)) +
  scale_color_discrete(name = "Gosti")



# Graf vseh prenocitev

prenocitve <- filter(vse.prenocitve, Tip=="Skupaj",
                     Leto %in% c(2000:2019))

graf.vseh.prenocitev <- ggplot(prenocitve) +
  aes(x=Leto, y=Stevilo) +
  geom_point(size=2) +
  geom_line(size=1, colour="blue") +
  scale_y_continuous(limits=c(5000000,16000000),
                     breaks=seq(5000000, 16000000, 1000000)) +
  scale_x_continuous(limits=c(2000, 2020), breaks=seq(2000, 2020, 2)) +
  labs(title="Število vseh prenoèitev", y="Število prenoèitev") +
  theme_hc()


# Zemljevid obcin









  


 

  


 


  
   





