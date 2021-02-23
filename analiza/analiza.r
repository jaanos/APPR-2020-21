# Metoda voditeljev

kapacitete.vrste.obcin.1 <- kapacitete.vrste.obcin[order(kapacitete.vrste.obcin$Leto),] 
prenocitve.tipi$Kapaciteta <- kapacitete.vrste.obcin.1$Stevilo
tabela_1 <- prenocitve.tipi %>% rename(Prenocitve = Stevilo)
tabela_1$Prenocitve <- tabela_1$Prenocitve / 1000000
tabela_1$Kapaciteta <- tabela_1$Kapaciteta / 1000

analiza_k <- tabela_1 %>% select(Prenocitve, Kapaciteta) %>% scale()
tabela_k <- kmeans(analiza_k, 3)

tabela_1$Skupina <- tabela_k$cluster

graf_k <- ggplot(tabela_1) +
  aes(x=Kapaciteta, y=Prenocitve, col=as.factor(Skupina)) +
  geom_point() +
  labs(title="Razvrstitev turističnih občin po metodi voditeljev",
       y="Število prenočitev (milijoni)", x="Nastanitvena kapaciteta (tisoči)") +
  theme_few() +
  theme(legend.position="none") 
  

  
# Regresija

dodatek <- data.frame(Tip=c("Skupaj", "Skupaj","Skupaj"),Leto=c(2021, 2022, 2023),
                      Stevilo=c(0,0, 0))

tabela_2 <- vse.prenocitve %>% filter(vse.prenocitve$Tip == "Skupaj")
tabela_2$Stevilo <- tabela_2$Stevilo / 1000000 

model <- lm(data=tabela_2, Stevilo~Leto + I(Leto)^2 + I(Leto)^3)
napoved <- as.data.frame(predict(model, dodatek))



dodatek[c(1,2, 3), 3] <- napoved



graf_r <- ggplot(tabela_2, aes(x=Leto, y=Stevilo)) +
  geom_line(color="blue") +
  geom_point(color="black") +
  geom_point(data=dodatek, aes(x=Leto, y=Stevilo), color="red", size=2) +
  labs(title="Napoved gibanja števila vseh prenočitev za leta 2021-2023",
       y="Število prenočitev (milijoni)", x="Leto") +
    scale_x_continuous(limits=c(2000, 2023), breaks=seq(2000, 2024, 2)) +
  scale_y_continuous(limits=c(5,16),
                     breaks=seq(5, 16, 1)) +
  theme_hc()






  
