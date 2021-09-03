# 4. faza: Analiza podatkov

tabelazaanalizo <- tabela2 %>% 
  filter(regija=="SLOVENIJA")
samopike <- ggplot(tabelazaanalizo, aes(x=leto, y=vrednost)) + geom_point()
model <- lm(vrednost ~ leto, data=tabelazaanalizo)
napoved <- ggplot(tabelazaanalizo, aes(x=leto, y=vrednost)) + geom_point() +
          geom_smooth(method=lm, formula=y~x) +
          ylab("Dobavljena voda na prebivalca v m^3")

prihodnjaleta <- data.frame(leto=c(2020:2025))
predict(model, prihodnjaleta)
napoved1 <- prihodnjaleta %>% mutate(vrednost=predict(model, prihodnjaleta))
napoved1$vrednost <- round(napoved1$vrednost, digit=1)
napoved2 <- rbind(tabelazaanalizo[,-1], napoved1)

izrisnapovedi <- ggplot(napoved2, aes(x=leto, y=vrednost)) +
  geom_point(shape=1) +
  geom_smooth(method=lm, formula=y~x) +
  geom_point(data=napoved1, aes(x=leto, y=vrednost), color="red", size=2) +
  xlab("Leto") +
  ylab("Dobavljena voda v m^3 na prebivalca")