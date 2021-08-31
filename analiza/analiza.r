# 4. faza: Analiza podatkov

tabelazaanalizo <- tabela2[,c(1,2)]
tabelazaanalizo$LETO <- as.numeric(as.character(tabelazaanalizo$LETO))
samopike <- ggplot(tabelazaanalizo, aes(x=LETO, y=SLOVENIJA)) + geom_point()
print(samopike)
model <- lm(SLOVENIJA ~ LETO, data=tabelazaanalizo)
napoved <- ggplot(tabelazaanalizo, aes(x=LETO, y=SLOVENIJA)) + geom_point() +
          geom_smooth(method=lm, formula=y~x) +
          ylab("Dobavljena voda na prebivalca v m^3")
print(napoved)

prihodnjaleta <- data.frame(LETO=c(2020:2025))
predict(model, prihodnjaleta)
napoved1 <- prihodnjaleta %>% mutate(SLOVENIJA=predict(model, prihodnjaleta))
napoved1$SLOVENIJA <- round(napoved1$SLOVENIJA, digit=1)
napoved1 <- rbind(tabelazaanalizo, napoved1)
izrisnapovedi <- ggplot(napoved1, aes(x=LETO, y=SLOVENIJA)) +
  geom_point(shape=1) +
  geom_smooth(method=lm, formula=y~x) 
#  geom_point(data=napoved1, aes(x=LETO, y=dobavljenavoda), color="red", size=2)
print(izrisnapovedi)
