# 4. faza: Analiza podatkov

# Podatke katere bomo modelirali

podatki <- GDP_by_state %>% filter(State=="Alaska")

#Prileganje podatkom

# ggpairs(leto.norm %>% select(H_MEAN, leto, leto, GDP))

# Različni modeli predikcij gibanja 

g <- ggplot(podatki, aes(x=leto, y=GDP)) + geom_point()

lin <- lm(data=podatki, leto ~ GDP)
g1 <- g +  geom_smooth(method="lm", se = FALSE) 

kv <- lm(data=podatki, leto ~ GDP + I(GDP^2))
g2 <- ggplot(podatki, aes(x=leto, y=GDP)) + 
  geom_point() + 
  geom_smooth(method="lm", formula=y ~ x+ I(x^2) ) 


mls <- loess(data=podatki, leto ~ GDP)
g3 <- g + geom_smooth(method = "loess")              


# modelček

quadratic <- lm(data = podatki, GDP ~ I(leto) + I(leto^2))
years <- data.frame(leto=seq(2011, 2022, 1))
prediction <- mutate(years, GDP=predict(quadratic, years))

regression <- prediction %>% ggplot(aes(x=leto, y=GDP)) + 
  geom_smooth(method='lm', fullrange=TRUE, color='red', formula=y ~ poly(x,1,raw=TRUE)) +
  geom_point(size=2, color="blue") + 
  scale_x_continuous('Leto', breaks = seq(2019, 2022, 1), limits = c(2019,2022)) +
  ylab("BDP per capita") +
  labs(title = "Napoved BDP per capita.")

# primer predikcije


# slika 

