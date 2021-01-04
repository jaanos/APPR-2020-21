# 4. faza: Analiza podatkov
#Krizi(2008,2020)
meseci <- c("12", "11","10","09","08","07","06","05","04","03","02","01",
            "12", "11","10","09","08","07","06","05","04","03","02","01",
            "12", "11","10","09","08","07","06","05","04","03","02","01")
leto <- c("2009","2009","2009","2009","2009","2009","2009","2009","2009","2009","2009","2009",
          "2008","2008","2008","2008","2008","2008","2008","2008","2008","2008","2008","2008",
          "2007","2007","2007","2007","2007","2007","2007","2007","2007","2007","2007","2007")
kriza2008_meseci <- data.frame(leto, meseci, kriza2008$placa)
kriza_leto2009 <- kriza2008_meseci %>%
  filter(leto == "2009")
kriza_leto2008 <- kriza2008_meseci %>%
  filter(leto == "2008")
kriza_leto2007 <- kriza2008_meseci %>%
  filter(leto == "2007")

#Kriza leta 2008
ggplot(kriza_leto2007, mapping=aes(x=meseci, y=kriza2008.placa)) +
  geom_point(color="red", size=2) +
  geom_point(kriza_leto2008,mapping=aes(x=meseci, y=kriza2008.placa), color="blue", size=2) +
  geom_point(kriza_leto2009,mapping=aes(x=meseci, y=kriza2008.placa), color="yellow", size=2) +
  labs(title="Primerjava plače v letih 2007, 2008, 2009") +
  ylab("Višina plače(€)") +
  xlab("Meseci")

#Primerjava leta 2007 z letom 2019
meseci <- c("09","08","07","06","05","04","03","02","01",
            "12", "11","10","09","08","07","06","05","04","03","02","01")
leto <- c("2020","2020","2020","2020","2020","2020","2020","2020","2020",
          "2019","2019","2019","2019","2019","2019","2019","2019","2019","2019","2019","2019")
kriza2020_meseci <- data.frame(leto, meseci, kriza2020$placa)
kriza_leto2019 <- kriza2020_meseci %>%
  filter(leto == "2019")

ggplot(kriza_leto2007, mapping=aes(x=meseci, y=kriza2008.placa)) +
  geom_point(color="red", size=2) +
  geom_point(kriza_leto2019, mapping=aes(x=meseci, y=kriza2020.placa), color="blue", size=2) +
  labs(title="Primerjava plače v letih pred krizo(2007,2019)") +
  ylab("Višina plače(€)") +
  xlab("Meseci")

#Primerjava leta 2008 in 2020(do meseca 09)
kriza_leto2020 <- kriza2020_meseci %>%
  filter(leto == "2020")

ggplot(kriza_leto2008, mapping=aes(x=meseci, y=kriza2008.placa)) +
  geom_point(color="red", size=2) +
  geom_point(kriza_leto2020, mapping=aes(x=meseci, y=kriza2020.placa), color="blue", size=2) +
  labs(title="Primerjava plače v letih 2008,2020") +
  ylab("Višina plače(€)") +
  xlab("Meseci")
