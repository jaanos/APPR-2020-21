library(ggplot2)
library(dplyr)
library(tidyverse)


# 1. graf: histogram vrste dohodka 2008 in 2019
vrste_dohodka_08_19 <- vrste_dohodka %>%
  filter(Leto == 2008 | Leto == 2019) %>%
  arrange(desc(Leto)) %>%
  select(Vrsta.dohodka, Leto, Dohodek)

graf_vrste_dohodka_08_19 <- ggplot(vrste_dohodka_08_19, aes(x=Vrsta.dohodka, 
                                                            y=Dohodek, fill=factor(Leto))) + 
  geom_histogram(stat = "identity", binwidth=1, position="identity", color="black") +
  labs(title="Dohodek glede na vrsto", x="Vrsta dohodka", y = "Dohodek", fill="Leto") +
  scale_x_discrete(guide=guide_axis(n.dodge=2))
print(graf_vrste_dohodka_08_19)


# 2. graf: razlika po spolu
spol <- starost_spol %>%
  filter(Starost == "Skupaj") %>%
  filter(Spol == "Moški" | Spol == "Ženske") %>%
  select(Starost, Spol, Leto, Dohodek)


graf_spol <- ggplot(spol, aes(x=factor(Leto), y=Dohodek, group=Spol)) +
  geom_line(aes(color=Spol)) +
  geom_point(aes(color=Spol)) +
  labs(title="Razlika med spoloma", x="Leto", y = "Dohodek", fill="Leto")
print(graf_spol)

# 3. graf: razlika po spolu 2
razlika_spol <- spol %>%
  arrange(desc(Leto)) # zelim razliko med dohodkoma in narediti histogram
