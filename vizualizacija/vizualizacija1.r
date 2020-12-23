library(ggplot2)
library(dplyr)
library(tidyverse)
library(tmap)
library(rgdal)
library(rgeos)
library(maptools)


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
  arrange(desc(Leto)) %>%
  pivot_wider(names_from = Spol, values_from = Dohodek) %>%
  mutate(Razlika = Moški - Ženske)

graf_razlika_spol <- ggplot(razlika_spol, aes(x=factor(Leto), y=Razlika, group=1)) +
  geom_line() +
  geom_point() +
  labs(title="Razlika med spoloma 2", x="Leto", y = "Razlika dohodka")
print(graf_spol)
print(graf_razlika_spol)

# Zemljevid statističnih regij
source("https://raw.githubusercontent.com/jaanos/APPR-2020-21/master/lib/uvozi.zemljevid.r")

zemljevid_regije <- uvozi.zemljevid("https://biogeo.ucdavis.edu/data/gadm3.6/shp/gadm36_SVN_shp.zip",
                                    "gadm36_SVN_1", encoding = "Utf-8")

zemljevid_regije$NAME_1 <- as.factor(iconv(as.character(zemljevid_regije$NAME_1),
                                           "Utf-8"))
print(tm_shape(zemljevid_regije) + tm_polygons("NAME_1"))

#Spodnjeposavska = posavska
#Notranjsko-kraška = primorsko-notranjska

levels(as.factor(zemljevid_regije$NAME_1)) %>%
  mutate(NAME_1 = gsub("Spodnjeposavska", "Posavska", NAME_1)) %>%
  mutate(NAME_1 = gsub("Notranjsko-kraška", "Primorsko-notranjska", NAME_1))

