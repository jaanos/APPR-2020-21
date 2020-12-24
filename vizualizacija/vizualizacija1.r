library(ggplot2)
library(dplyr)
library(tidyverse)
library(tmap)
library(rgdal)
library(rgeos)
library(maptools)
library(stringr)
library(RColorBrewer)


# 1. graf: histogram vrste dohodka 2008 in 2019

graf_vrste_dohodka <- ggplot(vrste_dohodka, aes(x=factor(Leto), y=Dohodek,group=Vrsta.dohodka, colour=Vrsta.dohodka)) + 
  geom_line(aes(colour=Vrsta.dohodka)) +
  geom_point(aes(colour=Vrsta.dohodka)) +
  labs(title="Dohodek glede na vrsto", x="Leto", y = "Dohodek")
print(graf_vrste_dohodka)


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

#Spodnjeposavska = posavska
#Notranjsko-kraška = primorsko-notranjska

levels(zemljevid_regije$NAME_1)[levels(zemljevid_regije$NAME_1)=="Spodnjeposavska"] <- "Posavska"
levels(zemljevid_regije$NAME_1)[levels(zemljevid_regije$NAME_1)=="Notranjsko-kraška"] <- "Primorsko-notranjska"

regije_8_19 <- regije %>%
  select(Regija, Leto, Dohodek) %>%
  filter(Leto == 2019 | Leto == 2008) %>%
  pivot_wider(names_from = Leto, values_from = Dohodek) %>%
  mutate(Rast = (((regije_19$"2019" - regije_19$"2008")/regije_19$"2008")*100))


narisi_zemljevid <- tm_shape(merge(zemljevid_regije, regije_8_19, by.x="NAME_1", by.y="Regija")) +
  tm_polygons(c("2019", "Rast") , style="jenks", palette = "YlGn") +
  tm_facets(sync = TRUE, ncol = 2)

print(narisi_zemljevid)





