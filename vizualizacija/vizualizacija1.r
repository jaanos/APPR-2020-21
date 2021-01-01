


# 1. graf: vrste dohodka 2019

graf_vrste_dohodka <- ggplot(vrste_dohodka, aes(x=factor(Leto), y=Dohodek,group=Vrsta.dohodka, colour=Vrsta.dohodka)) + 
  geom_line(aes(colour=Vrsta.dohodka)) +
  geom_point(aes(colour=Vrsta.dohodka)) +
  labs(title="Dohodek glede na vrsto", x="Leto", y = "Dohodek")
print(graf_vrste_dohodka)


# 2. graf: razlika po spolu, (starost = skupaj)

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

print(graf_razlika_spol)



# 4. graf: izobrazba
izobrazba_spol <- izobrazba_spol %>%
  mutate(Izobrazba2=Izobrazba, Spol2=Spol)


graf_izobrazba_spol <- ggplot(izobrazba_spol, aes(x=factor(Leto), y=Dohodek)) +
  geom_line(data=izobrazba_spol %>% dplyr::select(-Izobrazba, -Spol),
            aes(group=interaction(Spol2, Izobrazba2)), color="grey", size=0.5) +
  geom_line(aes(group=interaction(Spol, Izobrazba), color=Spol), size=1.2)+
  scale_color_manual(breaks=c("Moški", "Ženske"), values=c("#00BFC4", "#F8766D"))+
  theme_bw() +
  theme(plot.title = element_text(), panel.grid = element_blank()) +
  labs(title="Izobrazba in spol", x="Leto", y = "Dohodek")+
  facet_wrap(~Izobrazba)

print(graf_izobrazba_spol)


# Zemljevid statističnih regij

zemljevid_regije <- uvozi.zemljevid("https://biogeo.ucdavis.edu/data/gadm3.6/shp/gadm36_SVN_shp.zip",
                                    "gadm36_SVN_1", encoding = "UTF-8")

zemljevid_regije$NAME_1 <- as.factor(iconv(as.character(zemljevid_regije$NAME_1),
                                           "UTF-8"))

#Spodnjeposavska = Posavska
#Notranjsko-kraška = Primorsko-notranjska

levels(zemljevid_regije$NAME_1)[levels(zemljevid_regije$NAME_1)=="Spodnjeposavska"] <- "Posavska"
levels(zemljevid_regije$NAME_1)[levels(zemljevid_regije$NAME_1)=="Notranjsko-kraška"] <- "Primorsko-notranjska"

regije_8_19 <- regije %>%
  select(Regija, Leto, Dohodek) %>%
  filter(Leto %in% c(2008, 2019)) %>%
  pivot_wider(names_from = Leto, values_from = Dohodek) %>%
  mutate(Rast = (((`2019` - `2008`) / `2008`) * 100))


narisi_zemljevid <- tm_shape(merge(zemljevid_regije, regije_8_19, by.x="NAME_1", by.y="Regija")) +
  tm_polygons(c("2008", "2019", "Rast") , style="jenks", palette = "YlGn") +
  tm_facets(sync = TRUE, ncol = 2)

print(narisi_zemljevid)








