# V p17 SO STATISTIKE NORMIRANE NA 90 ODIGRANIH MINUT, ODSTRANILI SMO IGRALCE, KI SO V SEZONI ODGIRALI MANJ KOT 90 MINUT
p17 <- kom17 %>% filter(minutes >= 90) %>% group_by(Ime, Priimek, Pozicija, id, Ekipa, Igralec) %>%
   summarise(across(everything(),function(x) {round(x/minutes * 90, 2)})) %>% select(-minutes)



# SEDAJ PRIDOBIMO TABELE ZA NAŠE LINEARNE MODELE
# iskali bomo napadalce iz sezone 17/18, ki so bili v prejšnji sezoni klasificirani bodisi kot napadalci, bodisi kot vezisti, skupno
# so v sezoni morali odgirati vsaj 90 minut
ustrezni_napadalci <- kom18 %>% ungroup() %>% filter(Pozicija == "Napadalec", minutes >= 90) %>%
  mutate(zadetki90_letos = round(goals_scored/minutes * 90, 2)) %>% select(Ime, Priimek, Ekipa, zadetki90_letos, goals_scored, minutes) %>%
  rename(zadetki_letos = goals_scored, minute_letos = minutes)

ustrezni_napadalci_vezisti <- p17 %>% filter(Pozicija == "Napadalec" | Pozicija == "Vezist") %>% ungroup() %>%
  rename(zadetki90_lani = goals_scored, zapravljene.priloznosti90_lani = big_chances_missed) %>%
  select(Ime, Priimek, Ekipa, zadetki90_lani, zapravljene.priloznosti90_lani)

ustrezni_igralci <- ustrezni_napadalci %>% inner_join(ustrezni_napadalci_vezisti)

# za prileganje bomo uporabili tudi podatke ekip iz sezone, ki se nanašajo na to koliko malih in velikih priložnosti ustvarijo ekipe na
# 90 odigranih minut
p17_ekipa <- kom17 %>% ungroup() %>% select(-Ime, -Priimek, -Pozicija, -id, -Igralec) %>%
  group_by(Ekipa) %>% summarise(across(everything(), sum)) %>% 
  mutate(priloznosti.ekipa90_lani = round(big_chances_created/minutes * 90, 2),
         podaje.strel.ekipa90_lani = round(key_passes/minutes*90, 2)) %>%
  select(Ekipa, priloznosti.ekipa90_lani, podaje.strel.ekipa90_lani)

# tabela_za_prileganje1 <- ustrezni_igralci %>%
#   inner_join(statistika90_17 %>% ungroup() %>%
#               rename(zadetki90_lani = goals_scored, zapravljene.priloznosti90_lani = big_chances_missed) %>%
#               select(Ime, Priimek, Ekipa, zadetki90_lani, zapravljene.priloznosti90_lani)) %>%
#   left_join(statistike90_ekipa_17)

tabela_za_prileganje <- ustrezni_igralci %>% inner_join(p17_ekipa)

# lin1 za predikcijo uporabi minute v naslednji oz. tekoči sezoni, število zadetkov na 90 minut v lanski sezoni, stevilo zapravljenih
# priloznosti na 90 minut v lanski sezoni, število velikih priložnosti na 90 minut, ki jih je ekipa igralca ustvarila v lanski sezoni

lin1 <- lm(data=tabela_za_prileganje %>% ungroup(), zadetki90_letos ~ zadetki90_lani)

lin2 <- lm(data=tabela_za_prileganje %>% ungroup(), zadetki_letos ~ zadetki90_lani + minute_letos)

lin3 <- lm(data=tabela_za_prileganje %>% ungroup(), zadetki_letos ~ minute_letos + zadetki90_lani + zapravljene.priloznosti90_lani +
             priloznosti.ekipa90_lani)

lin4 <- lm(data=tabela_za_prileganje %>% ungroup(), zadetki90_letos ~ minute_letos + zadetki90_lani + zapravljene.priloznosti90_lani +
             priloznosti.ekipa90_lani)

lin5 <- lm(data=tabela_za_prileganje %>% ungroup(), zadetki90_letos ~ minute_letos + zadetki90_lani + zapravljene.priloznosti90_lani +
             podaje.strel.ekipa90_lani)


#TESTIRANJE MODELA NA PODATKIH
p18 <- kom18 %>% filter(minutes >= 90) %>% group_by(Ime, Priimek, Pozicija, id, Ekipa, Igralec) %>%
  summarise(across(everything(),function(x) {round(x/minutes * 90, 2)})) %>% select(-minutes)

p19 <- kom19 %>% filter(minutes >= 90) %>% group_by(Ime, Priimek, Pozicija, id, Ekipa, Igralec) %>%
  summarise(across(everything(),function(x) {round(x/minutes * 90, 2)})) %>% select(-minutes)

p18_ekipa <- kom18 %>% ungroup() %>% select(-Ime, -Priimek, -Pozicija, -id, -Igralec) %>%
  group_by(Ekipa) %>% summarise(across(everything(), sum)) %>% 
  mutate(priloznosti.ekipa90_lani = round(big_chances_created/minutes * 90, 2),
         podaje.strel.ekipa90_lani = round(key_passes/minutes*90, 2)) %>%
  select(Ekipa, priloznosti.ekipa90_lani, podaje.strel.ekipa90_lani)

p19_ekipa <- kom19 %>% ungroup() %>% select(-Ime, -Priimek, -Pozicija, -id, -Igralec) %>%
  group_by(Ekipa) %>% summarise(across(everything(), sum)) %>% 
  mutate(priloznosti.ekipa90_lani = round(big_chances_created/minutes * 90, 2),
         podaje.strel.ekipa90_lani = round(key_passes/minutes*90, 2)) %>%
  select(Ekipa, priloznosti.ekipa90_lani, podaje.strel.ekipa90_lani)


# TO JE TABELA S PODATKI POTREBNIMI ZA NAŠ TEST
testna_tabela <- kom19 %>% filter(Pozicija == "Napadalec", minutes >= 90) %>% ungroup() %>%
  select(Ime, Priimek, Igralec, goals_scored, minutes) %>%
  inner_join(p18 %>% ungroup() %>% select(Ime, Priimek, Igralec, Ekipa, goals_scored, big_chances_missed) %>%
               rename(zadetki90_lani = goals_scored, zapravljene.priloznosti90_lani = big_chances_missed)) %>%
  inner_join(p18_ekipa)


# TO JE TABELA Z IZRAČUNI NAŠE PREDIKCIJE
primerjava <- testna_tabela %>% mutate(lin3 = predict(lin3, data.frame(minute_letos = minutes, zadetki90_lani = zadetki90_lani,
                                                         zapravljene.priloznosti90_lani = zapravljene.priloznosti90_lani,
                                                         priloznosti.ekipa90_lani = priloznosti.ekipa90_lani)),
                                       lin4 = predict(lin4, data.frame(minute_letos = minutes, zadetki90_lani = zadetki90_lani,
                                                         zapravljene.priloznosti90_lani = zapravljene.priloznosti90_lani,
                                                         priloznosti.ekipa90_lani = priloznosti.ekipa90_lani)),
                                       lin1 = predict(lin1, data.frame(zadetki90_lani = zadetki90_lani)),
                                       lin2 = predict(lin2, data.frame(minute_letos = minutes, zadetki90_lani = zadetki90_lani)),
                                       lin5 = predict(lin5, data.frame(minute_letos = minutes, zadetki90_lani = zadetki90_lani,
                                                        zapravljene.priloznosti90_lani = zapravljene.priloznosti90_lani,
                                                        podaje.strel.ekipa90_lani = podaje.strel.ekipa90_lani))) %>%
  mutate(lin1 = round(lin1 * minutes / 90, 2), lin2 = round(lin2, 2), lin4 = round(lin4 * minutes / 90, 2), lin3 = round(lin3, 2),
         lin5 = round(lin5 * minutes / 90, 2), lin3 = round(lin3, 2)) %>%
  mutate(lin1 = sapply(lin1, function(x) {if (x <= 0) {x=0} else {x=x}}),
         lin2 = sapply(lin2, function(x) {if (x <= 0) {x=0} else {x=x}}),
         lin3 = sapply(lin3, function(x) {if (x <= 0) {x=0} else {x=x}}),
         lin4 = sapply(lin4, function(x) {if (x <= 0) {x=0} else {x=x}}),
         lin5 = sapply(lin5, function(x) {if (x <= 0) {x=0} else {x=x}})) %>%
  select(Igralec, Ekipa, goals_scored, lin1, lin2, lin3, lin4, lin5) %>%
  rename(dejansko_st_zadetkov = goals_scored, predikcija1 = lin1, predikcija2 = lin2, predikcija3 = lin3, predikcija4 = lin4, predikcija5 = lin5) %>%
  arrange(desc(dejansko_st_zadetkov))

kvadraticna_napaka = function(x, y) {sum((x - y)^2)}
# kvadraticna_napaka(primerjava$dejansko_st_zadetkov, primerjava$predikcija1)
# kvadraticna_napaka(primerjava$dejansko_st_zadetkov, primerjava$predikcija2)
# kvadraticna_napaka(primerjava$dejansko_st_zadetkov, primerjava$predikcija3)
# kvadraticna_napaka(primerjava$dejansko_st_zadetkov, primerjava$predikcija4)
# kvadraticna_napaka(primerjava$dejansko_st_zadetkov, primerjava$predikcija5)

# TABELA Z ODSTOPANJI PREDIKCIJ OD DEJANSKIH VREDNOSTI
tabela_napak <- data.frame(predikcija=c("1", "2", "3", "4", "5"),
                          napaka = (c(kvadraticna_napaka(primerjava$dejansko_st_zadetkov,primerjava[4]), 
                                      kvadraticna_napaka(primerjava$dejansko_st_zadetkov,primerjava[5]),
                                      kvadraticna_napaka(primerjava$dejansko_st_zadetkov,primerjava[6]),
                                      kvadraticna_napaka(primerjava$dejansko_st_zadetkov,primerjava[7]),
                                      kvadraticna_napaka(primerjava$dejansko_st_zadetkov,primerjava[8]))))

# GRAFI VSEH PREDIKCIJ POSAMEZNO
graf_predikcija1 <- ggplotly(ggplot(primerjava) + aes(x = dejansko_st_zadetkov, y = predikcija1) + 
                               geom_point(aes(Igralec = Igralec, Ekipa = Ekipa), colour = "blue") +
                               geom_smooth(aes(y = predikcija1), method="lm", col = "blue", se = FALSE) +
                               labs(title = "Predikcija 1 števila zadetkov") + ylab("Predikcija 1 števila zadetkov") +
                               xlab("Dejansko število zadetkov") + xlim(0, 23) + ylim(0, 23) +
                               geom_abline(intercept = 0, slope = 1, col = "grey", linetype="dashed") + 
                               theme(plot.title = element_text(color = "blue", size = 18)))

graf_predikcija2 <- ggplotly(ggplot(primerjava) + aes(x = dejansko_st_zadetkov, y = predikcija2) + 
                               geom_point(aes(Igralec = Igralec, Ekipa = Ekipa), colour = "burlywood") +
                               geom_smooth(aes(y = predikcija2), method="lm", col = "burlywood", se = FALSE) +
                               labs(title = "Predikcija 2 števila zadetkov") + ylab("Predikcija 2 števila zadetkov") +
                               xlab("Dejansko število zadetkov") + xlim(0, 23) + ylim(0, 23) +
                               geom_abline(intercept = 0, slope = 1, col = "grey", linetype="dashed") + 
                               theme(plot.title = element_text(color = "burlywood", size = 18)))


graf_predikcija3 <- ggplotly(ggplot(primerjava) + aes(x = dejansko_st_zadetkov, y = predikcija3) + 
                               geom_point(aes(Igralec = Igralec, Ekipa = Ekipa), colour = "darkorange3") +
                               geom_smooth(aes(y = predikcija3), method="lm", col = "darkorange3", se = FALSE) +
                               labs(title = "Predikcija 3 števila zadetkov") + ylab("Predikcija 3 števila zadetkov") +
                               xlab("Dejansko število zadetkov") + xlim(0, 23) + ylim(0, 23) +
                               geom_abline(intercept = 0, slope = 1, col = "grey", linetype="dashed") + 
                               theme(plot.title = element_text(color = "darkorange3", size = 18)))

graf_predikcija4 <- ggplotly(ggplot(primerjava) + aes(x = dejansko_st_zadetkov, y = predikcija4) + 
                               geom_point(aes(Igralec = Igralec, Ekipa = Ekipa), colour = "darkorchid2") +
                               geom_smooth(aes(y = predikcija4), method="lm", col = "darkorchid2", se = FALSE) +
                               labs(title = "Predikcija 4 števila zadetkov") + ylab("Predikcija 4 števila zadetkov") +
                               xlab("Dejansko število zadetkov") + xlim(0, 23) + ylim(0, 23) +
                               geom_abline(intercept = 0, slope = 1, col = "grey", linetype="dashed") + 
                               theme(plot.title = element_text(color = "darkorchid2", size = 18)))



graf_predikcija5 <- ggplotly(ggplot(primerjava) + aes(x = dejansko_st_zadetkov, y = predikcija5) + 
                               geom_point(aes(Igralec = Igralec, Ekipa = Ekipa), colour = "aquamarine3") +
                               geom_smooth(aes(y = predikcija5), method="lm", col = "aquamarine3", se = FALSE) +
                               labs(title = "Predikcija 5 števila zadetkov") + ylab("Predikcija 5 števila zadetkov") +
                               xlab("Dejansko število zadetkov") + xlim(0, 23) + ylim(0, 23) +
                               geom_abline(intercept = 0, slope = 1, col = "grey", linetype="dashed") + 
                               theme(plot.title = element_text(color = "aquamarine3", size = 18)))


# PRIMERJAVA GRAFOV PREDIKCIJ MED SABO, NOVA TABELA primerjava2, ker legendo lažje naredimo v tidydata
primerjava2 <- primerjava %>% rename("Predikcija 1" = predikcija1, "Predikcija 2" = predikcija2, "Predikcija 3" = predikcija3,
                                     "Predikcija 4" = predikcija4, "Predikcija 5" = predikcija5) %>%
  pivot_longer(c(-Igralec,-Ekipa, -dejansko_st_zadetkov), names_to = "podatek", values_to ="stevilo")

graf_vseh_zglajenih_predikcij1 <- ggplotly(ggplot(primerjava2) + aes(x=dejansko_st_zadetkov, color = podatek, y = stevilo) +
                                            geom_smooth(method = "lm", se=FALSE) + 
                                            # geom_smooth(aes(y = predikcija1), method="lm", col = "blue", se = FALSE) +
                                            # geom_smooth(aes(y = predikcija2), method="lm", col = "burlywood", se = FALSE) +
                                            # geom_smooth(aes(y = predikcija3), method="lm", col = "darkorange3", se = FALSE) +
                                            # geom_smooth(aes(y = predikcija4), method="lm", col = "darkorchid2", se = FALSE) +
                                            # geom_smooth(aes(y = predikcija5), method="lm", col = "aquamarine3", se = FALSE) +
                                            labs(title = "Predvideno število zadetkov") + ylab("Predikcija števila zadetkov") +
                                            xlab("Dejansko število zadetkov") + xlim(0, 23) + ylim(0, 23) +
                                            geom_abline(intercept = 0, slope = 1, col = "grey", linetype="dashed") + 
                                            theme(plot.title = element_text(color = "darkmagenta", size = 18)))

# OGLED VSEH GRAFOV
# print(graf_predikcija1)
# print(graf_predikcija2)
# print(graf_predikcija3)
# print(graf_predikcija4)
# print(graf_predikcija5)
# print(graf_vseh_zglajenih_predikcij1)

