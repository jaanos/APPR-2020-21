# 4. faza: Analiza podatkov

#napoved št. priseljencev v Slovenijo

skupno_leta <- tabela1 %>% group_by(Leto) %>% summarise(Stevilo_priseljenih_iz_tujine=sum(Priseljeni_iz_tujine))
skupno_leta$Vrsta <- "Meritev"
model <- lm(Stevilo_priseljenih_iz_tujine~Leto, data=skupno_leta)
pr <- predict(model, data.frame(Leto=seq.int(2020, 2025, 1)))
napoved_preseljevanja <- data.frame(Leto = c(2020, 2021, 2022, 2023, 2024, 2025), Stevilo_priseljenih_iz_tujine = c(pr[1], pr[2], pr[3], pr[4], pr[5], pr[6]))
napoved_preseljevanja$Vrsta <- "Napoved"
skupna_tabela <- rbind(skupno_leta, napoved_preseljevanja)

graf_napoved <- ggplot(skupna_tabela, aes(x=Leto, y=Stevilo_priseljenih_iz_tujine, shape=Vrsta)) +
  geom_smooth(method=lm, se=TRUE, fullrange = TRUE) +
  geom_point(data=skupno_leta, aes(x=Leto, y=Stevilo_priseljenih_iz_tujine), color="yellow", size=1) +
  labs(title="Napoved števila priseljenih v Slovenijo", y="Število priseljenih", x="Leto") + geom_point(color="yellow")
  
#napoved št. izseljencev iz Slovenije

skupno_leta_iz <- tabela2 %>% group_by(Leto) %>% summarise(Stevilo_odseljenih_v_tujino=sum(Odseljeni_v_tujino))
skupno_leta_iz$Vrsta <- "Meritev"
model2 <- lm(Stevilo_odseljenih_v_tujino~Leto, data=skupno_leta_iz)
pr2 <- predict(model2, data.frame(Leto=seq.int(2020, 2025, 1)))
napoved_izseljevanja <- data.frame(Leto = c(2020, 2021, 2022, 2023, 2024, 2025), Stevilo_odseljenih_v_tujino = c(pr2[1], pr2[2], pr2[3], pr2[4], pr2[5], pr2[6]))
napoved_izseljevanja$Vrsta <- "Napoved"
skupna_tabela2 <- rbind(skupno_leta_iz, napoved_izseljevanja)

graf_napoved2 <- ggplot(skupna_tabela2, aes(x=Leto, y=Stevilo_odseljenih_v_tujino, shape=Vrsta)) +
  geom_smooth(method=lm, se=TRUE, fullrange = TRUE) +
  geom_point(data=skupno_leta_iz, aes(x=Leto, y=Stevilo_odseljenih_v_tujino), color="yellow", size=1) +
  labs(title="Napoved števila odseljenih iz Slovenije", y="Število odseljenih", x="Leto") + geom_point(color="yellow")


#Razvrščanje v skupine na zemljevidu - Evropa
#združevanje tabel
tabela11skupno <- tabela11 %>% filter(between(Leto,2016,2019)) %>% group_by(Leto,Drzava) %>% summarise("Stevilo_priseljenih"=sum(Stevilo))

tabela12skupno <- tabela12 %>% filter(between(Leto,2016,2019)) %>% group_by(Leto,Drzava) %>% summarise("Stevilo_izseljenih"=sum(Stevilo))
t1 <- inner_join(tabela11skupno, tabela12skupno)
t2 <- inner_join(t1, tabela15)
evropa_master <- inner_join(t2, tabela10)

povprecjeevropa <- evropa_master %>% group_by(Drzava) %>% 
  summarise(emigracija = mean(Stevilo_izseljenih, na.rm = TRUE),
            imigracija = mean(Stevilo_priseljenih, na.rm = TRUE), 
            populacija = mean(Prebivalstvo, na.rm = TRUE),
            GDP = mean(GDP_per_capita_dolarji, na.rm = TRUE)) %>%
  mutate(emigracija = emigracija / populacija, imigracija = imigracija / populacija) 


#clustering emigracija (izseljeni)
skupineEmigracija <- kmeans(povprecjeevropa$emigracija, 6, nstart = 1500)
centersEmi <- sort(skupineEmigracija$centers)
skupineEmigracija <- kmeans(povprecjeevropa$emigracija, centers = centersEmi, nstart = 1500)

#zemljevid glede na skupine - izseljevanje
zemljevidskupineemi <- tm_shape(merge(zemljevid, data.frame(Drzava = povprecjeevropa$Drzava, 
                                                skupina = factor(skupineEmigracija$cluster)), 
                               by.x = "SOVEREIGNT", by.y = "Drzava"), xlim=c(-20,32), ylim=c(32,80)) + 
  tmap_options(max.categories = 6) + 
  tm_polygons("skupina", title = "Skupina") + 
  tm_layout(main.title = "Države razdeljene glede na povprečno emigracijo", main.title.size = 1, legend.title.size = 1) +
  tm_legend(position = c("left", "bottom"))

#clustering imigracija (priseljeni)
skupineImigracija <- kmeans(povprecjeevropa$imigracija, 6, nstart = 1500)
centersImi <- sort(skupineImigracija$centers)
skupineImigracija <- kmeans(povprecjeevropa$imigracija, centers = centersImi, nstart = 1500) 

#clustering imigracija (priseljeni)
zemljevidskupineimi <- tm_shape(merge(zemljevid, data.frame(Drzava = povprecjeevropa$Drzava, 
                                                            skupina = factor(skupineImigracija$cluster)), 
                                      by.x = "SOVEREIGNT", by.y = "Drzava"), xlim=c(-20,32), ylim=c(32,80)) + 
  tmap_options(max.categories = 6) + 
  tm_polygons("skupina", title = "Skupina") + 
  tm_layout(main.title = "Države razdeljene glede na povprečno imigracijo", main.title.size = 1, legend.title.size = 1) +
  tm_legend(position = c("left", "bottom"))


#Še eno razvrščanje: REGIJE_____________________
#združimo tabele:
tab14 <- tabela14 %>% group_by(Leto,Regija) %>% summarise("Stevilo_priseljenih"=sum(Stevilo_priseljenih_iz_tujine))
tab13 <- tabela13 %>% group_by(Leto,Regija) %>% summarise("Stevilo_odseljenih"=sum(Stevilo_odseljenih_v_tujino))
ta1 <- inner_join(tab13,tab14)
ta2 <- inner_join(tabela16,tabela17)
ta3 <- inner_join(ta2,vsezares)
master_regije <- inner_join(ta3,ta1)

povprecjeregije <- master_regije %>% group_by(Regija) %>% 
  summarise(emigracija = mean(Stevilo_odseljenih, na.rm = TRUE),
            imigracija = mean(Stevilo_priseljenih, na.rm = TRUE), 
            populacija = mean(Prebivalstvo, na.rm = TRUE),
            BDP = mean(BDP_na_prebivalca, na.rm = TRUE),
            SS = mean(razSS, na.rm = TRUE),
            VS = mean(razVS, na.rm = TRUE)) %>%
  mutate(emigracija = emigracija / populacija, imigracija = imigracija / populacija) 

#clustering emigracija -regije
skupineEmigracijaReg <- kmeans(povprecjeregije$emigracija, 4, nstart = 1500)
centersEmiReg <- sort(skupineEmigracijaReg$centers)
skupineEmigracijaReg <- kmeans(povprecjeregije$emigracija, centers = centersEmiReg, nstart = 1500)

zemljevid_reg_skup_emi <- tm_shape(merge(Slovenija, data.frame(Regija = povprecjeregije$Regija, 
                                                            skupina = factor(skupineEmigracijaReg$cluster)), 
                                      by.x = "NAME_1", by.y = "Regija")) + 
  tmap_options(max.categories = 4) + 
  tm_polygons("skupina", title = "Skupina") + 
  tm_layout(main.title="Skupine glede na povprečno število izseljenih ljudi v regijah", legend.position = c(0.75,0.1)) + 
  tm_text(text='NAME_1', size=0.6)

#clustering imigracija - regije
skupineImigracijaReg <- kmeans(povprecjeregije$imigracija, 4, nstart = 1500)
centersImiReg <- sort(skupineImigracijaReg$centers)
skupineImigracijaReg <- kmeans(povprecjeregije$imigracija, centers = centersImiReg, nstart = 1500)

zemljevid_reg_skup_imi <- tm_shape(merge(Slovenija, data.frame(Regija = povprecjeregije$Regija, 
                                                               skupina = factor(skupineImigracijaReg$cluster)), 
                                         by.x = "NAME_1", by.y = "Regija")) + 
  tmap_options(max.categories = 4) + 
  tm_polygons("skupina", title = "Skupina") + 
  tm_layout(main.title="Skupine glede na povprečno število priseljenih ljudi v regijah", legend.position = c(0.75,0.1)) + 
  tm_text(text='NAME_1', size=0.6)






