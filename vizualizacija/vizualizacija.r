# 3. faza: Vizualizacija podatkov

library(ggplot2)
library(tmap)

povprecje.dijakovindiplomantov.po.regijah <- tabela1nova %>% group_by(regija,kategorija) %>% summarise(povprecje=sum(stevilo)/10)
seznam <- split(povprecje.dijakovindiplomantov.po.regijah, povprecje.dijakovindiplomantov.po.regijah$kategorija)
povprecje.dijakov.po.regijah <- seznam[[1]]
povprecje.diplomantov.po.regijah <- seznam[[2]]
povprecje.dijakov.po.regijah <- povprecje.dijakov.po.regijah[-12,] %>% select(-"kategorija")
povprecje.diplomantov.po.regijah <- povprecje.diplomantov.po.regijah[-12,] %>% select(-"kategorija")

# GRAF POVPRECJA DIJAKOV PO REGIJAH

graf1 <- ggplot(povprecje.dijakov.po.regijah, aes(regija, povprecje, group = 1)) + geom_col() + coord_flip() +
  labs(x = "Regija", y = "Povprečje", 
       title = "Povprečno število dijakov po regijah v zadnjih desetih letih")

# ZEMLJEVID POVPRECJA DIPLOMANTOV PO REGIJAH

zemljevid <- uvozi.zemljevid("https://biogeo.ucdavis.edu/data/gadm3.6/shp/gadm36_SVN_shp.zip", "gadm36_SVN_1")
zemljevid1 <- tm_shape(merge(zemljevid, povprecje.diplomantov.po.regijah, by.x="NAME_1", by.y="regija" )) + 
  tm_polygons("povprecje",title="Povprečje",palette="Purples") + tm_style("grey") + tm_layout(title="Povprečno število diplomantov po regijah v zadnjih desetih letih")

# GRAF POVPRECJA DIJAKOV GLEDE NA VRSTO IZOBRAZEVANJA IN SPLOL

podatki5 <- tabela2nova %>% group_by(izobrazevanje, spol) %>% summarise(povprecje=sum(stevilo)/10)
graf5 <- ggplot(podatki5, aes(x=izobrazevanje, y=povprecje, fill = spol)) + geom_col(position = 'dodge')  + 
  coord_flip() + labs(x = "Vrsta izobraževanja", y = "Povprečno število", title = "Povprečno število moških in žensk na leto v \nposameznih vrstah izobraževanj v \nzadnjih desetih letih") +
  scale_fill_discrete(name = "Spol", labels = c("moški", "ženski"))

# GRAF POVPRECJA DIPLOMANTOV GLEDE NA VRSTO IZOBRAZEVANJA IN SPLOL

podatki6 <- tabela3nova %>% group_by(izobrazevanje, spol) %>% summarise(povprecje=sum(stevilo)/10)
graf6 <- ggplot(podatki6, aes(x=izobrazevanje, y=povprecje, fill = spol)) + geom_col(position = 'dodge')  + 
  coord_flip() + labs(x = "Vrsta izobraževanja", y = "Povprečno število", title = "Povprečno število diplomantov na leto  \nposameznih vrstah izobraževanj v \nzadnjih desetih letih") +
  scale_fill_discrete(name = "Spol", labels = c("moški", "ženski"))

# TORTNI DIAGRAM DIPLOMANTOV GLEDE NA NACIN STUDIJA

f <- tabela4nova %>% group_by(studij) %>% summarise(vsota=sum(stevilo))
tortnidiagram <- ggplot(f) + aes(x="", y = vsota,fill=studij) + geom_col(width=1) + coord_polar(theta="y") + xlab("") + ylab("") + scale_fill_discrete(name = "Študij", labels = c("izredni", "rednii"))
tortni <-  ggplot(tabela4nova %>% group_by(studij) %>% summarise(delez=100 * n() / nrow(tabela4nova))) +
  aes(x="", y=delez, fill=studij) + geom_col(width=1) +
  coord_polar(theta="y") + xlab("") + ylab("") +
  scale_y_continuous(breaks=seq(0, 100, 10),
                     labels=paste(seq(0, 100, 10), "%"))
tortni2 <- ggplot(tabela4nova %>% group_by(studij) %>% summarise(delez=100 * n() / nrow(tabela4nova)),
                  aes(x="", y=delez, fill = studij)) +
  geom_bar(width = 1, stat="identity", colour="black") + coord_polar("y", start=0)+
  xlab("") + ylab("") + theme(axis.text.x=element_blank(), panel.grid=element_blank())

# GRAF SPREMINJANJA STEVILA TUJCEV IN LJUDI, KI IMAJO NEZNANO BIVALISCE, MED DIJAKI IN DIPLOMANTI PO LETIH

#tujci <- tabela1nova %>% filter(regija == "Stalno bivališče neznano ali v tujini")
#graf7 <- tujci %>% ggplot(aes(x=leto, y=stevilo, col=kategorija)) + geom_line()

# GRAF POVPREČNEGA DELEŽA DIPLOMANTOV V ODSTOTKIH GLEDE NA ŠTEVILO PREBIVALCEV V POSAMEZNI REGIJI V DESETIH LETIH

povprecjedipl <- skupnatabela2 %>% group_by(regija) %>% summarise(povprecje=sum(delez)/10)
graf8 <- povprecjedipl %>% ggplot(aes(x=povprecje, y=regija, fill = regija)) + geom_col()

zemljevida <- uvozi.zemljevid("https://biogeo.ucdavis.edu/data/gadm3.6/shp/gadm36_SVN_shp.zip", "gadm36_SVN_1")
zemljevid2 <- tm_shape(merge(zemljevida, povprecjedipl, by.x="NAME_1", by.y="regija" )) + 
  tm_polygons("povprecje",title="Delež",palette="Purples")+ tm_style("grey") + tm_layout(title="Povprečni delež diplomantov po regijah v zadnjih desetih letih glede na število prebivalcev v posamezni regiji")

#graf2 <- ggplot(povprecje.diplomantov.po.regijah, aes(regija, povprecje, group = 1)) + geom_col() + coord_flip() +
#  labs(x = "Regija", y = "Povprecje", 
#       title = "Povprečno število diplomantov po regijah v zadnjih desetih letih")
#m <- tabela2nova %>% group_by(izobrazevanje, spol) %>% summarise(povprecje=sum(stevilo)/10) %>%filter(spol == "moski") %>% select(-"spol")
#z <- tabela2nova %>% group_by(izobrazevanje, spol) %>% summarise(povprecje=sum(stevilo)/10) %>%filter(spol == "zenski")%>% select(-"spol")
#graf3 <- ggplot(m, aes(izobrazevanje, povprecje, group = 1)) + geom_col() + coord_flip() + labs(x = "Vrsta izobraževanja", y = "Število moških", 
#                                                                                                title = "Povprečno število moških v \nposameznih vrstah izobraževanj v \nzadnjih desetih letih")
#graf4 <- ggplot(z, aes(izobrazevanje, povprecje, group = 1)) + geom_col() + coord_flip()+ labs(x = "Vrsta izobraževanja", y = "Število žensk", 
#                                                                                               title = "Povprečno število žensk v \nposameznih vrstah izobraževanj v \nzadnjih desetih letih")
#v graf5 združimo graf3 in graf4