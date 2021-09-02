# 3. faza: Vizualizacija podatkov
library(dplyr)
library(ggrepel)
library(tmap)


# Uvozimo zemljevid.
zemljevid <- uvozi.zemljevid(
  "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip", "ne_50m_admin_0_countries", encoding="UTF-8")
zemljevid <- zemljevid[zemljevid$CONTINENT == "Europe",]

dv <- c("Albania", "Andorra", "Armenia", "Australia", "Azerbaijan", "Belarus",
        "Bosnia & Herzegovina", "Bulgaria", "Croatia", "Czech Republic", "Estonia",
        "Georgia", "Hungary", "Latvia", "Lithuania", "Moldova", "Montenegro",
        "Morocco", "North Macedonia", "Poland", "Romania", "Russia", "San Marino",
        "Serbia", "Serbia & Montenegro", "Slovakia", "Slovenia", "Ukraine", "Yugoslavia")



dv2 <- unlist(lapply(dv, function(x) {which(grepl(x, tabela3$Drzava))}))
tabela3[-dv2,]


#_____________________________1_________________________________________________

tabela1$TOCKE <- as.numeric(tabela1$TOCKE)
tabela1$TOCKE[is.na(tabela1$TOCKE)] <- 0

graf1 <- ggplot(data=tabela1, aes(x=LETO, y=TOCKE, group = 1)) + 
  geom_point(data = tabela1[!is.na(tabela1$TOCKE),],col = "darkred") +
  geom_line(data = tabela1[!is.na(tabela1$TOCKE),],col = "darkred") +
  ggtitle('Točke Jugoslavije skozi leta') +
  labs(x = "Leto",
       y = "Točke",
       colour = "Legenda") +
  scale_color_manual(values = c("Tocke", "Uvrstitev"))


graf2 <- ggplot(data=tabela2, aes(x=LETO, y=as.numeric(TOCKE), group = 1)) + 
  geom_point(col = "darkred") +
  geom_line(col = "darkred") +
  ylab('Točke') + 
  xlab('Leto') + 
  ggtitle('Točke Slovenije skozi leta')

graf3 <- ggplot(data=rbind(tabela1, tabela2), aes(x=LETO, y=as.numeric(TOCKE), group = 1)) +
  geom_point(col = "dodgerblue") +
  geom_line(col = "dodgerblue") +
  geom_vline(xintercept="1992", linetype = 5, col = "indianred") +
  labs(x = "Leto", y= "Tocke", title = "Spreminjanje tock skozi leta", col = "Legenda")

#______________________________________2________________________________________

# katera država je dobila najvecč točk za vsako leto
# ju
pod3 <- tabela3 %>% group_by(Leto) %>% filter(Tocke == max(Tocke))
pod5 <- as.data.frame(sort(table(pod3$Drzava), decreasing=TRUE))

pod3 <- within(pod3, 
                   Drzava <- factor(Drzava, 
                                      levels=names(sort(table(Drzava), 
                                                        decreasing=TRUE))))


graf4 <- ggplot(pod3,aes(x=Drzava)) +
  geom_bar(binwidth=1) +
  xlab("Država") +
  ylab("Število dogodkov največ točk") +
  ggtitle("Pogostost dajanja največ točk")

# sl
pod4 <- tabela4 %>% group_by(Leto) %>% filter(Tocke == max(Tocke))
pod4 <- within(pod4, 
               Drzava <- factor(Drzava, 
                                levels=names(sort(table(Drzava), 
                                                  decreasing=TRUE))))
pod6 <- as.data.frame(sort(table(pod4$Drzava), decreasing=TRUE))
graf5 <- ggplot(pod4,aes(x=Drzava)) +
  geom_bar() +
  xlab("Država") +
  ylab("Število dogodkov največ točk") +
  ggtitle("Pogostost dajanja največ točk")


#___________________________________3___________________________________________
pod7 <- tabela5 %>% mutate(Stevilo_nastopov =
                             cut(Stevilo_nastopov, breaks = c(0,1,10,20,30,40,50,60,70),
                                          right = T, labels = F))

r <- 0
for (i in 1:8){
  r[i] <- sum(pod7$Stevilo_nastopov == i)
}

pod8 <- as.data.frame(cbind("Skupina" = c(1:8),"Stevilo" = r))
filter(pod7, Stevilo_nastopov == 5)$Drzava

pod8 <- pod8 %>% 
  arrange(desc(Skupina)) %>%
  mutate(prop = Stevilo / sum(pod8$Skupina) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )


graf6 <- ggplot(pod8, aes(x = factor(1), y = prop, fill = as.factor(Skupina))) +
  theme_void() +
  geom_bar(width=1,stat="identity", color="white") +
  ggtitle("Delež nastopov po državah") +
  theme(plot.title = element_text(hjust = 0.5), legend.key.size = unit(1.5, 'cm'),
        legend.text = element_text(size=12), legend.title = element_text(size=14)) +
  coord_polar("y", start=0) +
  scale_fill_discrete(name = "Skupina",
                      labels = c("Maroko",
                                 "Andora, Avstralija, Češka, Srbija in Črna Gora, Slovaška",
                                 "Albanija, Armenija, Azerbajdžan, Belorusija, BiH, Bolgarija, Gruzija, Madžarska,\nMoldavija, Črna Gora, Severna Makedonija, San Marino, Srbija, Ukrajina",
                                 "Hrvaška, Estonija, Latvija, Litva, Monako, Poljska, Romunija, Rusija, Slovenija,\nJugoslavija",
                                 "Ciper, Islandija, Luksemburg, Malta, Turčija",
                                 "Danska, Grčija, Izrael, Italija",
                                 "Avstrija, Finska, Irska, Norveška, Portugalska, Španija, Švedska",
                                 "Belgija, Francija,Nemčija, Švica, Nizozemska, Združeno kraljestvo")) +
  geom_text(aes(y = ypos, label = rev(c("1", "2-10", "11-20", "21-30", "31-40",
                                    "41-50", "51-60", "61-65"))), color = "white", size=5)



graf6



#_____________________________________4_________________________________________
tabela5$Drzava <- gsub("\n", "", tabela5$Drzava)
t <- c("Australia", "Serbia & Montenegro", "Yugoslavia")
t2 <- unlist(lapply(t, function(x) {match(x, tabela5$Drzava)}))
tabela6 <- tabela5[-t2,]

# spremenit države
tabela6$Drzava[tabela6$Drzava == "The Netherlands"] <- "Netherlands"
tabela6$Drzava[tabela6$Drzava == "Czech Republic"] <- "Czechia"
tabela6$Drzava[tabela6$Drzava == "Bosnia & Herzegovina"] <- "Bosnia and Herzegovina"
tabela6$Drzava[tabela6$Drzava == "Serbia"] <- "Republic of Serbia"
tabela6$Drzava[tabela6$Drzava == "North Macedonia"] <- "Macedonia"



zemljevid1 <- tm_shape(merge(zemljevid,
                             tabela6,duplicateGeoms = TRUE,
                             by.x="SOVEREIGNT", by.y="Drzava"),
                       xlim=c(-25,40), ylim=c(32,72)) +
  tm_polygons("Stevilo_nastopov", title = "Število nastopov",
              breaks=c(0,10,20,30,40,50,60,65)) + 
  tm_layout(bg.color = "skyblue") + 
  tm_layout(main.title = "Število nastopov posameznih držav", main.title.size = 1, legend.title.size = 1) 

#____________________________________5__________________________________________

# zemljevid2 <- tm_shape(merge(zemljevid,
#                              pod5,duplicateGeoms = TRUE,
#                              by.x="SOVEREIGNT", by.y="Var1"),
#                        xlim=c(-25,40), ylim=c(32,72)) +
#   tm_polygons("Freq", title = "Število nastopov")+
#               #breaks=c(0,10,20,30,40,50,60,65)) + 
#   tm_layout(bg.color = "skyblue") + 
#   tm_layout(main.title = "Najvišja vsota podeljenih točk", main.title.size = 1, legend.title.size = 1)
# 
# tabela7 <- pod6 %>% rename("Drzava" = Var1, "Stevilo" = Freq)
# 
# tabela7$Drzava[tabela7$Drzava == Bosnia & Herzegovina] <- wicdj
# tabela7$Drzava <- recode_factor(tabela7$Drzava, "Bosnia & Herzegovina" = "Bosnia and Herzegovina",
#                                 "Serbia" = "Republic of Serbia", "Serbia & Montenegro" = "Republic of Serbia",
#                                 "North Macedonia" = "Macedonia")
# tabela7 <- tabela7 %>% group_by(Drzava) %>% summarise(Stevilo = sum(Stevilo))
# 
# 
# zemljevid3 <- tm_shape(merge(zemljevid,
#                              tabela7,duplicateGeoms = TRUE,
#                              by.x="SOVEREIGNT", by.y="Drzava"),
#                        xlim=c(-25,40), ylim=c(32,72)) +
#   tm_polygons("Stevilo", title = "Število nastopov")+
#   tm_layout(bg.color = "skyblue") + 
#   tm_layout(main.title = "Najvišja vsota podeljenih točk", main.title.size = 1, legend.title.size = 1)

#________________________________6______________________________________________
tabela8 <- tabela3[-dv2,] %>% group_by(Drzava) %>% summarise(Vsota = sum(Tocke)) %>%
  mutate(Drzava = recode(Drzava, "Bosnia & Herzegovina" = "Bosnia and Herzegovina",
                "Serbia" = "Republic of Serbia", "Serbia & Montenegro" = "Republic of Serbia",
                "North Macedonia" = "Macedonia"))

zemljevid4 <- tm_shape(merge(zemljevid,
                             tabela8,duplicateGeoms = TRUE,
                             by.x="SOVEREIGNT", by.y="Drzava"),
                       xlim=c(-25,40), ylim=c(32,72)) +
  tm_polygons("Vsota", title = "Število točk", breaks = c(0,1,20,40,60,80,100))+
  tm_layout(bg.color = "skyblue") + 
  tm_layout(main.title = "Vsota podeljenih točk Jugoslavija", main.title.size = 1, legend.title.size = 1)


tabela9 <- tabela4 %>% group_by(Drzava) %>% summarise(Vsota=sum(Tocke)) %>%
  mutate(Drzava = recode(Drzava, "Bosnia & Herzegovina" = "Bosnia and Herzegovina",
                         "Serbia" = "Republic of Serbia", "Serbia & Montenegro" = "Republic of Serbia",
                         "North Macedonia" = "Macedonia", "The Netherlands" = "Netherlands",
                         "Czech Republic" = "Czechia")) %>%
  group_by(Drzava) %>% summarise(Vsota=sum(Vsota))

zemljevid5 <- tm_shape(merge(zemljevid,
                             tabela9,duplicateGeoms = TRUE,
                             by.x="SOVEREIGNT", by.y="Drzava"),
                       xlim=c(-25,40), ylim=c(32,72)) +
  tm_polygons("Vsota", title = "Število točk", breaks = c(0,1,20,40,60,80,100,120,140)) +
  tm_layout(bg.color = "skyblue") + 
  tm_layout(main.title = "Vsota podeljenih točk", main.title.size = 1, legend.title.size = 1)



