
library(readr)
library(dplyr)
library(stringr)
#__________________________TABELA 1________________
imenastolpcev <- c("Leto", "Drzava", "Selitev", "Spol", "Priseljeni_iz_tujine")
tabela1 <- read_csv2("podatki/priseljeni_drzava_spol.csv", na=c("..."), col_names = imenastolpcev, skip=3,
                    locale=locale(encoding = "Windows-1250"))

tabela1$Selitev <- NULL

tabela1$Drzava[tabela1$Drzava == "AVSTRALIJA IN OCEANIJA"] <- "Avstralija in Oceanija"
tabela1 <- subset(tabela1, Drzava!="EVROPA")
tabela1 <- subset(tabela1, Drzava!="SEVERNA IN SREDNJA AMERIKA")
tabela1$Drzava[tabela1$Drzava == "AFRIKA"] <- "Afrika"
tabela1$Drzava[tabela1$Drzava == "AZIJA"] <- "Azija"
tabela1$Drzava[tabela1$Drzava == "JUŽNA AMERIKA"] <- "Južna Amerika"

#__________________________TABELA 2__________________
imenastolpcev2 <- c("Leto", "Drzava_prihodnjega_bivalisca", "Selitev", "Spol", "Odseljeni_v_tujino")
tabela2 <- read_csv2("podatki/odseljenidrzavaspol.csv", na=c("..."), col_names = imenastolpcev2, skip=3,
                     locale=locale(encoding = "Windows-1250"))

tabela2$Selitev <- NULL
tabela2 <- subset(tabela2, Spol!="Državljani Republike Slovenije - SKUPAJ")
tabela2 <- subset(tabela2, Drzava_prihodnjega_bivalisca!="EVROPA")
tabela2$Spol[tabela2$Spol == "Državljani Republike Slovenije - moški"] <- "Moški"
tabela2$Spol[tabela2$Spol == "Državljani Republike Slovenije - ženske"] <- "Ženske"
tabela2 <- subset(tabela2, Drzava_prihodnjega_bivalisca!="SEVERNA IN SREDNJA AMERIKA")
tabela2$Drzava_prihodnjega_bivalisca[tabela2$Drzava_prihodnjega_bivalisca == "AFRIKA"] <- "Afrika"
tabela2$Drzava_prihodnjega_bivalisca[tabela2$Drzava_prihodnjega_bivalisca == "AZIJA"] <- "Azija"
tabela2$Drzava_prihodnjega_bivalisca[tabela2$Drzava_prihodnjega_bivalisca == "JUŽNA AMERIKA"] <- "Južna Amerika"
tabela2$Drzava_prihodnjega_bivalisca[tabela2$Drzava_prihodnjega_bivalisca == "AVSTRALIJA IN OCEANIJA"] <- "Avstralija in Oceanija"

#___________________________TABELA 3_______________________________________
imenastolpcev3 <- c("Leto", "Drzava", "Izobrazba", "Starost", "Spol", "Stevilo_odseljenih")
tabela3 <- read_csv2("podatki/odseljenistarost.csv", na=c("..."), col_names = imenastolpcev3, skip=3,
                     locale=locale(encoding = "Windows-1250"))
tabela3$Drzava <- NULL
tabela3$Izobrazba <- NULL

#_______________________TABELA 4_________________________________________
imenastolpcev4 <- c("Leto", "Drzavljanstvo", "Drzava", "Starost", "Stevilo_priseljenih")
tabela4 <- read_csv2("podatki/priseljeni_starost.csv", na=c("..."), col_names = imenastolpcev4, skip=3,
                     locale=locale(encoding = "Windows-1250"))
tabela4$Drzavljanstvo <- NULL
 
tabela4$Drzava <- gsub("\\.\\.\\.\\.","",tabela4$Drzava)

tabela4 <- subset(tabela4, Drzava!="- Države EU - SKUPAJ")
tabela4 <- subset(tabela4, Drzava!="- Druge evropske države - SKUPAJ")
tabela4$Drzava[tabela4$Drzava == "AFRIKA"] <- "Afrika"
tabela4$Drzava[tabela4$Drzava == "AZIJA"] <- "Azija"
tabela4$Drzava[tabela4$Drzava == "JUŽNA AMERIKA"] <- "Južna Amerika"
tabela4$Drzava[tabela4$Drzava == "AVSTRALIJA IN OCEANIJA"] <- "Avstralija in Oceanija"
tabela4$Drzava[tabela4$Drzava == "SEVERNA IN SREDNJA AMERIKA"] <- "Severna in Srednja Amerika"

#starostne skupine, so character, kaj narediti v tem primeru??


#_______________________ Tabela 5 _______________________________________________
imenastolpcev5 <- c("Leto", "Drzava_prihodnjega_bivalisca", "Drzavljanstvo", "Spol", "Izobrazba", "Stevilo_odseljenih")
tabela5 <- read_csv2("podatki/odseljeni_izobrazba.csv", na=c("..."), col_names = imenastolpcev5, skip=3,
                     locale=locale(encoding = "Windows-1250"))

tabela5 <- subset(tabela5, select = c("Leto",  "Drzava_prihodnjega_bivalisca", "Izobrazba","Spol", "Stevilo_odseljenih"))

tabela5$Drzava_prihodnjega_bivalisca <- gsub("\\.\\.\\.\\.","",tabela5$Drzava_prihodnjega_bivalisca)
tabela5 <- subset(tabela5, Drzava_prihodnjega_bivalisca!="Država prihodnjega prebivališča - SKUPAJ")
tabela5 <- subset(tabela5, Drzava_prihodnjega_bivalisca!="EVROPA - SKUPAJ")
tabela5 <- subset(tabela5, Drzava_prihodnjega_bivalisca!="- Države nastale na območju nekdanje Jugoslavije - SKUPAJ")
tabela5 <- subset(tabela5, Drzava_prihodnjega_bivalisca!="- Države EU - SKUPAJ")
tabela5 <- subset(tabela5, Drzava_prihodnjega_bivalisca!="- Druge evropske države - SKUPAJ")

tabela5$Drzava_prihodnjega_bivalisca[tabela5$Drzava_prihodnjega_bivalisca == "AFRIKA"] <- "Afrika"
tabela5$Drzava_prihodnjega_bivalisca[tabela5$Drzava_prihodnjega_bivalisca == "AZIJA"] <- "Azija"
tabela5$Drzava_prihodnjega_bivalisca[tabela5$Drzava_prihodnjega_bivalisca == "JUŽNA AMERIKA"] <- "Južna Amerika"
tabela5$Drzava_prihodnjega_bivalisca[tabela5$Drzava_prihodnjega_bivalisca == "AVSTRALIJA IN OCEANIJA"] <- "Avstralija in Oceanija"
tabela5$Drzava_prihodnjega_bivalisca[tabela5$Drzava_prihodnjega_bivalisca == "SEVERNA IN SREDNJA AMERIKA"] <- "Severna in Srednja Amerika"






