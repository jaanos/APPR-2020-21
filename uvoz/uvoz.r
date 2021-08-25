
library(readr)
library(dplyr)
#________________________________________
stolpci1 <- c("Leto", "Drzava", "Spol", "Selitev", "Priseljeni_iz_tujine")
tabela1 <- read_csv2("podatki/priseljevanje_drzave.csv", na=c("..."), col_names = stolpci1, skip =3,
                    locale=locale(encoding = "Windows-1250"))

tabela1$Selitev <- NULL

tabela1$Drzava[tabela1$Drzava == "AVSTRALIJA IN OCEANIJA"] <- "Avstralija in Oceanija"
tabela1 <- subset(tabela1, Drzava!="EVROPA")

