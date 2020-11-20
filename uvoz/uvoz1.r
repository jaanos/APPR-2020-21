library(readr)
library(tidyr)
library(dplyr)
library(rvest)


#Tabela za starost in spol
glava_starost_spol <- c("Vrsta dohodka","Starost", "Spol", "Leto", 
                        "Socialni transferji", "Dohodek")

starost_spol <- read.csv2("podatki/starost_spol.csv", skip = 3, header = F, 
                          col.names = glava_starost_spol, encoding = "utf-8")



#Tabela za izobrazbo in spol
glava_izobrazba_spol <- c("Vrsta dohodka","Izobrazba", "Spol", "Leto", 
                        "Socialni transferji", "Dohodek")

izobrazba_spol <- read.csv2("podatki/izobrazba_spol.csv", skip = 3, header = F,
                            col.names = glava_izobrazba_spol, encoding = "utf-8")


#Tabela za regije
glava_regije <- c("Vrsta dohodka","Regija", "Leto", 
                        "Socialni transferji", "Dohodek")

regije <- read.csv2("podatki/statistične_regije.csv", skip = 3, header = F,
                    col.names = glava_regije, encoding = "utf-8")

#Vrste dohodka
url <- "https://pxweb.stat.si:443/SiStatData/sq/1214"
vrste_dohodka <- read_html(url) %>%
  html_nodes(xpath = "//table") %>%
  .[[1]] %>%
  html_table(fill = TRUE, header = FALSE) %>%
  rename(
    Vrsta.dohodka=1,
    Vir=2,
    Leto=3,
    Dohodek=4
  ) %>%
  slice(-1, -2)
  




