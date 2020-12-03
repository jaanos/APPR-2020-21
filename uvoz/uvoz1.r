library(readr)
library(tidyr)
library(dplyr)
library(rvest)
library(tidyverse) # za str_sub


#Tabela za starost in spol
glava_starost_spol <- c("Meritev","Starost", "Spol", "Leto", 
                        "Socialni transferji", "Dohodek")

starost_spol <- read.csv2("podatki/starost_spol.csv", skip = 3, header = F, 
  col.names = glava_starost_spol, encoding = "utf-8") %>%
  mutate(Starost = gsub("Starost ", "", Starost)) %>%
  mutate(Spol = gsub("Spol - SKUPAJ", "Skupaj", Spol)) %>%
  mutate(Starost = gsub("Starostne skupine - SKUPAJ", "Skupaj", Starost))



#Tabela za izobrazbo in spol
glava_izobrazba_spol <- c("Meritev","Izobrazba", "Spol", "Leto", 
                        "Socialni transferji", "Dohodek")

izobrazba_spol <- read.csv2("podatki/izobrazba_spol.csv", skip = 3, header = F,
  col.names = glava_izobrazba_spol, encoding = "utf-8") %>%
  mutate(Spol = gsub("Spol - SKUPAJ", "Skupaj", Spol))


#Tabela za regije
glava_regije <- c("Meritev","Regija", "Leto", 
                        "Socialni transferji", "Dohodek")

regije <- read.csv2("podatki/statistične_regije.csv", skip = 3, header = F,
  col.names = glava_regije, encoding = "utf-8")

#Vrste dohodka
url <- "https://pxweb.stat.si:443/SiStatData/sq/1214"
vrste_dohodka <- read_html(url, encoding = "utf-8") %>%
  html_nodes(xpath = "//table") %>%
  .[[1]] %>%
  html_table(fill = TRUE, header = FALSE) %>%
  rename(
    Meritev=1,
    Vrsta.dohodka=2,
    Leto=3,
    Dohodek=4
  ) %>%
  slice(-1, -2) %>%
  mutate(Vrsta.dohodka = na_if(Vrsta.dohodka, "Povprečni dohodek na gospodinjstvo (EUR)")) %>%
  mutate(Vrsta.dohodka = na_if(Vrsta.dohodka, "Povprečni dohodek na člana gospodinjstva (EUR)")) %>%
  fill(Vrsta.dohodka) %>%
  mutate(Dohodek = gsub("\\.", "", Dohodek)) %>%
  mutate(Leto = as.integer(Leto), Dohodek = as.integer(Dohodek)) %>%
  mutate(Vrsta.dohodka = str_sub(Vrsta.dohodka, 3, -1)) %>%
  mutate(Meritev = str_sub(Meritev, 1, -7)) %>%
  subset(Meritev != "Povprečni dohodek na gospodinjstvo") #zanima me le povprečni na člana

#Izvoz v CSV
write.csv2(starost_spol, "podatki/starost_spol_tidy.csv", fileEncoding = "utf-8")
write.csv2(izobrazba_spol, "podatki/izobrazba_spol_tidy.csv", fileEncoding = "utf-8")
write.csv2(regije, "podatki/tidy_regije.csv", fileEncoding = "utf-8")
write.csv2(vrste_dohodka, "podatki/vrste_dohodka_tidy.csv", fileEncoding = "utf-8")
  




