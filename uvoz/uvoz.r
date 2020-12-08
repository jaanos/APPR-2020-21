require(dplyr)
require(tidyr)
require(readr)
require(readxl)

sl <- locale("sl", decimal_mark=",", grouping_mark=".")

#Povprečna bruto mesečna plača glede na gospodarsko dejavnost, izobrazbo in spol
gospodarskadejavnost <- read_csv2("podatki/placa_dejavnost.csv",
                                  col_names=c("gospodarska dejavnost","izobrazba","spol","leto","placa"),
                                  skip=3, na="-",
                                  locale=locale(encoding="Windows-1250"))


#Povprečna bruto mesečna plača glede na regijo in spol
regijainspol <- read_csv2("podatki/placa_regija.csv",
                          col_names=c("regija","spol","leto","placa"),
                          skip=3, na="-",
                          locale=locale(encoding="Windows-1250"))
View(regijainspol)


#Povprečna bruto mesečna plača v javnem in zasebnem sektorju glede na izobrazbo in spol
javnisektor <- read_csv2("podatki/sektor.csv",
                         col_names=c("sektor","izobrazba","spol","leto","placa"),
                         skip=3, na="-",
                         locale=locale(encoding="Windows-1250"))


#Povprečna bruto mesečna plača(kriza)
kriza2008 <- read_csv2("podatki/kriza2008.csv",
                       col_names=c("leto","tip place", "placa"),
                       skip=3, na="-",
                       locale=locale(encoding="Windows-1250")) %>% select(c(-2))

View(kriza2008)

kriza2020 <- read_xlsx("podatki/kriza2020.xlsx",
                       col_names=c("leto","tip place", "placa"),
                       skip=2, n_max=21) %>% select(-"tip place")



  
  


