

#Tabela za starost in spol
glava_starost_spol <- c("Meritev","Starost", "Spol", "Leto", 
                        "SocialniTransferji", "Dohodek")

starosti <- c("0-17", "18-24", "25-49", "50-64", "65+")

starost_spol_vsi <- read_csv2("podatki/starost_spol.csv", skip = 3, 
  col_names = glava_starost_spol, locale=locale(encoding="Windows-1250")) %>%
  filter(Spol != "Spol - SKUPAJ") %>%
  select(-Meritev, -SocialniTransferji)

starost_spol <- starost_spol_vsi %>%
  filter(Starost != "Starostne skupine - SKUPAJ") %>%
  mutate(Starost = gsub("Starost ", "", Starost) %>%
  parse_factor(levels=starosti, ordered=TRUE))

spol <- starost_spol_vsi %>%
  filter(Starost == "Starostne skupine - SKUPAJ") %>%
  select(-Starost)
 # mutate(Spol = gsub("Spol - SKUPAJ", "Skupaj", Spol)) %>%
 # mutate(Starost = gsub("Starostne skupine - SKUPAJ", "Skupaj", Starost))



#Tabela za izobrazbo in spol
glava_izobrazba_spol <- c("Meritev","Izobrazba", "Spol", "Leto", 
                        "SocialniTransferji", "Dohodek")

izobrazbe <- c("Osnovnošolska ali manj", "Srednješolska poklicna", 
               "Srednješolska strokovna, splošna", "Višješolska, visokošolska")

izobrazba_spol <- read_csv2("podatki/izobrazba_spol.csv", skip = 3,
  col_names = glava_izobrazba_spol, locale=locale(encoding="Windows-1250")) %>%
  filter(Spol != "Spol - SKUPAJ") %>%
  select(-Meritev, -SocialniTransferji) %>%
  mutate(Izobrazba=Izobrazba %>% parse_factor(levels = izobrazbe, ordered = TRUE))


#Tabela za regije
glava_regije <- c("Meritev","Regija", "Leto", 
                        "SocialniTransferji", "Dohodek")

regije <- read_csv2("podatki/statistične_regije.csv", skip = 3,
  col_names = glava_regije, locale = locale(encoding = "Windows-1250")) %>%
  select(-Meritev, -SocialniTransferji)


#Vrste dohodka
url <- "https://pxweb.stat.si:443/SiStatData/sq/1214"
vrste_dohodka <- read_html(url, encoding = "UTF-8") %>%
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
  mutate(Dohodek = gsub("\\.", "", Dohodek),
          Leto = as.integer(Leto), Dohodek = as.integer(Dohodek),
          Vrsta.dohodka = str_sub(Vrsta.dohodka, 3, -1),
          Meritev = str_sub(Meritev, 1, -7)) %>%
  subset(Meritev != "Povprečni dohodek na gospodinjstvo") %>% #zanima me le povprečni na člana
  select(-Meritev)

vrste_dohodka$Vrsta.dohodka <- factor(vrste_dohodka$Vrsta.dohodka, levels = c("DOHODEK IZ DELA", "POKOJNINE Z DODATKI",
                                                                              "DRUŽINSKI IN SOCIALNI PREJEMKI", "DRUGI DOHODKI"))

#Izvoz v CSV
write.csv2(starost_spol_vsi, "podatki/starost_spol_vsi_tidy.csv", fileEncoding = "UTF-8")
write.csv2(starost_spol, "podatki/starost_spol_tidy.csv", fileEncoding = "UTF-8")
write.csv2(spol, "podatki/spol_tidy.csv", fileEncoding = "UTF-8")
write.csv2(izobrazba_spol, "podatki/izobrazba_spol_tidy.csv", fileEncoding = "UTF-8")
write.csv2(regije, "podatki/tidy_regije.csv", fileEncoding = "UTF-8")
write.csv2(vrste_dohodka, "podatki/vrste_dohodka_tidy.csv", fileEncoding = "UTF-8")
  




