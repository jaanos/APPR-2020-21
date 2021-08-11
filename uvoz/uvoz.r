#library("readr")
#library("tidyr")
#library("rlang")
#library("dplyr")

source("lib/libraries.r", encoding="UTF-8")


#UVOZI STATTISTIKE IN INFORMACIJE O IGRALCIH ZA LETA 2017, 2018, 2019
uvoz17 <- read_csv("podatki/merged_gw_17.csv",
                      TRUE, locale=locale(encoding="Windows-1252"),
                      col_types=cols("was_home"=col_logical()))
stat17 <- uvoz17[c(1:5,8:10,12,15:16,18:20,24,29:40,42:44,53:55)] %>%
  separate(name, c("first_name", "second_name"), sep="_")
uvoz18 <- read_csv("podatki/merged_gw_18.csv", TRUE, locale=locale(encoding="Windows-1252"),
                      col_types=cols("was_home"=col_logical()))
stat18 <- uvoz18[c(1:5,8:10,12,15:16,18:20,24,29:40,42:44,53:55)] %>%
  separate(name, c("first_name", "second_name"), sep="_") #

uvoz19 <- read_csv("podatki/merged_gw_19.csv", TRUE, locale=locale(encoding="Windows-1252"),
                      col_types=cols("was_home"=col_logical()))
stat19 <- uvoz19[c(1:5,8:10,12,15:16,18:20,24,29:40,42:44,53:55)] %>%
  separate(name, c("first_name", "second_name", "id"), sep = "_") %>% select(-id)

uvoz_igralci17 <- read_csv("podatki/players_raw_17.csv", TRUE,
                      locale=locale(encoding="UTF-8"))
#info17 <- uvoz_igralci17[c(15,19,24,41,46,56)]

uvoz_igralci18 <- read_csv("podatki/players_raw_18.csv", TRUE,
                      locale=locale(encoding="UTF-8"))
#info18 <- uvoz_igralci18[c(15,19,24,42,47,57)]

uvoz_igralci19 <- read_csv("podatki/players_raw_19.csv", TRUE,
                      locale=locale(encoding="UTF-8"))

#info19 <- uvoz_igralci19[c(15,19,24,42,47,57)]
#vratarji <- filter(info19, element_type == 1)
#imena %>% filter(leto == 2013, spol == "ženske", stevilo <= 5) %>%
  #select(ime, stevilo)

#UPORABNA KOMANDA
#kres17 <- filter(stat17, second_name=="Cresswell")
#kres18 <- filter(stat18, second_name=="Cresswell")
#kres19 <- filter(stat19, second_name=="Cresswell")

#združitev tabele s statistikami s tabelo z informacijami
skupna17 <- merge(stat17, uvoz_igralci17[c(15,19,24,41,46,56)]) #%>% select(-fixture, -team_code)
skupna18 <- merge(stat18, uvoz_igralci18[c(15,19,24,42,47,57)]) #%>% select(-fixture, -team_code)
skupna19 <- merge(stat19, uvoz_igralci19[c(15,19,24,42,47,57)]) #%>% select(-fixture, -team_code)

#komulativne tabele za igralce, odstranili smo stolpec, ki belezi nasprotnika v krogu, krog, domace igrisce 
kom17 <- skupna17[c(-20,-27,-32)] %>%
  group_by(first_name, second_name, element_type, id, team, web_name) %>%
  summarise(across(everything(), sum))

kom18 <- skupna18[c(-20,-27,-32)] %>%
  group_by(first_name, second_name, element_type, id, team, web_name) %>%
  summarise(across(everything(), sum))

kom19 <- skupna19[c(-20,-27,-32)] %>%
  group_by(first_name, second_name, element_type, id, team, web_name) %>%
  summarise(across(everything(), sum))

#statistike glede na 90 odigranih minut, še prej smo izločili vse igralce, ki so odigrali
#skupno v sezoni manj kot 90 minut
p90_17 <- kom17 %>% filter(minutes >= 90) %>% group_by(first_name, second_name, element_type, id, team, web_name) %>% 
  summarise(across(everything(),function(x) {round(x/minutes * 90, 2)})) %>% select(-minutes)

p90_18 <- kom18 %>% filter(minutes >= 90) %>% group_by(first_name, second_name, element_type, id, team, web_name) %>% 
  summarise(across(everything(),function(x) {round(x/minutes * 90, 2)})) %>% select(-minutes)

p90_19 <- kom19 %>% filter(minutes >= 90) %>% group_by(first_name, second_name, element_type, id, team, web_name) %>% 
  summarise(across(everything(),function(x) {round(x/minutes * 90, 2)})) %>% select(-minutes)


#UPORABNA KOMANDA
#View(kom17[c("web_name", "minutes")])