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
  separate(name, c("first_name", "second_name"), sep="_")

uvoz19 <- read_csv("podatki/merged_gw_19.csv", TRUE, locale=locale(encoding="Windows-1252"),
                      col_types=cols("was_home"=col_logical()))

stat19 <- uvoz19[c(1:5,8:10,12,15:16,18:20,24,29:40,42:44,53:55)] %>%
  separate(name, c("first_name", "second_name", "id"), sep = "_") %>% mutate(id = parse_number(id)) %>%
  mutate(second_name = sapply(second_name, function(x) {gsub("De Gea", "de Gea", x, fixed = TRUE)}),
         first_name = sapply(first_name, function(x) {gsub("Caglar", "Çaglar", x, fixed = TRUE)}))

#c("Caglar", "De Gea") c("Çaglar", "de Gea")
uvoz_igralci17 <- read_csv("podatki/players_raw_17.csv", TRUE,
                      locale=locale(encoding="UTF-8"))
#info17 <- uvoz_igralci17[c(15,19,24,41,46,56)]

uvoz_igralci18 <- read_csv("podatki/players_raw_18.csv", TRUE,
                      locale=locale(encoding="UTF-8")) %>%
  mutate(second_name = sapply(second_name, function(x) {gsub("De Gea", "de Gea", x, fixed = TRUE)}))
#info18 <- uvoz_igralci18[c(15,19,24,42,47,57)]

uvoz_igralci19 <- read_csv("podatki/players_raw_19.csv", TRUE,
                      locale=locale(encoding="UTF-8"))

# UVOZ IMEN EKIP IN NJIHOVIH KONČNIH RAZVRSTITEV

#funkcija, ki angleski zapis o uvrstitvi v evropo prevede v slovenskega
uvrstitev <- function(x){ if (x == "") {x="Sredina Lestvice"} else {
  mgsub(x, c("Qualification for the Champions League group stage", "Qualification for the Europa League group stage[a]",
             "Qualification for the Europa League second qualifying round[a]", "Relegation to the EFL Championship",
             "Qualification for the Champions League group stage[b]", "Qualification for the Europa League third qualifying round[c]",
             "Qualification for the Champions League play-off round"),
        c("Liga Prvakov", "Evropska Liga", "Evropska Liga", "Izpad", "Liga Prvakov", "Evropska Liga", "Liga Prvakov"), fixed = TRUE)}}

#funkcija, ki na koncu imena ekipe odstrani oznako njene razvrstitve
odstrani_oznako <- function(x) {mgsub(x, c(" (C)", " (R)"), c("", ""), fixed = TRUE)}

# 2016/17
link1 <- "https://en.wikipedia.org/wiki/2016%E2%80%9317_Premier_League"

stran1 <- html_session(link1) %>% read_html()

tabela1 <- stran1 %>% html_nodes(xpath="//table[@class='wikitable sortable']") %>%
  .[[1]] %>% html_table(dec=",")

tabela4 <- stran1 %>% html_nodes(xpath="//table[@class='wikitable']") %>%
  .[[1]] %>% html_table() %>% rename(Uvrstitev = "Qualification or relegation", Ekipa = Team) %>%
  mutate(Ekipa = sapply(Ekipa, odstrani_oznako), Uvrstitev = sapply(Uvrstitev, uvrstitev)) %>% select(Ekipa, Uvrstitev)

ekipe17 <- tabela1$Team

# 2017/18
link2 <- "https://en.wikipedia.org/wiki/2017%E2%80%9318_Premier_League"
stran2 <- html_session(link2) %>% read_html()
tabela2 <- stran2 %>% html_nodes(xpath="//table[@class='wikitable sortable']") %>%
  .[[1]] %>% html_table(dec=",") %>% slice(-21) #odstranili smo zadnjo cudno vrstico

tabela5 <- stran2 %>% html_nodes(xpath="//table[@class='wikitable']") %>%
  .[[1]] %>% html_table() %>% rename(Uvrstitev = "Qualification or relegation", Ekipa = Team) %>%
  mutate(Ekipa = sapply(Ekipa, odstrani_oznako), Uvrstitev = sapply(Uvrstitev, uvrstitev)) %>% select(Ekipa, Uvrstitev)

ekipe18 <- tabela2$Team

# 2018/19
link3 <- "https://en.wikipedia.org/wiki/2018%E2%80%9319_Premier_League"

stran3 <- html_session(link3) %>% read_html()

tabela3 <- stran3 %>% html_nodes(xpath="//table[@class='wikitable sortable']") %>%
  .[[1]] %>% html_table(dec=",") %>% slice(-17) #odstranili smo podvojni Totteham ki je igral na dveh razlicnih stadionih

tabela6 <- stran3 %>% html_nodes(xpath="//table[@class='wikitable']") %>%
  .[[1]] %>% html_table() %>% rename(Uvrstitev = "Qualification or relegation", Ekipa = Team) %>%
  mutate(Ekipa = sapply(Ekipa, odstrani_oznako), Uvrstitev = sapply(Uvrstitev, uvrstitev)) %>% select(Ekipa, Uvrstitev)

ekipe19 <- tabela3$Team

#OPTIIONAL
#mutate(Team = parse_character(Team)) %>%







#ZDRUŽEVANJE TABEL
skupna17 <- stat17 %>% left_join(uvoz_igralci17[c(15,19,24,41,46,56)]) %>%
  mutate(element_type = c("Vratar", "Branilec", "Vezist", "Napadalec")[element_type], team = ekipe17[team]) %>%
  mutate(element_type = parse_factor(element_type), team = parse_factor(team)) %>%
  rename(Ime=first_name, Priimek=second_name, Pozicija = element_type, Ekipa=team, Igralec=web_name)

skupna18 <- stat18 %>% left_join(uvoz_igralci18[c(15,19,24,42,47,57)]) %>%
  mutate(element_type = c("Vratar", "Branilec", "Vezist", "Napadalec")[element_type], team = ekipe18[team]) %>%
  mutate(element_type = parse_factor(element_type), team = parse_factor(team)) %>%
  rename(Ime=first_name, Priimek=second_name, Pozicija = element_type, Ekipa=team, Igralec=web_name)

#dvakrat danny ward
skupna19 <- stat19 %>% left_join(uvoz_igralci19[c(15,19,24,42,47,57)]) %>%
  mutate(element_type = c("Vratar", "Branilec", "Vezist", "Napadalec")[element_type], team = ekipe19[team]) %>%
  mutate(element_type=parse_factor(element_type), team = parse_factor(team)) %>% rename(Ime=first_name, Priimek=second_name, Pozicija = element_type,
                                                                                        Ekipa=team, Igralec=web_name)


#komulativne tabele za igralce, odstranili smo stolpec, ki beleži nasprotnika v krogu, krog, domace igrisce 
kom17 <- skupna17 %>% select(-opponent_team, -round, -was_home) %>%
  group_by(Ime, Priimek, Pozicija, id, Ekipa, Igralec) %>% summarise(across(everything(), sum)) %>%
  left_join(tabela4) %>% mutate(Ekipa = parse_factor(Ekipa), Uvrstitev = parse_factor(Uvrstitev))

kom18 <- skupna18 %>% select(-opponent_team, -round, -was_home) %>%
  group_by(Ime, Priimek, Pozicija, id, Ekipa, Igralec) %>% summarise(across(everything(), sum)) %>%
  left_join(tabela5) %>% mutate(Ekipa = parse_factor(Ekipa), Uvrstitev = parse_factor(Uvrstitev))

kom19 <- skupna19 %>% select(-opponent_team, -round, -was_home) %>%
  group_by(Ime, Priimek, Pozicija, id, Ekipa, Igralec) %>% summarise(across(everything(), sum)) %>%
  left_join(tabela6) %>% mutate(Ekipa = parse_factor(Ekipa), Uvrstitev = parse_factor(Uvrstitev))

#statistike glede na 90 odigranih minut, še prej smo izločili vse igralce, ki so odigrali
#skupno v sezoni manj kot 90 minut
# p90_17 <- kom17 %>% filter(minutes >= 90) %>% group_by(first_name, second_name, element_type, id, team, web_name) %>% 
#   summarise(across(everything(),function(x) {round(x/minutes * 90, 2)})) %>% select(-minutes)
# 
# p90_18 <- kom18 %>% filter(minutes >= 90) %>% group_by(first_name, second_name, element_type, id, team, web_name) %>% 
#   summarise(across(everything(),function(x) {round(x/minutes * 90, 2)})) %>% select(-minutes)
# 
# p90_19 <- kom19 %>% filter(minutes >= 90) %>% group_by(Ime, Priimek, Pozicija, id, Ekipa, Igralec) %>%
#   summarise(across(everything(),function(x) {round(x/minutes * 90, 2)})) %>% select(-minutes)



#UPORABNA KOMANDA
#View(kom17[c("web_name", "minutes")])

