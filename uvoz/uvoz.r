library("readr")
library("tidyr")
library("rlang")
library("dplyr")


uvoz17 <- read_csv("podatki/merged_gw_17.csv",
                      TRUE, locale=locale(encoding="Windows-1252"),
                      col_types=cols("was_home"=col_logical()))
stat17 <- uvoz17[c(1:5,8:10,12,15:20,22,24,29:31,33:40,42:44,53:55)] %>%
  separate(name, c("first_name", "second_name"), sep="_")
uvoz18 <- read_csv("podatki/merged_gw_18.csv", TRUE, locale=locale(encoding="Windows-1252"),
                      col_types=cols("was_home"=col_logical()))
stat18 <- uvoz18[c(1:5,8:10,12,15:20,22,24,29:31,33:40,42:44,53:55)] %>%
  separate(name, c("first_name", "second_name"), sep="_") #

uvoz19 <- read_csv("podatki/merged_gw_19.csv", TRUE, locale=locale(encoding="Windows-1252"),
                      col_types=cols("was_home"=col_logical()))
stat19 <- uvoz19[c(1:5,8:10,12,15,17:20,22,24,29:31,33:40,42:44,53:55)] %>%
  separate(name, c("first_name", "second_name", "id"), sep = "_") %>% mutate(id=parse_number(id))

uvoz_igralci17 <- read_csv("podatki/players_raw_17.csv", TRUE,
                      locale=locale(encoding="UTF-8"))
info17 <- uvoz_igralci17[c(15,19,24,41,46:47,56)]

uvoz_igralci18 <- read_csv("podatki/players_raw_18.csv", TRUE,
                      locale=locale(encoding="UTF-8"))
info18 <- uvoz_igralci18[c(15,19,24,42,47:48,57)]

uvoz_igralci19 <- read_csv("podatki/players_raw_19.csv", TRUE,
                      locale=locale(encoding="UTF-8"))
info19 <- uvoz_igralci19[c(15,19,24,42,47:48,57)]
#vratarji <- filter(info19, element_type == 1)
#imena %>% filter(leto == 2013, spol == "ženske", stevilo <= 5) %>%
  #select(ime, stevilo)
#kres <- filter(stat19, name=="Aaron_Cresswell_402")


skupna17 <- merge(stat17[c(-17)], info17) %>% select(-fixture, -team_code)
skupna18 <- merge(stat18[c(-17)], info18) %>% select(-fixture, -team_code)
skupna19 <- merge(stat19, info19[c(-2, -4)]) %>% select(-fixture, -team_code)
