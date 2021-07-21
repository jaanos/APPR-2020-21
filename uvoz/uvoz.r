library("readr")
library("tidyr")
library("rlang")

uvoz17 <- read_csv("podatki/merged_gw_17.csv",
                      TRUE, locale=locale(encoding="Windows-1252"),
                      col_types=cols("was_home"=col_logical()))
stat17 <- uvoz17[c(2:5,8:10,12,15:20,22,24,29:31,33:40,42:44,53:55)]
uvoz18 <- read_csv("podatki/merged_gw_18.csv", TRUE, locale=locale(encoding="Windows-1252"),
                      col_types=cols("was_home"=col_logical()))
stat18 <- uvoz18[c(2:5,8:10,12,15:20,22,24,29:31,33:40,42:44,53:55)]

uvoz19 <- read_csv("podatki/merged_gw_19.csv", TRUE, locale=locale(encoding="Windows-1252"),
                      col_types=cols("was_home"=col_logical()))
stat19 <- uvoz19[c(2:5,8:10,12,15:20,22,24,29:31,33:40,42:44,53:55)]

uvoz_igralci17 <- read_csv("podatki/players_raw_17.csv", TRUE,
                      locale=locale(encoding="UTF-8"))
info17 <- uvoz_igralci17[c(15,19,24,41,46:47,56)]

uvoz_igralci18 <- read_csv("podatki/players_raw_18.csv", TRUE,
                      locale=locale(encoding="UTF-8"))
info18 <- uvoz_igralci18[c(15,19,24,42,47:48,57)]

uvoz_igralci19 <- read_csv("podatki/players_raw_19.csv", TRUE,
                      locale=locale(encoding="UTF-8"))
info19 <- uvoz_igralci19[c(15,19,24,42,47:48,57)]

