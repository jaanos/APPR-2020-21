library(readr)
library(tidyr)
library(dplyr)


sl <- locale("sl", decimal_mark=",", grouping_mark=".")


tabela1 <- read_csv2("podatki/st_dijakov_po_regijah.csv", skip = 1, col_names = c("regija", "leto", "dijaki"),
                  locale=locale(encoding="Windows-1250"))

tabela2 <- read_csv2("podatki/st_dijakov_po_spolu_vrsti_izobrazevanja.csv", skip = 1, col_names = c("izobrazevanje", "letnik", "starost", "leto", "moski", "zenski"),
                     locale=locale(encoding="Windows-1250")) %>% select(c(-2, -3)) %>% mutate(leto=parse_number(leto))

tabela3 <- read_csv2("podatki/st_diplomantov_po_regijah.csv", skip = 1, col_names = c("regija", "leto", "diplomanti"),
                     locale=locale(encoding="Windows-1250"))

tabela4 <- read_csv2("podatki/st_diplomantov_po_vrsti_izobraževanja_spolu.csv", skip = 1, na = c("-"),  col_names = c("izobrazevanje", "leto", "moski", "zenski"),
                     locale=locale(encoding="Windows-1250")) %>% drop_na(3)

tabela5 <- read_csv2("podatki/st_studentov_po_vrsti_izobrazevanja_nacinu_studija.csv", skip = 1,na = c("-"), col_names = c("izobrazevanje", "leto", "redni", "izredni"),
                     locale=locale(encoding="Windows-1250"))%>% drop_na(3) %>% mutate(leto=parse_number(leto))

# v tabela6 so podatki o številu prebivalstva po regijah po letih

# tabela6 <- read_xlsx("podatki/stevilo_prebivalcev_po_regijah.xlsx",col_names=c("regija", "leto", "prebivalci"), skip = 2)

#v tabela1nova združimo tabela1 in tabela3(stevilo dijakov in diplomantov po regijah)

tabela1nova <- tabela1 %>%
  mutate(leto=parse_number(leto), 
         regija=gsub("^stalno", "Stalno", regija)) %>% 
  full_join(tabela3) %>% 
  filter(regija != "SLOVENIJA") %>% 
  gather(key="kategorija", value="stevilo", -regija, -leto) 

# v tabela2nova so podatki o stevilu dijakov po spolu in vrsti izobrazevanja

tabela2nova <- gather(tabela2, -izobrazevanje, -leto, key=spol, value=stevilo, na.rm = TRUE)

# v tabela3nova so podatki o stevilu diplomantov po spolu in vrsti izobrazevanja

tabela3nova <- gather(tabela4, -izobrazevanje, -leto, key=spol, value=stevilo, na.rm = TRUE)

# v tabela4nova so podatki o stevilu studentov po vrsti izobrazevanja in nacinu studija

tabela4nova <- gather(tabela5, -izobrazevanje, -leto, key=studij, value=stevilo, na.rm = TRUE)

