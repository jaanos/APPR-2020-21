source("lib/libraries.r", encoding="UTF-8")
source("uvoz/drzave.r", encoding="UTF-8")

#povezave_drzave <- paste("https://eurovision.tv/country/", tolower(gsub("\\W+", "-", vektor_drzave)), sep = '')
povezave_drzave <- link_drzave %>%
  html_nodes(xpath = "//div[@class='flex flex-wrap']//a") %>%
  html_attr("href")

podatki_drzave <- lapply(povezave_drzave, . %>% read_html() %>%
    html_nodes(xpath = "//div[@class='space-y-4']//dd[@class='text-sm font-bold']") %>%
    html_text())

nastopi_drzave <- lapply(podatki_drzave, . %>% .[[3]] %>%
    lapply(function(x) {gsub("\n", "", x)})) %>% unlist() %>% as.numeric()

# to je število nastopov vseh držav

# prvi nastop vseh držav

prvic_drzave <- lapply(podatki_drzave, . %>% .[[4]] %>%
    lapply(function(x) {regmatches(x, regexpr("\\d{4}", x))})) %>%
  unlist() %>% as.numeric()

tabela_nastopi <- data.frame("Drzava"=vektor_drzave,
                             "Stevilo_nastopov"=nastopi_drzave,
                             "Prvi_nastop"=prvic_drzave,
                             stringsAsFactors=FALSE)

write_csv(tabela_nastopi, "podatki/tabela_nastopi.csv")
