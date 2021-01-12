library(readxl)
library(openxlsx)
require(tidyr)
require(readr)
require(dplyr)

leta <- c(7, 8, 9, 15, 16, 17, 18, 19, 20)
drzave <- c("SLO"="Slovenia", "ZDA"="United States of America")
podatki <- lapply(leta, function(leto)
  lapply(names(drzave), function(drzava)
    sprintf('projekt/%s%02d.xlsx', drzava, leto) %>% read_xlsx() %>%
      mutate(Drzava=drzave[drzava], Leto=2000+leto)) %>% bind_rows()) %>%
  bind_rows() %>% transmute(Leto, Drzava, Mesto, Film,
                            Zasluzek=parse_number(`Zaslužek v $`))

podatki %>% drop_na(Mesto)


require(rvest)
require(xml2)
stran <- "https://www.the-numbers.com/market/2019/genre/Adventure"

stran <- stran %>% html_nodes(xpath="//table") %>% .[[2]] %>% html_table()

require(ggplot2)
