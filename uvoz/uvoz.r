require(dplyr)

leta <- c(7, 8, 9, 15, 16, 17, 18, 19, 20)
drzave <- c("SLO"="Slovenija", "ZDA"="United States of America")
podatki <- lapply(leta, function(leto)
  lapply(names(drzave), function(drzava)
    sprintf('podatki/%s%02d.xlsx', drzava, leto) %>% read_xlsx() %>%
      mutate(Drzava=drzave[drzava], Leto=2000+leto)) %>% bind_rows()) %>%
  bind_rows() %>% transmute(Leto, Drzava, Mesto, Film,
                            Zasluzek=parse_number(`Zaslužek v $`))


podatki %>% drop_na(Zasluzek)

zasluzki.po.letih <- data.frame(rep(leta+2000,2), rep(c('Slovenija', 'ZDA'), 9))
zasluzki.po.letih %>% arrange('Leto') 
names(zasluzki.po.letih)[1] <- 'Leto' 
names(zasluzki.po.letih)[2] <- 'Država'
zasluzki.po.letih %>% mutate('Zaslužek'= 69420)
zasluzki.po.letih %>% spread('Država', 'Zaslužek')
  
