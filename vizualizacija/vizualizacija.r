# 3. faza: Vizualizacija podatkov

library(ggplot2)
library(readr)

a1 <- t_e %>% mutate(emp2=parse_number(emp, locale=locale(decimal_mark = ",",grouping_mark = ".") )) 

graf1 <- a1 %>% filter(occ_title == "All Occupations") %>%
  ggplot(aes(x=leto, y=emp2)) + 
  geom_line(size=2, colour="green") + 
  geom_point(size=4, colour="blue") +
  xlab("Leto") +
  ylab("Število zaposlednih") +
  labs(title="Zaposlenost po letih") 

# porazdelitev 
graf2 <- a1 %>% filter(occ_title == "All Occupations") %>%
  ggplot(aes(x=leto)) + 
  xlab("Leto") +
  geom_density() 


# histogram
 
graf3 <- a1 %>% filter(occ_title == "All Occupations") %>%
  ggplot(aes(x=leto, fill=emp2)) + 
  ylab("Število zaposlednih") +
  xlab("Leto") +
  geom_histogram(color="red") 


# ločeni grafi facet_grif(~ime_stolpca), bomo uporabili za države kaneje

