# 3. faza: Vizualizacija podatkov
# vzamemo prvi 4 vrstice
# a2 <- a1 %>% slice(1:4)
# tail, head isto 

source("lib/uvozi.zemljevid.r",encoding="UTF-8")
library(ggplot2)
library(readr)
library(scales)
library(rgdal)
library(mosaic) 
library(maps)
library(tmap)
library(tidyverse)

# ZAPOSLENOST PO LETIH, BOMO UPORABILI PRI KASNEJSI ANALIZI


# a <- t_e %>% mutate(emp2=parse_number(emp, locale=locale(decimal_mark = ",",grouping_mark = ".") )) %>% arrange(emp2)

# TOP 5 ZAPOSLENOSTI PO POKLICIH PO LETIH

te_08 <- t_e %>% filter(leto=="2008") %>%  tail(n = 6) %>% head(n = 5)
te_10 <- t_e %>% filter(leto=="2010") %>%  tail(n = 6) %>% head(n = 5)
te_12 <- t_e %>% filter(leto=="2012") %>%  tail(n = 6) %>% head(n = 5)
te_14 <- t_e %>% filter(leto=="2014") %>%  tail(n = 6) %>% head(n = 5)
te_16 <- t_e %>% filter(leto=="2016") %>%  tail(n = 6) %>% head(n = 5)
te_18 <- t_e %>% filter(leto=="2018") %>%  tail(n = 6) %>% head(n = 5)

# PRIPRAVA ZA ANALIZO 

# TOP 5 in BOTTOM 5

### MEAN DATA:

## GLEDE URNE POSTAVKE TOP

h_mean_08_T <- h_mean %>% filter(leto=="2008") %>%  tail(n = 5) 
h_mean_10_T <- h_mean %>% filter(leto=="2010") %>%  tail(n = 5)
h_mean_12_T <- h_mean %>% filter(leto=="2012") %>%  tail(n = 5) 
h_mean_14_T <- h_mean %>% filter(leto=="2014") %>%  tail(n = 5) 
h_mean_16_T <- h_mean %>% filter(leto=="2016") %>%  tail(n = 5) 
h_mean_18_T <- h_mean %>% filter(leto=="2018") %>%  tail(n = 5) 

# IZ TABEL VIDIMO, DA SO Anesthesiologists in surgeons VEDNO NA VRHU
surgeons_h <- h_mean %>% filter(occ_title == "Surgeons") 
anesthesiologists_h <- h_mean %>% filter(occ_title == "Anesthesiologists")
top <- rbind(surgeons_h, anesthesiologists_h) 

## GLEDE URNE POSTAVKE BOTTOM 

h_mean_08_B <- h_mean %>% filter(leto=="2008") %>%  slice(1:5)
h_mean_10_B <- h_mean %>% filter(leto=="2010") %>%  slice(1:5)
h_mean_12_B <- h_mean %>% filter(leto=="2012") %>%  slice(1:5)
h_mean_14_B <- h_mean %>% filter(leto=="2014") %>%  slice(1:5)
h_mean_16_B <- h_mean %>% filter(leto=="2016") %>%  slice(1:5)
h_mean_18_B <- h_mean %>% filter(leto=="2018") %>%  slice(1:5)

## GLEDE AVRAGE TOP

a_mean_08_T <- a_mean %>% filter(leto=="2008") %>%  tail(n = 5) 
a_mean_10_T <- a_mean %>% filter(leto=="2010") %>%  tail(n = 5)
a_mean_12_T <- a_mean %>% filter(leto=="2012") %>%  tail(n = 5) 
a_mean_14_T <- a_mean %>% filter(leto=="2014") %>%  tail(n = 5) 
a_mean_16_T <- a_mean %>% filter(leto=="2016") %>%  tail(n = 5) 
a_mean_18_T <- a_mean %>% filter(leto=="2018") %>%  tail(n = 5) 

## GLEDE AVRAGE BOTTOM

a_mean_08_B <- a_mean %>% filter(leto=="2008") %>%  slice(1:5)
a_mean_10_B <- a_mean %>% filter(leto=="2010") %>%  slice(1:5)
a_mean_12_B <- a_mean %>% filter(leto=="2012") %>%  slice(1:5)
a_mean_14_B <- a_mean %>% filter(leto=="2014") %>%  slice(1:5)
a_mean_16_B <- a_mean %>% filter(leto=="2016") %>%  slice(1:5)
a_mean_18_B <- a_mean %>% filter(leto=="2018") %>%  slice(1:5)




# GRAF, KI ANALIZIRA ZAPOSLITEV

graf1 <- t_e %>% filter(occ_title == "All Occupations") %>%
  ggplot(aes(x=leto, y=emp)) + 
  geom_line(size=2, colour="green") + 
  geom_point(size=4, colour="blue") +
  xlab("Leto") +
  ylab("Število zaposlednih") +
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ",")) +
  # scale_x_continuous(limits=c(2006, 2018)) +
  labs(title="Zaposlenost po letih") +
  stat_smooth(method = "lm") +
  theme(axis.line = element_line(colour = "darkblue", 
                                 size = 1, linetype = "solid"),
        axis.text.x = element_text(face="plain", color="red", 
                                   size=10, angle=7),
        axis.text.y = element_text(face="bold", color="red", 
                                   size=10, angle=7))


# PORAZDELITEV TOP PLAC PO LETIH

graf2 <- top %>%
  ggplot(aes(x=occ_title, y=HM)) +
  geom_boxplot(fill="red", colour="red" , alpha=I(0.2)) +
  geom_jitter(alpha=I(0.2)) +
  geom_point() 

# S. IN A.  

graf3 <- top %>%
 ggplot(aes(x=leto,y=HM)) +
 geom_line(color="blue") + 
 geom_point() + 
 facet_grid(~occ_title)

# graf3 <- top %>%
#   ggplot(aes(x=leto,y=HM)) +
#   geom_line(color="blue") + 
#   geom_point() +
#   facet_grid(occ_title~.)

graf4 <- top %>%
  ggplot(aes(x=leto,y=HM, col=occ_title)) +
  xlab("Leto") +
  ylab("Povprečna plača na uro") +
  labs(title="Povprečna plača") +
  geom_line(size=1) +
  geom_point(size=2) 
  




# MAPE ZVEZNIH DRZAV

states <- map_data("state")

