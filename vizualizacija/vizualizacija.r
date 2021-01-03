# 3. faza: Vizualizacija podatkov

# TOP 5 ZAPOSLENOSTI PO POKLICIH PO LETIH (NE VEM ČE BOMO UPORABILI)

#                                              te_08 <- t_e %>% filter(leto=="2008") %>%  tail(n = 6) %>% head(n = 5)
#                                              te_10 <- t_e %>% filter(leto=="2010") %>%  tail(n = 6) %>% head(n = 5)
#                                              te_12 <- t_e %>% filter(leto=="2012") %>%  tail(n = 6) %>% head(n = 5)
#                                              te_14 <- t_e %>% filter(leto=="2014") %>%  tail(n = 6) %>% head(n = 5)
#                                              te_16 <- t_e %>% filter(leto=="2016") %>%  tail(n = 6) %>% head(n = 5)
#                                              te_18 <- t_e %>% filter(leto=="2018") %>%  tail(n = 6) %>% head(n = 5)

# PRIPRAVA ZA ANALIZO 

# TOP 5 in BOTTOM 5

### MEAN DATA:

## GLEDE URNE POSTAVKE TOP

#          h_mean_08_T <- h_mean %>% filter(leto=="2008") %>%  tail(n = 5) 
#          h_mean_10_T <- h_mean %>% filter(leto=="2010") %>%  tail(n = 5)
#          h_mean_12_T <- h_mean %>% filter(leto=="2012") %>%  tail(n = 5) 
#          h_mean_14_T <- h_mean %>% filter(leto=="2014") %>%  tail(n = 5) 
#          h_mean_16_T <- h_mean %>% filter(leto=="2016") %>%  tail(n = 5) 
#          h_mean_18_T <- h_mean %>% filter(leto=="2018") %>%  tail(n = 5) 


## GLEDE URNE POSTAVKE BOTTOM 

#            h_mean_08_B <- h_mean %>% filter(leto=="2008") %>%  slice(1:5)
#            h_mean_10_B <- h_mean %>% filter(leto=="2010") %>%  slice(1:5)
#            h_mean_12_B <- h_mean %>% filter(leto=="2012") %>%  slice(1:5)
#            h_mean_14_B <- h_mean %>% filter(leto=="2014") %>%  slice(1:5)
#            h_mean_16_B <- h_mean %>% filter(leto=="2016") %>%  slice(1:5)
#            h_mean_18_B <- h_mean %>% filter(leto=="2018") %>%  slice(1:5)
#            
#            ## GLEDE AVRAGE TOP
#            
#            a_mean_08_T <- a_mean %>% filter(leto=="2008") %>%  tail(n = 5) 
#            a_mean_10_T <- a_mean %>% filter(leto=="2010") %>%  tail(n = 5)
#            a_mean_12_T <- a_mean %>% filter(leto=="2012") %>%  tail(n = 5) 
#            a_mean_14_T <- a_mean %>% filter(leto=="2014") %>%  tail(n = 5) 
#            a_mean_16_T <- a_mean %>% filter(leto=="2016") %>%  tail(n = 5) 
#            a_mean_18_T <- a_mean %>% filter(leto=="2018") %>%  tail(n = 5) 

## GLEDE AVRAGE BOTTOM

#    a_mean_08_B <- a_mean %>% filter(leto=="2008") %>%  slice(1:5)
#    a_mean_10_B <- a_mean %>% filter(leto=="2010") %>%  slice(1:5)
#    a_mean_12_B <- a_mean %>% filter(leto=="2012") %>%  slice(1:5)
#    a_mean_14_B <- a_mean %>% filter(leto=="2014") %>%  slice(1:5)
#    a_mean_16_B <- a_mean %>% filter(leto=="2016") %>%  slice(1:5)
#    a_mean_18_B <- a_mean %>% filter(leto=="2018") %>%  slice(1:5)
#   


# GRAF, KI ANALIZIRA ZAPOSLITEV

graf1 <- t_e %>% filter(occ_code == "00-0000") %>%
  ggplot(aes(x=leto, y=emp)) + 
  geom_line(size=2, colour="green") + 
  geom_point(size=4, colour="blue") +
  ylab("Število zaposlednih") +
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_continuous(name = "Leto", breaks = seq(2006,2018,1)) + 
  labs(title="Zaposlenost po letih") +
  stat_smooth(method = "lm") +
  theme(axis.line = element_line(colour = "darkblue", 
                                 size = 1, linetype = "solid"),
        axis.text.x = element_text(face="plain", color="red", 
                                   size=10, angle=7),
        axis.text.y = element_text(face="bold", color="red", 
                                   size=10, angle=7))

# PORAZDELITEV TOP3 PLAC PO LETIH
joined <- inner_join(nat.ha, kode, by="occ_code") 
pr <- joined %>% filter(sredina=="mean") 
top <- pr[pr$occ_code %in% c("29-1067","29-1061","29-1023"), ]  %>% 
  rename(poklic=occ_title)


graf2 <- top %>%
  ggplot(aes(x=poklic, y=h)) +
  xlab("Poklic") + 
  ylab("Urna postavka plač") + 
  geom_boxplot(fill="red", colour="red" , alpha=I(0.2)) +
  geom_jitter(alpha=I(0.2)) +
  geom_point() + 
  labs(title="Porazdelitev plač boljše plačanih poklicev") 

# TOP 3 GRAFICNO PO DREVESIH

# graf4* <- top %>%
#  ggplot(aes(x=leto,y=HM)) +
#  geom_line(color="blue") + 
#  geom_point() + 
#  facet_grid(~poklic)

# graf4* <- top %>%
#   ggplot(aes(x=leto,y=HM)) +
#   geom_line(color="blue") + 
#   geom_point() +
#   facet_grid(occ_title~.)

# TOP 3 GLEDE NA URNE POSTAVEK 

graf3 <- top %>%
  ggplot(aes(x=leto, y=h, col=poklic)) +
  xlab("Leto") +
  ylab("Povprečna plača na uro") +
  labs(title="Povprečna urna plača treh najboljše plačanih poklicev.") +
  geom_line(size=1) +
  geom_point(size=2)

# BOTTOM 3 GLEDE NA AVRAGE WAGE 
a_b <- pr[pr$occ_code %in%  c("39-5093","35-9021"), ] %>% 
  rename(poklic=occ_title)

graf4 <- a_b %>%
  ggplot(aes(x=leto,y=a)) +
  geom_line(color="blue") +
  geom_point() +
  xlab("Leto") +
  ylab("Povprečna plača") +
  labs(title="Povprečna plača dveh najslabše plačanih poklicev.") +
  facet_grid(~poklic) 

# Zaposlenost top 3 poklicev in bot. 2 poklicev
joined_t_e <- inner_join(t_e, kode, by="occ_code") 
t_e_l_b <- joined_t_e[joined_t_e$occ_code %in%  
          c("39-5093","35-9021","29-1067","29-1061","29-1023"), ] %>% 
         rename(poklic=occ_title)

graf5 <- t_e_l_b %>% filter(leto=="2018") %>%
  ggplot() +
  aes(x="", y=emp, fill=poklic) + 
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ",")) +
  geom_col(width=1) +
  coord_polar(theta="y") + xlab("") + ylab("") +
  labs(title="Deleži poklicev (najboljše plačanih v primeravi z najmanj) v letu 2018")+
  guides(fill=guide_legend("Poklic"))

# MAPE ZVEZNIH DRZAV (D.F.)

states <- map_data("state") 



# Izriše ZDA 

# mapa_zda <- states %>% 
#   ggplot(aes(x=long, y=lat, group=group, color=region)) +
#   geom_polygon() +
#   labs(title="ZDA") +
#   theme(legend.position = "none") 

# mapa_zda2 <- getMap(resolution="low") %>% 
#   fortify() %>%
#   View()


# POVPREČNE PLAČE (URNE) V ZDA 

zemljevid <- uvozi.zemljevid("http://baza.fmf.uni-lj.si/states_21basic.zip", "states",
                             encoding="UTF-8") 

map1_db <- st.ha  %>%
  filter(sredina=="median") %>%
  drop_na(h) %>%
  group_by(state) %>% 
  summarise(povprecje= mean(h))

#names(zemljevid)
zem1 <-  tm_shape(merge(zemljevid, map1_db, by.x="STATE_NAME", by.y="state")) +
  tm_polygons("povprecje") +
  tm_style("cobalt") 

graf8 <- GDP_by_state %>% filter(State=="Alaska") %>%
  ggplot(aes(x=leto, y=GDP)) + 
  geom_point() +
  geom_line()



