# 3. faza: Vizualizacija podatkov
library(jcolors)
library(dplyr)
#uvozimo zemljevid Evrope

zemljevid <- uvozi.zemljevid(
  "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip", "ne_50m_admin_0_countries", encoding="UTF-8")
zemljevid <- zemljevid[zemljevid$CONTINENT == "Europe",]


#-----------------------------------------------------------------------------------------------------------------------------------------------------
#Grafi o skupnem priseljevanju v Slovenijo

skupnopriseljevanje <- tabela1 
skupno_leta <- skupnopriseljevanje %>% group_by(Leto) %>% summarise(Vsota=sum(Priseljeni_iz_tujine))
skupno_leta_mz <- skupnopriseljevanje %>% group_by(Leto,Spol) %>% summarise(Vsota_mz=sum(Priseljeni_iz_tujine))

graf1 <- ggplot(data=skupno_leta, aes(x=Leto, y=Vsota)) + 
  geom_point(color=rgb(0.8,0.4,0.1,0.7))+
  geom_line(color=rgb(0.8,0.4,0.1,0.7)) +
  ylab('Skupno število priseljencev') + 
  xlab('Leto') + 
  ggtitle('Število priseljenih tujcev v Slovenijo skozi leta') +
  scale_x_continuous(breaks = 1*2011:2020) + 
  theme(axis.text.x=element_text(angle=0))

# graf 2: število priseljenih po spolu__________________________-

graf2 <- skupno_leta_mz %>%  ggplot(aes(x=Leto, y=Vsota_mz, col=Spol, palette="Pastel1")) + 
  geom_line() +
  ylab('Število priseljenih') + 
  xlab('Leto') + 
  labs(col = "Spol")+
  ggtitle('Število priseljenih ljudi v Slovenijo') +
  scale_x_continuous(breaks = 1*2011:2020) +
  theme(axis.text.x=element_text(vjust=0.5, hjust=0.5))

# graf 3: število priseljenih po državi:

graf3 <- skupnopriseljevanje %>%  ggplot(aes(x=Leto, y=Priseljeni_iz_tujine, col=Drzava, palette = "default")) + 
  geom_point() +
  ylab('Število priseljenih') + 
  xlab('Leto') + 
  ggtitle('Število priseljenih ljudi v Slovenijo') +
  scale_x_continuous(breaks = 1*2011:2020) +
  theme(axis.text.x=element_text(vjust=0.5, hjust=0.5))

#_____________________________________________________________________
#naredila graf skupaj s podatki z izseljenimi in priseljenimi
tabela2 <- tabela2 %>% rename("Drzava"="Drzava_prihodnjega_bivalisca") 
priseljeniinizseljeni <- inner_join(tabela1, tabela2, by=NULL) %>% mutate(Selitveni_prirast=Priseljeni_iz_tujine-Odseljeni_v_tujino) 
skupnopi_leta <- priseljeniinizseljeni %>% group_by(Leto) %>% summarise(Priseljeni_iz_tujine=sum(Priseljeni_iz_tujine), Odseljeni_v_tujino=sum(Odseljeni_v_tujino), Selitveni_prirast=sum(Selitveni_prirast))   
skupnopi_leta <- pivot_longer(skupnopi_leta,2:4, names_to="Vrsta", values_to="Stevilo") 

graf4 <- skupnopi_leta %>%  ggplot(aes(x=Leto, y=Stevilo, col=Vrsta, palette="Pastel1")) + 
  geom_line() +
  geom_point()+
  ylab('Število ljudi') + 
  xlab('Leto') + 
  labs(col = "Legenda:")+
  ggtitle('Število priseljenih ali izseljenih ljudi v Sloveniji') +
  scale_x_continuous(breaks = 1*2011:2020) +
  theme(axis.text.x=element_text(vjust=0.5, hjust=0.5))

# graf 5: število izseljenih po spolu
skupno_leta_mz2 <- tabela2 %>% group_by(Leto,Spol) %>% summarise(Vsota_mz2=sum(Odseljeni_v_tujino))
graf5 <- skupno_leta_mz2 %>%  ggplot(aes(x=Leto, y=Vsota_mz2, col=Spol, palette="Pastel1")) + 
  geom_line() +
  ylab('Število ljudi') + 
  xlab('Leto') + 
  labs(col = "Spol")+
  ggtitle('Število odseljenih ljudi iz Slovenije') +
  scale_x_continuous(breaks = 1*2011:2020) +
  theme(axis.text.x=element_text(vjust=0.5, hjust=0.5))

#graf 6: število izseljenih po državi
graf6 <- tabela2 %>%  ggplot(aes(x=Leto, y=Odseljeni_v_tujino, col=Drzava, palette = "default")) + 
  geom_point() +
  ylab('Število izseljenih') + 
  xlab('Leto') + 
  ggtitle('Izseljevanje iz Slovenije') +
  scale_x_continuous(breaks = 1*2011:2020) +
  theme(axis.text.x=element_text(vjust=0.5, hjust=0.5))


