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
#naredila graf skupaj s podatki z izseljenimi in priseljenimi, število, letno
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

# STAROST
#Starost izseljencev
starosti <- c(seq(15, 65, 5) %>% paste0(., "-", .+4), "65 +")
starost_graf1 <- ggplot(data=tabela3, aes(x=Leto, y=Stevilo_odseljenih, fill=factor(Starost, starosti))) +
  geom_bar(stat="identity") + scale_x_continuous(breaks = 1*2011:2020) + ggtitle("Starost pri izselitvi iz Slovenije") +
  xlab("Leto") + ylab("Število") + labs(fill='Starost') + theme(axis.text.x=element_text(angle=0))
#Starost priseljencev
starosti2 <- c("0-14", seq(15, 65, 5) %>% paste0(., "-", .+4), "65 +")
starost_graf2 <- ggplot(data=tabela4, aes(x=Leto, y=Stevilo_priseljenih, fill=factor(Starost, starosti2))) +
  geom_bar(stat="identity") + scale_x_continuous(breaks = 1*2011:2020) + ggtitle("Starost pri priselitvi v Slovenijo") +
  xlab("Leto") + ylab("Število") + labs(fill='Starost') + theme(axis.text.x=element_text(angle=0))


# IZOBRAZBA
tabela5 <- tabela5 %>% rename("Drzava"="Drzava_prihodnjega_bivalisca")
tabela6 <- tabela6 %>% rename("Stevilo_priseljenih"="Stevilo") 
tabela_izobrazba <- inner_join(tabela5, tabela6, by=NULL) %>% 
  pivot_longer(5:6, names_to="Vrsta", values_to="Stevilo") 

vrsta_izobrazbe <- c("Osnovnošolska ali manj",	"Srednješolska","Višješolska, visokošolska")
izobrazba_graf <- ggplot(data=tabela_izobrazba, aes(x=Leto, y=Stevilo, fill=factor(Izobrazba, vrsta_izobrazbe))) +
  geom_bar(stat="identity") + facet_wrap(.~Vrsta) + scale_x_continuous(breaks = 1*2011:2020) + ggtitle("Izobrazba ljudi, ki so se preselili ali izselili") +
  xlab("Leto") + ylab("Število") + labs(fill='vrsta_izobrazbe') + theme(axis.text.x=element_text(angle=90))


#NAMEN PRISELITVE

namen_priselitve <- ggplot(tabela7, aes(x = factor(1), y = Stevilo, fill = Namen)) + xlab("") + ylab("") +
  geom_bar(width = 1, stat = "identity") + ggtitle("Namen preselitve v Slovenijo za priseljence v letih od 2011 do 2019") + theme(legend.title=element_blank())


namen_priselitve <- namen_priselitve + coord_polar("y", start=0)

#DEJAVNOST

#Priseljeni prebivalci po dejavnosti
skupno_dejavnost <- tabela8 %>% group_by(Dejavnost) %>% summarise(Vsota=sum(Stevilo)) %>% mutate(Povprecje=round(Vsota/9,1))

dejavnost <- skupno_dejavnost %>% ggplot(aes(x=Dejavnost, y=Povprecje,fill=Povprecje)) + 
  geom_bar(position="dodge", stat="identity") + 
  ylab('Povrečje') + 
  ggtitle('Povprečno število priseljencev glede na dejavnost v letih 2011-19') +
  theme(text = element_text(size=5), axis.text.x=element_text(vjust=0.5, hjust=0.5, angle=90))

#Izseljeni prebivalci po dejavnosti
skupno_dejavnost2 <- tabela9 %>% group_by(Dejavnost) %>% summarise(Vsota=sum(Stevilo)) %>% mutate(Povprecje=round(Vsota/9,1))

dejavnost2 <- skupno_dejavnost2 %>% ggplot(aes(x=Dejavnost, y=Povprecje,fill=Povprecje)) + 
  geom_bar(position="dodge", stat="identity") + 
  ylab('Povprečje') + 
  ggtitle('Povprečno število priseljencev glede na dejavnost v letih 2011-19') +
  theme(text = element_text(size=5), axis.text.x=element_text(vjust=0.5, hjust=0.5, angle=90))


#Kaj moram še narediti?
# tabela: group by država (priseljevanje in izseljevanje za Slo) in nek graf, ki kaže koliko se je agregatno preselilo ljudi iz katerih držav/kam smo mi šli
# Primerjava BDP in države kamor se Slovenci največ preseljujejo
# Primerjava BDP in država od koder se priseljujejo v Slovenijo
#1. Zemljevid priseljevanja v Evropi - povprečje
#2. Zemljevid izseljevanja v Evropi - povprečje
#3. Zemljevid izseljevanja v regijah - Slovenija - povprečje
#4. Zemljevid priseljevanja v regijah - Slovenija - povprečje
#5. Zemljevid iz kje se največ preseljujejo v Slovenijo - glede na tabeli 1/2 (2 zemljevida...)


