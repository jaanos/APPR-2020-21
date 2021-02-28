#VIZUALIZACIJA


#PE GRAF

pe_graf <-right_join(podatki_quandl_pe,PE) %>%  ggplot() + 
  geom_line(aes(x=Leto,y=P.E_SP500,colour="SP 500"),size=1) + 
  geom_line(aes(x=Leto,y=PE,colour="Najvišji P/E Applove delnice"),size=1) +
  scale_colour_manual(name="LEGENDA",values=c("red","skyblue"))+
  labs(title="Primerjava P/E")+
  scale_x_continuous(name = "Leto", breaks = seq(2011,2020,1))+
  scale_y_continuous(name = "P/E", breaks = seq(0,50,2))+
  theme(legend.position = c(0.3, 0.8),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5))
  

#RAST KNJIGOVODSKE VREDNOSTI

colnames(Rast_knjigovodske_vrednosti) <- c("Leto","Rast1")
rast_knjigovodske_graf <- right_join(Rast_knjigovodske_vrednosti,Rast_SP_knjigovodske_vrednosti)%>%
  filter(Leto>2011)%>%
  ggplot() + geom_line(aes(x=Leto,y=Rast1,colour="Rast Applove kjigovodkse vrednosti"),size=1) + 
  geom_line(aes(x=Leto,y=Rast,colour="Rast knjigovodske vrednosti SP 500"),size=1) +
  scale_colour_manual(name="LEGENDA",values=c("red","skyblue"))+
  labs(title="Primerjava rasti knjigovodkse vrednosti Appla in SP 500")+
  scale_x_continuous(name = "Leto", breaks = seq(2012,2020,1))+
  scale_y_continuous(name = "Rast(%)", breaks = seq(-30,50,5))+
  theme(legend.position = c(0.4, 0.8),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5))


#TORTNA GRAFA

priprava <- select(morningstar,Leto,Prodaja,Neto_dobicek)%>%mutate(Stroski=Prodaja-Neto_dobicek)%>%
 select(Leto,Neto_dobicek,Stroski)%>% filter(Leto==2020)
priprava1<-as.data.frame(t(priprava[-1]))%>%mutate(delez=100*V1/sum(V1))%>%
  arrange(desc(V1)) %>%
  mutate(f=cumsum(delez) - 0.5*delez)

graf_tortni <- ggplot(priprava1) +
  aes(x="", y=delez, fill=c("Stroški","Dobiček")) +
  geom_bar(stat="identity",width=1) +
  coord_polar("y",start = 0) +
  geom_text(aes(y=f, label=paste0(round(delez, 2), "%")),
          x=1.3, color="white", size=4) +
  labs(title="Profitna marža podjetja Apple",fill="LEGENDA") +
  scale_fill_manual(values = c("#FF0033","#990000"))+
  theme_void()+
  theme(plot.title = element_text(hjust = 0.5))
  

 
priprava_SP <- right_join(podatki_quandl_prodaja,podatki_quandl_earning)%>%
  mutate(Stroski=Prodaja_SP500-Earning_SP500)%>%
  select(Leto,Earning_SP500,Stroski)%>% filter(Leto==2020)
priprava1_SP<-as.data.frame(t(priprava_SP[-1]))%>%mutate(delez1=100*V1/sum(V1))
colnames(priprava1_SP) <- c("vrednosti","delezSP")
priprava1_SP<- arrange(priprava1_SP,desc(vrednosti)) %>% 
  mutate(g=cumsum(delezSP) - 0.5*delezSP)

graf_tortni_SP <- ggplot(priprava1_SP) +
  aes(x="", y=delezSP, fill=c("Stroški","Dobiček")) +
  geom_bar(stat="identity",width=1)+
  coord_polar("y",start = 0)  +
  geom_text(aes(y=g, label=paste0(round(delezSP, 2), "%")),
            x=1.3, color="white", size=4) +
  labs(title="Povprečna profitna marža za podjetja v SP500",fill="LEGENDA")+
  scale_fill_manual(values = c("#00FFFF","#0000FF"))+
  theme_void()+
  theme(plot.title = element_text(hjust = 0.5))


#ZEMLJEVID

#Uvoz zemljevida
svet <- uvozi.zemljevid("https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/cultural/ne_110m_admin_0_countries.zip",
                        ime.zemljevida = "ne_110m_admin_0_countries",encoding="UTF-8")%>%
  fortify() 
#Podatki prodaje po svetu
svet_prodaja <- filter(podatki_prodaja_svet,Podatki=="Net sales"&Leto=="2019")%>%
  mutate(celina=c("Americas","Europe","China","Japan","Oceania"))
  
#Podatki za zemljevid
svet1<-right_join(svet,kontinenti,by="GU_A3")%>%
  mutate(celina= ifelse(GU_A3=="CHN","China",
                     ifelse(GU_A3=="JPN","Japan",
                            ifelse(Continent_Name=="Asia","Europe",
                                   ifelse(Continent_Name=="North America","Americas",
                                          ifelse(Continent_Name=="Africa","Europe",
                                                  ifelse(Continent_Name=="South America","Americas",Continent_Name)))))))%>%
  right_join(svet_prodaja)%>%rename("Drzava"="Country_Name") %>% 
  select("long","lat","celina","Drzava","Vrednost","group")

#Izris zemlejvida
zemljevid <- ggplot(svet1,aes(x=long,y=lat,group=group, fill=Vrednost))+
  geom_polygon()+
  ggtitle("Prodaja po celinah") + 
  theme(axis.title=element_blank(), axis.text=element_blank(), 
        axis.ticks=element_blank(), panel.background = element_blank()) +
  scale_fill_gradient(low = "#330000", high="#FF0033",limits=c(0,120000)) +
  labs(fill="Prodaja v USD")




