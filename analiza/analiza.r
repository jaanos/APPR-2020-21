#ANALIZA


#MODEL LOESS ZA NAPOVED EARNINGOV

model_earning <-loess(dat=morningstar,
                      Neto_dobicek_na_delnico~Leto,control=loess.control(surface="direct"))

loe_ear <- function(zeljena_leta){
prihodnost_dobicek_na_delnico <- data.frame(Leto=c(zeljena_leta))
napoved_dobicek_na_delnico <- prihodnost_dobicek_na_delnico %>%
  mutate(Neto_dobicek_na_delnico=predict(model_earning,.))

dobicek_na_delnico_graf <- ggplot(data=morningstar,
                                  aes(x=Leto,y=Neto_dobicek_na_delnico,color="Dobiček na delnico"))+
  geom_smooth(method = "loess",formula = y~x,se=F) +
  geom_point(aes(color="Točne vrdednosti"))+
  geom_point(data=napoved_dobicek_na_delnico,
             aes(x=Leto,y=Neto_dobicek_na_delnico,color="Napoved dobička za naprej"),size=3)+
  scale_colour_manual(name="LEGENDA",values=c("red","green","black"))+
  labs(title="Graf dobička na delnico po metodi loess")+
  scale_x_continuous(name = "Leto", breaks = seq(2011,2025,1))+
  scale_y_continuous(name = "Dobiček na delnico (USD)", breaks = seq(0,5,0.2))+
  theme(legend.position = c(0.3, 0.8),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5))
return(dobicek_na_delnico_graf)}


#MODEL NAPOVEDI EARNINGOV REGRESIJA

model_dobicek <- lm(data=morningstar,Neto_dobicek_na_delnico ~ Leto) #ce klices ta model na fukcijo reg_pov ali opt_pov moras leta vnesti kot data frame

reg_ear<- function(zeljena_leta){
prihodnost_dobicek <- data.frame(Leto=c(zeljena_leta))
napoved_dobicek <- prihodnost_dobicek%>%
  mutate(Neto_dobicek_na_delnico=predict(model_dobicek, .))

premica_dobicek<-ggplot(morningstar,aes(x=Leto,y=Neto_dobicek_na_delnico,
                                        color="Dobiček na delnico"))+
  geom_point(aes(color="Točne vrdednosti"))+
  geom_smooth(method = lm,formula = y~x,se=F)+
  geom_point(data = napoved_dobicek,aes(x=Leto,y=Neto_dobicek_na_delnico,
                                        color="Napoved dobička za naprej"),size=3)+
  scale_colour_manual(name="LEGENDA",values=c("red","green","black"))+
  labs(title="Graf dobička na delnico s pomočjo linearne regresije")+
  scale_x_continuous(name = "Leto", breaks = seq(2011,2025,1))+
  scale_y_continuous(name = "Dobiček na delnico (USD)", breaks = seq(0,5,0.2))+
  theme(legend.position = c(0.3, 0.8),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5))
return(premica_dobicek)}


#REGRESIJA ZA POVEZAVO MED EARNINGI IN CENO

yah.mor <- right_join(morningstar,yahoo)
reg_pov <- function(prihodnost_dobicek_na_delnico,vrsta_modela){
  model_odvisnost1 <- lm(data=yah.mor,Najvisja_cena~Neto_dobicek_na_delnico)
  prihodnost_povezava1 <- data.frame(Neto_dobicek_na_delnico=c(predict(vrsta_modela,prihodnost_dobicek_na_delnico)))
  napoved_povezava1 <- prihodnost_povezava1%>%
    mutate(Najvisja_cena=predict(model_odvisnost1, .))

  premica_odvisnost<-ggplot(yah.mor,aes(x=Neto_dobicek_na_delnico,y=Najvisja_cena,
                                        color="Približek za naše podatke"))+
    geom_point(aes(color="Točne vrednosti"))+
    geom_smooth(method = lm,formula = y~x,se=F)+
    geom_point(data = napoved_povezava1,
             aes(x=Neto_dobicek_na_delnico,y=Najvisja_cena,
                 color="Napoved cene preko dobička"),size=3)+
    scale_colour_manual(name="LEGENDA",values=c("green","red","black"))+
    labs(title="Napoved cene preko dobička s pomočjo linearne regresije")+
    scale_x_continuous(name = "Dobiček na delnico (USD)", breaks = seq(0,5,0.2))+
    scale_y_continuous(name = "Cena (USD)", breaks = seq(0,140,10))+
    theme(legend.position = c(0.3, 0.8),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5))
return(premica_odvisnost)}


#SKUPINE

cena.earning <- select(yah.mor,Neto_dobicek_na_delnico,Najvisja_cena)
standardizacija <-as.matrix(cena.earning)%>%scale()
D <- dist(standardizacija)
model22 <- hclust(D)
cena.earning<-mutate(cena.earning,skupina=cutree(model22,k=2))


#OPT

opt_pov <- function(prihodnost_dobicek_na_delnico,vrsta_modela){
model_opt <- lm(data=filter(cena.earning,skupina==1),
                Najvisja_cena~Neto_dobicek_na_delnico) 
prihodnost_opt <- data.frame(Neto_dobicek_na_delnico=c(predict(vrsta_modela,prihodnost_dobicek_na_delnico)))
napoved_opt<- prihodnost_opt%>%mutate(Najvisja_cena=predict(model_opt,.))

premica_odvisnost_opt <- ggplot(filter(cena.earning,skupina==1),
                                aes(x=Neto_dobicek_na_delnico,y=Najvisja_cena,
                                    color="Izboljšan približek"))+
  geom_smooth(method = lm,formula = y~x,se=F)+
  geom_point(aes(color="Točne vrdednosti"))+
  geom_point(data = napoved_opt,
             aes(x=Neto_dobicek_na_delnico,y=Najvisja_cena,
                 color="Izboljšana napoved cene"),size=3)+
  scale_colour_manual(name="LEGENDA",values=c("red","green","black"))+
  labs(title="Izboljšana napoved cene preko dobička s pomočjo linearne regresije")+
  scale_x_continuous(name = "Dobiček na delnico (USD)", breaks = seq(0,5,0.2))+
  scale_y_continuous(name = "Cena delnice (USD)", breaks = seq(0,110,5))+
  theme(legend.position = c(0.3, 0.8),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5))
return(premica_odvisnost_opt)}















