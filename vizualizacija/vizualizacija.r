# 3. faza: Vizualizacija podatkov

# Uvozimo zemljevid.
#zemljevid <- uvozi.zemljevid("http://baza.fmf.uni-lj.si/OB.zip", "OB",
        #                     pot.zemljevida="OB", encoding="Windows-1250")
# Če zemljevid nima nastavljene projekcije, jo ročno določimo
#proj4string(zemljevid) <- CRS("+proj=utm +zone=10+datum=WGS84")

#levels(zemljevid$OB_UIME) <- levels(zemljevid$OB_UIME) %>%
 # { gsub("Slovenskih", "Slov.", .) } %>% { gsub("-", " - ", .) }
#zemljevid$OB_UIME <- factor(zemljevid$OB_UIME, levels=levels(obcine$obcina))

# Izračunamo povprečno velikost družine
#povprecja <- druzine %>% group_by(obcina) %>%
 # summarise(povprecje=sum(velikost.druzine * stevilo.druzin) / sum(stevilo.druzin))

#--------------------
# GRAFA 1 IN 2

graf1 <- ggplot(tabela6, aes(x=LETO, y=NACRPANO.SKUPAJ, group=1)) + 
          geom_point(color="red", size=1) +
          geom_line() +
          ylab("Načrpana voda skupaj v 100.000 * m^3")
print(graf1)

povprecjepod <- mean(tabela6$`PODZEMNE.VODE`)
povprecjenad <- mean(tabela6$`POVRSINSKE.VODE`)
podatki2 <- c(povprecjepod, povprecjenad)
imeni2 <- c("Podzemne vode", "Površinske vode")
delezpod <- (povprecjepod) / (povprecjenad + povprecjepod)
delezpod <- round(delezpod, digits=3) * 100
deleznad <- (povprecjenad) / (povprecjenad + povprecjepod)
deleznad <- round(deleznad, digits=3) * 100
delez <- c(delezpod, deleznad)
zapito <- data.frame(imeni2, podatki2, delez)
zapito <- zapito %>% 
        arrange(desc(imeni2)) %>%
        mutate(lab.ypos = cumsum(delez) - 0.5*delez)
        
        

dvojnistolpci <- ggplot(zadvojnigraf, aes(fill=VRSTA.VODE, x=LETO, y=NACRPANO)) +
                geom_bar(position="dodge", stat="identity")
print(dvojnistolpci)

graf2 <- ggplot(zapito, aes(x="", y=delez, fill=imeni2)) +
        geom_bar(width=1, stat="identity", color="white") +
        coord_polar("y", start=0) + 
        geom_text(aes(y=lab.ypos, label=delez), color="white") +
        theme_void() +
        labs(fill="Vrsta vode")

#--------------------------
# GRAFA 3 IN 4

graf3 <- ggplot(tabela2, aes(x=LETO, y=SLOVENIJA), group=1, asp=2) +
        geom_point(color="red") +
        geom_line(group=1) +
        xlab("Dobavljena voda v m^3 na prebivalca")
print(graf3)

imena4 <- colnames(tabela2[,3:16])
podatki4 <- as.numeric(tabela2[17,3:16])
zapito2 <- data.frame(imena4,podatki4)
zapito2 <- zapito2 %>% mutate(delezi=100*podatki4 / sum(podatki4)) %>%
        arrange(desc(imena4)) %>%
        mutate(lab.ypos=cumsum(delezi) - 0.5*delezi)

graf4 <- ggplot(zapito2, aes(x="", y=delezi, fill=imena4)) +
        geom_bar(width=1, stat="identity", color="white") +
        coord_polar("y", start=0) + 
        geom_text(aes(y=lab.ypos, label=paste0(round(delezi, 2), "%"), x=1.3)) +
        theme_void() +
        labs(title="Porazdelitev dostavljene vode po regijah", fill="Regija")
#-----------------------------
# GRAF 5 
graf5 <- ggplot(odpadkiletnonaprebivalca, aes(x=povprecje, y=REGIJA)) + 
          geom_col(color="Brown", fill="Brown") +
          xlab("Povprečen letni odpad na prebivalca v kg") +
        ylab("Regija") + 
        labs("Povprečen letni odpad na prebivalca")

print(graf5)

#------------
# ZEMLJEVID


zemljevid <- uvozi.zemljevid("http://baza.fmf.uni-lj.si/OB.zip", "OB",
                     pot.zemljevida="OB", encoding="Windows-1250")
proj4string(zemljevid) <- CRS("+proj=utm +zone=10+datum=WGS84")

levels(zemljevid$OB_UIME) <- levels(zemljevid$OB_UIME) %>%
        { gsub("Slovenskih", "Slov.", .) } %>% { gsub("-", " - ", .) }

zemljevid$OB_UIME <- factor(zemljevid$OB_UIME, levels=levels(obcine$obcina))

zemljevid1 <- tm_shape(merge(obcine, tabelazakartoobcine, by.x="OB_UIME", by.y="obcina")) +
                tm_polygons("odpadki19kgnaprebivalca")








