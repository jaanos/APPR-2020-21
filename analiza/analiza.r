# 4. faza: Analiza podatkov


tabela3[-dv2,] %>% group_by(Drzava)

# komu v povprečju najvec: tabela5$Stevilo_nastopov in tabela

pod9 <- tabela5[-which(tabela5$Stevilo_nastopov + tabela5$Prvi_nastop < 1993),]

# komu smo dali max točke in kdo je zmagal SLO
df1 <- tabela4 %>% group_by(Leto) %>% filter(Tocke == max(Tocke))

pod_zmagovalci2 <- pod_zmagovalci[-which(grepl(1994, pod_zmagovalci$LETO)),]
pod_zmagovalci2 <- pod_zmagovalci2[-which(grepl(2000, pod_zmagovalci2$LETO)),]
pod_zmagovalci2 <- subset(pod_zmagovalci2, LETO > 1992)

df1 <- df1 %>% merge(pod_zmagovalci2, by.x="Leto", by.y="LETO") %>%
  select(-c(NASTOPAJOCI, NASLOV)) %>%
  rename("Zmagovalna" = DRZAVA, "Zmag_tocke" = TOCKE)

# število glasovanj za zmagovalce
sum(df1$Drzava == df1$Zmagovalna)
sort(df1[which(df1$Drzava != df1$Zmagovalna),]$Drzava)

# jugoslavija
df2 <- tabela3 %>% group_by(Leto) %>% filter(Tocke == max(Tocke))

pod_zmagovalci3 <- subset(pod_zmagovalci, LETO < 1993 & LETO > 1960)
pod_zmagovalci3 <- pod_zmagovalci3[-which(grepl(1977, pod_zmagovalci3$LETO)),]
pod_zmagovalci3 <- pod_zmagovalci3[-which(grepl(1978, pod_zmagovalci3$LETO)),]
pod_zmagovalci3 <- pod_zmagovalci3[-which(grepl(1979, pod_zmagovalci3$LETO)),]
pod_zmagovalci3 <- pod_zmagovalci3[-which(grepl(1980, pod_zmagovalci3$LETO)),]
pod_zmagovalci3 <- pod_zmagovalci3[-which(grepl(1985, pod_zmagovalci3$LETO)),]

df2 <- df2 %>% merge(pod_zmagovalci3, by.x="Leto", by.y="LETO") %>%
  select(-c(NASTOPAJOCI, NASLOV)) %>%
  rename("Zmagovalna" = DRZAVA, "Zmag_tocke" = TOCKE)

# število glasovanj za zmagovalce
sum(df2$Drzava == df2$Zmagovalna)

# ali lahko napovemo komu da slovenija max točk?
summary(lm(Vsota ~ Drzava, data = tabela9))

summary(lm(Zmag_tocke ~ Tocke, data=df1))

summary(model6 <- lm(Tocke ~ Drzava, data=subset(tabela4, Drzava != "Slovenia" &
                                                   Drzava != "Morocco" &
                                                   Drzava != "Luxembourg" &
                                                   Drzava != "Yugoslavia" &
                                                   Drzava != "Czech Republic" &
                                                   Drzava != "Slovakia" &
                                                   Drzava != "Monaco" &
                                                   Drzava != "Australia" &
                                                   Drzava != "Andorra" &
                                                   Drzava != "Serbia & Montenegro")))



# za koga glasujemo, ko ne glasujemo pravilno
df1$Drzava[which(df1$Drzava != df1$Zmagovalna)]



# jezik, finale, vrsta zasedbe (Ž,M,SKUPINA) pesmi
graf2

summary(lm(UVRSTITEV ~ TOCKE, data = tabela1))
summary(lm(UVRSTITEV ~ as.numeric(TOCKE), data = df3))

# slovenija uvrstitev, finale, jezik, zasedba
summary(model1 <- lm(Finale ~ Jezik, data=df3))

summary(model2 <- lm(UVRSTITEV ~ Jezik, data=df3))

summary(model3 <- lm(Finale ~ Zasedba, data=df3))

summary(model4 <- lm(UVRSTITEV ~ Zasedba, data=df3))

summary(model5 <- lm(UVRSTITEV ~ Jezik + Zasedba, data=df3))

 