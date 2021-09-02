# 4. faza: Analiza podatkov


tabela3[-dv2,] %>% group_by(Drzava)

# komu v povprečju najvec: tabela5$Stevilo_nastopov in tabela

pod9 <- tabela5[-which(tabela5$Stevilo_nastopov + tabela5$Prvi_nastop < 1993),]

# komu smo dali max točke in kdo je zmagal SLO
df1 <- tabela4 %>% group_by(Leto) %>% filter(Tocke == max(Tocke))

pod_zmagovalci2 <- pod_zmagovalci[-which(grepl(1994, pod_zmagovalci$LETO)),]
pod_zmagovalci2 <- pod_zmagovalci2[-which(grepl(2000, pod_zmagovalci2$LETO)),]
pod_zmagovalci2 <- subset(pod_zmagovalci2, LETO > 1992)

df1 <- df1 %>% bind_cols(pod_zmagovalci2$DRZAVA) %>% rename("Zmagovalna" = ...5)
sum(df1$Drzava == df1$Zmagovalna)


# jugosl
df2 <- tabela3 %>% group_by(Leto) %>% filter(Tocke == max(Tocke))
