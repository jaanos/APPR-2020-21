# LE ZA ZGLED KAKO DODAJAMO ČRTE POSTOPOMA
ggplot(cbind(skupna17[c("round", "goals_scored", "own_goals")] %>% group_by(round) %>%
               summarise(st_zadetkov_17 = sum(goals_scored + own_goals)),
             skupna18[c("round", "goals_scored", "own_goals")] %>% group_by(round) %>%
               summarise(st_zadetkov_18 = sum(goals_scored + own_goals)) %>% select(-round),
             skupna19[c("round", "goals_scored", "own_goals")] %>% group_by(round) %>%
               summarise(st_zadetkov_19 = sum(goals_scored + own_goals)) %>% select(-round))) +
  aes(x=round) + aes(y = st_zadetkov_17, colour = "st_zadetkov_17") + geom_line() + 
  geom_line(aes(y = st_zadetkov_18, colour = "st_zadetkov_18")) +
  geom_line(aes(y = st_zadetkov_19, colour = "st_zadetkov_19"))




goli_po_sezonah <- cbind(skupna17[c("goals_scored", "own_goals","was_home")] %>%
                           group_by(was_home) %>%
                           summarise("2016/17"=sum(goals_scored + own_goals)) %>%
                           rename(Teren=was_home),
                         skupna18[c("goals_scored", "own_goals","was_home")] %>%
                           group_by(was_home) %>%
                           summarise("2017/18"=sum(goals_scored + own_goals)) %>% select(-was_home),
                         skupna19[c("goals_scored", "own_goals","was_home")] %>%
                           group_by(was_home) %>%
                           summarise("2018/19"=sum(goals_scored + own_goals)) %>%
                           select(-was_home)) %>%
  pivot_longer(-Teren,names_to="sezona", values_to="stevilo_golov") %>%
  mutate(Teren=sapply(Teren, function(x) {if (x == TRUE) {x="Doma"} else {x="V gosteh"}}))

ggplot(goli_po_sezonah) + aes(x=sezona, y=stevilo_golov, fill=Teren) + geom_col(position="dodge") +
  geom_text(aes(label = stevilo_golov), vjust=1.5, position=position_dodge(0.9)) +
  ggtitle("Število zadetkov doma in v gosteh") + ylab("Število zadetkov") + xlab("Sezona")


goli_pozicija <- cbind(kom17[c("goals_scored","element_type")] %>% filter(element_type < 5) %>%
                         group_by(element_type) %>%
                         summarise(odstotek_zadetkov17 = sum(goals_scored)/sum(kom17$goals_scored) * 100),
                       kom18[c("goals_scored","element_type")] %>% filter(element_type < 5) %>%
                         group_by(element_type) %>% 
                         summarise(odstotek_zadetkov18 = sum(goals_scored)/sum(kom17$goals_scored) * 100) %>%
                         select(-element_type),
                       kom19[c("goals_scored","element_type")] %>% filter(element_type < 5) %>%
                         group_by(element_type) %>%
                         summarise(odstotek_zadetkov19 = sum(goals_scored)/sum(kom17$goals_scored) * 100) %>%
                         select(-element_type)) %>%
  mutate(odstotek_zadetkov17=round(odstotek_zadetkov17,2),
         odstotek_zadetkov18=round(odstotek_zadetkov18,2),
         odstotek_zadetkov19=round(odstotek_zadetkov19,2)) %>% rename(Pozicija = element_type) %>%
    mutate(Pozicija = c("Vratar", "Branilec", "Vezist", "Napadalec")[Pozicija]) %>%
  filter(odstotek_zadetkov17 > 0, odstotek_zadetkov18 > 0, odstotek_zadetkov19 > 0) %>%
  rename("2016/17" = odstotek_zadetkov17, "2017/18" = odstotek_zadetkov18,
         "2018/19" = odstotek_zadetkov19) %>%
  pivot_longer(-Pozicija, names_to = "sezona", values_to = "odstotek_zadetkov")
    
ggplot(goli_pozicija) + aes(x=sezona, y=odstotek_zadetkov, fill=Pozicija) +
  geom_col(position = "dodge") +
  geom_text(aes(label = odstotek_zadetkov), vjust=1.5, position=position_dodge(0.9)) +
  ggtitle("Odstotek vseh zadetkov razdeljen po igralnih položajih") + ylab("Odstotek zadetkov") + xlab("Sezona")


# SEDAJ NAS ZANIMAJO LE VEZISTI IN NAPADALCI Z VEC KOT 900 ODIGRANIMI MINUTAMI (~10 celih tekem)
napadalci_vezisti <- kom19 %>% filter(minutes >= 90*10, element_type=="Vezist" | element_type=="Napadalec")
branilci <- kom19 %>% filter(minutes >= 90*10, element_type=="Branilec")
# asistenc na tekmo 0.3379515, uspesnih podaj na tekmo 50.36697 
uspesnost_asistence <- napadalci_vezisti %>% filter(assists/minutes >= quantile(napadalci_vezisti$assists/napadalci_vezisti$minutes, 2/3, na.rm = TRUE) |
                               completed_passes/minutes >= quantile(napadalci_vezisti$completed_passes/napadalci_vezisti$minutes, 2/3, na.rm=TRUE)) %>%
  mutate(odstotek_uspesnih_podaj = round(completed_passes/attempted_passes * 100, 2), st_asistenc_90 = round(assists/minutes * 90, 2)) %>%
  select(web_name, st_asistenc_90, odstotek_uspesnih_podaj, attempted_passes, completed_passes, assists)

ggplotly(ggplot(uspesnost_asistence) + aes(ime_igralca = web_name, ekipa = team, st_asistenc = assists,
                                           x = st_asistenc_90, y = odstotek_uspesnih_podaj, color = element_type) + geom_point() + 
           ggtitle("Primerjava najnatančnejših podajalcev za najboljšimi asistenti") + ylab("Odstotek uspešnih podaj") +
           xlab("Povprečno število asistenc na tekmo"))


priloznosti_driblingi <- napadalci_vezisti %>%  filter(dribbles/minutes >= quantile(napadalci_vezisti$dribbles/napadalci_vezisti$minutes, 0.85, na.rm = TRUE) |
                                                       big_chances_created/minutes >= quantile(
                                                         napadalci_vezisti$big_chances_created/napadalci_vezisti$minutes, 0.85, na.rm = TRUE)) %>%
  mutate(st_driblingov_90 = round(dribbles/minutes*90, 2), st_velikih_priloznosti_90 = round(big_chances_created/minutes*90, 2)) %>%
  select(web_name, st_driblingov_90, st_velikih_priloznosti_90, dribbles, big_chances_created, minutes)

driblingi_odvzete_zoge <- napadalci_vezisti %>% filter(dribbles/minutes >= quantile(napadalci_vezisti$dribbles/napadalci_vezisti$minutes, 0.85, na.rm = TRUE) |
                                               tackled/minutes >= quantile(napadalci_vezisti$tackled/napadalci_vezisti$minutes, 0.85, na.rm = TRUE)) %>%
  mutate(odvzete_zoge_90 = round(tackled/minutes*90, 2), st_driblingov_90 = round(dribbles/minutes*90, 2)) %>%
  select(web_name, odvzete_zoge_90, st_driblingov_90, dribbles, tackled, minutes)

blokade_napake <- branilci %>% filter(errors_leading_to_goal_attempt/minutes >=
                                            quantile(branilci$errors_leading_to_goal_attempt/branilci$minutes, 0.85, na.rm = TRUE) |
                                            clearances_blocks_interceptions/minutes >= quantile(branilci$clearances_blocks_interceptions/
                                                                                                  branilci$minutes, 0.85, na.rm = TRUE)) %>%
  mutate(obrambne_akcije_90 = round(clearances_blocks_interceptions/minutes*90,2), napake_za_priloznost =
           round(errors_leading_to_goal_attempt/minutes*90,2)) %>% select(web_name, napake_za_priloznost, obrambne_akcije_90, minutes,
                                                                          errors_leading_to_goal_attempt, clearances_blocks_interceptions)

odvzete_zoge_favli <- branilci %>% filter(tackles/minutes >= quantile(branilci$tackles/branilci$minutes, 0.85, na.rm = TRUE) |
                               fouls/minutes >= quantile(branilci$fouls/branilci$minutes, 0.85, na.rm = TRUE)) %>%
  mutate(odvzete_zoge_90 = round(tackles/minutes*90,2), prekrski_90 = round(fouls/minutes*90,2)) %>%
  select(web_name, prekrski_90, odvzete_zoge_90, minutes, fouls, tackles)


#DODANO ŠE FILTRIRANJE PO ODSTOTKU NATANCNOSTI
# & completed_passes/attempted_passes >= quantile(tabela_vezistov$completed_passes/tabela_vezistov$attempted_passes, 0.75, na.rm = TRUE))
