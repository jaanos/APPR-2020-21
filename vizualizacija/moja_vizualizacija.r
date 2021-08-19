# # LE ZA ZGLED KAKO DODAJAMO ČRTE POSTOPOMA
# ggplot(cbind(skupna17[c("round", "goals_scored", "own_goals")] %>% group_by(round) %>%
#                summarise(st_zadetkov_17 = sum(goals_scored + own_goals)),
#              skupna18[c("round", "goals_scored", "own_goals")] %>% group_by(round) %>%
#                summarise(st_zadetkov_18 = sum(goals_scored + own_goals)) %>% select(-round),
#              skupna19[c("round", "goals_scored", "own_goals")] %>% group_by(round) %>%
#                summarise(st_zadetkov_19 = sum(goals_scored + own_goals)) %>% select(-round))) +
#   aes(x=round) + aes(y = st_zadetkov_17, colour = "st_zadetkov_17") + geom_line() + 
#   geom_line(aes(y = st_zadetkov_18, colour = "st_zadetkov_18")) +
#   geom_line(aes(y = st_zadetkov_19, colour = "st_zadetkov_19"))
# 
# 
# 
# 
# goli_po_sezonah <- cbind(skupna17[c("goals_scored", "own_goals","was_home")] %>%
#                            group_by(was_home) %>%
#                            summarise("2016/17"=sum(goals_scored + own_goals)) %>%
#                            rename(Teren=was_home),
#                          skupna18[c("goals_scored", "own_goals","was_home")] %>%
#                            group_by(was_home) %>%
#                            summarise("2017/18"=sum(goals_scored + own_goals)) %>% select(-was_home),
#                          skupna19[c("goals_scored", "own_goals","was_home")] %>%
#                            group_by(was_home) %>%
#                            summarise("2018/19"=sum(goals_scored + own_goals)) %>%
#                            select(-was_home)) %>%
#   pivot_longer(-Teren,names_to="sezona", values_to="stevilo_golov") %>%
#   mutate(Teren=sapply(Teren, function(x) {if (x == TRUE) {x="Doma"} else {x="V gosteh"}}))
# 
# ggplot(goli_po_sezonah) + aes(x=sezona, y=stevilo_golov, fill=Teren) + geom_col(position="dodge") +
#   geom_text(aes(label = stevilo_golov), vjust=1.5, position=position_dodge(0.9)) +
#   ggtitle("Število zadetkov doma in v gosteh") + ylab("Število zadetkov") + xlab("Sezona")
# 
# 
# goli_pozicija <- cbind(kom17[c("goals_scored","element_type")] %>% filter(element_type < 5) %>%
#                          group_by(element_type) %>%
#                          summarise(odstotek_zadetkov17 = sum(goals_scored)/sum(kom17$goals_scored) * 100),
#                        kom18[c("goals_scored","element_type")] %>% filter(element_type < 5) %>%
#                          group_by(element_type) %>% 
#                          summarise(odstotek_zadetkov18 = sum(goals_scored)/sum(kom17$goals_scored) * 100) %>%
#                          select(-element_type),
#                        kom19[c("goals_scored","element_type")] %>% filter(element_type < 5) %>%
#                          group_by(element_type) %>%
#                          summarise(odstotek_zadetkov19 = sum(goals_scored)/sum(kom17$goals_scored) * 100) %>%
#                          select(-element_type)) %>%
#   mutate(odstotek_zadetkov17=round(odstotek_zadetkov17,2),
#          odstotek_zadetkov18=round(odstotek_zadetkov18,2),
#          odstotek_zadetkov19=round(odstotek_zadetkov19,2)) %>% rename(Pozicija = element_type) %>%
#     mutate(Pozicija = c("Vratar", "Branilec", "Vezist", "Napadalec")[Pozicija]) %>%
#   filter(odstotek_zadetkov17 > 0, odstotek_zadetkov18 > 0, odstotek_zadetkov19 > 0) %>%
#   rename("2016/17" = odstotek_zadetkov17, "2017/18" = odstotek_zadetkov18,
#          "2018/19" = odstotek_zadetkov19) %>%
#   pivot_longer(-Pozicija, names_to = "sezona", values_to = "odstotek_zadetkov")
#     
# ggplot(goli_pozicija) + aes(x=sezona, y=odstotek_zadetkov, fill=Pozicija) +
#   geom_col(position = "dodge") +
#   geom_text(aes(label = odstotek_zadetkov), vjust=1.5, position=position_dodge(0.9)) +
#   ggtitle("Odstotek vseh zadetkov razdeljen po igralnih položajih") + ylab("Odstotek zadetkov") + xlab("Sezona")


# SEDAJ NAS ZANIMAJO LE IGRALCI V POLJU Z VEC KOT SEDMIMI CELOTNIMI ODIGRANIMI TEKMAMI
igralci <- kom19 %>% filter(minutes >= 90*7, Pozicija=="Vezist" | Pozicija=="Napadalec" | Pozicija=="Branilec")
#branilci <- kom19 %>% filter(minutes >= 90*10, element_type=="Branilec")
# asistenc na tekmo 0.3379515, uspesnih podaj na tekmo 50.36697 
podaje_asistence <- igralci %>% filter(assists/minutes >= quantile(igralci$assists/igralci$minutes, 2/3, na.rm = TRUE) |
                               completed_passes/minutes >= quantile(igralci$completed_passes/igralci$minutes, 2/3, na.rm=TRUE)) %>%
  mutate(odstotek_uspesnih_podaj = round(completed_passes/attempted_passes * 100, 2), st_asistenc_90 = round(assists/minutes * 90, 2)) %>%
  select(Igralec, st_asistenc_90, odstotek_uspesnih_podaj, attempted_passes, completed_passes, assists, minutes)

graf_podaje_asistence <- ggplotly(ggplot(podaje_asistence) + aes(igralec = Igralec, ekipa = Ekipa, st_asistenc = assists, minute = minutes,
                                           x = st_asistenc_90, y = odstotek_uspesnih_podaj, color = Pozicija) + geom_point() + 
           ggtitle("Primerjava najnatančnejših podajalcev za najboljšimi asistenti") + ylab("Odstotek uspešnih podaj") +
           xlab("Povprečno število asistenc na 90 minut"))


priloznosti_driblingi <- igralci %>%  filter(dribbles/minutes >= quantile(igralci$dribbles/igralci$minutes, 2/3, na.rm = TRUE) |
                                                       big_chances_created/minutes >= quantile(
                                                         igralci$big_chances_created/igralci$minutes, 2/3, na.rm = TRUE)) %>%
  mutate(st_driblingov_90 = round(dribbles/minutes*90, 2), st_velikih_priloznosti_90 = round(big_chances_created/minutes*90, 2)) %>%
  select(Igralec, st_driblingov_90, st_velikih_priloznosti_90, dribbles, big_chances_created, minutes)

graf_priloznosti_driblingi <- ggplotly(ggplot(priloznosti_driblingi) + aes(igralec = Igralec, ekipa = Ekipa, st_driblingov = dribbles,
                                                                           priloznosti = big_chances_created, x = st_velikih_priloznosti_90, minute = minutes,
                                                                           y = st_driblingov_90, color = Pozicija) +
                                         geom_point() + ggtitle("Primerjava driblerjev in kreativnih igralcev") +
                                         ylab("Število driblingov na 90 minut") + xlab("Število ustvarjenih velikih priložnosti na 90 minut"))



driblingi_odvzete_zoge <- igralci %>% filter(dribbles/minutes >= quantile(igralci$dribbles/igralci$minutes, 2/3, na.rm = TRUE) |
                                               tackled/minutes >= quantile(igralci$tackled/igralci$minutes, 2/3, na.rm = TRUE)) %>%
  mutate(odvzete_zoge_90 = round(tackled/minutes*90, 2), st_driblingov_90 = round(dribbles/minutes*90, 2)) %>%
  select(Igralec, odvzete_zoge_90, st_driblingov_90, dribbles, tackled, minutes)

graf_driblingi_odvzete_zoge <- ggplotly(ggplot(driblingi_odvzete_zoge) + aes(igralec = Igralec, ekipa = Ekipa, izguba_zoge = tackled,
                                                                           st_driblingov = dribbles, x = odvzete_zoge_90, minute = minutes,
                                                                           y = st_driblingov_90, color = Pozicija) +
                                         geom_point() + ggtitle("Primerjava driblingov in izgubljenih žog") +
                                         ylab("Število driblingov na 90 minut") + xlab("Število izgubljenih žog na 90 minut"))


blokade_napake <- igralci %>% filter(errors_leading_to_goal_attempt/minutes >=
                                            quantile(igralci$errors_leading_to_goal_attempt/igralci$minutes, 2/3, na.rm = TRUE) |
                                            clearances_blocks_interceptions/minutes >= quantile(igralci$clearances_blocks_interceptions/
                                                                                                  igralci$minutes, 2/3, na.rm = TRUE)) %>%
  mutate(obrambne_akcije_90 = round(clearances_blocks_interceptions/minutes*90,2), napake_za_priloznost_90 =
           round(errors_leading_to_goal_attempt/minutes*90,2)) %>% select(Igralec, napake_za_priloznost_90, obrambne_akcije_90, minutes,
                                                                          errors_leading_to_goal_attempt, clearances_blocks_interceptions)

graf_blokade_napake <- ggplotly(ggplot(blokade_napake) + aes(igralec = Igralec, ekipa = Ekipa,
                                                             izbijanja_blokiranja_prestrezanja = clearances_blocks_interceptions,
                                                             st_napak_za_priloznost = errors_leading_to_goal_attempt,
                                                             x = napake_za_priloznost_90, minute = minutes, y = obrambne_akcije_90, color = Pozicija) +
                                  geom_point() +
                                  ggtitle("Primerjava obrambnih akcij in napak") + ylab("Število obrambnih posredovanj na 90 minut") +
                                  xlab("Število napak, ki vodijo do priložnosti za nasprotnika na 90 minut"))



odvzete_zoge_favli <- igralci %>% filter(tackles/minutes >= quantile(igralci$tackles/igralci$minutes, 2/3, na.rm = TRUE) |
                               fouls/minutes >= quantile(igralci$fouls/igralci$minutes, 2/3, na.rm = TRUE)) %>%
  mutate(odvzete_zoge_90 = round(tackles/minutes*90,2), prekrski_90 = round(fouls/minutes*90,2)) %>%
  select(Igralec, prekrski_90, odvzete_zoge_90, minutes, fouls, tackles)

graf_odvzete_zoge_favli <- ggplotly(ggplot(odvzete_zoge_favli) + aes(igralec = Igralec, ekipa = Ekipa, prekrski = fouls, odvzemanja_zoge = tackles,
                                                                     x = prekrski_90, minute = minutes, y = odvzete_zoge_90, color = Pozicija) +
                                      geom_point() + ggtitle("Primerjava odvzetih žog in prekrškov") + ylab("Število odvzetih žog na 90 minut") +
                                  xlab("Število prekrškov na 90 minut"))

vratarji <- kom19 %>% filter(minutes >= 90*3, Pozicija == "Vratar")

obrambe_clean_sheeti <- vratarji %>% mutate(obrambe_90 = round(saves/minutes*90,2)) %>% select(Igralec, obrambe_90, clean_sheets, minutes, saves)

graf_obrambe_clean_sheeti <- ggplotly(ggplot(obrambe_clean_sheeti) + aes(igralec = Igralec, ekipa = Ekipa, obrambe = saves,
                                                                         tekma_brez_prejetega_zadetka = clean_sheets,
                                                                       x = clean_sheets, minute = minutes, y = obrambe_90) +
                                        geom_point() + ggtitle("Tekme brez prejetega zadetka in obrambe") + ylab("Število obranjenih strelov na 90 minut") +
                                        xlab("Število tekem brez prejetega gola"))


print(graf_podaje_asistence)
print(graf_priloznosti_driblingi)
print(graf_driblingi_odvzete_zoge)
print(graf_blokade_napake)
print(graf_odvzete_zoge_favli)
print(graf_obrambe_clean_sheeti)
#DODANO ŠE FILTRIRANJE PO ODSTOTKU NATANCNOSTI
# & completed_passes/attempted_passes >= quantile(tabela_vezistov$completed_passes/tabela_vezistov$attempted_passes, 0.75, na.rm = TRUE))
